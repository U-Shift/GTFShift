library(GTFShift)
library(sf)
library(gtfstools)
library(dplyr)
library(mapview)

METRIC_CRS = 3763 # ETRS89 / Portugal TM06

GTFS_FEED_URL = "https://github.com/U-Shift/busclar/releases/download/0.9/gtfs_tcb.zip"
OSM_SHAPES = "https://github.com/U-Shift/busclar/releases/download/0.9/shapes_match_tcb_gtfs20260527_run20260626.gpkg"
SHAPE_ID = "1-VA-TERM"

GTFS_FEED_URL = "https://github.com/U-Shift/busclar/releases/download/0.9/gtfs_carris_metropolitana.zip"
OSM_SHAPES = "https://github.com/U-Shift/busclar/releases/download/0.9/shapes_match_carris_metropolitana_gtfs20260527_run20260626.gpkg"
SHAPE_ID = "3526_1_1"

GTFS_FEED_URL = "https://github.com/U-Shift/busclar/releases/download/0.9/gtfs_carris.zip"
OSM_SHAPES = "https://github.com/U-Shift/busclar/releases/download/0.9/shapes_match_carris_gtfs20260527_run20260626.gpkg"
SHAPE_ID = "226_0_CIRC_shp" # Circular
SHAPE_ID = "109_3_ASC_shp" # 751
SHAPE_ID = "221_0_CIRC_shp" # 79B
SHAPE_ID = "163_1_ASC_shp" # 797 (circular sub-path)
SHAPE_ID = "190_0_DESC_shp" # 751

gtfs <- GTFShift::load_feed(GTFS_FEED_URL)
summary(gtfs)
sf_shapes_original <- tidytransit::shapes_as_sf(gtfs$shapes)

gtfs$trips$shape_id <- gsub("^\\[[^]]*\\]\\s*", "", gtfs$trips$shape_id)

trip_id <- gtfs$trips |> filter(shape_id == SHAPE_ID) |> slice(1) |> pull(trip_id)
gtfs_trip <- tidytransit::filter_feed_by_trips(gtfs, trip_ids = trip_id)
summary(gtfs_trip)

sf_shapes_original$shape_id <- gsub("^\\[[^]]*\\]\\s*", "", sf_shapes_original$shape_id)
mapview(sf_shapes_original |> filter(shape_id==SHAPE_ID))

sf_shapes <- sf::st_read(OSM_SHAPES)
summary(sf_shapes)
mapview(sf_shapes |> filter(shape_id==SHAPE_ID))


# To debug GTFShift::multiline_to_sorted_linestring()
multilinestring = (sf_shapes |> filter(shape_id==SHAPE_ID)) |> pull(geom)
start_point = gtfs_trip$stop_times |> arrange(stop_sequence) |> slice(1) |> left_join(gtfs_trip$trips, by="trip_id") |> left_join(gtfs_trip$stops, by="stop_id") |> st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |> pull(geometry)
points = gtfs_trip$stop_times |> arrange(stop_sequence) |> left_join(gtfs_trip$trips, by="trip_id") |> left_join(gtfs_trip$stops, by="stop_id") |> st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |> pull(geometry)
mapview(st_as_sf(data.frame(geometry = points) |> mutate(nrow = row_number())), zcol="nrow") +
mapview(multilinestring) + mapview(start_point, col.regions="green")
metric_crs = METRIC_CRS

# > All points
result <- multiline_to_sorted_linestring(multilinestring, points = points, metric_crs = metric_crs)
# > No points
result <- multiline_to_sorted_linestring(multilinestring, points = NULL, metric_crs = metric_crs)
# > Only start points
result <- multiline_to_sorted_linestring(multilinestring, points = points[c(1, 2)], metric_crs = metric_crs)

mapview(result, color="black", lwd=3) + mapview(start_point, col.regions="green") + mapview(sf_shapes |> filter(shape_id==SHAPE_ID), color="orange", alpha=0.5, lwd=10) 

line_len_m = st_length(result |> st_transform(metric_crs)) |> as.numeric()
result_sampled <- sf::st_line_sample(result |> st_transform(metric_crs), density = 1 / 10)
result_sampled_points <- sf::st_cast(result_sampled, "POINT")
cumdist_m <- seq(0, line_len_m, length.out = length(result_sampled_points))
cumdist_m_reversed <- rev(cumdist_m)
result_sampled_points_df <- sf::st_sf(geometry = result_sampled_points, cumdist_m = cumdist_m, cumdist_m_reversed = cumdist_m_reversed)
mapview(result_sampled_points_df, zcol = "cumdist_m", layer.name = "Cumulative Distance (m)", legend = TRUE)


# Debug maps
# During start point definition
mapview(current_line, color="red") + mapview(current_start, col.regions="pink") + mapview(current_end, col.regions="gray") 
# After start_point definition
mapview(start_point, col.regions = "gray") + mapview(current_line) + mapview(current_start, col.regions="pink") + mapview(current_end, col.regions="gray")
# After ordered_lines[[1]] definition
mapview(linestrings, layer.name="OSM original route relation", homebutton=FALSE, color="#440154") + mapview(ordered_lines[[1]], color = "red", homebutton=FALSE) + mapview(start_point, col.regions = "gray", homebutton=FALSE)
# After last_point definition (inside while loop)
mapview(linestrings) + mapview(ordered_lines, color="yellow") + mapview(current_line, color="red") + mapview(last_point, color="blue")
# After candidate_df definition (inside while loop)
mapview(ordered_lines, color="gray", layer.name="Ordered lines", homebutton=FALSE) +
  mapview(remaining_lines, color="yellow", layer.name="Remaining segments", homebutton=FALSE) +
  mapview(current_line, color="red", layer.name="Current segment", homebutton=FALSE) + 
  mapview(last_point, col.regions="orange", layer.name="Last Point", homebutton=FALSE) +
  mapview(remaining_lines[candidate_df$idx, ], color="blue", layer.name="Candidate segments", homebutton=FALSE)
# After nearest_idx definition (inside while loop)
mapview(ordered_lines, color="gray", layer.name="Ordered lines", homebutton=FALSE) +
  mapview(remaining_lines, color="yellow", layer.name="Remaining segments", homebutton=FALSE) +
  mapview(current_line, color="red", layer.name="Current segment", homebutton=FALSE) + 
  mapview(remaining_lines[nearest_idx, ], color="blue", layer.name="Selected next segment", homebutton=FALSE) + 
  mapview(last_point, col.regions="orange", layer.name="Last Point", homebutton=FALSE) +
  mapview(next_point, col.regions="pink", layer.name="Next Point", homebutton=FALSE) +
  mapview(points_df, zcol="visited", layer.name="Stops", homebutton=FALSE)
mapview(ordered_lines, color="yellow", layer.name="Ordered lines", homebutton=FALSE)
# After all_coords definition (after while loop)
combined_sfc <- do.call(c, ordered_lines)
line_df <- st_sf(geometry = combined_sfc) |> mutate(order = row_number())
mapview(start_point, col.regions="gray") + mapview(line_df, zcol = "order") + mapview(points_df, zcol = "visited")

# To debug GTFShift:create_shapes_from_sf()
gtfs_osm_shapes <- create_shapes_from_sf(
  sf_shapes = sf_shapes |> filter(shape_id==SHAPE_ID),
  gtfs = gtfs_trip,
  metric_crs = METRIC_CRS,
  shape_dist_traveled = TRUE
)
mapview(gtfs_osm_shapes |> filter(shape_id==SHAPE_ID) |> st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326), zcol="shape_dist_traveled")
gtfs_osm_shapes_sf <- tidytransit::shapes_as_sf(gtfs_osm_shapes)
mapview(gtfs_osm_shapes_sf, zcol="shape_id")
gtfs_osm_shapes <- create_shapes_from_sf(
  sf_shapes = sf_shapes |> filter(shape_id %in% c("15-CASQ-TERM", "1-VA-TERM")),
  gtfs = gtfs,
  metric_crs = METRIC_CRS,
  shape_dist_traveled = TRUE
)
gtfs_osm_shapes
View(gtfs_osm_shapes)


gtfs_osm <- load_feed("../GTFShift-web/scripts/osm_gtfs/aml_barreiro_cascais_lisboa/run_20260526_102424/gtfs_cascais_osm.zip")
summary(gtfs_osm)
sf_shapes_osm <- tidytransit::shapes_as_sf(gtfs_osm$shapes)

mapview::mapview(sf_shapes_osm, zcol = "shape_id")

shapes_gtfstools
shapes_gtfstools_sf <- tidytransit::shapes_as_sf(shapes_gtfstools)
shapes_gtfstools_sf
mapview(shapes_gtfstools_sf, zcol="shape_id")

bbox <- sf::st_bbox(sf_shapes_original <- tidytransit::shapes_as_sf(gtfs$shapes))
mapview::mapview(bbox)
library(osmdata)
q <- opq(bbox = bbox) |>
  add_osm_feature(key = "route", value = "bus", key_exact = TRUE) |>
  add_osm_feature(key = "operator", value = c("TCB", "Transportes Colectivos do Barreiro"), key_exact = TRUE)
  