library(GTFShift)

output_madrid = "releases/v0_7_0/shapes_match_madrid_gtfs20250802_run20250802"

gtfs_madrid = GTFShift::load_feed("http://servicios.emtmadrid.es:8080/GTFS/transitEMT.zip", create_transfers=FALSE)
tidytransit::write_gtfs(gtfs_madrid, "releases/v0_7_0/gtfs_madrid_20250802.zip")
total_network_madrid = length(unique(gtfs_madrid$routes$route_short_name))
total_network_madrid # 237

gtfs_madrid_shapes = tidytransit::shapes_as_sf(gtfs_madrid$shapes)
bbox_madrid = sf::st_bbox(gtfs_madrid_shapes)

# Build OSM query
library(osmdata)
q_madrid = opq(bbox_madrid)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "operator", value = "Empresa Municipal de Transportes de Madrid", key_exact = TRUE)

# Match shapes geometry
shapes_madrid_match_routes = GTFShift::osm_shapes_match_routes(gtfs_madrid, q_madrid, log_file = sprintf("%s.r.log", output_madrid))
summary(shapes_madrid_match_routes)
# View(shapes_madrid_match_routes |> sf::st_drop_geometry())

total_shapes = length(unique(gtfs_madrid$shapes$shape_id))
total_shapes #

matches_shapes = nrow(shapes_madrid_match_routes)
matches_shapes #
matches_shapes/total_shapes*100 #

valid_shapes = nrow(shapes_madrid_match_routes |> filter(distance_diff<1000 & points_diff<500))
valid_shapes #
valid_shapes/total_shapes*100 #

total_routes = length(unique(gtfs_madrid$routes$route_id))
total_routes #

matches_routes = length(unique(shapes_madrid_match_routes$route_id))
matches_routes #
matches_routes/total_routes*100 #
# View(shapes_madrid_match_routes |> sf::st_drop_geometry())
write.csv(shapes_madrid_match_routes |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff)
), sprintf("%s.csv", output_madrid), row.names = FALSE)
sf::st_write(shapes_madrid_match_routes, sprintf("%s.gpkg", output_madrid))
