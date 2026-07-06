library(sf)
library(gtfstools)
library(dplyr)
library(GTFShift)
library(mapview)

METRIC_CRS = 3763 # ETRS89 / Portugal TM06


GTFS_FEED_URL = "../GTFShift-web/scripts/osm_gtfs/aml_barreiro_cascais_lisboa/run_20260521_165353/gtfs_barreiro.zip"
OSM_SHAPES = "../GTFShift-web/scripts/osm_match/barreiro/gtfs_20260518/run_20260518_123051/shapes_match_barreiro_gtfs20260518_run20260518.gpkg"
SHAPE_ID = "1-VA-TERM"

GTFS_FEED_URL = "https://github.com/U-Shift/busclar/releases/download/0.9/gtfs_carris_metropolitana.zip"
OSM_SHAPES = "https://github.com/U-Shift/busclar/releases/download/0.9/shapes_match_carris_metropolitana_gtfs20260527_run20260626.gpkg"
SHAPE_ID = "3526_1_1"

gtfs <- load_feed(GTFS_FEED_URL)
sf_shapes_original <- tidytransit::shapes_as_sf(gtfs$shapes)

gtfs$trips$shape_id <- gsub("^\\[[^]]*\\]\\s*", "", gtfs$trips$shape_id)
trip_id <- gtfs$trips |> filter(shape_id == SHAPE_ID) |> slice(1) |> pull(trip_id)
gtfs_trip <- tidytransit::filter_feed_by_trips(gtfs, trip_ids = trip_id)
summary(gtfs)

sf_shapes_original$shape_id <- gsub("^\\[[^]]*\\]\\s*", "", sf_shapes_original$shape_id)
# mapview(sf_shapes_original |> filter(shape_id==SHAPE_ID))

sf_shapes <- sf::st_read(OSM_SHAPES)
summary(sf_shapes)
mapview(sf_shapes |> filter(shape_id==SHAPE_ID))


# To debug GTFShift::multiline_to_sorted_linestring()
multilinestring = (sf_shapes |> filter(shape_id==SHAPE_ID)) |> pull(geom)
start_point = gtfs_trip$stop_times |> arrange(stop_sequence) |> slice(1) |> left_join(gtfs_trip$trips, by="trip_id") |> left_join(gtfs_trip$stops, by="stop_id") |> st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |> pull(geometry)
mapview(multilinestring) + mapview(start_point)
metric_crs = METRIC_CRS



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
  