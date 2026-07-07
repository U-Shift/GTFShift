library(dplyr)
library(sf)
library(mapview)

METRIC_CRS = 3763 # ETRS89 / Portugal TM06

# TCB. Barreiro
GTFS_RT_SAMPLE = "https://github.com/U-Shift/GTFShift-web/releases/download/v1.1/updates_case_study_tcb_20260513_DUPE_7-PTABB-TERM_0_DUPE_22_0925.csv"
GTFS_FEED_URL = "https://github.com/U-Shift/busclar/releases/download/0.9/gtfs_tcb.zip"
OSM_SHAPES = "https://github.com/U-Shift/busclar/releases/download/0.9/shapes_match_tcb_gtfs20260527_run20260626.gpkg"
TRIP_ID_UPDATES = "20260513_DUPE_7-PTABB-TERM_0_DUPE_22_0925"
TRIP_ID_GTFS = "7-PTABB-TERM_0_DUPE_22_0925"

# Carris 
GTFS_RT_SAMPLE = "https://github.com/U-Shift/GTFShift-web/releases/download/v1.1/updates_case_study_carris_20260428_5977_20260101_108_3_2.csv"
TRIP_ID_UPDATES = "20260428_5977_20260101_108_3_2"
GTFS_FEED_URL = "https://github.com/U-Shift/busclar/releases/download/0.9/gtfs_carris.zip"
OSM_SHAPES = "https://github.com/U-Shift/busclar/releases/download/0.9/shapes_match_carris_gtfs20260527_run20260626.gpkg"
TRIP_ID_GTFS = "5977_20260101_108_3_2"

# Carris Metropolitana
GTFS_RT_SAMPLE = "https://github.com/U-Shift/GTFShift-web/releases/download/v1.1/updates_case_study_cmet_20260413_.KFULM.3526_1_1_0930_0959_0_ESC_DU.csv"
GTFS_FEED_URL = "https://github.com/U-Shift/busclar/releases/download/0.9/gtfs_carris_metropolitana.zip"
OSM_SHAPES = "https://github.com/U-Shift/busclar/releases/download/0.9/shapes_match_carris_metropolitana_gtfs20260527_run20260626.gpkg"
TRIP_ID_UPDATES = "20260413_[KFULM]3526_1_1_0930_0959_0_ESC_DU"
TRIP_ID_GTFS = "3526_1_1_0930_0959_0_ESC_DU"

gtfs = tidytransit::read_gtfs(GTFS_FEED_URL)
trip_info = gtfs$trips |> filter(grepl(TRIP_ID_GTFS, trip_id))
trip_shape_id = trip_info$shape_id[[1]]
trip_shape_id <- gsub("^\\[[^]]*\\]\\s*", "", trip_shape_id)
if (is.na(trip_shape_id)) {
  stop("No shape_id found for trip_id: ", TRIP_ID_GTFS)
}
gtfs_shapes_sf = tidytransit::shapes_as_sf(gtfs$shapes)

osm_shapes = sf::st_read(OSM_SHAPES)
osm_shapes_trip <- osm_shapes |> filter(shape_id == trip_shape_id) |> mutate(trip_id = TRIP_ID_UPDATES)
start_point = gtfs$stops |> filter(
    stop_id == (gtfs$stop_times |> 
    filter(grepl(TRIP_ID_GTFS, trip_id)) |> arrange(stop_sequence) |> slice(1) |> 
    select(stop_id) |> pull(stop_id)
  )) |> st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
osm_shapes_trip_linestring = multiline_to_sorted_linestring(
  osm_shapes_trip,
  start_point = start_point$geometry,
  metric_crs = METRIC_CRS
)
mapview(osm_shapes_trip_linestring)
osm_shapes_trip$geom = osm_shapes_trip_linestring

rt_collection = read.csv(GTFS_RT_SAMPLE) |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
trip_df = rt_collection
points = trip_df
# trips_geometries = gtfs_shapes_sf |> filter(shape_id == trip_shape_id) |> mutate(trip_id = TRIP_ID_UPDATES) 
trips_geometries = osm_shapes_trip |> filter(shape_id == trip_shape_id) |> mutate(trip_id = TRIP_ID_UPDATES) 
geometry_sample_meters = 10
metric_crs = METRIC_CRS

mapview(trips_geometries)
