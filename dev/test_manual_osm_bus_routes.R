library(osmdata)
library(dplyr)
library(GTFShift)

# RENNES, FR
gtfs = GTFShift::load_feed("https://eu.ftp.opendatasoft.com/star/gtfs/GTFS_2_20250621_20250629_20250610143116.zip", create_transfers=FALSE)
summary(gtfs)

q = opq("Rennes")  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "network", value = "FR:STAR", key_exact = TRUE)

shapes_match_routes = GTFShift::osm_shapes_match_routes(gtfs, q, gtfs_match="route_id", osm_match = "gtfs:route_id")
# Found 515 GTFS shapes and 1806 stops...
# Found 179 OSM route relations and 2279 bus stops/platforms...
# OSM is really incomplete!!
View(shapes_match_routes)

result = shapes_match_routes |> sf::st_drop_geometry()
nrow(result) # 314
nrow(result |> filter(distance_diff<500 & points_diff<100)) # 92
View(result |> filter(distance_diff<500 & points_diff<100))
# Validated! Manually checked some matches and all of them were correct

shapes_geometry_osm = GTFShift::osm_shapes_to_routes(gtfs, q)
nrow(shapes_geometry_osm) # 172
