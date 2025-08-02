library(GTFShift)

output_braga = "releases/v0_7_0/shapes_match_braga_gtfs20250802_run20250802"

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_braga = GTFShift::load_feed(data$URL[data$ID == "braga"], create_transfers=FALSE)
tidytransit::write_gtfs(gtfs_braga, "releases/v0_7_0/gtfs_braga_20250802.zip")
total_network_braga = length(unique(gtfs_braga$routes$route_short_name))
total_network_braga # 78

gtfs_braga_shapes = tidytransit::shapes_as_sf(gtfs_braga$shapes)
bbox_braga = sf::st_bbox(gtfs_braga_shapes)

# Build OSM query
library(osmdata)
q_braga = opq(bbox_braga)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "operator", value = "Transportes Urbanos de Braga", key_exact = TRUE)

# Match shapes geometry
shapes_braga_match_routes = GTFShift::osm_shapes_match_routes(gtfs_braga, q_braga, log_file = sprintf("%s.r.log", output_braga))
# TODO! Debug error!
summary(shapes_braga_match_routes)
# View(shapes_braga_match_routes |> sf::st_drop_geometry())

total_shapes = length(unique(gtfs_braga$shapes$shape_id))
total_shapes #

matches_shapes = nrow(shapes_braga_match_routes)
matches_shapes #
matches_shapes/total_shapes*100 #

valid_shapes = nrow(shapes_braga_match_routes |> filter(distance_diff<1000 & points_diff<500))
valid_shapes #
valid_shapes/total_shapes*100 #

total_routes = length(unique(gtfs_braga$routes$route_id))
total_routes #

matches_routes = length(unique(shapes_braga_match_routes$route_id))
matches_routes #
matches_routes/total_routes*100 #

# View(shapes_braga_match_routes |> sf::st_drop_geometry())
write.csv(shapes_braga_match_routes |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff)
), sprintf("%s.csv", output_braga), row.names = FALSE)
sf::st_write(shapes_braga_match_routes, sprintf("%s.gpkg", output_braga))
