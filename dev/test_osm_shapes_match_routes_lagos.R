library(GTFShift)

output_lagos = "releases/v0_7_0/shapes_match_lagos_gtfs20250802_run20250802"

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_lagos = GTFShift::load_feed(data$URL[data$ID == "lagos"], create_transfers=FALSE)
tidytransit::write_gtfs(gtfs_lagos, "releases/v0_7_0/gtfs_lagos_20250802.zip")
total_network_lagos = length(unique(gtfs_lagos$routes$route_short_name))
total_network_lagos # 10

gtfs_lagos_shapes = tidytransit::shapes_as_sf(gtfs_lagos$shapes)
bbox_lagos = sf::st_bbox(gtfs_lagos_shapes)

# Build OSM query
library(osmdata)
q_lagos = opq(bbox_lagos)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "operator", value = "ONDA", key_exact = TRUE)

# Match shapes geometry
shapes_lagos_match_routes = GTFShift::osm_shapes_match_routes(gtfs_lagos, q_lagos, log_file = sprintf("%s.r.log", output_lagos))
# TODO! Debug error!
summary(shapes_lagos_match_routes)
# View(shapes_lagos_match_routes |> sf::st_drop_geometry())

total_shapes = length(unique(gtfs_lagos$shapes$shape_id))
total_shapes #

matches_shapes = nrow(shapes_lagos_match_routes)
matches_shapes #
matches_shapes/total_shapes*100 #

valid_shapes = nrow(shapes_lagos_match_routes |> filter(distance_diff<1000 & points_diff<500))
valid_shapes #
valid_shapes/total_shapes*100 #

total_routes = length(unique(gtfs_lagos$routes$route_id))
total_routes #

matches_routes = length(unique(shapes_lagos_match_routes$route_id))
matches_routes #
matches_routes/total_routes*100 #

# View(shapes_lagos_match_routes |> sf::st_drop_geometry())
write.csv(shapes_lagos_match_routes |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff)
), sprintf("%s.csv", output_lagos), row.names = FALSE)
sf::st_write(shapes_lagos_match_routes, sprintf("%s.gpkg", output_lagos))
