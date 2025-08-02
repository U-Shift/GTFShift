library(GTFShift)

output_barreiro = "releases/v0_7_0/shapes_match_barreiro_gtfs20250802_run20250802"

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_barreiro = GTFShift::load_feed(data$URL[data$ID == "barreiro"], create_transfers=FALSE)
tidytransit::write_gtfs(gtfs_barreiro, "releases/v0_7_0/gtfs_barreiro_20250802.zip")
total_network_barreiro = length(unique(gtfs_barreiro$routes$route_short_name))
total_network_barreiro # 20

gtfs_barreiro_shapes = tidytransit::shapes_as_sf(gtfs_barreiro$shapes)
bbox_barreiro = sf::st_bbox(gtfs_barreiro_shapes)

# Build OSM query
library(osmdata)
q_barreiro = opq(bbox_barreiro)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "network", value = "Transportes Coletivos do Barreiro", key_exact = TRUE)

# Match shapes geometry
shapes_barreiro_match_routes = GTFShift::osm_shapes_match_routes(gtfs_barreiro, q_barreiro, log_file = sprintf("%s.r.log", output_barreiro))
summary(shapes_barreiro_match_routes)
# View(shapes_barreiro_match_routes |> sf::st_drop_geometry())

total_shapes = length(unique(gtfs_barreiro$shapes$shape_id))
total_shapes # 75

matches_shapes = nrow(shapes_barreiro_match_routes)
matches_shapes # 11
matches_shapes/total_shapes*100 # 14.6

valid_shapes = nrow(shapes_barreiro_match_routes |> filter(distance_diff<1000 & points_diff<500))
valid_shapes # 11
valid_shapes/total_shapes*100 # 14.6

total_routes = length(unique(gtfs_barreiro$routes$route_id))
total_routes # 20

matches_routes = length(unique(shapes_barreiro_match_routes$route_id))
matches_routes # 5
matches_routes/total_routes*100 # 25.0

# View(shapes_barreiro_match_routes |> sf::st_drop_geometry())
write.csv(shapes_barreiro_match_routes |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff)
), sprintf("%s.csv", output_barreiro), row.names = FALSE)
sf::st_write(shapes_barreiro_match_routes, sprintf("%s.gpkg", output_barreiro))
