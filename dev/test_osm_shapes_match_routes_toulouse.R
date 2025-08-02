library(GTFShift)

output_toulouse = "releases/v0_7_0/shapes_match_toulouse_gtfs20250802_run20250802"

gtfs_toulouse = GTFShift::load_feed("https://data.toulouse-metropole.fr/explore/dataset/tisseo-gtfs/files/fc1dda89077cf37e4f7521760e0ef4e9/download/", create_transfers=FALSE)
tidytransit::write_gtfs(gtfs_toulouse, "releases/v0_7_0/gtfs_toulouse_20250802.zip")
total_network_toulouse = length(unique(gtfs_toulouse$routes$route_short_name))
total_network_toulouse # 120

gtfs_toulouse_shapes = tidytransit::shapes_as_sf(gtfs_toulouse$shapes)
bbox_toulouse = sf::st_bbox(gtfs_toulouse_shapes)

# Build OSM query
library(osmdata)
q_toulouse = opq(bbox_toulouse)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "operator", value = "Tisséo", key_exact = TRUE)

# Match shapes geometry
shapes_toulouse_match_routes = GTFShift::osm_shapes_match_routes(gtfs_toulouse, q_toulouse, log_file = sprintf("%s.r.log", output_toulouse))
summary(shapes_toulouse_match_routes)
# View(shapes_toulouse_match_routes |> sf::st_drop_geometry())

total_shapes = length(unique(gtfs_toulouse$shapes$shape_id))
total_shapes # 275

matches_shapes = nrow(shapes_toulouse_match_routes)
matches_shapes # 182
matches_shapes/total_shapes*100 # 66.18

valid_shapes = nrow(shapes_toulouse_match_routes |> filter(distance_diff<1000 & points_diff<500))
valid_shapes # 172
valid_shapes/total_shapes*100 # 62.54

total_routes = length(unique(gtfs_toulouse$routes$route_id))
total_routes # 120

matches_routes = length(unique(shapes_toulouse_match_routes$route_id))
matches_routes # 82
matches_routes/total_routes*100 # 68.33
# View(shapes_toulouse_match_routes |> sf::st_drop_geometry())
write.csv(shapes_toulouse_match_routes |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff)
), sprintf("%s.csv", output_toulouse), row.names = FALSE)
sf::st_write(shapes_toulouse_match_routes, sprintf("%s.gpkg", output_toulouse))
