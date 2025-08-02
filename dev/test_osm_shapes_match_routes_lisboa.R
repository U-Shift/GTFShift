library(GTFShift)

output_lisboa = "releases/v0_7_0/shapes_match_lisboa_gtfs20250801_run20250801"

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_lisboa = GTFShift::load_feed(data$URL[data$ID == "lisboa"], create_transfers=FALSE)
tidytransit::write_gtfs(gtfs_lisboa, "releases/v0_7_0/gtfs_lisboa_20250801.zip")
total_network_lisboa = length(unique(gtfs_lisboa$routes$route_short_name))
total_network_lisboa # 111

gtfs_lisboa_shapes = tidytransit::shapes_as_sf(gtfs_lisboa$shapes)
bbox_lisboa = sf::st_bbox(gtfs_lisboa_shapes)

# Build OSM query
library(osmdata)
q_lisboa = opq("Lisbon")  |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

# Match shapes geometry
shapes_lisboa_match_routes = GTFShift::osm_shapes_match_routes(gtfs_lisboa, q_lisboa, log_file = sprintf("%s.r.log", output_lisboa))
summary(shapes_lisboa_match_routes)
# View(shapes_lisboa_match_routes |> sf::st_drop_geometry())

total_shapes = length(unique(gtfs_lisboa$shapes$shape_id))
total_shapes # 307

matches_shapes = nrow(shapes_lisboa_match_routes)
matches_shapes # 273
matches_shapes/total_shapes*100 # 88.9

valid_shapes = nrow(shapes_lisboa_match_routes |> filter(distance_diff<1000 & points_diff<500))
valid_shapes # 270
valid_shapes/total_shapes*100 # 87.94

total_routes = length(unique(gtfs_lisboa$routes$route_id))
total_routes # 174

matches_routes = length(unique(shapes_lisboa_match_routes$route_id))
matches_routes # 155
matches_routes/total_routes*100 # 89.0

# View(shapes_lisboa_match_routes |> sf::st_drop_geometry())
write.csv(shapes_lisboa_match_routes |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff)
), sprintf("%s.csv", output_lisboa), row.names = FALSE)
sf::st_write(shapes_lisboa_match_routes, sprintf("%s.gpkg", output_lisboa))
