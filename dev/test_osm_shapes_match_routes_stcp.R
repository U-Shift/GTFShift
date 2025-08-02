library(GTFShift)

output_stcp = "releases/v0_7_0/shapes_match_stcp_gtfs20250802_run20250802"

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_stcp = GTFShift::load_feed(data$URL[data$ID == "stcp"], create_transfers=FALSE)
tidytransit::write_gtfs(gtfs_stcp, "releases/v0_7_0/gtfs_stcp_20250802.zip")
total_network_stcp = length(unique(gtfs_stcp$routes$route_short_name))
total_network_stcp # 73

gtfs_stcp_shapes = tidytransit::shapes_as_sf(gtfs_stcp$shapes)
bbox_stcp = sf::st_bbox(gtfs_stcp_shapes)

# Build OSM query
library(osmdata)
q_stcp = opq(bbox_stcp)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "operator", value = "STCP", key_exact = TRUE)

# Match shapes geometry
shapes_stcp_match_routes = GTFShift::osm_shapes_match_routes(gtfs_stcp, q_stcp, log_file = sprintf("%s.r.log", output_stcp))
summary(shapes_stcp_match_routes)
# View(shapes_stcp_match_routes |> sf::st_drop_geometry())

total_shapes = length(unique(gtfs_stcp$shapes$shape_id))
total_shapes # 142

matches_shapes = nrow(shapes_stcp_match_routes)
matches_shapes # 138
matches_shapes/total_shapes*100 # 97.18

valid_shapes = nrow(shapes_stcp_match_routes |> filter(distance_diff<1000 & points_diff<500))
valid_shapes # 116
valid_shapes/total_shapes*100 # 81.69

total_routes = length(unique(gtfs_stcp$routes$route_id))
total_routes # 73

matches_routes = length(unique(shapes_stcp_match_routes$route_id))
matches_routes # 71
matches_routes/total_routes*100 # 97.26

# View(shapes_stcp_match_routes |> sf::st_drop_geometry())
write.csv(shapes_stcp_match_routes |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff)
), sprintf("%s.csv", output_stcp), row.names = FALSE)
sf::st_write(shapes_stcp_match_routes, sprintf("%s.gpkg", output_stcp))
