library(GTFShift)

output_funchal = "releases/v0_7_0/shapes_match_funchal_gtfs20250802_run20250802"

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_funchal = GTFShift::load_feed(data$URL[data$ID == "funchal"], create_transfers=FALSE)
tidytransit::write_gtfs(gtfs_funchal, "releases/v0_7_0/gtfs_funchal_20250802.zip")
total_network_funchal = length(unique(gtfs_funchal$routes$route_short_name)) # ??
total_network_funchal # 60

gtfs_funchal_shapes = tidytransit::shapes_as_sf(gtfs_funchal$shapes)
bbox_funchal = sf::st_bbox(gtfs_funchal_shapes)

# Build OSM query
library(osmdata)
q_funchal = opq(bbox_funchal)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "operator", value = "HF", key_exact = TRUE)

# Match shapes geometry
shapes_funchal_match_routes = GTFShift::osm_shapes_match_routes(gtfs_funchal, q_funchal, log_file = sprintf("%s.r.log", output_funchal))
summary(shapes_funchal_match_routes)
# View(shapes_funchal_match_routes |> sf::st_drop_geometry())

total_shapes = length(unique(gtfs_funchal$shapes$shape_id))
total_shapes # 256

matches_shapes = nrow(shapes_funchal_match_routes)
matches_shapes # 21
matches_shapes/total_shapes*100 # 8.2

valid_shapes = nrow(shapes_funchal_match_routes |> filter(distance_diff<1000 & points_diff<500))
valid_shapes # 11
valid_shapes/total_shapes*100 # 4.29

total_routes = length(unique(gtfs_funchal$routes$route_id))
total_routes # 161

matches_routes = length(unique(shapes_funchal_match_routes$route_id))
matches_routes # 11
matches_routes/total_routes*100 # 6.83

# View(shapes_funchal_match_routes |> sf::st_drop_geometry())
write.csv(shapes_funchal_match_routes |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff)
), sprintf("%s.csv", output_funchal), row.names = FALSE)
sf::st_write(shapes_funchal_match_routes, sprintf("%s.gpkg", output_funchal))
