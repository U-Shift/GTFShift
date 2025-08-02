library(GTFShift)

output_cascais = "releases/v0_7_0/shapes_match_cascais_gtfs20250802_run20250802_DEBUGPRINTS"

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_cascais = GTFShift::load_feed(data$URL[data$ID == "cascais"], create_transfers=FALSE)
tidytransit::write_gtfs(gtfs_cascais, "releases/v0_7_0/gtfs_cascais_20250802.zip")
gtfs_cascais$routes$route_short_name = sub("_.*", "", gtfs_cascais$routes$route_id) # Fix route_short_name to enable match with OSM
total_network_cascais = length(unique(gtfs_cascais$routes$route_short_name))
total_network_cascais # 44

gtfs_cascais_shapes = tidytransit::shapes_as_sf(gtfs_cascais$shapes)
bbox_cascais = sf::st_bbox(gtfs_cascais_shapes)

# Build OSM query
library(osmdata)
q_cascais = opq(bbox_cascais)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "network", value = "MobiCascais", key_exact = TRUE)

# Match shapes geometry
shapes_cascais_match_routes = GTFShift::osm_shapes_match_routes(gtfs_cascais, q_cascais, log_file = sprintf("%s.r.log", output_cascais))
summary(shapes_cascais_match_routes)
# View(shapes_cascais_match_routes |> sf::st_drop_geometry())

total_shapes = length(unique(gtfs_cascais$shapes$shape_id))
total_shapes # 139

matches_shapes = nrow(shapes_cascais_match_routes)
matches_shapes # 95
matches_shapes/total_shapes*100 # 68.34

valid_shapes = nrow(shapes_cascais_match_routes |> filter(distance_diff<1000 & points_diff<500))
valid_shapes # 69
valid_shapes/total_shapes*100 # 49.64

total_routes = length(unique(gtfs_cascais$routes$route_id))
total_routes # 94

matches_routes = length(unique(shapes_cascais_match_routes$route_id))
matches_routes # 61
matches_routes/total_routes*100 # 64.89

# View(shapes_cascais_match_routes |> sf::st_drop_geometry())
write.csv(shapes_cascais_match_routes |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff)
), sprintf("%s.csv", output_cascais), row.names = FALSE)
sf::st_write(shapes_cascais_match_routes, sprintf("%s.gpkg", output_cascais))
