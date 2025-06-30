library(GTFShift)

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_cascais = GTFShift::load_feed(data$URL[data$ID == "cascais"], create_transfers=FALSE)
gtfs_cascais$routes$route_short_name = sub("_.*", "", gtfs_cascais$routes$route_id)

gtfs_cascais_shapes = tidytransit::shapes_as_sf(gtfs_cascais$shapes)
bbox_cascais = sf::st_bbox(gtfs_cascais_shapes)

# Build OSM query
library(osmdata)
q_cascais = opq(bbox_cascais)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "network", value = "MobiCascais", key_exact = TRUE)

# Match shapes geometry
shapes_cascais_match_routes = GTFShift::osm_shapes_match_routes(gtfs_cascais, q_cascais)

total = nrow(shapes_cascais_match_routes) # 139
total = length(unique(gtfs_cascais$shapes$shape_id)) # 139
summary(shapes_cascais_match_routes)

valid = nrow(shapes_cascais_match_routes |> filter(distance_diff<1000 & points_diff<500)) # 107
valid/total*100 # 76.97842 %

# View(shapes_cascais_match_routes |> sf::st_drop_geometry())
#
write.csv(shapes_cascais_match_routes |> sf::st_drop_geometry() |> mutate(distance_diff=round(distance_diff), points_diff=round(points_diff)), "dev/shapes_match_cascais.csv", row.names = FALSE)
sf::st_write(shapes_cascais_match_routes, "dev/shapes_match_cascais.gpkg")
