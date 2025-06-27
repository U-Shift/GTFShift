library(GTFShift)

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_aml = GTFShift::load_feed(data$URL[data$ID == "AML"], create_transfers=FALSE)

gtfs_aml_shapes = tidytransit::shapes_as_sf(gtfs_aml$shapes)
bbox_aml = sf::st_bbox(gtfs_aml_shapes)

# Build OSM query
library(osmdata)
q_aml = opq(bbox)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "network", value = "Carris Metropolitana", key_exact = TRUE)

# Match shapes geometry
shapes_aml_match_routes = GTFShift::osm_shapes_match_routes(gtfs_aml, q_aml)

nrow(shapes_aml_match_routes)
summary(shapes_aml_match_routes)

View(shapes_aml_match_routes |> sf::st_drop_geometry())

nrow(shapes_aml_match_routes |> filter(distance_diff<1000 & points_diff<250))


write.csv(shapes_aml_match_routes, "dev/shapes_match_aml.csv")
sf::st_write(shapes_aml_match_routes, "dev/shapes_match_aml.gpkg")
