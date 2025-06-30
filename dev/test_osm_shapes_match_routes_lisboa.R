library(GTFShift)

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_lisboa = GTFShift::load_feed(data$URL[data$ID == "lisboa"], create_transfers=FALSE)

gtfs_lisboa_shapes = tidytransit::shapes_as_sf(gtfs_lisboa$shapes)
bbox_lisboa = sf::st_bbox(gtfs_lisboa_shapes)

# Build OSM query
library(osmdata)
q_lisboa = opq("Lisbon")  |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

# Match shapes geometry
shapes_lisboa_match_routes = GTFShift::osm_shapes_match_routes(gtfs_lisboa, q_lisboa)

total = nrow(shapes_lisboa_match_routes) # 300
total = length(unique(gtfs_lisboa$shapes$shape_id)) # 308
summary(shapes_lisboa_match_routes)

valid = nrow(shapes_lisboa_match_routes |> filter(distance_diff<500 & points_diff<250)) # 273
valid/total*100 # 88.63 %

View(shapes_lisboa_match_routes |> sf::st_drop_geometry())
#
write.csv(shapes_lisboa_match_routes |> sf::st_drop_geometry() |> mutate(distance_diff=round(distance_diff), points_diff=round(points_diff)), "dev/shapes_match_lisboa.csv", row.names = FALSE)
sf::st_write(shapes_lisboa_match_routes, "dev/shapes_match_lisboa.gpkg")
