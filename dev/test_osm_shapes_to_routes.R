library(osmdata)
library(dplyr)
library(GTFShift)

# STCP, Porto, PT
gtfs = load_feed("https://opendata.porto.digital/dataset/5275c986-592c-43f5-8f87-aabbd4e4f3a4/resource/89a6854f-2ea3-4ba0-8d2f-6558a9df2a98/download/horarios_gtfs_stcp_16_04_2025.zip", create_transfers = FALSE)
summary(gtfs)

q = opq(bbox=sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes)))  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "operator", value = "STCP", key_exact = TRUE)

match = osm_shapes_to_routes(gtfs, q)
View(match)

result = match |> sf::st_drop_geometry() |> left_join(gtfs$trips |> select(shape_id, route_id), by=c("shape_id"="shape_id"), multiple="first")
nrow(result)
length(unique(result$route_id))
