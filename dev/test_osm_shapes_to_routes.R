library(osmdata)
library(dplyr)
library(GTFShift)

get_overpass_url()
set_overpass_url("https://overpass-api.de/api/interpreter/") # Default
set_overpass_url("https://maps.mail.ru/osm/tools/overpass/api/interpreter") # 2 servers with 56 physical cores, 384Gb RAM, SSD each
set_overpass_url("https://overpass.private.coffee/api/interpreter") # 4 servers with 20 cores, 256GB RAM, SSD each
get_overpass_url()

# STCP, Porto, PT
gtfs <- load_feed("https://opendata.porto.digital/dataset/5275c986-592c-43f5-8f87-aabbd4e4f3a4/resource/89a6854f-2ea3-4ba0-8d2f-6558a9df2a98/download/horarios_gtfs_stcp_16_04_2025.zip", create_transfers = FALSE)
summary(gtfs)

q <- opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "operator", value = "STCP", key_exact = TRUE)

# > Match route_id with shape_id
match <- osm_shapes_to_routes(gtfs, q)
View(match |> sf::st_drop_geometry())


result_shapes <- match |>
  sf::st_drop_geometry() |>
  left_join(gtfs$trips |> select(shape_id, route_id), by = c("shape_id" = "shape_id"), multiple = "first")
nrow(result_shapes)
length(unique(result_shapes$route_id))

mapview::mapview(match |> filter(shape_id == "107_0_1_shp"), zcol = "shape_id")

# > Match route_id with shape_id an osm_way_id
match_ways <- osm_shapes_to_routes(gtfs, q, TRUE)
View(match_ways |> sf::st_drop_geometry())
names(match_ways)

result_ways <- match_ways |>
  sf::st_drop_geometry() |>
  left_join(gtfs$trips |> select(shape_id, route_id), by = c("shape_id" = "shape_id"), multiple = "first")
nrow(result_ways)
length(unique(result_ways$route_id))
View(result_ways)

length(result_ways$way_osm_id)
length(unique(result_ways$way_osm_id))

mapview::mapview(match_ways |> filter(shape_id == "12M_1_1_shp"), zcol = "way_osm_id")
mapview::mapview(match_ways |> filter(shape_id == "12M_1_1_shp"), zcol = "lanes")
mapview::mapview(match_ways |> filter(shape_id == "12M_1_1_shp"), zcol = "lanes:bus")
