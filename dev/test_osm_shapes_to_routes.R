library(osmdata)
library(dplyr)
library(GTFShift)

get_overpass_url()
set_overpass_url("https://overpass-api.de/api/interpreter/") # Default
set_overpass_url("https://maps.mail.ru/osm/tools/overpass/api/interpreter") # 2 servers with 56 physical cores, 384Gb RAM, SSD each
set_overpass_url("https://overpass.private.coffee/api/interpreter") # 4 servers with 20 cores, 256GB RAM, SSD each
get_overpass_url()

osm_file <- osmextract::oe_download(
  "https://download.geofabrik.de/europe/portugal-latest.osm.pbf",
  file_basename = sprintf("%s_%s.osm.pbf", "PT", format(Sys.Date(), "%Y%m%d"))
)
osm_file

# STCP, Porto, PT
gtfs <- load_feed("https://opendata.porto.digital/dataset/5275c986-592c-43f5-8f87-aabbd4e4f3a4/resource/1aae01cb-1814-4cff-b328-81af3dd6e1b5/download/gtfs_feed.zip", create_transfers = FALSE)
summary(gtfs)

q <- opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "operator", value = "STCP", key_exact = TRUE)

# > Match route_id with shape_id
match_osm_file <- osm_shapes_to_routes(gtfs, q, osm_file = osm_file)
View(match_osm_file |> sf::st_drop_geometry())


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

match_ways <- osm_shapes_to_routes(gtfs, q, TRUE, osm_file = osm_file)
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
