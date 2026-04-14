library(GTFShift)
library(dplyr)
library(units)

# https://github.com/U-Shift/GTFShift/issues/32

# Carris, Lisboa, PT
library(GTFShift)
library(dplyr)
library(units)
library(osmdata)

# Expected 749km (single); 1849km (2 ways) of bus, 67km (2 ways) of tram (https://www.carris.pt/media/lwsixqty/relatorio-e-contas-2023.pdf)
gtfs_carris <- load_feed("https://files.mobilitydatabase.org/mdb-1032/mdb-1032-202505170214/mdb-1032-202505170214.zip", create_transfers = FALSE)
summary(gtfs_carris)

route_extent_carris_2ways <- get_network_extension(gtfs_carris, date = "2025-05-21", route_identifier = "route_short_name")
drop_units(route_extent_carris_2ways / 1000) # 2486.455 km

bus_routes <- gtfs_carris$routes |>
  filter(!grepl("E", route_short_name)) |>
  left_join(gtfs_carris$trips)
gtfs_carris_bus <- tidytransit::filter_feed_by_trips(gtfs_carris, trip_ids = bus_routes$trip_id)
summary(gtfs_carris_bus)
length(unique(gtfs_carris_bus$routes$route_short_name))

route_extent_carris_bus_2ways <- get_network_extension(gtfs_carris_bus, date = "2025-05-21", route_identifier = "route_short_name")
drop_units(route_extent_carris_bus_2ways / 1000) # 2413.167 km

route_extent_carris_bus_1way <- get_network_extension(gtfs_carris_bus, date = "2025-05-21", route_identifier = "route_short_name", direction_wise = FALSE)
drop_units(route_extent_carris_bus_1way / 1000) # 1327.426

tram_routes <- gtfs_carris$routes |>
  filter(grepl("E", route_short_name) & !grepl("Ascensor", route_long_name) & !grepl("Elevador", route_long_name)) |>
  left_join(gtfs_carris$trips)
gtfs_carris_tram <- tidytransit::filter_feed_by_trips(gtfs_carris, trip_ids = tram_routes$trip_id)
summary(gtfs_carris_tram)
length(unique(gtfs_carris_tram$routes$route_short_name))

route_extent_carris_tram_2ways <- get_network_extension(gtfs_carris_tram, date = "2025-05-21", route_identifier = "route_short_name")
drop_units(route_extent_carris_tram_2ways / 1000) # 71.48166 km

carris_q <- opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs_carris_bus$shapes))) |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

route_extent_carris_bus_2ways_osm <- get_network_extension(gtfs_carris_bus, date = "2025-05-21", route_identifier = "route_short_name", use_osm_routes = carris_q)
drop_units(route_extent_carris_bus_2ways_osm / 1000) # 2430.878 km

route_extent_carris_bus_1way_osm <- get_network_extension(gtfs_carris_bus, date = "2025-05-21", route_identifier = "route_short_name", direction_wise = FALSE, use_osm_routes = carris_q)
drop_units(route_extent_carris_bus_1way_osm / 1000) # 1336.073 km

route_extent_carris_bus_2ways_osm_unified <- get_network_extension(gtfs_carris_bus, date = "2025-05-21", route_identifier = "route_short_name", use_osm_routes = carris_q, unified = TRUE)
drop_units(route_extent_carris_bus_2ways_osm_unified / 1000) # 754.029 km

route_extent_carris_bus_1way_osm_unified <- get_network_extension(gtfs_carris_bus, date = "2025-05-21", route_identifier = "route_short_name", direction_wise = FALSE, use_osm_routes = carris_q, unified = TRUE)
drop_units(route_extent_carris_bus_1way_osm_unified / 1000) # 625.5131 km


# Metro Lisboa
library(GTFShift)
library(dplyr)
library(units)

# Expected 44.5km (https://pt.wikipedia.org/wiki/Metropolitano_de_Lisboa)
gtfs_ml <- GTFShift::load_feed("https://files.mobilitydatabase.org/tld-716/tld-716-202505200132/tld-716-202505200132.zip", create_transfers = FALSE)
summary(gtfs_ml)

route_extent_ml <- get_network_extension(gtfs_ml, date = "2025-05-21")
drop_units(route_extent_ml / 1000) # 116.7961 km

route_extent_ml_single <- get_network_extension(gtfs_ml, date = "2025-05-21", direction_wise = FALSE)
drop_units(route_extent_ml_single / 1000) # 51.77 km

# STCP, Porto, PT
library(GTFShift)
library(dplyr)
library(units)
library(osmdata)

# Expected 503 km (https://stcp.pt/pt/stcp-em-numeros)
gtfs_stcp <- load_feed("https://opendata.porto.digital/dataset/5275c986-592c-43f5-8f87-aabbd4e4f3a4/resource/89a6854f-2ea3-4ba0-8d2f-6558a9df2a98/download/horarios_gtfs_stcp_16_04_2025.zip", create_transfers = FALSE)
summary(gtfs_stcp)
length(unique(gtfs_stcp$routes$route_id))

route_extent_stcp_2ways <- get_network_extension(gtfs_stcp, date = "2025-11-05")
drop_units(route_extent_stcp_2ways / 1000) # 2398.016 km

route_extent_stcp_1way <- get_network_extension(gtfs_stcp, date = "2025-11-05", direction_wise = FALSE)
drop_units(route_extent_stcp_1way / 1000) # 1241.562 km

route_extent_stcp_1way_unified <- get_network_extension(gtfs_stcp, date = "2025-11-05", direction_wise = FALSE, unified = TRUE)
drop_units(route_extent_stcp_1way_unified / 1000) # 948.3196 km

stcp_q <- opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs_stcp$shapes))) |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "operator", value = "STCP", key_exact = TRUE)

route_extent_stcp_1way_osm <- get_network_extension(gtfs_stcp, date = "2025-11-05", direction_wise = FALSE, use_osm_routes = stcp_q)
drop_units(route_extent_stcp_1way_osm / 1000) # 1241.562 km

route_extent_stcp_1way_osm_unified <- get_network_extension(gtfs_stcp, date = "2025-11-05", direction_wise = FALSE, use_osm_routes = stcp_q, unified = TRUE)
drop_units(route_extent_stcp_1way_osm_unified / 1000) # 634.779 km
