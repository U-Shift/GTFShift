library(GTFShift)
library(dplyr)
library(units)

# https://github.com/U-Shift/GTFShift/issues/32

# Carris, Lisboa
# Expected 1849km of bus, 67km of tram (https://www.carris.pt/media/lwsixqty/relatorio-e-contas-2023.pdf)
gtfs_carris = load_feed("https://files.mobilitydatabase.org/mdb-1032/mdb-1032-202505170214/mdb-1032-202505170214.zip", create_transfers = FALSE)
summary(gtfs_carris)

route_extent_carris = get_network_extension(gtfs_carris, date="2025-05-21", route_identifier="route_short_name")
drop_units(route_extent_carris/1000) # 2486.455 km

bus_routes = gtfs_carris$routes |> filter(!grepl("E", route_short_name)) |> left_join(gtfs_carris$trips)
gtfs_carris_bus = tidytransit::filter_feed_by_trips(gtfs_carris, trip_ids = bus_routes$trip_id)
summary(gtfs_carris_bus)
length(unique(gtfs_carris_bus$routes$route_short_name))

route_extent_carris_bus = get_network_extension(gtfs_carris_bus, date="2025-05-21", route_identifier="route_short_name", direction_wise=TRUE)
drop_units(route_extent_carris_bus/1000) # 2413.167 km

tram_routes = gtfs_carris$routes |> filter(grepl("E", route_short_name) & !grepl("Ascensor", route_long_name) & !grepl("Elevador", route_long_name)) |> left_join(gtfs_carris$trips)
gtfs_carris_tram = tidytransit::filter_feed_by_trips(gtfs_carris, trip_ids = tram_routes$trip_id)
summary(gtfs_carris_tram)
length(unique(gtfs_carris_tram$routes$route_short_name))

route_extent_carris_tram = get_network_extension(gtfs_carris_tram, date="2025-05-21", route_identifier="route_short_name")
drop_units(route_extent_carris_tram/1000) # 71.48166 km

# Metro Lisboa
# Expected 44.5km (https://pt.wikipedia.org/wiki/Metropolitano_de_Lisboa)
gtfs_ml = GTFShift::load_feed("https://files.mobilitydatabase.org/tld-716/tld-716-202505200132/tld-716-202505200132.zip", create_transfers = FALSE)
summary(gtfs_ml)

route_extent_ml = get_network_extension(gtfs_ml, date="2025-05-21")
drop_units(route_extent_ml/1000) # 116.7961 km

route_extent_ml_single = get_network_extension(gtfs_ml, date="2025-05-21", direction_wise=FALSE)
drop_units(route_extent_ml_single/1000) # 51.77 km

# STCP
# Expected 503 km (https://stcp.pt/pt/stcp-em-numeros)
gtfs_stcp = load_feed("https://opendata.porto.digital/dataset/5275c986-592c-43f5-8f87-aabbd4e4f3a4/resource/89a6854f-2ea3-4ba0-8d2f-6558a9df2a98/download/horarios_gtfs_stcp_16_04_2025.zip", create_transfers = FALSE)
summary(gtfs_stcp)

length(unique(gtfs_stcp$routes$route_id))

route_extent_stcp_2ways = get_network_extension(gtfs_stcp, date="2025-11-05")
drop_units(route_extent_stcp_2ways/1000) # 2398.016 km

route_extent_stcp_1way = get_network_extension(gtfs_stcp, date="2025-11-05", direction_wise = FALSE)
drop_units(route_extent_stcp_1way/1000) # 1241.562 km
