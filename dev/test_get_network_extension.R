library(GTFShift)
library(dplyr)

# Carris, Lisboa
# Expected 1849km of bus, 67km of tram (https://www.carris.pt/media/lwsixqty/relatorio-e-contas-2023.pdf)
gtfs_carris = load_feed("https://files.mobilitydatabase.org/mdb-1032/mdb-1032-202505170214/mdb-1032-202505170214.zip", create_transfers = FALSE)
summary(gtfs_carris)

route_extent_carris = get_network_extension(gtfs_carris, date="2025-05-21")
route_extent_carris/1000 # CRS4326: 1937.284 // CRS3857: 2486.455 km

bus_routes = gtfs_carris$routes |> filter(!grepl("E", route_short_name)) |> left_join(gtfs_carris$trips)
gtfs_carris_bus = tidytransit::filter_feed_by_trips(gtfs_carris, trip_ids = bus_routes$trip_id)
summary(gtfs_carris_bus)
length(unique(gtfs_carris_bus$routes$route_short_name))

route_extent_carris_bus = get_network_extension(gtfs_carris_bus, date="2025-05-21")
route_extent_carris_bus/1000 # CRS4326: 1880.159 // CRS3857: 2413.167 km

tram_routes = gtfs_carris$routes |> filter(grepl("E", route_short_name) & !grepl("Ascensor", route_long_name) & !grepl("Elevador", route_long_name)) |> left_join(gtfs_carris$trips)
gtfs_carris_tram = tidytransit::filter_feed_by_trips(gtfs_carris, trip_ids = tram_routes$trip_id)
summary(gtfs_carris_tram)
length(unique(gtfs_carris_tram$routes$route_short_name))

route_extent_carris_tram = get_network_extension(gtfs_carris_tram, date="2025-05-21")
route_extent_carris_tram/1000 # CRS4326: 55.71755 // CRS3857: 71.48166 km

# Metro Lisboa
# Expected 44.5km (https://pt.wikipedia.org/wiki/Metropolitano_de_Lisboa)
gtfs_ml = GTFShift::load_feed("https://files.mobilitydatabase.org/tld-716/tld-716-202505200132/tld-716-202505200132.zip", create_transfers = FALSE)
summary(gtfs_ml)

route_extent_ml = get_network_extension(gtfs_ml, date="2025-05-21")
route_extent_ml/1000 # CRS4326: 90.99035 // CRS3857: 116.7961 km



