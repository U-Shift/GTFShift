library(GTFShift)
library(dplyr)

# Get GTFS from library GTFS database for Portugal
data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))

# Carris, Lisboa
# Expected 1849km of bus, 67km of tram (https://www.carris.pt/media/lwsixqty/relatorio-e-contas-2023.pdf)
gtfs_carris = load_feed(data$URL[data$ID == "lisboa"], create_transfers = FALSE)
summary(gtfs_carris)

route_extent_carris = get_network_extension(gtfs_carris)
route_extent_carris/1000 # CRS4326: 1951.018 // CRS3857: 2504.07 km

bus_routes = gtfs_carris$routes |> filter(!grepl("E", route_short_name)) |> left_join(gtfs_carris$trips)
gtfs_carris_bus = tidytransit::filter_feed_by_trips(gtfs_carris, trip_ids = bus_routes$trip_id)
summary(gtfs_carris_bus)
length(unique(gtfs_carris_bus$routes$route_short_name))

route_extent_carris_bus = get_network_extension(gtfs_carris_bus)
route_extent_carris_bus/1000 # CRS4326: 1880.46 // CRS3857: 2413 km

tram_routes = gtfs_carris$routes |> filter(grepl("E", route_short_name) & !grepl("Ascensor", route_long_name) & !grepl("Elevador", route_long_name)) |> left_join(gtfs_carris$trips)
gtfs_carris_tram = tidytransit::filter_feed_by_trips(gtfs_carris, trip_ids = tram_routes$trip_id)
summary(gtfs_carris_tram)
length(unique(gtfs_carris_tram$routes$route_short_name))

route_extent_carris_tram = get_network_extension(gtfs_carris_tram)
route_extent_carris_tram/1000 # CRS4326: 69.14 // CRS3857: 88.7 km

# Metro Lisboa
# Expected 44.5km (https://pt.wikipedia.org/wiki/Metropolitano_de_Lisboa)
gtfs_ml = GTFShift::load_feed(data$URL[data$ID == "metroLisboa"], create_transfers = FALSE)
summary(gtfs_ml)

route_extent_ml = get_network_extension(gtfs_ml)
route_extent_ml/1000 # CRS4326: 122.61 // CRS3857: 157.38 km



