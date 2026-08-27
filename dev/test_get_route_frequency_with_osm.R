library(mapview)

# Get GTFS from library GTFS database for Portugal
data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_id = "lisboa"
gtfs = GTFShift::load_feed(data$URL[data$ID == gtfs_id], create_transfers=FALSE)
summary(gtfs)

# Normal
frequencies_route = GTFShift::get_route_frequency_hourly(gtfs)
mapview::mapview(
  frequencies_route |> filter(hour == 8 & frequency > 2),
  zcol = "frequency",
  layer.name = "Frequency (hour)"
)

# With OSM
library(osmdata)
q = opq("Lisbon")  |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

frequencies_route_osm = GTFShift::get_route_frequency_hourly(gtfs, use_osm_routes=q)
mapview::mapview(
  frequencies_route_osm |> filter(hour == 8 & frequency > 2),
  zcol = "frequency",
  layer.name = "Frequency (hour) using OSM routes"
)

frequencies_route_osm_overline = GTFShift::get_route_frequency_hourly(gtfs, use_osm_routes=q, overline=TRUE)
mapview::mapview(
  frequencies_route_osm_overline |> filter(hour == 8 & frequency > 2),
  zcol = "frequency",
  layer.name = "Frequency (hour) using OSM routes with overline"
)

# OSM ways
frequencies_way_osm = get_way_frequency_hourly(gtfs, q)
mapview::mapview(
  frequencies_way_osm |> filter(hour == 8 & frequency > 2),
  zcol = "frequency",
  layer.name = "Frequency (hour) using OSM ways"
)
