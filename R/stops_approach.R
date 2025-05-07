stops = tidytransit::stops_as_sf(gtfs$stops)
View(stops)
mapview::mapview(stops)

