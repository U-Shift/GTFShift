# Associate route_id (routes.txt) and direction_id (trips.txt) to shape_id (shapes.txt)

summary(gtfs)
gtfs = tidytransit::filter_feed_by_date(gtfs, extract_date = Sys.Date())

route_shapes = gtfs$routes |>
  left_join(gtfs$trips, by = "route_id") |>
  left_join(gtfs$shapes, by = "shape_id") |>
  left_join(gtfs$stop_times |> filter(stop_sequence==1), by = "trip_id") |>
  group_by(route_id, route_short_name, trip_headsign, direction_id, shape_id) |>
  summarise(
    n_trips = n()
  )
View(route_shapes)
nrow(route_shapes)
