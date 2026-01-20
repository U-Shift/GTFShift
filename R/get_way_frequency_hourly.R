#' Get aggregated frequency per hour for each OSM way
#'
#' For each OSM way with GTFS service, returns the number of departures aggregated per hour and direction.
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param q osmdata::opq. Overpass query for transit network, to obtain OSM route ways, using \code{GTFShift::osm_shapes_to_routes()}.
#' @param date Date (Default \code{GTFShift::calendar_nextBusinessWednesday()}). Reference date to consider when analyzing the GTFS file.
#' @param keep_osm_attributes Boolean (Default FALSE). Whether to keep all OSM way attributes in the output \code{sf} object.
#'
#' @details
#' This method analyses the GTFS feed for a representative day, finding for each route the corresponding OSM ways using \code{GTFShift::osm_shapes_to_routes()}
#' (routes not on OSM are ignored), aggregating the number of services per hour and direction for each.
#'
#' For a detailed example, see the \code{vignette("analyse")}.
#'
#' @returns An \code{sf} \code{data.frame} object with the following columns:
#' \itemize{
#'  \item \code{way_osm_id}, the \code{osm_id} attribute from OSM way.
#'  \item \code{hour}, the hour for which the frequency applies (24 hour format).
#'  \item \code{frequency}, the number of services for the route that depart from the first stop for the corresponding 60 minutes period.
#'  \item \code{geometry}, the route shape.
#'  \ietm (if \code{keep_osm_attributes = TRUE}) all OSM way attributes.
#' }
#'
#' @examples
#' \dontrun{
#' gtfs = GTFShift::load_feed("gtfs.zip")
#' q = opq(bbox=sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |> add_osm_feature(key = "route", value = "bus")
#' frequency_analysis = GTFShift::get_way_frequency_hourly(gtfs, q)
#' }
#'
#' @seealso \code{GTFShift::calendar_nextBusinessWednesday()}
#' @seealso \code{GTFShift::osm_shapes_to_routes()}
#'
#' @import tidytransit
#' @import dplyr
#' @import sf
#' @import tidyverse
#' @import lubridate
#'
#' @export
get_way_frequency_hourly = function(
    gtfs,
    q,
    date = GTFShift::calendar_nextBusinessWednesday(),
    keep_osm_attributes = FALSE
) {
  message(sprintf("Analysing GTFS for %s...", date))

  ## Consider transit data for one day only
  message(sprintf("> Filtering by reference date %s...", date))
  gtfs_date = tidytransit::filter_feed_by_date(gtfs, extract_date = date)

  # PROCESS GTFS, generating table calculating the frequencies per route
  trips = gtfs_date$trips
  stops = gtfs_date$stops
  ways = GTFShift::osm_shapes_to_routes(gtfs, q, TRUE)

  routes = gtfs_date$routes
  stop_times = gtfs_date$stop_times

  stop_times = stop_times |>
    left_join(trips) |>
    left_join(routes) |>
    select(any_of(c(
      "route_id",
      "route_short_name",
      "trip_id",
      "stop_id",
      "service_id",
      "arrival_time",
      "departure_time",
      "direction_id",
      "shape_id",
      "stop_sequence"
    )))

  stop_times = stop_times |> # Only departures from origin (first stop)
    filter(stop_sequence == 1)

  stop_times = stop_times |>
    mutate(
      hour = lubridate::hour(departure_time)
    )

  freq_data = stop_times |>
    group_by(across(any_of(c("route_id", "route_short_name", "direction_id", "hour")))) |>
    summarize(frequency = n()) |>
    ungroup()

  routes_freq =
    freq_data |>
    left_join(trips |>
                select(any_of(c("route_id", "direction_id", "shape_id"))) |>
                distinct(), relationship="many-to-many") |>
    as.data.frame()

  # Join with ways
  ways_unique_geometry = ways |>
    distinct(way_osm_id, .keep_all = TRUE)

  if (!keep_osm_attributes) {
    ways_unique_geometry = ways_unique_geometry |>
      select(way_osm_id, geometry)
  }

  ways_freq = routes_freq |>
    inner_join(ways |> sf::st_drop_geometry() |> select(shape_id, way_osm_id), by="shape_id", relationship = "many-to-many") |>
    group_by(way_osm_id, hour) |>
    summarize(frequency = sum(frequency)) |>
    ungroup() |>
    inner_join(ways_unique_geometry, by="way_osm_id") |>
    st_as_sf()

  return(ways_freq)
}
