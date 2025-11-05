#' Get aggregated frequency per hour for each bus route
#'
#' For each route, returns the number of departures aggregated per hour and direction.
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param date Date (Default \code{GTFShift::calendar_nextBusinessWednesday()}). Reference date to consider when analyzing the GTFS file.
#' @param use_osm_routes osmdata::opq (Default NA). If overpass query for transit network is defined, analysis is performed considering OSM route geometry, using \code{GTFShift::osm_shapes_to_routes}.
#' @param overline Boolean (Default FALSE). If TRUE, routes are aggregated using \code{stplanr::overline2()}, overlapping lines and converting them into a single route network.
#'
#' @details
#' This method analyses the GTFS feed for a representative day, generating for each route the number of services aggregated per hour and direction.
#' For a detailed example, see the \code{vignette("analyse")}.
#'
#' Adapted from \url{https://github.com/Bondify/GTFS_in_R/}.
#'
#' @returns An \code{sf} \code{data.frame} object with the following columns (the first three are only present if \code{overline=FALSE}):
#' \itemize{
#'  \item \code{route_id}, the \code{route_id} attribute from \code{routes.txt} file.
#'  \item \code{route_short_name}, the \code{route_short_name} attribute from \code{routes.txt} file.
#'  \item \code{shape_id}, the \code{shape_id} attribute from \code{shapes.txt} file.
#'  \item \code{direction_id}, the \code{direction_id} attribute from \code{trips.txt} file (if attribute present in GTFS feed).
#'  \item \code{hour}, the hour for which the frequency applies (24 hour format).
#'  \item \code{frequency}, the number of services for the route that depart from the first stop for the corresponding 60 minutes period.
#'  \item \code{geometry}, the route shape.
#' }
#'
#' @examples
#' \dontrun{
#' gtfs = GTFShift::load_feed("gtfs.zip")
#' frequency_analysis = GTFShift::get_route_frequency_hourly(gtfs)
#' }
#'
#' @seealso [tidytransit::read_gtfs()], [stplanr::overline2], [GTFShift::calendar_nextBusinessWednesday]
#'
#' @import tidytransit
#' @import dplyr
#' @import sf
#' @import tidyverse
#' @import lubridate
#' @import stplanr
#'
#' @export
get_route_frequency_hourly = function(
    gtfs,
    date = GTFShift::calendar_nextBusinessWednesday(),
    use_osm_routes=NA,
    overline = FALSE
) {
  message(sprintf("Analysing GTFS for %s...", date))

  ## Consider transit data for one day only
  message(sprintf("> Filtering by reference date %s...", date))
  gtfs_date = tidytransit::filter_feed_by_date(gtfs, extract_date = date)

  # PROCESS GTFS, generating table calculating the frequencies per route
  trips = gtfs_date$trip
  stops = gtfs_date$stops
  if (any(!is.na(use_osm_routes))) {
    shapes = GTFShift::osm_shapes_to_routes(gtfs, use_osm_routes)
  } else {
    shapes = tidytransit::shapes_as_sf(gtfs_date$shapes)
  }

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
    as.data.frame() |>
    inner_join(shapes) |>
    st_as_sf()

  # Overline?
  if (overline) {
    routes_freq_all = data.frame()
    for (h in unique(routes_freq$hour)) { # hours of the day
      routes_freq_h = routes_freq |>
        filter(hour == h) |>
        stplanr::overline2(attrib = "frequency") |>
        arrange(frequency) |>
        mutate(hour = h)

      routes_freq_all = rbind(routes_freq_all, routes_freq_h)
    }
    return (routes_freq_all)
  }

  return(routes_freq)
}
