#' Get aggregated frequency per hour for each bus route
#'
#' For each route, returns the number of departures aggregated per hour and direction.
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param date Date (Default \code{GTFShift::calendar_nextBusinessWednesday()}). Reference date to consider when analyzing the GTFS file.
#' @param use_osm_routes osmdata::opq (Default NA). If overpass query for transit network is defined, analysis is performed considering OSM route geometry, using \code{GTFShift::osm_shapes_to_routes()}.
#' @param overline Boolean (Default FALSE). If TRUE, routes are aggregated using \code{stplanr::overline2()}, overlapping lines and converting them into a single route network.
#'
#' @details
#' This method analyses the GTFS feed for a representative day, generating for each route the number of services aggregated per hour and direction.
#' It assumes the time of departure at the first stop as a reference for each trip geometry.
#'
#' By default, it estimates the next business Wednesday, relevant for the peak hour.
#'
#' The \code{overline} parameter enables the aggregation of bus routes that share common line segments, returning a sum of frequencies per road segment, using \code{stplanr::overline2()}.
#'
#' Optionally, using \code{use_osm_routes} parameter, it retrieves the geometries from OpenStreetMap by matching the tag \code{gtfs:shape_id}, overwriting the original GTFS \code{shapes.txt}.
#' This is particularly useful if the GTFS shapes do not share the same geometry. For instance, if the edges of the lines do not overlap or do not follow the same route-over-the-road – which is very common, even besides \href{https://gtfs.org/documentation/schedule/schedule-best-practices/#shapestxt}{GTFS recommendation} – geometries might not be aggregated correctly, causing inconsistent results.
#' By relying on a common road network, such as OSM, it is possible to overcome this issue and aggregate the bus routes correctly.
#'
#' For a detailed example, see the \code{vignette("analyse")}.
#'
#' Adapted from \url{https://github.com/Bondify/GTFS_in_R/}.
#'
#' @returns An \code{sf} \code{data.frame} object with the following columns (the first three are only present if \code{overline=FALSE}):
#' \describe{
#'   \item{route_id}{The \code{route_id} attribute from \code{routes.txt} file.}
#'   \item{route_short_name}{The \code{route_short_name} attribute from \code{routes.txt} file.}
#'   \item{shape_id}{The \code{shape_id} attribute from \code{shapes.txt} file.}
#'   \item{direction_id}{The \code{direction_id} attribute from \code{trips.txt} file (if attribute present in GTFS feed).}
#'   \item{hour}{The hour for which the frequency applies (24 hour format).}
#'   \item{frequency}{The number of services for the route that depart from the first stop for the corresponding 60 minutes period.}
#'   \item{geometry}{The route shape.}
#' }
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::load_feed("gtfs.zip")
#' frequency_analysis <- GTFShift::get_route_frequency_hourly(gtfs)
#' }
#'
#' @seealso \code{GTFShift::calendar_nextBusinessWednesday()}
#' @seealso \code{GTFShift::osm_shapes_to_routes()}
#' @seealso \code{stplanr::overline2()}
#'
#' @import tidytransit
#' @import dplyr
#' @import sf
#' @import tidyverse
#' @import lubridate
#' @import stplanr
#'
#' @export
get_route_frequency_hourly <- function(
  gtfs,
  date = GTFShift::calendar_nextBusinessWednesday(),
  use_osm_routes = NA,
  overline = FALSE
) {
  message(sprintf("Analysing GTFS for %s...", date))

  ## Consider transit data for one day only
  message(sprintf("> Filtering by reference date %s...", date))
  gtfs_date <- tidytransit::filter_feed_by_date(gtfs, extract_date = date)

  # PROCESS GTFS, generating table calculating the frequencies per route
  trips <- gtfs_date$trip
  stops <- gtfs_date$stops
  if (any(!is.na(use_osm_routes))) {
    shapes <- GTFShift::osm_shapes_to_routes(gtfs, use_osm_routes)
  } else {
    shapes <- tidytransit::shapes_as_sf(gtfs_date$shapes)
  }

  routes <- gtfs_date$routes
  stop_times <- gtfs_date$stop_times

  stop_times <- stop_times |>
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

  stop_times <- stop_times |>
    arrange(stop_sequence) |>
    group_by(trip_id) |>
    slice(1) |> # Only departures from origin (first stop)
    ungroup() |>
    mutate(hour = lubridate::hour(departure_time))

  freq_data <- stop_times |>
    group_by(across(any_of(c("route_id", "shape_id", "route_short_name", "direction_id", "hour")))) |>
    summarize(frequency = n()) |>
    ungroup()

  routes_freq <-
    freq_data |>
    inner_join(shapes) |>
    st_as_sf()

  # Overline?
  if (overline) {
    routes_freq_all <- data.frame()
    for (h in unique(routes_freq$hour)) { # hours of the day
      routes_freq_h <- routes_freq |>
        filter(hour == h) |>
        stplanr::overline2(attrib = "frequency") |>
        arrange(frequency) |>
        mutate(hour = h)

      routes_freq_all <- rbind(routes_freq_all, routes_freq_h)
    }
    return(routes_freq_all)
  }

  return(routes_freq)
}
