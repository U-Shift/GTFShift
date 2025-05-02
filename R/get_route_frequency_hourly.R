#' Get aggregated frequency per hour for each bus route
#'
#' For each route, returns the number of departures aggregated per hour.
#'
#' @param gtfs tidygtfs. GTFS loaded using tidytransit::read_gtfs.
#' @param date Date. Reference date to consider when analysing the GTFS file. Defaults to next business Wednesday in Portugal.
#' @param overline Boolean. Defaults to FALSE. If TRUE, routes are aggregated using stplanr::overline2, overlapping lines and converting them into a single route network.
#'
#' @details
#' This method analyses the GTFS feed for a representative day, generating for each route the number of services aggregated per hour.
#' For a detailed example, see the \code{vignette("analyse")}.
#'
#' Adapted from https://github.com/Bondify/GTFS_in_R/.
#'
#' @returns An `sf` `data.frame` object with the following columns (the first three are only present if `overline=FALSE`):
#' - `route_id`, the `route_id` attribute from `routes.txt` file;
#' - `route_short_name`, the `route_short_name` attribute from `routes.txt` file;
#' - `direction_id`, the `direction_id` attribute from `trips.txt` file;
#' - `arrival_hour`, the hour for which the frequency applies (24 hour format);
#' - `frequency`, the number of services for the route that depart from the first stop for the corresponding 60 minutes period;
#' - `geometry`, the route shape.
#'
#' @examples
#' \dontrun{
#' gtfs <- tidytransit::read_gtfs("gtfs.zip")
#' frequency_analysis <- get_route_frequency_hourly(gtfs)
#' }
#'
#' @seealso [tidytransit::read_gtfs()], [stplanr::overline2]
#'
#' @import tidytransit
#' @import dplyr
#' @import sf
#' @import tidyverse
#' @import lubridate
#' @import stplanr
#'
#' @export
get_route_frequency_hourly <- function(gtfs, date=NULL, overline=FALSE) {
  message(sprintf("Analysing GTFS..."))

  # DATE FILTER
  # Consider transit data for one day only
  if (is.null(date)) {
    date = calendar_nextBusinessWednesday()
    message(sprintf("> Reference date not provided, considering next business wednesday: %s...", date))
  }

  message(sprintf("> Filtering by reference date %s...", date))
  gtfs_date <- tidytransit::filter_feed_by_date(
    gtfs, extract_date = date
  )

  # PROCESS GTFS, generating table calculating the frequencies per route
  trips = gtfs_date$trip
  stops = gtfs_date$stops
  shapes = tidytransit::shapes_as_sf(gtfs_date$shapes)
  routes = gtfs_date$routes
  stop_times = gtfs_date$stop_times

  stop_times <- stop_times %>%
    left_join(trips) %>%
    left_join(routes) %>%
    select(route_id, route_short_name, trip_id, stop_id, service_id, arrival_time, departure_time, direction_id, shape_id, stop_sequence)

  stop_times <- stop_times %>% # Only departures from origin (first stop)
    filter(stop_sequence == 1)

  stop_times <- stop_times %>%
    mutate(
      arrival_hour = lubridate::hour(arrival_time)
    )

  freq_data <- stop_times %>%
    group_by(route_id, route_short_name, direction_id, arrival_hour) %>%
    summarize(frequency = n()) %>%
    ungroup()

  routes_freq =
    freq_data %>%
    left_join(trips %>%
                select(route_id, direction_id, shape_id) %>%
                distinct()) %>%
    as.data.frame() %>%
    left_join(shapes) %>%
    st_as_sf()

  # Overline?
  if (overline) {
    routes_freq_all = data.frame()
    for (h in unique(routes_freq$arrival_hour)) { # hours of the day
      routes_freq_h = routes_freq %>%
        filter(arrival_hour == h) %>%
        stplanr::overline2(attrib = "frequency") %>%
        arrange(frequency) %>%
        mutate(arrival_hour = h)

      routes_freq_all = rbind(routes_freq_all, routes_freq_h)
    }
    return (routes_freq_all)
  }

  return(routes_freq)
}
