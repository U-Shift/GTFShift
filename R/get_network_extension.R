#' Get routes extension
#'
#' Get total extension of GTFS feed routes
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param date Date (Default \code{GTFShift::calendar_nextBusinessWednesday()}). Reference date to consider when analyzing the GTFS file.
#' @param use_osm_routes osmdata::opq (Default NA). If overpass query for transit network is defined, analysis is performed considering OSM route geometry, using \code{GTFShift::osm_shapes_to_routes}.
#' @param overline Boolean (Default FALSE). If TRUE, routes are aggregated using \code{stplanr::overline2()}, overlapping lines and converting them into a single route network.
#'
#' @details
#' This method calculates the sum of the GTFS feed routes length, considering, for each, the shape of the variant with the highest frequency for the given date
#' (using \code{GTFShift::get_route_frequency_hourly()}).
#' For a detailed example, see the \code{vignette("analyse")}.
#'
#' @returns The routes extension, in meters.
#'
#' @examples
#' \dontrun{
#' gtfs = GTFShift::load_feed("gtfs.zip")
#' route_extension = GTFShift::get_network_extension(gtfs)
#' }
#'
#' @seealso [GTFShift::get_route_frequency_hourly()]
#'
#' @import dplyr
#' @import sf
#'
#' @export
get_network_extension <- function(gtfs, date = GTFShift::calendar_nextBusinessWednesday(), use_osm_routes = NA, overline = FALSE) {

  # Compute hourly frequencies for each route
  network = gtfs |> GTFShift::get_route_frequency_hourly(date = date, use_osm_routes = use_osm_routes, overline = overline)

  # Get unique shapes
  shapes_unique = tidytransit::shapes_as_sf(gtfs$shapes) |>
    select(shape_id) |>
    distinct()

  # Compute daily frequencies per route shape
  network_redux = network |>
    st_drop_geometry() |>
    group_by(route_short_name, route_id, direction_id, shape_id) |>
    summarise(frequency_day = sum(frequency)) |>
    ungroup()

  # Get shape with max frequencies per route
  network_redux_max = network_redux |>
    group_by(route_short_name, direction_id) |>
    summarise(frequency_max = max(frequency_day)) |>
    ungroup()

  # Join with the original network to get the shapes and compute its distance
  network_redux_shapes = network_redux_max |>
    left_join(network_redux |> select(-route_id), by = c("route_short_name", "direction_id", "frequency_max" = "frequency_day")) |>
    select(-frequency_max) |>
    left_join(shapes_unique, by = "shape_id") |>
    st_as_sf() |>
    st_transform(crs = 3857) |> # For units in meters
    mutate(length = st_length(geometry))

  return(sum(network_redux_shapes$length))
}
