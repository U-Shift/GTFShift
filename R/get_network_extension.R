#' Get network routes extension
#'
#' Get total extension of GTFS feed routes
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param route_identifier. String. (Default \code{"route_id"}). routes.txt attribute that identifies routes. Accepted values: route_id, route_short_name, route_long_name.
#' @param direction_wise Boolean (Default \code{TRUE}). If TRUE, extension considers sum of both directions. Otherwise, only one direction is considered.
#' @param unified Boolean (Default \code{FALSE}). If TRUE, overlapping route segments are only counted once in the total extension.
#' @param date Date (Default \code{GTFShift::calendar_nextBusinessWednesday()}). Reference date to consider when analyzing the GTFS file.
#' @param use_osm_routes osmdata::opq (Default NA). If overpass query for transit network is defined, analysis is performed considering OSM route geometry, using \code{GTFShift::osm_shapes_to_routes}.
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
get_network_extension <- function(
    gtfs,
    route_identifier = "route_id",
    direction_wise = TRUE,
    unified = FALSE,
    date = GTFShift::calendar_nextBusinessWednesday(),
    use_osm_routes = NA,
    overline = FALSE
) {

  # 0. Validations
  if (!(route_identifier %in% c("route_id", "route_short_name", "route_long_name"))) {
    stop("route_identifier should be one of: route_id, route_short_name or route_long_name")
  }

  # Compute hourly frequencies for each route
  length(unique(gtfs$routes$route_id))
  network = gtfs |> GTFShift::get_route_frequency_hourly(date = date, use_osm_routes = use_osm_routes, overline = FALSE)
  length(unique(network$route_id))

  # Get unique shapes
  shapes_unique = network |>
    st_drop_geometry() |>
    select(shape_id) |>
    distinct() |>
    left_join(network, by = "shape_id", multiple="first")

  # Compute daily frequencies per route shape
  network_redux = network |>
    st_drop_geometry() |>
    group_by(.data[[route_identifier]], direction_id, shape_id) |>
    summarise(frequency_day = sum(frequency)) |>
    ungroup()

  # Get shape with max frequencies per route
  network_redux_max = network_redux |>
    # Get max frequency shape per route (and direction, if direction_wise=TRUE)
    group_by(.data[[route_identifier]], shape_id, !!!if (direction_wise) rlang::syms("direction_id")) |>
    summarise(frequency_max = max(frequency_day)) |>
    # Get shape with max frequency per route (and direction, if direction_wise=TRUE)
    group_by(
      .data[[route_identifier]],
      !!!if (direction_wise) rlang::syms("direction_id")
    ) |>
    slice_max(order_by = frequency_max, n = 1, with_ties = FALSE) |>
    ungroup()

  # Join with the original network to get the shapes and compute its distance
  network_redux_shapes = network_redux_max |>
    left_join(shapes_unique, by = "shape_id") |>
    st_as_sf() |>
    st_transform(crs = 3857) |> # For units in meters
    mutate(length = st_length(geometry))

  # Compute unified network extension
  if (unified) {
    network_union = network_redux_shapes |>
      st_union() |>
      stplanr::line_cast() |>
      st_as_sf() |>
      mutate(length = st_length(x))
    return(sum(network_union$length))
  }

  return(sum(network_redux_shapes$length))
}
