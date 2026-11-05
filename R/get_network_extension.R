#' Get network routes extension
#'
#' Get total extension of GTFS feed routes
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param route_identifier String. (Default \code{"route_id"}). routes.txt attribute that identifies routes. Accepted values: route_id, route_short_name, route_long_name.
#' @param direction_wise Boolean (Default \code{TRUE}). If TRUE, extension considers sum of both directions. Otherwise, only one direction is considered.
#' @param unified Boolean (Default \code{FALSE}). If TRUE, overlapping route segments are only counted once in the total extension.
#' @param date Date (Default \code{GTFShift::calendar_nextBusinessWednesday()}). Reference date to consider when analyzing the GTFS file.
#' @param use_osm_routes osmdata::opq (Default NA). If overpass query for transit network is defined, analysis is performed considering OSM route geometry, using \code{GTFShift::osm_shapes_to_routes}.
#' @param metric_crs Integer or character (Default NULL). Optional projected CRS used to compute route lengths in meters. If NULL, spherical geometry (s2) is used by default.
#'
#' @details
#' This method calculates the sum of the GTFS feed routes length, considering, for each, the shape of the variant with the highest frequency for the given date
#' (using \code{GTFShift::get_route_frequency_hourly()}).
#' For a detailed example, see the \code{vignette("analyse")}.
#'
#' @returns Numeric. The routes extension, in meters.
#'
#' @examples
#' # Load GTFS
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_tcb_sample.zip",
#'   package = "GTFShift"
#' ))
#'
#' # Get route extension
#' GTFShift::get_network_extension(
#'   gtfs,
#'   date = gtfs$calendar$start_date[1]
#' )
#'
#' @seealso \code{GTFShift::get_route_frequency_hourly()}
#'
#' @import dplyr
#' @import sf
#' @importFrom rlang .data
#' @srrstats {G2.2} Validates univariate `route_identifier` parameter.
#' @srrstats {G2.3} Validates character input `route_identifier`.
#' @srrstats {G2.3a} Restricts `route_identifier` to `"route_id"`, `"route_short_name"`, or `"route_long_name"`.
#' @srrstats {SP1.0} Applies to both curvilinear/geographic (`metric_crs = NULL`) and Cartesian/projected (`metric_crs`) spatial domains.
#' @srrstats {SP1.1} Operates on two-dimensional (`XY`) route `LINESTRING` geometries.
#' @srrstats {SP2.4} Uses `sf::st_crs()` and `sf::st_transform()` compliant with `PROJ >= 6`.
#' @srrstats {SP2.4a} Validates `metric_crs` via `WKT2` (`sf::st_crs()`).
#' @srrstats {SP4.1} Preserves `units` returned by `sf::st_length()` in the summed route extension.
#' @srrstats {SP4.2} Documents the returned numeric/units extension in meters.
#'
#' @export
get_network_extension <- function(
  gtfs,
  route_identifier = "route_id",
  direction_wise = TRUE,
  unified = FALSE,
  date = GTFShift::calendar_nextBusinessWednesday(),
  use_osm_routes = NA,
  metric_crs = NULL
) {
  # 0. Validations
  if (!(route_identifier %in% c("route_id", "route_short_name", "route_long_name"))) {
    stop("route_identifier should be one of: route_id, route_short_name or route_long_name")
  }
  if (!is.null(metric_crs)) {
    metric_crs <- suppressWarnings(sf::st_crs(metric_crs))
    if (is.na(metric_crs)) {
      stop("metric_crs should be a valid CRS value (e.g., 3857 or 'EPSG:3857')")
    }
  }

  # Compute hourly frequencies for each route
  network <- gtfs |> GTFShift::get_route_frequency_hourly(date = date, use_osm_routes = use_osm_routes, overline = FALSE)

  # Get unique shapes
  shapes_unique <- network |>
    st_drop_geometry() |>
    select("shape_id") |>
    distinct() |>
    left_join(network, by = "shape_id", multiple = "first")

  # Compute daily frequencies per route shape
  network_redux <- network |>
    st_drop_geometry() |>
    group_by(.data[[route_identifier]], .data$direction_id, .data$shape_id) |>
    summarise(frequency_day = sum(.data$frequency)) |>
    ungroup()

  # Get shape with max frequencies per route
  network_redux_max <- network_redux |>
    # Get max frequency shape per route (and direction, if direction_wise=TRUE)
    group_by(.data[[route_identifier]], .data$shape_id, !!!if (direction_wise) rlang::syms("direction_id")) |>
    summarise(frequency_max = max(.data$frequency_day)) |>
    # Get shape with max frequency per route (and direction, if direction_wise=TRUE)
    group_by(
      .data[[route_identifier]],
      !!!if (direction_wise) rlang::syms("direction_id")
    ) |>
    slice_max(order_by = .data$frequency_max, n = 1, with_ties = FALSE) |>
    ungroup()

  # Join with the original network to get the shapes and compute its distance
  network_redux_shapes <- network_redux_max |>
    left_join(shapes_unique, by = "shape_id") |>
    st_as_sf()
  if (!is.null(metric_crs)) {
    network_redux_shapes <- network_redux_shapes |>
      st_transform(crs = metric_crs) # For units in meters
  }

  geom_col <- st_geometry(network_redux_shapes)
  network_redux_shapes <- network_redux_shapes |> mutate(length = st_length(geom_col))

  # Compute unified network extension
  if (unified) {
    if (!requireNamespace("stplanr", quietly = TRUE)) {
      stop("Package 'stplanr' is required when unified=TRUE. Install it with: install.packages('stplanr')")
    }
    network_union <- network_redux_shapes |>
      st_union() |>
      stplanr::line_cast() |>
      st_as_sf()
    return(sum(st_length(st_geometry(network_union))))
  }

  return(sum(network_redux_shapes$length))
}
