#' Get prioritisation stats
#'
#' Get statistics about lane prioritisation
#'
#' @param lane_prioritisation sf data.frame. Lane prioritisation.
#' @param weight Character. Weight to use for weighted mean. Accepted values: "length", "frequency".
#' @param metric_crs Integer or character (Default NULL). Optional projected CRS used to compute lengths in meters. If NULL, spherical geometry (s2) is used by default.
#'
#' @returns List. Statistics about lane prioritisation, with the following attributes:
#' \describe{
#'   \item{extension}{Total length of the prioritised network, in meters.}
#'   \item{extension_bus_lane}{Total length of the bus lane segments, in meters.}
#'   \item{speed_avg}{Average speed of the prioritised network, in km/h.}
#'   \item{speed_min}{Minimum speed of the prioritised network, in km/h.}
#'   \item{speed_max}{Maximum speed of the prioritised network, in km/h.}
#'   \item{n_lanes_circulation_avg}{Average number of lanes in the prioritised network.}
#'   \item{n_lanes_circulation_min}{Minimum number of lanes in the prioritised network.}
#'   \item{n_lanes_circulation_max}{Maximum number of lanes in the prioritised network.}
#' }
#'
#' @examplesIf nzchar(Sys.which("osmium"))
#' # Subset GTFS for one route only, for demo purposes
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_tcb_sample.zip", package = "GTFShift")
#' )
#' gtfs <- GTFShift::filter_by_route_name(gtfs, c("4"))
#' 
#' # Build query and prepare osm extract (possible to use API as alternative)
#' q <- osmdata::opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |> 
#'   osmdata::add_osm_feature(key = "route", value = "bus") |> 
#'   osmdata::add_osm_feature(key = "operator", value = "Transportes Colectivos do Barreiro")
#' osm_file <- system.file("extdata/samples", "osmextract_tcb_network.pbf", package = "GTFShift")
#' 
#' # Prioritise lanes
#' lane_prioritisation <- GTFShift::prioritise_lanes(
#'   gtfs, q, 
#'   osm_file = osm_file,  
#'   date = gtfs$calendar$start_date[1]
#' )
#' 
#' # Get statistics for prioritisation
#' stats <- GTFShift::get_prioritisation_stats(lane_prioritisation)
#' 
#' data.frame(metric = names(stats), value = unlist(stats, use.names = FALSE))
#'
#' @import dplyr
#' @import sf
#' @importFrom stats weighted.mean
#' @importFrom rlang .data
#' @srrstats {G2.1} Validates that `lane_prioritisation` is an `sf` object and `weight` is character.
#' @srrstats {G2.2} Restricts `weight` to a single univariate choice via `match.arg()`.
#' @srrstats {G2.3} Validates character parameter `weight`.
#' @srrstats {G2.3a} Uses `match.arg()` to permit only `"length"` or `"frequency"`.
#' @srrstats {G2.3b} Normalizes `weight` with `tolower()` for case-insensitive matching.
#' @srrstats {G2.4b} Uses `as.numeric()` explicitly when converting `sf::st_length()` units to numeric lengths.
#' @srrstats {G2.11} Supports `units`-classed columns in `lane_prioritisation` (e.g., `speed_avg` or `frequency`).
#' @srrstats {G2.12} Ignores extra `list` columns in `lane_prioritisation` when computing summary metrics.
#' @srrstats {G2.15} Passes `na.rm = TRUE` to all `sum()`, `weighted.mean()`, `min()`, and `max()` calls.
#' @srrstats {EA3.0} Automates reporting of network-level bus lane extension, speed, and lane count statistics.
#' @srrstats {EA3.1} Enables standardized comparison of prioritised transit networks weighted by length or frequency.
#' @srrstats {SP1.0} Applies to both curvilinear/geographic (`metric_crs = NULL`) and Cartesian/projected (`metric_crs`) spatial domains.
#' @srrstats {SP1.1} Operates on two-dimensional (`XY`) road segment geometries.
#' @srrstats {SP2.4} Uses `sf::st_crs()` and `sf::st_transform()` compliant with `PROJ >= 6`.
#' @srrstats {SP2.4a} Validates `metric_crs` via `validate_metric_crs()`, explicitly rejecting PROJ4-strings.
#' @srrstats {SP2.8} Uses `preprocess_spatial_crs()` as a single pre-processing routine to validate and transform `lane_prioritisation`.
#' @srrstats {SP2.9} Preserves non-spatial attributes during coordinate transformation.
#' @srrstats {SP4.1} Preserves `units` on `speed_avg`, `speed_min`, and `speed_max` when input `speed_avg` has `units` attributes, and documents physical units in `@returns`.
#' @srrstats {SP4.2} Explicitly documents the returned `list` structure and all elements.
#'
#' @export
get_prioritisation_stats <- function(
  lane_prioritisation,
  weight = c("length", "frequency"),
  metric_crs = NULL
) {
    if (!inherits(lane_prioritisation, "sf")) {
        stop("lane_prioritisation must be an sf object")
    }
    if (!is.character(weight) || length(weight) < 1) {
        stop("weight must be a character string ('length' or 'frequency')")
    }
    weight <- match.arg(tolower(weight), c("length", "frequency"))
    metric_crs <- validate_metric_crs(metric_crs)

    prioritisation_internal <- preprocess_spatial_crs(
        lane_prioritisation,
        metric_crs = metric_crs,
        arg_name = "lane_prioritisation"
    )

    geom_col <- st_geometry(prioritisation_internal)
    prioritisation_internal <- prioritisation_internal |>
        mutate(
            length = as.numeric(st_length(geom_col))
        ) |>
        st_drop_geometry()
    stats <- list()
    weight_vals <- as.numeric(prioritisation_internal[[weight]])

    # Compute bus lane extension
    stats$extension <- sum(prioritisation_internal$length, na.rm = TRUE)
    stats$extension_bus_lane <- sum(
        prioritisation_internal |> filter(.data$is_bus_lane) |> pull(.data$length),
        na.rm = TRUE
    )

    # Compute average speed, weighted by chosen weight
    if ("speed_avg" %in% names(prioritisation_internal)) {
        speed_units <- if (inherits(prioritisation_internal$speed_avg, "units")) {
            units(prioritisation_internal$speed_avg)
        } else {
            NULL
        }
        speed_vals <- as.numeric(prioritisation_internal$speed_avg)
        stats$speed_avg <- weighted.mean(speed_vals, weight_vals, na.rm = TRUE)
        stats$speed_min <- min(speed_vals, na.rm = TRUE)
        stats$speed_max <- max(speed_vals, na.rm = TRUE)
        if (!is.null(speed_units)) {
            units(stats$speed_avg) <- speed_units
            units(stats$speed_min) <- speed_units
            units(stats$speed_max) <- speed_units
        }
    }

    # Compute number of lanes, weighted by chosen weight
    stats$n_lanes_circulation_avg <- weighted.mean(as.numeric(prioritisation_internal$n_lanes_circulation), weight_vals, na.rm = TRUE)
    stats$n_lanes_circulation_min <- min(as.numeric(prioritisation_internal$n_lanes_circulation), na.rm = TRUE)
    stats$n_lanes_circulation_max <- max(as.numeric(prioritisation_internal$n_lanes_circulation), na.rm = TRUE)

    stats$n_lanes_parking_avg <- weighted.mean(as.numeric(prioritisation_internal$n_lanes_parking), weight_vals, na.rm = TRUE)
    stats$n_lanes_parking_min <- min(as.numeric(prioritisation_internal$n_lanes_parking), na.rm = TRUE)
    stats$n_lanes_parking_max <- max(as.numeric(prioritisation_internal$n_lanes_parking), na.rm = TRUE)

    return(stats)
}
