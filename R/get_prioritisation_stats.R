#' Get prioritisation stats
#'
#' Get statistics about lane prioritisation
#'
#' @param lane_prioritisation sf data.frame. Lane prioritisation.
#' @param weight Character. Weight to use for weighted mean. Accepted values: "length", "frequency".
#' @param metric_crs Integer or character (Default 3857). Projected CRS used to compute lengths in meters.
#'
#' @returns List with statistics about lane prioritisation, with the following attributes:
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
#' @examples
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
#' stats <- GTFShift::get_prioritisation_stats(lane_prioritisation, metric_crs = 3763)
#' 
#' data.frame(metric = names(stats), value = unlist(stats, use.names = FALSE))
#'
#' @import dplyr
#' @import sf
#' @importFrom stats weighted.mean
#' @importFrom rlang .data
#'
#' @export
get_prioritisation_stats <- function(
  lane_prioritisation,
  weight = c("length", "frequency"),
  metric_crs = 3857
) {
    metric_crs_is_default <- missing(metric_crs)
    weight <- match.arg(weight)
    metric_crs <- suppressWarnings(sf::st_crs(metric_crs))
    if (is.na(metric_crs)) {
        stop("metric_crs should be a valid CRS value (e.g., 3857 or 'EPSG:3857')")
    }
    if (metric_crs_is_default) {
        warning(
            "Using default metric_crs (EPSG:3857). Consider setting metric_crs to a projected CRS better suited to your local context for more accurate distance calculations.",
            call. = FALSE
        )
    }

    prioritisation_internal <- lane_prioritisation |>
        st_as_sf() |>
        st_transform(crs = metric_crs)

    geom_col <- st_geometry(prioritisation_internal)
    prioritisation_internal <- prioritisation_internal |>
        mutate(
            length = as.numeric(st_length(geom_col))
        ) |>
        st_drop_geometry()
    stats <- list()

    # Compute bus lane extension
    stats$extension <- sum(prioritisation_internal$length, na.rm = TRUE)
    stats$extension_bus_lane <- sum(
        prioritisation_internal |> filter(.data$is_bus_lane) |> pull(.data$length),
        na.rm = TRUE
    )

    # Compute average speed, weighted by chosen weight
    if ("speed_avg" %in% names(prioritisation_internal)) {
        stats$speed_avg <- weighted.mean(prioritisation_internal$speed_avg, prioritisation_internal[[weight]], na.rm = TRUE)
        stats$speed_min <- min(prioritisation_internal$speed_avg, na.rm = TRUE)
        stats$speed_max <- max(prioritisation_internal$speed_avg, na.rm = TRUE)
    }

    # Compute number of lanes, weighted by chosen weight
    stats$n_lanes_circulation_avg <- weighted.mean(prioritisation_internal$n_lanes_circulation, prioritisation_internal[[weight]], na.rm = TRUE)
    stats$n_lanes_circulation_min <- min(prioritisation_internal$n_lanes_circulation, na.rm = TRUE)
    stats$n_lanes_circulation_max <- max(prioritisation_internal$n_lanes_circulation, na.rm = TRUE)

    stats$n_lanes_parking_avg <- weighted.mean(prioritisation_internal$n_lanes_parking, prioritisation_internal[[weight]], na.rm = TRUE)
    stats$n_lanes_parking_min <- min(prioritisation_internal$n_lanes_parking, na.rm = TRUE)
    stats$n_lanes_parking_max <- max(prioritisation_internal$n_lanes_parking, na.rm = TRUE)

    return(stats)
}
