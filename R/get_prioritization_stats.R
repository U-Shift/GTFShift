#' Get prioritization stats
#'
#' Get statistics about lane prioritization
#'
#' @param lane_prioritization sf data.frame. Lane prioritization.
#' @param weight Character. Weight to use for weighted mean. Accepted values: "length", "frequency".
#' @param metric_crs Integer or character (Default 3857). Projected CRS used to compute lengths in meters.
#'
#' @returns List with statistics about lane prioritization, with the following attributes:
#' \describe{
#'   \item{extension}{Total length of the prioritized network, in meters.}
#'   \item{extension_bus_lane}{Total length of the bus lane segments, in meters.}
#'   \item{speed_avg}{Average speed of the prioritized network, in km/h.}
#'   \item{speed_min}{Minimum speed of the prioritized network, in km/h.}
#'   \item{speed_max}{Maximum speed of the prioritized network, in km/h.}
#'   \item{n_lanes_circulation_avg}{Average number of lanes in the prioritized network.}
#'   \item{n_lanes_circulation_min}{Minimum number of lanes in the prioritized network.}
#'   \item{n_lanes_circulation_max}{Maximum number of lanes in the prioritized network.}
#' }
#'
#' @examples
#' \dontrun{
#' lane_prioritization <- GTFShift::prioritize_lanes(gtfs, q)
#' stats <- GTFShift::get_prioritization_stats(lane_prioritization)
#' }
#'
#' @import dplyr
#' @import sf
#'
#' @export
get_prioritization_stats <- function(
  lane_prioritization,
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

    prioritization_internal <- lane_prioritization |>
        st_as_sf() |>
        st_transform(crs = metric_crs)

    geom_col <- st_geometry(prioritization_internal)
    prioritization_internal <- prioritization_internal |>
        mutate(
            length = units::drop_units(st_length(geom_col))
        ) |>
        st_drop_geometry()
    stats <- list()

    # Compute bus lane extension
    stats$extension <- sum(prioritization_internal$length, na.rm = TRUE)
    stats$extension_bus_lane <- sum(
        prioritization_internal |> filter(is_bus_lane) |> pull(length),
        na.rm = TRUE
    )

    # Compute average speed, weighted by chosen weight
    if ("speed_avg" %in% names(prioritization_internal)) {
        stats$speed_avg <- weighted.mean(prioritization_internal$speed_avg, prioritization_internal[[weight]], na.rm = TRUE)
        stats$speed_min <- min(prioritization_internal$speed_avg, na.rm = TRUE)
        stats$speed_max <- max(prioritization_internal$speed_avg, na.rm = TRUE)
    }

    # Compute number of lanes, weighted by chosen weight
    stats$n_lanes_circulation_avg <- weighted.mean(prioritization_internal$n_lanes_circulation, prioritization_internal[[weight]], na.rm = TRUE)
    stats$n_lanes_circulation_min <- min(prioritization_internal$n_lanes_circulation, na.rm = TRUE)
    stats$n_lanes_circulation_max <- max(prioritization_internal$n_lanes_circulation, na.rm = TRUE)

    stats$n_lanes_parking_avg <- weighted.mean(prioritization_internal$n_lanes_parking, prioritization_internal[[weight]], na.rm = TRUE)
    stats$n_lanes_parking_min <- min(prioritization_internal$n_lanes_parking, na.rm = TRUE)
    stats$n_lanes_parking_max <- max(prioritization_internal$n_lanes_parking, na.rm = TRUE)

    return(stats)
}
