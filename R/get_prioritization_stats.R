#' Get prioritization stats
#'
#' Get statistics about lane prioritization
#'
#' @param lane_prioritization sf data.frame. Lane prioritization.
#' @param weight Character. Weight to use for weighted mean. Accepted values: "length", "frequency".
#'
#' @returns List with statistics about lane prioritization, with the following attributes:
#' \describe{
#'   \item{extension}{Total length of the prioritized network, in meters.}
#'   \item{extension_bus_lane}{Total length of the bus lane segments, in meters.}
#'   \item{speed_avg}{Average speed of the prioritized network, in km/h.}
#'   \item{speed_min}{Minimum speed of the prioritized network, in km/h.}
#'   \item{speed_max}{Maximum speed of the prioritized network, in km/h.}
#'   \item{n_lanes_avg}{Average number of lanes in the prioritized network.}
#'   \item{n_lanes_min}{Minimum number of lanes in the prioritized network.}
#'   \item{n_lanes_max}{Maximum number of lanes in the prioritized network.}
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
  weight = c("length", "frequency")
) {
    weight <- match.arg(weight)
    prioritization_internal <- lane_prioritization |>
        st_as_sf() |>
        st_transform(crs = 3857)

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
    stats$n_lanes_avg <- weighted.mean(prioritization_internal$n_lanes, prioritization_internal[[weight]], na.rm = TRUE)
    stats$n_lanes_min <- min(prioritization_internal$n_lanes, na.rm = TRUE)
    stats$n_lanes_max <- max(prioritization_internal$n_lanes, na.rm = TRUE)

    return(stats)
}
