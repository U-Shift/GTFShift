#' Extend prioritization with GTFS-RT metrics
#'
#' This function extends lane segment indicators for prioritization with metrics produced with GTFS-RT data.
#'
#' @param lane_prioritization Result of \code{GTFShift::prioritize_lanes()}
#' @param rt_collection A data.frame containing GTFS-RT data collection. Must include `speed`, `latitude` and `longitude` columns.
#'
#' @details
#' ...
#'
#'
#' @returns The \code{lane_prioritization} \code{sf} \code{data.frame}, extended with the following columns:
#' \itemize{
#'  \item \code{speed_avg}, the average speed of the vehicles on the way.
#'  \item \code{speed_median}, the median speed of the vehicles on the way.
#'  \item \code{speed_p25}, the 25th percentile speed of the vehicles on the way.
#'  \item \code{speed_p75}, the 75th percentile speed of the vehicles on the way.
#'  \item \code{speed_count}, the number of speed observations on the way.
#' }
#'
rt_extend_prioritization <- function(
  lane_prioritization,
  rt_collection
) {
  # Validate inputs
  required_cols = c("way_osm_id", "geometry")
  missing_cols = setdiff(required_cols, colnames(lane_prioritization))
  if (length(missing_cols) > 0) {
    stop(paste("lane_prioritization is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  rt_attr_speed = "speed"
  rt_attr_lat = "latitude"
  rt_attr_lon = "longitude"
  required_rt_cols = c(rt_attr_speed, rt_attr_lat, rt_attr_lon)
  missing_rt_cols = setdiff(required_rt_cols, colnames(rt_collection))
  if (length(missing_rt_cols) > 0) {
    stop(paste("rt_collection is missing required columns:", paste(missing_rt_cols, collapse = ", ")))
  }

  # TODO! Implement
}
