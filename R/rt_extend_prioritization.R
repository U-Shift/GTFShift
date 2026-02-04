#' Extend prioritization with GTFS-RT metrics
#'
#' This function extends lane segment indicators for prioritization with metrics produced with GTFS-RT data.
#'
#' @param lane_prioritization sf data.frame. Result of \code{GTFShift::prioritize_lanes()}
#' @param rt_collection sf data.frame. GTFS-RT data collection. Must include \code{speed} column.
#' @param lane_buffer numeric (Default 15). Buffer distance (in meters) to create around lane segments to capture nearby GTFS-RT points.
#'
#' @details
#' Extends the \code{lane_prioritization} data with speed metrics calculated from the GTFS-RT data points that fall within a buffer around each lane segment.
#'
#' Refer to \code{GTFShift::rt_collect_json()} or \code{GTFShift::rt_collect_protobuf()} for details on GTFS-RT data collection.
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
#' @examples
#' \dontrun{
#' rt_collect_file <- "gtfs_rt_data.csv"
#' GTFShift::rt_collect_json("https://api.example.com/gtfs-rt", rt_collect_file)
#' lane_prioritization <- GTFShift::prioritize_lanes(gtfs, osm_query)
#'
#' rt_collection <- read.csv(rt_collect_file) |> sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
#' lane_prioritization_extended <- GTFShift::rt_extend_prioritization(
#'   lane_prioritization = lane_prioritization,
#'   rt_collection = rt_collection
#' )
#' }
#'
#' @import progress
#' @import dplyr
#' @import callr
#'
#' @export
rt_extend_prioritization <- function(
  lane_prioritization,
  rt_collection,
  lane_buffer = 15 # in meters
) {
  # 1. Validate inputs
  required_cols = c("way_osm_id")
  missing_cols = setdiff(required_cols, colnames(lane_prioritization))
  if (length(missing_cols) > 0) {
    stop(paste("lane_prioritization is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  rt_attr_speed = "speed"
  required_rt_cols = c(rt_attr_speed)
  missing_rt_cols = setdiff(required_rt_cols, colnames(rt_collection))
  if (length(missing_rt_cols) > 0) {
    stop(paste("rt_collection is missing required columns:", paste(missing_rt_cols, collapse = ", ")))
  }

  # Display feedback
  pb <- progress::progress_bar$new( # Track progress
    format = "Extending prioritization with GTFS-RT metrics [:bar] :percent :spin elapsed=:elapsed",
    clear = FALSE, show_after=0
  )
  pb$update(0)

  # 2. Get only updates IN_TRANSIT
  if ("current_status" %in% colnames(rt_collection)) {
    rt_collection <- rt_collection %>%
      dplyr::filter(current_status == "IN_TRANSIT_TO")
  }
  pb$update(0.166)

  # 3. Get unique lane segments (to optimize spatial join)
  job <- callr::r_bg(function(lane_prioritization) { # update spinner while blocking method call
    library(sf)
    return(lane_prioritization |>
            dplyr::distinct(way_osm_id, .keep_all = TRUE) |>
            dplyr::select(way_osm_id))
  }, args=list(lane_prioritization))
  while (job$is_alive()) { pb$tick(0); Sys.sleep(0.1) }
  lanes_unique <- job$get_result()
  pb$update(0.333)

  # 4. Create buffers in lane segments to overlap with updates
  job <- callr::r_bg(function(lanes_unique, lane_buffer) { # update spinner while blocking method call
    return(sf::st_buffer(
            sf::st_transform(lanes_unique, crs=3857),
            dist=lane_buffer
          ) |> sf::st_transform(crs=sf::st_crs(lanes_unique)))
  }, args=list(lanes_unique, lane_buffer))
  while (job$is_alive()) { pb$tick(0); Sys.sleep(0.1) }
  lane_buffers <- job$get_result()
  pb$update(0.5)

  # 4. Spatial join between lane buffers and rt_collection points
  job <- callr::r_bg(function(rt_collection, lane_buffers) { # update spinner while blocking method call
    return(sf::st_join(
            rt_collection,
            lane_buffers |> dplyr::select(way_osm_id),
            left = FALSE,
            join = sf::st_within
          ) |> sf::st_drop_geometry())
  }, args=list(rt_collection, lane_buffers))
  while (job$is_alive()) { pb$tick(0); Sys.sleep(0.1) }
  overlap <- job$get_result()
  pb$update(0.666)

  # 5. Aggregate speed metrics by way_osm_id
  job <- callr::r_bg(function(overlap, rt_attr_speed) { # update spinner while blocking method call
    return(overlap |>
             dplyr::group_by(way_osm_id) |>
             dplyr::summarise(
               speed_avg = mean(.data[[rt_attr_speed]], na.rm = TRUE),
               speed_median = stats::median(.data[[rt_attr_speed]], na.rm = TRUE),
               speed_p25 = stats::quantile(.data[[rt_attr_speed]], probs = 0.25, na.rm = TRUE),
               speed_p75 = stats::quantile(.data[[rt_attr_speed]], probs = 0.75, na.rm = TRUE),
               speed_count = dplyr::n()
             ) |>
             dplyr::ungroup())
  }, args=list(overlap, rt_attr_speed))
  while (job$is_alive()) { pb$tick(0); Sys.sleep(0.1) }
  speed_metrics <- job$get_result()
  pb$update(0.833)

  # 6. Join speed metrics back to lane_prioritization
  job <- callr::r_bg(function(lane_prioritization, speed_metrics) { # update spinner while blocking method call
    library(sf)
    return(lane_prioritization |>
             dplyr::left_join(speed_metrics, by = "way_osm_id"))
  }, args=list(lane_prioritization, speed_metrics))
  while (job$is_alive()) { pb$tick(0); Sys.sleep(0.1) }
  lane_prioritization_extended <- job$get_result()
  pb$update(1)
  pb$terminate()

  return(lane_prioritization_extended)
}
