#' Extend prioritisation with GTFS-RT based speed metrics
#'
#' This function extends lane segment indicators for prioritisation with speed metrics produced with GTFS-RT data.
#'
#' @param lane_prioritisation sf data.frame. Result of \code{GTFShift::prioritise_lanes()}
#' @param rt_collection sf data.frame. GTFS-RT data collection. Must include \code{speed} column.
#' @param rt_current_status Character vector (Default \code{c("IN_TRANSIT_TO")}). If the \code{current_status} column is present in the \code{rt_collection} data, only points with \code{current_status} in this vector are considered.
#' @param lane_buffer numeric (Default 15). Buffer distance (in meters) to create around lane segments to capture nearby GTFS-RT points.
#' @param metric_crs Integer or character (Default 3857). Projected CRS used to apply lane buffer distances in meters.
#'
#' @details
#' Extends the \code{lane_prioritisation} data with speed metrics calculated from the GTFS-RT data points that fall within a buffer around each lane segment.
#' 
#' If GTFS-RT data does not provide speed information, it can be inferred from the progression of position updates through time using \code{GTFShift::rt_average_speed()}.
#'
#' Refer to \code{GTFShift::rt_collect_json()} or \code{GTFShift::rt_collect_protobuf()} for details on GTFS-RT data collection.
#'
#'
#' @returns The \code{lane_prioritisation} \code{sf} \code{data.frame}, extended with the following columns:
#' \describe{
#'   \item{speed_avg}{The average speed of the vehicles on the way.}
#'   \item{speed_median}{The median speed of the vehicles on the way.}
#'   \item{speed_p25}{The 25th percentile speed of the vehicles on the way.}
#'   \item{speed_p75}{The 75th percentile speed of the vehicles on the way.}
#'   \item{speed_count}{The number of speed observations on the way.}
#' }
#'
#' @examples
#' # Subset GTFS for one route only, for demo purposes
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"))
#' gtfs <- GTFShift::filter_by_route_name(gtfs, c("4"))
#' 
#' # Build query and prepare osm extract (possible to use API as alternative)
#' q <- osmdata::opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |> osmdata::add_osm_feature(key = "route", value = "bus") |> osmdata::add_osm_feature(key = "operator", value = "Transportes Colectivos do Barreiro")
#' osm_file <- system.file("extdata/samples", "osmextract_tcb_network.pbf", package = "GTFShift")
#' 
#' # Prioritise lanes
#' lane_prioritisation <- GTFShift::prioritise_lanes(gtfs, q, osm_file = osm_file, date = gtfs$calendar$start_date[1])
#' 
#' # Extend with GTFS-RT data collection
#' rt_collect_file <- system.file("extdata/samples", "gtfs_rt_sample_tcb_4_4-CS-TERM.csv", package = "GTFShift")
#' rt_collection <- read.csv(rt_collect_file) |> sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
#' 
#' lane_prioritisation_extended <- GTFShift::rt_extend_prioritisation(
#'   lane_prioritisation = lane_prioritisation, 
#'   rt_collection = rt_collection, 
#'   metric_crs = 3763 # Make sure to addapt to the projection that better suits your location
#' )
#' 
#' head(
#'   lane_prioritisation_extended |> 
#'      sf::st_drop_geometry() |>
#'      dplyr::filter(!is.na(speed_count)) |> 
#'      dplyr::select(way_osm_id, speed_avg, speed_count)
#' )
#'
#' @importFrom progress progress_bar
#' @import dplyr
#' @importFrom callr r_bg
#'
#' @export
rt_extend_prioritisation <- function(
  lane_prioritisation,
  rt_collection,
  rt_current_status = c("IN_TRANSIT_TO"),
  lane_buffer = 15, # in meters
  metric_crs = 3857
) {
  metric_crs_is_default <- missing(metric_crs)
  # 1. Validate inputs
  required_cols <- c("way_osm_id")
  missing_cols <- setdiff(required_cols, colnames(lane_prioritisation))
  if (length(missing_cols) > 0) {
    stop(paste("lane_prioritisation is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  rt_attr_speed <- "speed"
  required_rt_cols <- c(rt_attr_speed)
  missing_rt_cols <- setdiff(required_rt_cols, colnames(rt_collection))
  if (length(missing_rt_cols) > 0) {
    stop(paste("rt_collection is missing required columns:", paste(missing_rt_cols, collapse = ", ")))
  }
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
  rt_collection_crs <- sf::st_crs(rt_collection)

  # Display feedback
  pb <- progress::progress_bar$new( # Track progress
    format = "Extending prioritisation with GTFS-RT metrics [:bar] :percent :spin elapsed=:elapsed",
    clear = FALSE, show_after = 0
  )
  pb$update(0)

  # 2. Get only updates IN_TRANSIT
  if (!is.null(rt_current_status) && "current_status" %in% colnames(rt_collection)) {
    rt_collection <- rt_collection %>%
      dplyr::filter(current_status %in% rt_current_status)
  }
  pb$update(0.166)

  # 3. Get unique lane segments (to optimize spatial join)
  job <- callr::r_bg(function(lane_prioritisation) { # update spinner while blocking method call
    library(sf)
    return(lane_prioritisation |>
      dplyr::distinct(way_osm_id, .keep_all = TRUE) |>
      dplyr::select(way_osm_id))
  }, args = list(lane_prioritisation))
  while (job$is_alive()) {
    pb$tick(0)
    Sys.sleep(0.1)
  }
  lanes_unique <- job$get_result()
  pb$update(0.333)

  # 4. Create buffers in lane segments to overlap with updates
  job <- callr::r_bg(function(lanes_unique, lane_buffer, metric_crs, rt_collection_crs) { # update spinner while blocking method call
    return(sf::st_buffer(
      sf::st_transform(lanes_unique, crs = metric_crs),
      dist = lane_buffer
    ) |> sf::st_transform(crs = rt_collection_crs))
  }, args = list(lanes_unique, lane_buffer, metric_crs, rt_collection_crs))
  while (job$is_alive()) {
    pb$tick(0)
    Sys.sleep(0.1)
  }
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
  }, args = list(rt_collection, lane_buffers))
  while (job$is_alive()) {
    pb$tick(0)
    Sys.sleep(0.1)
  }
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
  }, args = list(overlap, rt_attr_speed))
  while (job$is_alive()) {
    pb$tick(0)
    Sys.sleep(0.1)
  }
  speed_metrics <- job$get_result()
  pb$update(0.833)

  # 6. Join speed metrics back to lane_prioritisation
  job <- callr::r_bg(function(lane_prioritisation, speed_metrics) { # update spinner while blocking method call
    library(sf)
    return(lane_prioritisation |>
      dplyr::left_join(speed_metrics, by = "way_osm_id"))
  }, args = list(lane_prioritisation, speed_metrics))
  while (job$is_alive()) {
    pb$tick(0)
    Sys.sleep(0.1)
  }
  lane_prioritisation_extended <- job$get_result()
  pb$update(1)
  pb$terminate()

  return(lane_prioritisation_extended)
}
