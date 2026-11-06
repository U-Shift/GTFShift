#' Extend prioritisation with GTFS-RT based speed metrics
#'
#' This function extends lane segment indicators for prioritisation with speed metrics produced with GTFS-RT data.
#'
#' @param lane_prioritisation sf data.frame. Result of \code{GTFShift::prioritise_lanes()}
#' @param rt_collection sf data.frame. GTFS-RT data collection. Must include \code{speed} column.
#' @param rt_current_status Character vector (Default \code{c("IN_TRANSIT_TO")}). If the \code{current_status} column is present in the \code{rt_collection} data, only points with \code{current_status} in this vector are considered.
#' @param lane_buffer numeric (Default 15). Buffer distance (in meters) to create around lane segments to capture nearby GTFS-RT points.
#' @param metric_crs Integer or character (Default NULL). Optional projected CRS used to apply lane buffer distances in meters. If NULL, spherical geometry (s2) is used by default.
#'
#' @details
#' Extends the \code{lane_prioritisation} data with speed metrics calculated from the GTFS-RT data points that fall within a buffer around each lane segment.
#'
#' If GTFS-RT data does not provide speed information, it can be inferred from the progression of position updates through time using \code{GTFShift::rt_average_speed()}.
#'
#' Refer to \code{GTFShift::rt_collect_json()} or \code{GTFShift::rt_collect_protobuf()} for details on GTFS-RT data collection.
#'
#'
#' @returns sf data.frame. Extended lane prioritisation with the following columns:
#' \describe{
#'   \item{speed_avg}{The average speed of the vehicles on the way.}
#'   \item{speed_median}{The median speed of the vehicles on the way.}
#'   \item{speed_p15}{The 15th percentile speed of the vehicles on the way.}
#'   \item{speed_p25}{The 25th percentile speed of the vehicles on the way.}
#'   \item{speed_p75}{The 75th percentile speed of the vehicles on the way.}
#'   \item{speed_p85}{The 85th percentile speed of the vehicles on the way.}
#'   \item{speed_count}{The number of speed observations on the way.}
#' }
#'
#' @examplesIf nzchar(Sys.which("osmium"))
#' # Subset GTFS for one route only, for demo purposes
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_tcb_sample.zip",
#'   package = "GTFShift"
#' ))
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
#' # Extend with GTFS-RT data collection
#' rt_collect_file <- system.file(
#'   "extdata/samples", "gtfs_rt_sample_tcb_4_4-CS-TERM.csv",
#'   package = "GTFShift"
#' )
#' rt_collection <- read.csv(rt_collect_file) |>
#'   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
#'
#' lane_prioritisation_extended <- GTFShift::rt_extend_prioritisation(
#'   lane_prioritisation = lane_prioritisation,
#'   rt_collection = rt_collection
#' )
#'
#' head(
#'   lane_prioritisation_extended |>
#'     sf::st_drop_geometry() |>
#'     dplyr::filter(!is.na(speed_count)) |>
#'     dplyr::select(way_osm_id, speed_avg, speed_count)
#' )
#'
#' @importFrom callr r_bg
#' @importFrom rlang .data
#' @srrstats {G2.0a} Documents vector inputs such as `rt_current_status`.
#' @srrstats {G2.13} Validates presence of required `way_osm_id` and `speed` columns and valid `metric_crs`.
#' @srrstats {G2.15} Passes `na.rm = TRUE` to `mean()`, `stats::median()`, and `stats::quantile()`.
#' @srrstats {EA4.0} Returns an `sf` data frame extending `lane_prioritisation`.
#' @srrstats {SP1.0} Applies to both curvilinear/geographic (`metric_crs = NULL`) and Cartesian/projected (`metric_crs`) spatial domains.
#' @srrstats {SP1.1} Operates on two-dimensional (`XY`) road segment and vehicle position geometries.
#' @srrstats {SP2.0b} Errors when `metric_crs` is invalid or required spatial table columns are missing.
#' @srrstats {SP2.4} Uses `sf::st_crs()` and `sf::st_transform()` compliant with `PROJ >= 6`.
#' @srrstats {SP2.4a} Validates `metric_crs` using `WKT2` via `validate_metric_crs()`, rejecting PROJ4-strings.
#' @srrstats {SP2.8} Transforms buffer geometries to `rt_collection_crs` prior to `sf::st_join()`.
#' @srrstats {SP2.9} Preserves all attributes and geometry of `lane_prioritisation` in the returned `sf` object.
#' @srrstats {SP4.0a} Returns an `sf` object matching the class of `lane_prioritisation`.
#' @srrstats {SP4.2} Explicitly documents all added speed columns in `@returns`.
#'
#' @export
rt_extend_prioritisation <- function(
  lane_prioritisation,
  rt_collection,
  rt_current_status = c("IN_TRANSIT_TO"),
  lane_buffer = 15, # in meters
  metric_crs = NULL
) {
  # 1. Validate inputs
  if (!inherits(lane_prioritisation, "sf")) {
    stop("lane_prioritisation must be an sf object")
  }
  if (!inherits(rt_collection, "sf")) {
    stop("rt_collection must be an sf object")
  }
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
  metric_crs <- validate_metric_crs(metric_crs)
  rt_collection_crs <- sf::st_crs(rt_collection)

  # Display feedback
  if (!requireNamespace("progress", quietly = TRUE)) {
    stop("Package 'progress' is required for this function. Install it with: install.packages('progress')")
  }
  pb <- progress::progress_bar$new( # Track progress
    format = "Extending prioritisation with GTFS-RT metrics [:bar] :percent :spin elapsed=:elapsed",
    clear = FALSE, show_after = 0
  )
  pb$update(0)

  # 2. Get only updates IN_TRANSIT
  if (!is.null(rt_current_status) && "current_status" %in% colnames(rt_collection)) {
    rt_collection <- rt_collection |>
      dplyr::filter(.data$current_status %in% rt_current_status)
  }
  pb$update(0.166)

  # 3. Get unique lane segments (to optimize spatial join)
  job <- callr::r_bg(function(lane_prioritisation) { # update spinner while blocking method call
    requireNamespace("sf", quietly = TRUE)
    return(lane_prioritisation |>
      dplyr::distinct(.data$way_osm_id, .keep_all = TRUE) |>
      dplyr::select("way_osm_id"))
  }, args = list(lane_prioritisation))
  while (job$is_alive()) {
    pb$tick(0)
    Sys.sleep(0.1)
  }
  lanes_unique <- job$get_result()
  pb$update(0.333)

  # 4. Create buffers in lane segments to overlap with updates
  job <- callr::r_bg(function(lanes_unique, lane_buffer, metric_crs, rt_collection_crs) { # update spinner while blocking method call
    if (!is.null(metric_crs)) {
      lanes_unique <- sf::st_transform(lanes_unique, crs = metric_crs)
    }
    return(sf::st_buffer(
      lanes_unique,
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
      lane_buffers |> dplyr::select("way_osm_id"),
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
    requireNamespace("rlang", quietly = TRUE)
    return(overlap |>
      dplyr::group_by(.data$way_osm_id) |>
      dplyr::summarise(
        speed_avg = mean(.data[[rt_attr_speed]], na.rm = TRUE),
        speed_median = stats::median(.data[[rt_attr_speed]], na.rm = TRUE),
        speed_p15 = stats::quantile(.data[[rt_attr_speed]], probs = 0.15, na.rm = TRUE),
        speed_p25 = stats::quantile(.data[[rt_attr_speed]], probs = 0.25, na.rm = TRUE),
        speed_p75 = stats::quantile(.data[[rt_attr_speed]], probs = 0.75, na.rm = TRUE),
        speed_p85 = stats::quantile(.data[[rt_attr_speed]], probs = 0.85, na.rm = TRUE),
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
    requireNamespace("sf", quietly = TRUE)
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
