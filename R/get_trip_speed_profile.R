#' Get trip speed profile from GTFS-RT speed estimates
#'
#' Computes aggregated speed metrics for trips or groups from the output of
#' \code{GTFShift::rt_average_speed()}. Aggregates by trip, route, and day
#' (customizable via the \code{by} parameter), calculating commercial speed
#' (distance between first and last updates along geometry divided by elapsed time),
#' alternative commercial speed (considering the 2nd and penultimate updates to avoid
#' terminal wait biases), and measures of centrality and spread for speed observations.
#'
#' @param rt_speed data.frame or sf data.frame. The result of
#'   \code{GTFShift::rt_average_speed()}. Must contain at least the columns
#'   \code{distance_along_geometry} and \code{timestamp}.
#' @param by Character vector (Default \code{c("trip_id", "route_id", "day")}).
#'   Columns to aggregate by. If \code{"day"} is included in \code{by} but not present
#'   in \code{rt_speed}, it is automatically derived from the \code{timestamp} column.
#'   Set to \code{NULL} or \code{character(0)} to compute metrics across the entire dataset.
#' @param dist_col Character (Default \code{"distance_along_geometry"}). Column name
#'   present in \code{rt_speed} representing cumulative distance along geometry (in meters).
#' @param speed_col Character (Default \code{"speed_kmh"}). Column name present in
#'   \code{rt_speed} representing estimated speed between consecutive updates (in km/h).
#' @param time_col Character (Default \code{"timestamp"}). Column name present in
#'   \code{rt_speed} representing update timestamps (numeric epoch, POSIXct, or Date).
#'
#' @details
#' For each group defined by \code{by} (by default, each unique combination of
#' \code{trip_id}, \code{route_id}, and \code{day}), observations are ordered
#' chronologically by \code{timestamp}.
#'
#' Let \eqn{\{(d_i, t_i)\}_{i=1}^n} denote the ordered sequence of updates, where
#' \eqn{d_i} is the distance along geometry (meters) and \eqn{t_i} is the timestamp (seconds).
#'
#' \strong{Commercial speed} is calculated as the total distance traveled between the
#' first and last updates divided by the elapsed time:
#' \deqn{v_{\mathrm{commercial}} = \frac{|d_n - d_1|}{1000} \div \frac{t_n - t_1}{3600}}
#' If \eqn{n < 2} or \eqn{t_n \le t_1}, \code{commercial_speed} is \code{NA}.
#'
#' \strong{Alternative commercial speed} (\code{commercial_speed_alt}) uses the 2nd and
#' penultimate (\eqn{n-1}) observations to eliminate potential dwell times or layovers at
#' the terminal stops:
#' \deqn{v_{\mathrm{commercial\_alt}} = \frac{|d_{n-1} - d_2|}{1000} \div \frac{t_{n-1} - t_2}{3600}}
#' If \eqn{n < 4} or \eqn{t_{n-1} \le t_2}, \code{commercial_speed_alt} is \code{NA}.
#'
#' If \code{by} does not include \code{trip_id} (e.g., aggregating at route or day level),
#' \code{commercial_speed} and \code{commercial_speed_alt} are computed per trip and averaged
#' across trips within each group.
#'
#' \strong{Measures of centrality and spread} are calculated from all valid (non-NA, finite)
#' speed observations in \code{speed_kmh} for each group:
#' \describe{
#'   \item{timestamp_min}{Earliest timestamp in the trip/group.}
#'   \item{timestamp_max}{Latest timestamp in the trip/group.}
#'   \item{commercial_speed}{Commercial speed between first and last update (km/h).}
#'   \item{commercial_speed_alt}{Alternative commercial speed between 2nd and penultimate update (km/h).}
#'   \item{speed_avg}{Arithmetic mean speed (km/h).}
#'   \item{speed_median}{Median speed (km/h).}
#'   \item{speed_sd}{Standard deviation of speeds (km/h).}
#'   \item{speed_var}{Variance of speeds (km/h)^2.}
#'   \item{speed_min}{Minimum observed speed (km/h).}
#'   \item{speed_max}{Maximum observed speed (km/h).}
#'   \item{speed_p15}{15th percentile speed (km/h).}
#'   \item{speed_p25}{25th percentile (1st quartile) speed (km/h).}
#'   \item{speed_p75}{75th percentile (3rd quartile) speed (km/h).}
#'   \item{speed_p85}{85th percentile speed (km/h).}
#'   \item{speed_iqr}{Interquartile range (speed_p75 - speed_p25) (km/h).}
#'   \item{speed_count}{Count of valid speed observations.}
#'   \item{n_updates}{Total number of position updates in the group.}
#' }
#'
#' In addition, an attribute \code{"global_summary"} is attached to the returned data frame,
#' providing the same metrics evaluated over the entire input dataset.
#'
#' @returns data.frame. Aggregated speed profile metrics with one row per group.
#'
#' @examples
#' \donttest{
#' # Get GTFS-RT data collection and route geometries
#' rt_collect_file <- system.file(
#'   "extdata/samples", "gtfs_rt_sample_tcb_4_4-CS-TERM.csv", package = "GTFShift"
#' )
#' rt_collection <- read.csv(rt_collect_file) |>
#'   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
#'   dplyr::select(-speed)
#'
#' osm_routes <- sf::st_read(
#'   system.file("extdata/samples", "osm_routes_tcb.gpkg", package = "GTFShift"),
#'   quiet = TRUE
#' ) |>
#'   dplyr::filter(route_id %in% rt_collection$route_id) |>
#'   dplyr::mutate(geom = GTFShift::multiline_to_sorted_linestring(geom, metric_crs = 3763))
#'
#' # Compute speeds with rt_average_speed()
#' speed <- GTFShift::rt_average_speed(
#'   rt_collection = rt_collection,
#'   trips_geometries = osm_routes,
#'   rt_collection_trips_geometries_match_col = "route_id",
#'   metric_crs = 3763
#' )
#'
#' # Compute trip speed profile
#' profile <- GTFShift::get_trip_speed_profile(speed)
#' head(profile)
#'
#' # Customize aggregation (e.g. by route and day)
#' route_profile <- GTFShift::get_trip_speed_profile(speed, by = c("route_id", "day"))
#' head(route_profile)
#' }
#'
#' @seealso \code{GTFShift::rt_average_speed()}
#'
#' @import dplyr
#' @importFrom stats median sd var quantile IQR
#' @importFrom rlang .data
#'
#' @export
get_trip_speed_profile <- function(
  rt_speed,
  by = c("trip_id", "route_id", "day"),
  dist_col = "distance_along_geometry",
  speed_col = "speed_kmh",
  time_col = "timestamp"
) {
  # 0. Input validations
  if (!is.data.frame(rt_speed)) {
    stop("rt_speed must be a data.frame or sf object")
  }

  # Drop sf geometry if present for aggregation performance
  if (inherits(rt_speed, "sf")) {
    rt_speed <- sf::st_drop_geometry(rt_speed)
  }
  rt_speed <- dplyr::ungroup(rt_speed)

  # Check required columns
  required_cols <- c(dist_col, time_col)
  missing_cols <- setdiff(required_cols, colnames(rt_speed))
  if (length(missing_cols) > 0) {
    stop(paste("rt_speed is missing required column(s):", paste(missing_cols, collapse = ", ")))
  }

  # 1. Handle "day" column derivation if included in 'by' but absent in rt_speed
  if (!is.null(by) && "day" %in% by && !"day" %in% colnames(rt_speed)) {
    if (is.numeric(rt_speed[[time_col]])) {
      rt_speed$day <- as.Date(as.POSIXct(rt_speed[[time_col]], origin = "1970-01-01", tz = "UTC"))
    } else {
      rt_speed$day <- as.Date(rt_speed[[time_col]])
    }
  }

  # 2. Validate / resolve grouping columns
  if (!is.null(by) && length(by) > 0) {
    if (missing(by)) {
      # When default 'by' is used, keep only columns that exist in rt_speed
      by <- intersect(by, colnames(rt_speed))
      if (length(by) == 0) {
        stop("None of the default grouping columns ('trip_id', 'route_id', 'day') were found in rt_speed.")
      }
    } else {
      missing_by <- setdiff(by, colnames(rt_speed))
      if (length(missing_by) > 0) {
        stop(paste("The following grouping columns specified in 'by' were not found in rt_speed:",
                   paste(missing_by, collapse = ", ")))
      }
    }
  }

  # Helper: compute time difference in seconds between two timestamps
  get_time_diff_sec <- function(t_start, t_end) {
    if (is.na(t_start) || is.na(t_end)) return(NA_real_)
    if (inherits(t_start, "POSIXt") || inherits(t_start, "Date")) {
      return(as.numeric(difftime(t_end, t_start, units = "secs")))
    }
    if (is.numeric(t_start) && is.numeric(t_end)) {
      return(as.numeric(t_end - t_start))
    }
    t1 <- suppressWarnings(as.POSIXct(t_start))
    t2 <- suppressWarnings(as.POSIXct(t_end))
    if (!is.na(t1) && !is.na(t2)) {
      return(as.numeric(difftime(t2, t1, units = "secs")))
    }
    return(as.numeric(t_end - t_start))
  }

  # Helper: compute commercial speed in km/h
  compute_speed_kmh <- function(dist_m, time_sec) {
    if (is.na(dist_m) || is.na(time_sec) || time_sec <= 0) {
      return(NA_real_)
    }
    round((dist_m / 1000) / (time_sec / 3600), 2)
  }

  # Helper: compute commercial speeds for a single sequence of updates
  compute_single_trip_commercial_speeds <- function(trip_df) {
    trip_df <- trip_df[order(trip_df[[time_col]]), , drop = FALSE]
    n_trip <- nrow(trip_df)

    # First and last updates
    if (n_trip >= 2) {
      dt <- get_time_diff_sec(trip_df[[time_col]][1], trip_df[[time_col]][n_trip])
      dd <- abs(trip_df[[dist_col]][n_trip] - trip_df[[dist_col]][1])
      comm_speed <- compute_speed_kmh(dd, dt)
    } else {
      comm_speed <- NA_real_
    }

    # 2nd and penultimate updates
    if (n_trip >= 4) {
      dt_alt <- get_time_diff_sec(trip_df[[time_col]][2], trip_df[[time_col]][n_trip - 1])
      dd_alt <- abs(trip_df[[dist_col]][n_trip - 1] - trip_df[[dist_col]][2])
      comm_speed_alt <- compute_speed_kmh(dd_alt, dt_alt)
    } else {
      comm_speed_alt <- NA_real_
    }

    list(commercial_speed = comm_speed, commercial_speed_alt = comm_speed_alt)
  }

  # Helper: compute full profile for a group
  compute_profile <- function(group_df) {
    group_df <- group_df[order(group_df[[time_col]]), , drop = FALSE]
    n <- nrow(group_df)

    # Time boundaries to locate trip/group in time
    timestamp_min <- if (n > 0) group_df[[time_col]][1] else NA
    timestamp_max <- if (n > 0) group_df[[time_col]][n] else NA

    # Commercial speed calculations
    if ("trip_id" %in% colnames(group_df) && length(unique(group_df$trip_id)) > 1) {
      # If group contains multiple trips (e.g. grouped by route_id/day without trip_id),
      # compute commercial speeds per trip and take average
      trip_list <- split(group_df, group_df$trip_id)
      trip_res <- lapply(trip_list, compute_single_trip_commercial_speeds)
      comm_vec <- vapply(trip_res, function(x) x$commercial_speed, numeric(1))
      comm_alt_vec <- vapply(trip_res, function(x) x$commercial_speed_alt, numeric(1))

      commercial_speed <- if (any(!is.na(comm_vec))) round(mean(comm_vec, na.rm = TRUE), 2) else NA_real_
      commercial_speed_alt <- if (any(!is.na(comm_alt_vec))) round(mean(comm_alt_vec, na.rm = TRUE), 2) else NA_real_
    } else {
      # Single trip within group (standard case)
      trip_res <- compute_single_trip_commercial_speeds(group_df)
      commercial_speed <- trip_res$commercial_speed
      commercial_speed_alt <- trip_res$commercial_speed_alt
    }

    # Measures of centrality and spread for speed observations
    if (speed_col %in% colnames(group_df)) {
      raw_speeds <- group_df[[speed_col]]
      valid_speeds <- raw_speeds[!is.na(raw_speeds) & is.finite(raw_speeds)]
    } else {
      valid_speeds <- numeric(0)
    }

    if (length(valid_speeds) > 0) {
      speed_avg <- round(mean(valid_speeds), 2)
      speed_median <- round(stats::median(valid_speeds), 2)
      speed_sd <- if (length(valid_speeds) > 1) round(stats::sd(valid_speeds), 2) else NA_real_
      speed_var <- if (length(valid_speeds) > 1) round(stats::var(valid_speeds), 2) else NA_real_
      speed_min <- round(min(valid_speeds), 2)
      speed_max <- round(max(valid_speeds), 2)
      speed_p15 <- round(as.numeric(stats::quantile(valid_speeds, probs = 0.15, names = FALSE)), 2)
      speed_p25 <- round(as.numeric(stats::quantile(valid_speeds, probs = 0.25, names = FALSE)), 2)
      speed_p75 <- round(as.numeric(stats::quantile(valid_speeds, probs = 0.75, names = FALSE)), 2)
      speed_p85 <- round(as.numeric(stats::quantile(valid_speeds, probs = 0.85, names = FALSE)), 2)
      speed_iqr <- round(stats::IQR(valid_speeds), 2)
      speed_count <- length(valid_speeds)
    } else {
      speed_avg <- NA_real_
      speed_median <- NA_real_
      speed_sd <- NA_real_
      speed_var <- NA_real_
      speed_min <- NA_real_
      speed_max <- NA_real_
      speed_p15 <- NA_real_
      speed_p25 <- NA_real_
      speed_p75 <- NA_real_
      speed_p85 <- NA_real_
      speed_iqr <- NA_real_
      speed_count <- 0L
    }

    data.frame(
      timestamp_min = timestamp_min,
      timestamp_max = timestamp_max,
      commercial_speed = commercial_speed,
      commercial_speed_alt = commercial_speed_alt,
      speed_avg = speed_avg,
      speed_median = speed_median,
      speed_sd = speed_sd,
      speed_var = speed_var,
      speed_min = speed_min,
      speed_max = speed_max,
      speed_p15 = speed_p15,
      speed_p25 = speed_p25,
      speed_p75 = speed_p75,
      speed_p85 = speed_p85,
      speed_iqr = speed_iqr,
      speed_count = speed_count,
      n_updates = n,
      stringsAsFactors = FALSE
    )
  }

  # 3. Compute speed profile
  if (is.null(by) || length(by) == 0) {
    result <- compute_profile(rt_speed)
  } else {
    result <- rt_speed |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::group_modify(function(group_df, keys) {
        compute_profile(group_df)
      }) |>
      dplyr::ungroup()
  }

  # Attach global summary across the entire dataset as attribute
  attr(result, "global_summary") <- compute_profile(rt_speed)

  return(result)
}
