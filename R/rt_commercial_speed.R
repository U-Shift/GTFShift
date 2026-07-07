#' Estimate commercial speed from GTFS-RT trip updates
#'
#' Projects each real-time vehicle position to its corresponding trip geometry,
#' computes cumulative distance along the shape, and derives segment speed
#' between consecutive updates.
#'
#' @param rt_collection sf data.frame with GTFS-RT updates for multiple trips.
#'   Must include at least \code{trip_id} and \code{timestamp} columns.
#' @param trips_geometries sf data.frame with trip geometries. Geometry must be
#'   LINESTRING.
#' @param rt_collection_trips_geometries_match_col Character (Default
#'   \code{"trip_id"}). Column name present in both \code{rt_collection} and
#'   \code{trips_geometries} used to match updates to trip geometry.
#' @param geometry_sample_meters Numeric (Default 10). Sampling step used when
#'   projecting points along trip geometry and estimating cumulative distance.
#' @param metric_crs Integer or character (Default 3857). Projected CRS used to
#'   compute distances and speeds.
#'
#' @details
#' For each trip (grouped by \code{trip_id}), updates are ordered by
#' \code{timestamp}. Point-to-line projection and cumulative distance are
#' computed with \code{GTFShift::project_points_along_geometry()}. Speed is then estimated
#' between consecutive updates.
#'
#' Distance between consecutive updates is computed as the minimum between two
#' alternatives, both using absolute differences:
#' \enumerate{
#'   \item Normal direction: difference in \code{distance_along_geometry}.
#'   \item Reversed direction: difference using \code{distance_along_geometry_reversed}
#'   to better handle circular shapes.
#' }
#'
#' The selected distance increment is used to compute speed as:
#' \deqn{speed_{km/h} = \frac{\Delta distance\ (m)}{1000} \div \frac{\Delta time\ (s)}{3600}}
#'
#' Trips with fewer than 2 updates are ignored with a warning.
#' 
#' Method \code{GTFShift::multiline_to_sorted_linestring()} can be used to convert MULTILINESTRING 
#' geometries to LINESTRING if needed.
#'
#' @returns An \code{sf} object based on \code{rt_collection}, with added columns:
#' \describe{
#'   \item{closest_on_shape}{Projected point on trip geometry.}
#'   \item{distance_to_closest_on_geometry}{Distance from each update point to its projected location on the shape (meters).}
#'   \item{distance_along_geometry}{Cumulative distance along trip geometry (meters).}
#'   \item{distance_along_geometry_reversed}{Cumulative distance from shape end to projected location (meters).}
#'   \item{time_since_prev_sec}{Elapsed time since previous update (seconds).}
#'   \item{distance_since_prev_meters}{Distance increment since previous update (meters).}
#'   \item{speed_kmh}{Estimated speed between consecutive updates (km/h).}
#' }
#'
#' @examples
#' \dontrun{
#' rt_collection <- read.csv("rt_collection.csv") # sf object with GTFS-RT updates (trip_id, timestamp, geometry)
#' trips_geometries <- sf::st_read("osm_geometries.gpkg") # sf object with LINESTRING geometry per trip
#' speeds <- GTFShift::rt_commercial_speed(rt_collection, trips_geometries)
#' }
#'
#' @seealso \code{GTFShift::project_points_along_geometry()}
#' @seealso \code{GTFShift::multiline_to_sorted_linestring()}
#'
#' @import sf
#' @import dplyr
#' @import purrr
#' @import rlang
#'
#' @export
rt_commercial_speed <- function(
  rt_collection, # sf data.frame with GTFS-RT updates for multiple trips 
  trips_geometries, # sf data.frame with trips geometry (LINESTRING) 
  rt_collection_trips_geometries_match_col = "trip_id", # column name in rt_collection and trips_geometries to match trips
  geometry_sample_meters = 10, # sample trip geometry every X meters to compute distance along shape
  metric_crs = 3857
) {
  # 0. Validations 
  # > rt_collection and trips_geometries must be spatial objects
  if (!inherits(rt_collection, "sf")) {
    stop("rt_collection must be an sf object")
  }
  if (!inherits(trips_geometries, "sf")) {
    stop("trips_geometries must be an sf object")
  }
  # > if trips_geometries geometry has MULTILINESTRING, throw error
  if (any(sf::st_geometry_type(trips_geometries) != "LINESTRING")) {
    stop("trips_geometries geometry must be LINESTRING. Use GTFShift::multiline_to_sorted_linestring() to convert MULTILINESTRING to LINESTRING.")
  }
  # > rt_collection_trips_geometries_match_col must be one of the columns in rt_collection and trips_geometries
  if (!rt_collection_trips_geometries_match_col %in% colnames(rt_collection)) {
    stop(paste("rt_collection_trips_geometries_match_col must be one of the columns in rt_collection. Available columns:", paste(colnames(rt_collection), collapse = ", ")))
  }
  if (!rt_collection_trips_geometries_match_col %in% colnames(trips_geometries)) {
    stop(paste("rt_collection_trips_geometries_match_col must be one of the columns in trips_geometries. Available columns:", paste(colnames(trips_geometries), collapse = ", ")))
  }

  # > rt_collection must have trip_id and timestamp columns
  required_cols <- c("trip_id", "timestamp")
  missing_cols <- setdiff(required_cols, colnames(rt_collection))
  if (length(missing_cols) > 0) {
    stop(paste("rt_collection is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # 1. Compute speed for each trip update
  rt_collection |>
    sf::st_transform(crs = metric_crs) |>
    group_split(trip_id) |>
    purrr::map_dfr(function(trip_df) {
      # If trip has less than 2 updates, ignore it
      if (nrow(trip_df) < 2) {
        warning(paste("Trip", trip_df[[rt_collection_trips_geometries_match_col]][[1]], "has less than 2 updates. Ignoring it."))
        return(NULL)
      }
      trip_df <- trip_df |> arrange(timestamp) |> st_transform(crs = metric_crs)
      trip_geometry <- trips_geometries |> 
        filter(!!sym(rt_collection_trips_geometries_match_col) == trip_df[[rt_collection_trips_geometries_match_col]][[1]]) |> 
        st_transform(crs = metric_crs)
      # mapview(trip_geometry)
      
      projected <- project_points_along_geometry(
        geometry = trip_geometry,
        points = trip_df,
        geometry_sample_meters = geometry_sample_meters,
        metric_crs = metric_crs
      )
      trip_df <- trip_df |>
        dplyr::mutate(
          closest_on_shape = projected$closest_on_geometry,
          distance_along_geometry = projected$distance_along_geometry,
          distance_along_geometry_reversed = projected$distance_along_geometry_reversed,
          distance_to_closest_on_geometry = projected$distance_to_closest_on_geometry
        )
      trip_df <- trip_df |> mutate(
        time_since_prev_sec = timestamp - lag(timestamp),
        # When computing distances
        # 1. Use absolute value to avoid negative distances
        # 2. Consider both normal and reversed distances (to work with circular shapes) and take the minimum
        distance_since_prev_meters_normal = abs(distance_along_geometry - lag(distance_along_geometry)), 
        distance_since_prev_meters_reversed = abs(distance_along_geometry - lag(distance_along_geometry_reversed)),
        distance_since_prev_meters = pmin(distance_since_prev_meters_normal, distance_since_prev_meters_reversed, na.rm = TRUE),
        # Compute speed in km/h
        speed_kmh = (distance_since_prev_meters / 1000) / (time_since_prev_sec / 3600),
        distance_since_prev_meters = round(distance_since_prev_meters, 2),
        speed_kmh = round(speed_kmh, 2),
        distance_along_geometry = round(distance_along_geometry, 2)
      ) |> select(-distance_since_prev_meters_normal, -distance_since_prev_meters_reversed)
      # mapview(trip_df, zcol = "distance_along_geometry", layer.name = "Distance along geometry") + mapview(trip_df, zcol = "speed_kmh", layer.name = "Speed (km/h)") +  mapview(trip_geometry, color = "blue", lwd = 3, layer.name = "Trip geometry")
      # |> filter(
      #  !is.na(speed_kmh) &
      #  speed_kmh>=0 # Remove negative speeds
      # )

      return(trip_df)
    })
}