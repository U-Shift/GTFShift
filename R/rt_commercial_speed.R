#' @export
rt_commercial_speed <- function(
  rt_collection, # sf data.frame with GTFS-RT updates for multiple trips 
  trips_geometries, # sf data.frame with trip_id and geometry (LINESTRING) for each trip
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
      trip_df <- trip_df |> arrange(timestamp) |> st_transform(crs = metric_crs)
      trip_geometry <- trips_geometries |> filter(trip_id == trip_df$trip_id[[1]]) |> st_transform(crs = metric_crs)
      
      # Find the closest point on the shape for each update
      trip_df_geometry <- st_geometry(trip_df)
      trip_geometry_geometry <- st_geometry(trip_geometry)
      closest_points <- st_nearest_points(trip_df_geometry, trip_geometry_geometry)
      pts_all <- st_cast(closest_points, "POINT")
      n <- length(closest_points)
      matched_on_shape <- pts_all[seq(2, length(pts_all), by = 2)]
      trip_df <- trip_df |> dplyr::mutate(closest_on_shape = matched_on_shape)

      # Sample trip_geometry with segments of 10 meters
      line_len_m <- as.numeric(st_length(trip_geometry))
      trip_geometry_sampled <- st_line_sample(trip_geometry, density = 1/geometry_sample_meters)
      trip_geometry_sampled_points <- st_cast(trip_geometry_sampled, "POINT")
      cumdist_m <- seq(0, line_len_m, length.out = length(trip_geometry_sampled_points))
      
      # Get closest sampled point on shape for each update and compute distance along shape
      idx <- st_nearest_feature(trip_df$closest_on_shape, trip_geometry_sampled_points)
      dist_along_m <- cumdist_m[idx]
      trip_df$distance_along_geometry <- dist_along_m # TODO! Isolate in separate method
      trip_df <- trip_df |> mutate(
        time_since_prev_sec = timestamp - lag(timestamp),
        distance_since_prev_meters = distance_along_geometry - lag(distance_along_geometry),
        speed_kmh = (distance_since_prev_meters / 1000) / (time_since_prev_sec / 3600),
        distance_since_prev_meters = round(distance_since_prev_meters, 2),
        speed_kmh = round(speed_kmh, 2),
        distance_along_geometry = round(distance_along_geometry, 2)
      )
      # |> filter(
      #  !is.na(speed_kmh) &
      #  speed_kmh>=0 # Remove negative speeds
      # )

      return(trip_df)
    })
}