#' @export
#' # TODO! Validate this against before and after View(df)!!
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
      
      projected_after <- project_points_along_geometry(
        geometry = trip_geometry,
        points = trip_df,
        geometry_sample_meters = geometry_sample_meters
      )
      trip_df <- trip_df |>
        dplyr::mutate(
          closest_on_shape = projected$closest_on_geometry,
          distance_along_geometry = projected$distance_along_geometry
        )
      trip_df <- trip_df |> mutate(
        time_since_prev_sec = timestamp - lag(timestamp),
        distance_since_prev_meters = distance_along_geometry - lag(distance_along_geometry),
        speed_kmh = (distance_since_prev_meters / 1000) / (time_since_prev_sec / 3600),
        distance_since_prev_meters = round(distance_since_prev_meters, 2),
        speed_kmh = round(speed_kmh, 2),
        distance_along_geometry = round(distance_along_geometry, 2)
      )
      # mapview(trip_df, zcol = "distance_along_geometry", layer.name = "Distance along geometry") + mapview(trip_df, zcol = "speed_kmh", layer.name = "Speed (km/h)") +  mapview(trip_geometry, color = "blue", lwd = 3, layer.name = "Trip geometry")
      # |> filter(
      #  !is.na(speed_kmh) &
      #  speed_kmh>=0 # Remove negative speeds
      # )

      return(trip_df)
    })
}