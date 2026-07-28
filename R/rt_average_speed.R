#' Estimate average speed for GTFS-RT trip updates
#'
#' Projects each real-time vehicle position to its corresponding trip geometry,
#' computes cumulative distance along the geometry, and derives segment speed
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
#' For each trip (grouped by \code{trip_id}), let \eqn{\{(x_i, t_i)\}_{i=1}^n} 
#' denote the ordered sequence of
#' real-time observations, where \eqn{x_i} is the vehicle position and
#' \eqn{t_i} the corresponding timestamp, with
#' \eqn{t_1 \le t_2 \le \dots \le t_n}. Each observation is projected onto the
#' trip geometry using \code{GTFShift::project_points_along_geometry()}, yielding
#' a projected point \eqn{\hat{x}_i} and two cumulative distances:
#' \deqn{d_i = \text{distance_along_geometry}(\hat{x}_i)}
#' \deqn{d_i^{\mathrm{rev}} = \text{distance_along_geometry_reversed}(\hat{x}_i)}
#'
#' For each pair of consecutive observations \eqn{(i-1, i)}, the elapsed time is
#' computed as
#' \deqn{\Delta t_i = t_i - t_{i-1}.}
#'
#' The distance increment is defined as the minimum of the forward and reversed
#' cumulative-distance differences:
#' \deqn{\Delta d_i^{\mathrm{fwd}} = \left| d_i - d_{i-1} \right|}
#' \deqn{\Delta d_i^{\mathrm{rev}} = \left| d_i^{\mathrm{rev}} - d_{i-1}^{\mathrm{rev}} \right|}
#' \deqn{\Delta d_i = \min\left(\Delta d_i^{\mathrm{fwd}}, \Delta d_i^{\mathrm{rev}}\right).}
#'
#' Trips with fewer than 2 updates are ignored with a warning.
#' The distance increment is defined as the minimum of two alternative
#' cumulative-distance differences:
#' \deqn{\Delta d_i^{\mathrm{fwd}} = \left| d_i - d_{i-1} \right|}
#' \deqn{\Delta d_i^{\mathrm{circ}} = \left| d_i - d_{i-1}^{\mathrm{rev}} \right|}
#' \deqn{\Delta d_i = \min\left(\Delta d_i^{\mathrm{fwd}}, \Delta d_i^{\mathrm{circ}}\right).}
#'
#' The second term is a redundancy designed to avoid overstating movement on circular
#' geometries. In particular, after a vehicle completes a loop, a
#' forward comparison may treat two nearby physical positions as far apart in
#' cumulative distance if the geometry origin has been crossed.
#' Comparing \eqn{d_i} against \eqn{d_{i-1}^{\mathrm{rev}}} provides an auxiliary
#' distance candidate that helps avoid overstating movement in that situation.
#'
#' Average speed is then estimated by
#' \deqn{v_i = \frac{\Delta d_i}{\Delta t_i}}
#' and reported in kilometers per hour as
#' \deqn{v_i^{\mathrm{km/h}} = \frac{\Delta d_i}{1000} \cdot \frac{3600}{\Delta t_i}.}
#'
#' Trips with fewer than two observations are ignored with a warning because
#' \eqn{\Delta t_i} and \eqn{\Delta d_i} are undefined in that case.
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
#' # Get GTFS-RT data collection
#' rt_collect_file <- system.file("extdata/samples", "gtfs_rt_sample_tcb_4_4-CS-TERM.csv", package = "GTFShift")
#' rt_collection <- read.csv(rt_collect_file) |> sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> dplyr::select(-speed)
#' 
#' head(rt_collection |> dplyr::select(trip_id, timestamp, geometry))
#' 
#' nrow(rt_collection)
#' 
#' # Get route geometry for data collected
#' osm_routes <- sf::st_read(system.file("extdata/samples", "osm_routes_tcb.gpkg", package = "GTFShift")) |> 
#'   dplyr::filter(route_id %in% rt_collection$route_id) |>
#'   dplyr::mutate(geom = GTFShift::multiline_to_sorted_linestring(geom, metric_crs = 3763))
#' 
#' head(osm_routes)
#' 
#' # Compute average speed (aggregated at route level) based on cumulative distance along the geometry
#' speed <- GTFShift::rt_average_speed(
#'   rt_collection = rt_collection, 
#'   trips_geometries = osm_routes,
#'   rt_collection_trips_geometries_match_col = "route_id",
#'   metric_crs = 3763 # Make sure to addapt to the projection that better suits your location
#' )
#' 
#' head(speed |> 
#'   dplyr::filter(!is.na(speed_kmh)) |>
#'   dplyr::select(trip_id, timestamp, speed_kmh, distance_along_geometry, distance_to_closest_on_geometry)
#' )
#' 
#' nrow(speed)
#'
#' @seealso \code{GTFShift::project_points_along_geometry()}
#' @seealso \code{GTFShift::multiline_to_sorted_linestring()}
#'
#' @import sf
#' @import dplyr
#' @importFrom purrr map_dfr
#' @import rlang
#' @importFrom rlang .data
#'
#' @export
rt_average_speed <- function(
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
    group_split(.data$trip_id) |>
    purrr::map_dfr(function(trip_df) {
      # If trip has less than 2 updates, ignore it
      if (nrow(trip_df) < 2) {
        warning(paste("Trip", trip_df[[rt_collection_trips_geometries_match_col]][[1]], "has less than 2 updates. Ignoring it."))
        return(NULL)
      }
      trip_df <- trip_df |> arrange(.data$timestamp) |> st_transform(crs = metric_crs)
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
        time_since_prev_sec = .data$timestamp - lag(.data$timestamp),
        # When computing distances
        # 1. Use absolute value to avoid negative distances
        # 2. Consider both normal and reversed distances (to work with circular shapes) and take the minimum
        distance_since_prev_meters_normal = abs(.data$distance_along_geometry - lag(.data$distance_along_geometry)), 
        distance_since_prev_meters_reversed = abs(.data$distance_along_geometry - lag(.data$distance_along_geometry_reversed)),
        distance_since_prev_meters = pmin(.data$distance_since_prev_meters_normal, .data$distance_since_prev_meters_reversed, na.rm = TRUE),
        # Compute speed in km/h
        speed_kmh = (.data$distance_since_prev_meters / 1000) / (.data$time_since_prev_sec / 3600),
        distance_since_prev_meters = round(.data$distance_since_prev_meters, 2),
        speed_kmh = round(.data$speed_kmh, 2),
        distance_along_geometry = round(.data$distance_along_geometry, 2)
      ) |> select(-.data$distance_since_prev_meters_normal, -.data$distance_since_prev_meters_reversed)
      # mapview(trip_df, zcol = "distance_along_geometry", layer.name = "Distance along geometry") + mapview(trip_df, zcol = "speed_kmh", layer.name = "Speed (km/h)") +  mapview(trip_geometry, color = "blue", lwd = 3, layer.name = "Trip geometry")
      # |> filter(
      #  !is.na(speed_kmh) &
      #  speed_kmh>=0 # Remove negative speeds
      # )

      return(trip_df)
    })
}