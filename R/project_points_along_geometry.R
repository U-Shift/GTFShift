#' Project points onto a linear geometry
#'
#' Projects point geometries to the closest location along a single LINESTRING or
#' MULTILINESTRING and estimates each projected point position as cumulative
#' distance from the start of the line.
#'
#' @param geometry sf or sfc object with exactly one linear geometry
#'   (LINESTRING or MULTILINESTRING).
#' @param points sf or sfc object with point geometries to be projected.
#' @param geometry_sample_meters Numeric (Default 10). Sampling step used to
#'   discretize the line when estimating cumulative distance along geometry.
#' @param metric_crs Integer or character (Default NULL). Optional projected CRS used to
#'   compute nearest points, line sampling, and cumulative distances. If NULL, spherical geometry (s2) is used by default.
#'
#' @details
#' The function first computes nearest points from each input point to
#' \code{geometry} with \code{sf::st_nearest_points()}, keeping the point on the
#' line. Then, it samples the line at regular intervals and assigns cumulative
#' distance by nearest sampled location.
#'
#' Distances are always computed in \code{metric_crs} units. The returned
#' projected points are transformed back to the original \code{geometry} CRS.
#'
#' @returns data.frame. Input points projected along geometry with four columns:
#' \describe{
#'   \item{closest_on_geometry}{An \code{sfc_POINT} column with the projected location on the line.}
#'   \item{distance_to_closest_on_geometry}{Numeric distance from each input point to its projected location on the line.}
#'   \item{distance_along_geometry}{Numeric cumulative distance from the line start to the projected location.}
#'   \item{distance_along_geometry_reversed}{Numeric cumulative distance from the line end to the projected location.}
#' }
#'
#' If \code{points} is empty, returns an empty data.frame with the same columns.
#'
#' @examples
#' # Get sample points from GTFS-RT collection
#' rt_collect_file <- system.file(
#'   "extdata/samples", "gtfs_rt_sample_tcb_4_4-CS-TERM.csv", package = "GTFShift"
#' )
#' points <- read.csv(rt_collect_file) |> 
#'   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> dplyr::sample_n(5)
#' 
#' head(points |> dplyr::select(geometry))
#' 
#' # Get route geometry for points
#' osm_routes <- sf::st_read(
#'   system.file("extdata/samples", "osm_routes_tcb.gpkg", package = "GTFShift"),
#'   quiet = TRUE
#' ) |> dplyr::filter(route_id %in% points$route_id)
#' 
#' head(osm_routes)
#' 
#' # Project points to geometry
#' points_projected <- GTFShift::project_points_along_geometry(
#'   geometry = osm_routes,
#'   points = points
#' )
#' 
#' head(points_projected)
#'
#' @importFrom units set_units
#' @srrstats {G2.0} Validates that `geometry` has length 1 and handles zero-length `points`.
#' @srrstats {G2.0a} Documents length expectations for `geometry` (single linear feature) and `points`.
#' @srrstats {G2.1} Validates that `geometry` and `points` inherit from `sf` or `sfc` and checks `st_geometry_type()`.
#' @srrstats {G2.1a} Documents expected spatial and numeric data types for all parameters.
#' @srrstats {G2.2} Restricts `geometry` to a single univariate feature (`length(geometry_sfc) != 1`).
#' @srrstats {G2.4} Converts between `units`, `sfc`, and base vector types explicitly.
#' @srrstats {G2.4b} Uses `as.numeric()` explicitly to convert `units` distance vectors to numeric values.
#' @srrstats {G2.4c} Uses `as.character()` explicitly when coercing geometry type factors.
#' @srrstats {G2.6} Processes 1D distance outputs regardless of `units` class attributes.
#' @srrstats {G2.7} Accepts both `sf` data frames and `sfc` geometry lists.
#' @srrstats {G2.8} Pre-processes `geometry` and `points` to `sfc` objects in a unified CRS.
#' @srrstats {G2.11} Handles `sfc` geometry columns and `units` vectors appropriately.
#' @srrstats {G2.13} Checks for missing (`NA`) CRS prior to coordinate transformations.
#' @srrstats {EA1.3} Explicitly identifies accepted spatial input classes (`sf` and `sfc`).
#' @srrstats {EA2.6} Processes vector distances regardless of `units` attributes.
#' @srrstats {SP2.0} Accepts only dedicated spatial classes (`sf` and `sfc`).
#' @srrstats {SP2.0b} Errors appropriately when spatial inputs lack a valid CRS or have inappropriate geometry types.
#' @srrstats {SP2.3} Examples load spatial geometries from a standard `.gpkg` file via `sf::st_read()`.
#' @srrstats {SP2.4} Uses `sf::st_crs()` and `sf::st_transform()` compliant with `PROJ >= 6`.
#' @srrstats {SP2.4a} Represents and validates coordinate reference systems via `WKT2` (`sf::st_crs()`).
#' @srrstats {SP2.5} Verifies CRS metadata on input `sf`/`sfc` objects.
#' @srrstats {SP2.6} Documents accepted spatial input classes (`sf` or `sfc`) for `geometry` and `points`.
#' @srrstats {SP2.7} Validates input spatial classes and geometry types prior to projection.
#' @srrstats {SP2.8} Pre-processes inputs into `sfc` objects in a common CRS.
#' @srrstats {SP2.9} Preserves original CRS metadata (`geometry_crs_original`) in the returned `sf` object.
#' @srrstats {SP4.0} Returns an `sf` object consistent with spatial inputs.
#' @srrstats {SP4.0a} Returns an `sf` object when given `sf`/`sfc` inputs.
#' @srrstats {SP4.0b} Returns a documented `sf` data frame structure with projected point geometries.
#' @srrstats {SP4.1} Computes distances in meters (using `units::set_units(..., "m")` for spherical segmentization).
#' @srrstats {SP4.2} Explicitly documents the return class and columns in `@returns`.
#' @export
project_points_along_geometry <- function(
  geometry,
  points,
  geometry_sample_meters = 10,
  metric_crs = NULL
) {
  if (!is.null(metric_crs)) {
    metric_crs <- suppressWarnings(sf::st_crs(metric_crs))
    if (is.na(metric_crs)) {
      stop("metric_crs should be a valid CRS value (e.g., 3857 or 'EPSG:3857')")
    }
  }

  geometry_sfc <- if (inherits(geometry, "sf")) sf::st_geometry(geometry) else geometry
  points_sfc <- if (inherits(points, "sf")) sf::st_geometry(points) else points

  if (!inherits(geometry_sfc, "sfc")) {
    stop("geometry must be an sf object or sfc geometry")
  }
  if (!inherits(points_sfc, "sfc")) {
    stop("points must be an sf object or sfc geometry")
  }
  if (length(geometry_sfc) != 1) {
    stop("geometry must contain exactly one feature")
  }
  if (length(points_sfc) == 0) {
    return(sf::st_sf(
      closest_on_geometry = points_sfc,
      distance_to_closest_on_geometry = numeric(0),
      distance_along_geometry = numeric(0),
      distance_along_geometry_reversed = numeric(0),
      sf_column_name = "closest_on_geometry"
    ))
  }

  geometry_type <- as.character(sf::st_geometry_type(geometry_sfc))
  if (!geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    stop("geometry must be LINESTRING or MULTILINESTRING")
  }

  geometry_crs_original <- sf::st_crs(geometry_sfc)
  if (!is.null(metric_crs)) {
    if (is.na(sf::st_crs(geometry_sfc)) || is.na(sf::st_crs(points_sfc))) {
      stop("geometry and points must have a valid CRS to use metric_crs")
    }
    geometry_metric <- sf::st_transform(geometry_sfc, metric_crs)
    points_metric <- sf::st_transform(points_sfc, metric_crs)
  } else {
    geometry_metric <- geometry_sfc
    if (!is.na(sf::st_crs(geometry_sfc)) && !is.na(sf::st_crs(points_sfc)) &&
        sf::st_crs(points_sfc) != sf::st_crs(geometry_sfc)) {
      points_metric <- sf::st_transform(points_sfc, sf::st_crs(geometry_sfc))
    } else {
      points_metric <- points_sfc
    }
  }

  if (geometry_type == "MULTILINESTRING") {
    geom_merged <- sf::st_line_merge(geometry_metric)
    geom_cast <- sf::st_cast(geom_merged, "LINESTRING")
    geometry_metric <- sf::st_sfc(geom_cast[[1]], crs = sf::st_crs(geometry_metric))
  }

  closest_points <- sf::st_nearest_points(points_metric, geometry_metric)
  closest_points_length <- sf::st_length(closest_points)
  pts_all <- sf::st_cast(sf::st_cast(closest_points, "LINESTRING"), "POINT")
  closest_on_geometry_metric <- pts_all[seq(2, length(pts_all), by = 2)]
  if (!is.na(geometry_crs_original) && sf::st_crs(closest_on_geometry_metric) != geometry_crs_original) {
    closest_on_geometry <- sf::st_transform(closest_on_geometry_metric, geometry_crs_original)
  } else {
    closest_on_geometry <- closest_on_geometry_metric
  }
  # mapview(closest_on_geometry) + mapview(closest_points) + mapview(geometry_sfc)
  line_len_m <- as.numeric(sf::st_length(geometry_metric))
  if (isTRUE(sf::st_is_longlat(geometry_metric))) {
    geometry_sampled <- sf::st_segmentize(geometry_metric, dfMaxLength = units::set_units(geometry_sample_meters, "m"))
    geometry_sampled_points <- sf::st_cast(geometry_sampled, "POINT")
    if (length(geometry_sampled_points) > 1) {
      seg_dists <- as.numeric(sf::st_distance(
        geometry_sampled_points[-length(geometry_sampled_points)],
        geometry_sampled_points[-1],
        by_element = TRUE
      ))
      cumdist_m <- c(0, cumsum(seg_dists))
      cumdist_m_reversed <- c(rev(cumsum(rev(seg_dists))), 0)
    } else {
      cumdist_m <- rep(0, length(geometry_sampled_points))
      cumdist_m_reversed <- cumdist_m
    }
  } else {
    geometry_sampled <- sf::st_line_sample(geometry_metric, density = 1 / geometry_sample_meters)
    geometry_sampled_points <- sf::st_cast(geometry_sampled, "POINT")
    cumdist_m <- seq(0, line_len_m, length.out = length(geometry_sampled_points))
    cumdist_m_reversed <- rev(cumdist_m)
  }

  idx <- sf::st_nearest_feature(closest_on_geometry_metric, geometry_sampled_points)
  distance_along_geometry <- cumdist_m[idx]
  distance_along_geometry_reversed <- cumdist_m_reversed[idx]

  sf::st_sf(
    closest_on_geometry = closest_on_geometry,
    distance_to_closest_on_geometry = as.numeric(closest_points_length),
    distance_along_geometry = distance_along_geometry,
    distance_along_geometry_reversed = distance_along_geometry_reversed,
    sf_column_name = "closest_on_geometry"
  )
}
