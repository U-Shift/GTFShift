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
