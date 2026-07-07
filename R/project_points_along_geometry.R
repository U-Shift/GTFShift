# Projects points to a linear geometry and computes cumulative distance along it.
project_points_along_geometry <- function(
  geometry,
  points,
  geometry_sample_meters = 10
) {
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
    return(list(
      closest_on_geometry = points_sfc,
      distance_along_geometry = numeric(0)
    ))
  }

  geometry_type <- as.character(sf::st_geometry_type(geometry_sfc))
  if (!geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    stop("geometry must be LINESTRING or MULTILINESTRING")
  }

  # Ensure both inputs are in the same CRS for distance-based operations.
  if (!is.na(sf::st_crs(geometry_sfc)) && !is.na(sf::st_crs(points_sfc)) &&
      sf::st_crs(points_sfc) != sf::st_crs(geometry_sfc)) {
    points_sfc <- sf::st_transform(points_sfc, sf::st_crs(geometry_sfc))
  }

  closest_points <- sf::st_nearest_points(points_sfc, geometry_sfc)
  pts_all <- sf::st_cast(closest_points, "POINT")
  closest_on_geometry <- pts_all[seq(2, length(pts_all), by = 2)]
  # mapview(closest_on_geometry) + mapview(closest_points) + mapview(geometry_sfc)

  line_len_m <- as.numeric(sf::st_length(geometry_sfc))
  geometry_sampled <- sf::st_line_sample(geometry_sfc, density = 1 / geometry_sample_meters)
  geometry_sampled_points <- sf::st_cast(geometry_sampled, "POINT")
  cumdist_m <- seq(0, line_len_m, length.out = length(geometry_sampled_points))

  idx <- sf::st_nearest_feature(closest_on_geometry, geometry_sampled_points)
  distance_along_geometry <- cumdist_m[idx]

  data.frame(
    closest_on_geometry = closest_on_geometry,
    distance_along_geometry = distance_along_geometry
  )
}
