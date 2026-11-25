#' @srrstats {G5.2} Explicitly tests all error conditions triggered by invalid inputs in `project_points_along_geometry()`.
#' @srrstats {G5.2a} Verifies unique error messages for invalid `metric_crs`, PROJ4-strings, non-`sf`/`sfc` inputs, multi-feature `geometry`, invalid geometry type, and missing CRS.
#' @srrstats {G5.2b} Tests every error condition and compares messages against expected strings via `expect_error()`.
#' @srrstats {G5.3} Explicitly verifies that returned numeric distance columns contain no `NA`, `NaN`, or `Inf` values (`all(is.finite(...))`).
#' @srrstats {G5.4} Tests correctness of point projection and cumulative distance calculation against known geometric coordinates.
#' @srrstats {G5.4a} Verifies projection accuracy on trivial horizontal lines with exact known perpendicular offsets and cumulative distances.
#' @srrstats {G5.8} Tests edge conditions including empty point sets, non-spatial objects, and missing CRS.
#' @srrstats {G5.8a} Explicitly tests zero-length `points` (`st_sfc(crs = 3857)`) and verifies a zero-row `sf` return object.
#' @srrstats {G5.8b} Explicitly tests unsupported input types (character strings and `POINT` instead of `LINESTRING`).
#' @srrstats {EA6.0} Tests return object classes, dimensions, column names, column types, and numeric values.
#' @srrstats {EA6.0a} Tests S3 classes (`sf` and `sfc_POINT`) of returned objects.
#' @srrstats {EA6.0b} Tests `nrow()` and `ncol()` dimensions of returned tabular objects.
#' @srrstats {EA6.0c} Tests expected column names via `expect_contains(names(res), ...)`.
#' @srrstats {EA6.0d} Tests classes/types of columns inside the returned `sf` data frame (`sfc_POINT` and numeric distances).
#' @srrstats {EA6.0e} Tests numeric distance values with explicit `tolerance`.
#' @srrstats {SP2.4a} Tests that legacy PROJ4-strings are explicitly rejected for `metric_crs`.
#' @srrstats {SP6.0} Verifies that the returned `closest_on_geometry` preserves the original CRS of the input geometry.
#' @srrstats {SP6.1} Tests `project_points_along_geometry()` on both Cartesian (`EPSG:3857`, `EPSG:3763`) and curvilinear (`EPSG:4326`) coordinates.
#' @srrstats {SP6.1b} Demonstrates equivalent accuracy between curvilinear (`EPSG:4326`, `metric_crs = NULL`) and rectilinear (`metric_crs = 3763`) coordinates, and exact equivalence with pre-transforming inputs via `sf::st_transform()`.
#' @srrstats {SP6.2} Tests extreme geographical coordinates extending to polar extremes of +90 and -90 degrees latitude.
test_that(
  "project_points_along_geometry computes projection and cumulative distance",
  {
    l_100 <- sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE))
    line <- sf::st_sfc(l_100, crs = 3857)
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    res <- GTFShift::project_points_along_geometry(
      line, pts, geometry_sample_meters = 5, metric_crs = 3857
    )
    expect_s3_class(res, "sf")
    expect_identical(nrow(res), 1L)
    expect_identical(ncol(res), 4L)
    expect_contains(
      names(res),
      c(
        "distance_to_closest_on_geometry",
        "distance_along_geometry",
        "distance_along_geometry_reversed"
      )
    )
    expect_type(res$distance_to_closest_on_geometry, "double")
    expect_type(res$distance_along_geometry, "double")
    expect_type(res$distance_along_geometry_reversed, "double")
    expect_true(all(is.finite(res$distance_to_closest_on_geometry)))
    expect_true(all(is.finite(res$distance_along_geometry)))
    expect_true(all(is.finite(res$distance_along_geometry_reversed)))
    expect_equal(res$distance_to_closest_on_geometry, 10, tolerance = 1e-6)
    expect_equal(res$distance_along_geometry, 20, tolerance = 5)
    expect_equal(res$distance_along_geometry_reversed, 80, tolerance = 5)
  }
)

test_that(
  paste0(
    "project_points_along_geometry works when default metric_crs (NULL) ",
    "is used on geographic CRS"
  ),
  {
    l_geo <- sf::st_linestring(
      matrix(c(-9.1, 38.7, -9.11, 38.71), ncol = 2, byrow = TRUE)
    )
    line <- sf::st_sfc(l_geo, crs = 4326)
    pts <- sf::st_sfc(
      sf::st_point(c(-9.1, 38.7)),
      sf::st_point(c(-9.105, 38.702)),
      crs = 4326
    )

    expect_no_warning(
      res <- GTFShift::project_points_along_geometry(line, pts)
    )
    expect_identical(nrow(res), 2L)
    expect_identical(res$distance_along_geometry[1], 0)
    expect_gt(res$distance_along_geometry[2], 0)
  }
)

test_that(
  "project_points_along_geometry stops when metric_crs is invalid NA",
  {
    l_100 <- sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE))
    line <- sf::st_sfc(l_100, crs = 3857)
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    expect_error(
      GTFShift::project_points_along_geometry(line, pts, metric_crs = NA),
      "metric_crs should be a valid CRS value"
    )
  }
)

test_that(
  "project_points_along_geometry stops when geometry is not sf or sfc",
  {
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    expect_error(
      GTFShift::project_points_along_geometry(
        "not_a_geometry", pts, metric_crs = 3857
      ),
      "geometry must be an sf object or sfc geometry"
    )
  }
)

test_that("project_points_along_geometry stops when points is not sf or sfc", {
  l_100 <- sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE))
  line <- sf::st_sfc(l_100, crs = 3857)

  expect_error(
    GTFShift::project_points_along_geometry(
      line, "not_points", metric_crs = 3857
    ),
    "points must be an sf object or sfc geometry"
  )
})

test_that(
  paste0(
    "project_points_along_geometry stops when geometry ",
    "does not have exactly one feature"
  ),
  {
    l1 <- sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE))
    l2 <- sf::st_linestring(matrix(c(100, 0, 200, 0), ncol = 2, byrow = TRUE))
    line_multi <- sf::st_sfc(l1, l2, crs = 3857)
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    expect_error(
      GTFShift::project_points_along_geometry(
        line_multi, pts, metric_crs = 3857
      ),
      "geometry must contain exactly one feature"
    )
  }
)

test_that("project_points_along_geometry handles empty points input", {
  l_100 <- sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE))
  line <- sf::st_sfc(l_100, crs = 3857)
  pts_empty <- sf::st_sfc(crs = 3857)

  res <- GTFShift::project_points_along_geometry(
    line, pts_empty, metric_crs = 3857
  )
  expect_identical(nrow(res), 0L)
  expect_contains(
    names(res),
    c(
      "distance_to_closest_on_geometry",
      "distance_along_geometry",
      "distance_along_geometry_reversed"
    )
  )
})

test_that(
  paste0(
    "project_points_along_geometry stops when geometry ",
    "is not LINESTRING or MULTILINESTRING"
  ),
  {
    point_geom <- sf::st_sfc(sf::st_point(c(50, 50)), crs = 3857)
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    expect_error(
      GTFShift::project_points_along_geometry(
        point_geom, pts, metric_crs = 3857
      ),
      "geometry must be LINESTRING or MULTILINESTRING"
    )
  }
)

test_that(
  "project_points_along_geometry stops when geometry or points CRS is NA",
  {
    l_100 <- sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE))
    line_nocrs <- sf::st_sfc(l_100, crs = NA_crs_)
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    expect_error(
      GTFShift::project_points_along_geometry(
        line_nocrs, pts, metric_crs = 3857
      ),
      "geometry and points must have a valid CRS to use metric_crs"
    )
  }
)

test_that(
  "project_points_along_geometry works with MULTILINESTRING and sf inputs",
  {
    mls <- sf::st_multilinestring(list(
      matrix(c(0, 0, 50, 0), ncol = 2, byrow = TRUE),
      matrix(c(50, 0, 100, 0), ncol = 2, byrow = TRUE)
    ))
    line_sf <- sf::st_sf(geometry = sf::st_sfc(mls, crs = 3857))
    pts_sf <- sf::st_sf(
      geometry = sf::st_sfc(
        sf::st_point(c(25, 5)), sf::st_point(c(75, -5)), crs = 3857
      )
    )

    res <- GTFShift::project_points_along_geometry(
      line_sf, pts_sf, geometry_sample_meters = 100, metric_crs = 3857
    )
    expect_identical(nrow(res), 2L)
    expect_s3_class(res$closest_on_geometry, "sfc_POINT")
    expect_identical(sf::st_crs(res$closest_on_geometry), sf::st_crs(3857))
  }
)

test_that(
  "project_points_along_geometry handles polar extreme coordinates",
  {
    l_np <- sf::st_linestring(matrix(c(0, 89, 0, 90), ncol = 2, byrow = TRUE))
    line_north_pole <- sf::st_sfc(l_np, crs = 4326)
    pts_north_pole <- sf::st_sfc(
      sf::st_point(c(0, 89)), sf::st_point(c(0, 90)), crs = 4326
    )

    res_np <- GTFShift::project_points_along_geometry(
      line_north_pole, pts_north_pole, geometry_sample_meters = 1000
    )
    expect_identical(nrow(res_np), 2L)
    expect_identical(res_np$distance_along_geometry[1], 0)
    expect_gt(res_np$distance_along_geometry[2], 110000)

    l_sp <- sf::st_linestring(
      matrix(c(0, -89, 0, -90), ncol = 2, byrow = TRUE)
    )
    line_south_pole <- sf::st_sfc(l_sp, crs = 4326)
    pts_south_pole <- sf::st_sfc(
      sf::st_point(c(0, -89)), sf::st_point(c(0, -90)), crs = 4326
    )

    res_sp <- GTFShift::project_points_along_geometry(
      line_south_pole, pts_south_pole, geometry_sample_meters = 1000
    )
    expect_identical(nrow(res_sp), 2L)
    expect_identical(res_sp$distance_along_geometry[1], 0)
    expect_gt(res_sp$distance_along_geometry[2], 110000)
  }
)

test_that(
  "project_points_along_geometry rejects PROJ4-strings for metric_crs",
  {
    l_geo <- sf::st_linestring(
      matrix(c(-9.1, 38.7, -9.11, 38.71), ncol = 2, byrow = TRUE)
    )
    line <- sf::st_sfc(l_geo, crs = 4326)
    pts <- sf::st_sfc(sf::st_point(c(-9.1, 38.7)), crs = 4326)
    proj4_str <- paste0(
      "+proj=merc +lat_ts=0 +lon_0=0 +k=1 +x_0=0 +y_0=0 ",
      "+datum=WGS84 +units=m +no_defs"
    )

    expect_error(
      GTFShift::project_points_along_geometry(
        line, pts, metric_crs = proj4_str
      ),
      "PROJ4-strings are not permitted"
    )
  }
)

test_that(
  "project_points_along_geometry yields equivalent accuracy across CRS modes",
  {
    l_geo <- sf::st_linestring(
      matrix(c(-9.10, 38.70, -9.11, 38.71), ncol = 2, byrow = TRUE)
    )
    line_4326 <- sf::st_sfc(l_geo, crs = 4326)
    pts_4326 <- sf::st_sfc(
      sf::st_point(c(-9.10, 38.70)),
      sf::st_point(c(-9.105, 38.705)),
      sf::st_point(c(-9.11, 38.71)),
      crs = 4326
    )

    # 1. Curvilinear (EPSG:4326, s2 spherical distances)
    res_curv <- GTFShift::project_points_along_geometry(
      line_4326, pts_4326, geometry_sample_meters = 1, metric_crs = NULL
    )
    # 2. Geographic input with metric_crs = 3763
    res_metric_param <- GTFShift::project_points_along_geometry(
      line_4326, pts_4326, geometry_sample_meters = 1, metric_crs = 3763
    )
    # 3. Explicitly pre-transformed input via sf::st_transform(..., 3763)
    line_3763 <- sf::st_transform(line_4326, 3763)
    pts_3763 <- sf::st_transform(pts_4326, 3763)
    res_pre_transformed <- GTFShift::project_points_along_geometry(
      line_3763, pts_3763, geometry_sample_meters = 1
    )

    # Exact equivalence between metric_crs = 3763 and explicit st_transform
    expect_equal(
      res_metric_param$distance_along_geometry,
      res_pre_transformed$distance_along_geometry,
      tolerance = 1e-9
    )
    expect_equal(
      res_metric_param$distance_to_closest_on_geometry,
      res_pre_transformed$distance_to_closest_on_geometry,
      tolerance = 1e-9
    )

    # Equivalent accuracy (< 0.5% relative diff) between s2 and EPSG:3763
    expect_equal(
      res_curv$distance_along_geometry,
      res_metric_param$distance_along_geometry,
      tolerance = 5e-3
    )
  }
)
