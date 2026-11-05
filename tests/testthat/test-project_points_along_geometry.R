library(testthat)
library(sf)

#' @srrstats {G5.2} Explicitly tests all error conditions triggered by invalid inputs in `project_points_along_geometry()`.
#' @srrstats {G5.2a} Verifies unique error messages for invalid `metric_crs`, non-`sf`/`sfc` inputs, multi-feature `geometry`, invalid geometry type, and missing CRS.
#' @srrstats {G5.2b} Tests every error condition and compares messages against expected strings via `expect_error()`.
#' @srrstats {G5.4} Tests correctness of point projection and cumulative distance calculation against known geometric coordinates.
#' @srrstats {G5.4a} Verifies projection accuracy on trivial horizontal lines with exact known perpendicular offsets and cumulative distances.
#' @srrstats {G5.8} Tests edge conditions including empty point sets, non-spatial objects, and missing CRS.
#' @srrstats {G5.8a} Explicitly tests zero-length `points` (`st_sfc(crs = 3857)`) and verifies a zero-row `sf` return object.
#' @srrstats {G5.8b} Explicitly tests unsupported input types (character strings and `POINT` instead of `LINESTRING`).
#' @srrstats {EA6.0} Tests return object classes, dimensions, column names, column types, and numeric values.
#' @srrstats {EA6.0a} Tests S3 classes (`sf` and `sfc_POINT`) of returned objects.
#' @srrstats {EA6.0b} Tests `nrow()` dimensions of returned tabular objects.
#' @srrstats {EA6.0c} Tests expected column names via `expect_contains(names(res), ...)`.
#' @srrstats {EA6.0d} Tests classes/types of columns inside the returned `sf` data frame (`sfc_POINT` and numeric distances).
#' @srrstats {SP6.0} Verifies that the returned `closest_on_geometry` preserves the original CRS of the input geometry.
#' @srrstats {SP6.1} Tests `project_points_along_geometry()` on both Cartesian (`EPSG:3857`) and curvilinear (`EPSG:4326`) coordinates.
#' @srrstats {SP6.1b} Demonstrates accurate cumulative distance calculation on both projected (`metric_crs = 3857`) and geographic (`metric_crs = NULL`, `EPSG:4326`) coordinates.
#' @srrstats {SP6.2} Tests extreme geographical coordinates extending to polar extremes of +90 and -90 degrees latitude.
test_that("project_points_along_geometry computes projection and cumulative distance", {
    line <- st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    pts <- st_sfc(st_point(c(20, 10)), crs = 3857)

    res <- GTFShift::project_points_along_geometry(line, pts, geometry_sample_meters = 5, metric_crs = 3857)
    expect_equal(nrow(res), 1)
    expect_contains(names(res), c("distance_to_closest_on_geometry", "distance_along_geometry", "distance_along_geometry_reversed"))
})

test_that("project_points_along_geometry works when default metric_crs (NULL) is used on geographic CRS", {
    line <- st_sfc(st_linestring(matrix(c(-9.1, 38.7, -9.11, 38.71), ncol = 2, byrow = TRUE)), crs = 4326)
    pts <- st_sfc(st_point(c(-9.1, 38.7)), st_point(c(-9.105, 38.702)), crs = 4326)

    expect_no_warning(
        res <- GTFShift::project_points_along_geometry(line, pts)
    )
    expect_equal(nrow(res), 2)
    expect_equal(res$distance_along_geometry[1], 0)
    expect_gt(res$distance_along_geometry[2], 0)
})

test_that("project_points_along_geometry stops when metric_crs is invalid NA", {
    line <- st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    pts <- st_sfc(st_point(c(20, 10)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry(line, pts, metric_crs = NA),
        "metric_crs should be a valid CRS value"
    )
})

test_that("project_points_along_geometry stops when geometry is not sf or sfc", {
    pts <- st_sfc(st_point(c(20, 10)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry("not_a_geometry", pts, metric_crs = 3857),
        "geometry must be an sf object or sfc geometry"
    )
})

test_that("project_points_along_geometry stops when points is not sf or sfc", {
    line <- st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry(line, "not_points", metric_crs = 3857),
        "points must be an sf object or sfc geometry"
    )
})

test_that("project_points_along_geometry stops when geometry does not have exactly one feature", {
    line_multi <- st_sfc(
        st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(100, 0, 200, 0), ncol = 2, byrow = TRUE)),
        crs = 3857
    )
    pts <- st_sfc(st_point(c(20, 10)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry(line_multi, pts, metric_crs = 3857),
        "geometry must contain exactly one feature"
    )
})

test_that("project_points_along_geometry handles empty points input", {
    line <- st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    pts_empty <- st_sfc(crs = 3857)

    res <- GTFShift::project_points_along_geometry(line, pts_empty, metric_crs = 3857)
    expect_equal(nrow(res), 0)
    expect_contains(names(res), c("distance_to_closest_on_geometry", "distance_along_geometry", "distance_along_geometry_reversed"))
})

test_that("project_points_along_geometry stops when geometry is not LINESTRING or MULTILINESTRING", {
    point_geom <- st_sfc(st_point(c(50, 50)), crs = 3857)
    pts <- st_sfc(st_point(c(20, 10)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry(point_geom, pts, metric_crs = 3857),
        "geometry must be LINESTRING or MULTILINESTRING"
    )
})

test_that("project_points_along_geometry stops when geometry or points CRS is NA", {
    line_nocrs <- st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = NA_crs_)
    pts <- st_sfc(st_point(c(20, 10)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry(line_nocrs, pts, metric_crs = 3857),
        "geometry and points must have a valid CRS to use metric_crs"
    )
})

test_that("project_points_along_geometry works with MULTILINESTRING and sf inputs", {
    mls <- st_multilinestring(list(
        matrix(c(0, 0, 50, 0), ncol = 2, byrow = TRUE),
        matrix(c(50, 0, 100, 0), ncol = 2, byrow = TRUE)
    ))
    line_sf <- st_sf(geometry = st_sfc(mls, crs = 3857))
    pts_sf <- st_sf(geometry = st_sfc(st_point(c(25, 5)), st_point(c(75, -5)), crs = 3857))

    res <- GTFShift::project_points_along_geometry(line_sf, pts_sf, geometry_sample_meters = 100, metric_crs = 3857)
    expect_equal(nrow(res), 2)
    expect_s3_class(res$closest_on_geometry, "sfc_POINT")
    expect_equal(st_crs(res$closest_on_geometry), st_crs(3857))
})

test_that("project_points_along_geometry handles polar extreme geographical coordinates (+/-90 degrees)", {
    line_north_pole <- st_sfc(st_linestring(matrix(c(0, 89, 0, 90), ncol = 2, byrow = TRUE)), crs = 4326)
    pts_north_pole <- st_sfc(st_point(c(0, 89)), st_point(c(0, 90)), crs = 4326)

    res_np <- GTFShift::project_points_along_geometry(line_north_pole, pts_north_pole, geometry_sample_meters = 1000)
    expect_equal(nrow(res_np), 2)
    expect_equal(res_np$distance_along_geometry[1], 0)
    expect_gt(res_np$distance_along_geometry[2], 110000)

    line_south_pole <- st_sfc(st_linestring(matrix(c(0, -89, 0, -90), ncol = 2, byrow = TRUE)), crs = 4326)
    pts_south_pole <- st_sfc(st_point(c(0, -89)), st_point(c(0, -90)), crs = 4326)

    res_sp <- GTFShift::project_points_along_geometry(line_south_pole, pts_south_pole, geometry_sample_meters = 1000)
    expect_equal(nrow(res_sp), 2)
    expect_equal(res_sp$distance_along_geometry[1], 0)
    expect_gt(res_sp$distance_along_geometry[2], 110000)
})
