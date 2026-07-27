library(testthat)
library(sf)

test_that("project_points_along_geometry computes projection and cumulative distance", {
    line <- st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    pts <- st_sfc(st_point(c(20, 10)), crs = 3857)

    res <- GTFShift::project_points_along_geometry(line, pts, geometry_sample_meters = 5, metric_crs = 3857)
    expect_equal(nrow(res), 1)
    expect_contains(names(res), c("distance_to_closest_on_geometry", "distance_along_geometry", "distance_along_geometry_reversed"))
})

test_that("project_points_along_geometry emits warning when default metric_crs is used", {
    line <- st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    pts <- st_sfc(st_point(c(20, 10)), crs = 3857)

    expect_warning(
        GTFShift::project_points_along_geometry(line, pts),
        "Using default metric_crs \\(EPSG:3857\\)"
    )
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
