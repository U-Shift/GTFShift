test_that("project_points_along_geometry computes projection and cumulative distance", {
    line <- sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    res <- GTFShift::project_points_along_geometry(line, pts, geometry_sample_meters = 5, metric_crs = 3857)
    expect_equal(nrow(res), 1)
    expect_contains(names(res), c("distance_to_closest_on_geometry", "distance_along_geometry", "distance_along_geometry_reversed"))
})

test_that("project_points_along_geometry works when default metric_crs (NULL) is used on geographic CRS", {
    line <- sf::st_sfc(sf::st_linestring(matrix(c(-9.1, 38.7, -9.11, 38.71), ncol = 2, byrow = TRUE)), crs = 4326)
    pts <- sf::st_sfc(sf::st_point(c(-9.1, 38.7)), sf::st_point(c(-9.105, 38.702)), crs = 4326)

    expect_no_warning(
        res <- GTFShift::project_points_along_geometry(line, pts)
    )
    expect_equal(nrow(res), 2)
    expect_equal(res$distance_along_geometry[1], 0)
    expect_gt(res$distance_along_geometry[2], 0)
})

test_that("project_points_along_geometry stops when metric_crs is invalid NA", {
    line <- sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry(line, pts, metric_crs = NA),
        "metric_crs should be a valid CRS value"
    )
})

test_that("project_points_along_geometry stops when geometry is not sf or sfc", {
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry("not_a_geometry", pts, metric_crs = 3857),
        "geometry must be an sf object or sfc geometry"
    )
})

test_that("project_points_along_geometry stops when points is not sf or sfc", {
    line <- sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry(line, "not_points", metric_crs = 3857),
        "points must be an sf object or sfc geometry"
    )
})

test_that("project_points_along_geometry stops when geometry does not have exactly one feature", {
    line_multi <- sf::st_sfc(
        sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
        sf::st_linestring(matrix(c(100, 0, 200, 0), ncol = 2, byrow = TRUE)),
        crs = 3857
    )
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry(line_multi, pts, metric_crs = 3857),
        "geometry must contain exactly one feature"
    )
})

test_that("project_points_along_geometry handles empty points input", {
    line <- sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    pts_empty <- sf::st_sfc(crs = 3857)

    res <- GTFShift::project_points_along_geometry(line, pts_empty, metric_crs = 3857)
    expect_equal(nrow(res), 0)
    expect_contains(names(res), c("distance_to_closest_on_geometry", "distance_along_geometry", "distance_along_geometry_reversed"))
})

test_that("project_points_along_geometry stops when geometry is not LINESTRING or MULTILINESTRING", {
    point_geom <- sf::st_sfc(sf::st_point(c(50, 50)), crs = 3857)
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry(point_geom, pts, metric_crs = 3857),
        "geometry must be LINESTRING or MULTILINESTRING"
    )
})

test_that("project_points_along_geometry stops when geometry or points CRS is NA", {
    line_nocrs <- sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = NA_crs_)
    pts <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 3857)

    expect_error(
        GTFShift::project_points_along_geometry(line_nocrs, pts, metric_crs = 3857),
        "geometry and points must have a valid CRS to use metric_crs"
    )
})

test_that("project_points_along_geometry works with MULTILINESTRING and sf inputs", {
    mls <- sf::st_multilinestring(list(
        matrix(c(0, 0, 50, 0), ncol = 2, byrow = TRUE),
        matrix(c(50, 0, 100, 0), ncol = 2, byrow = TRUE)
    ))
    line_sf <- sf::st_sf(geometry = sf::st_sfc(mls, crs = 3857))
    pts_sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(25, 5)), sf::st_point(c(75, -5)), crs = 3857))

    res <- GTFShift::project_points_along_geometry(line_sf, pts_sf, geometry_sample_meters = 100, metric_crs = 3857)
    expect_equal(nrow(res), 2)
    expect_s3_class(res$closest_on_geometry, "sfc_POINT")
    expect_equal(sf::st_crs(res$closest_on_geometry), sf::st_crs(3857))
})
