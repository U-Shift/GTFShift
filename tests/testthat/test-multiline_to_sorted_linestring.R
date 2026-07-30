library(testthat)
library(sf)

test_that("multiline_to_sorted_linestring sorts multilinestring into linestring", {
    mls <- st_multilinestring(list(
        matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
        matrix(c(1, 0, 2, 0), ncol = 2, byrow = TRUE)
    ))
    mls_sf <- st_sf(geometry = st_sfc(mls, crs = 4326))

    res <- GTFShift::multiline_to_sorted_linestring(mls_sf, metric_crs = 3857)
    expect_s3_class(res, "sfc")
    expect_equal(st_geometry_type(res)[1], factor("LINESTRING", levels = levels(st_geometry_type(res))))
})

test_that("multiline_to_sorted_linestring emits warning when default metric_crs is used", {
    mls <- st_multilinestring(list(
        matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
        matrix(c(1, 0, 2, 0), ncol = 2, byrow = TRUE)
    ))
    mls_sf <- st_sf(geometry = st_sfc(mls, crs = 4326))

    expect_warning(
        GTFShift::multiline_to_sorted_linestring(mls_sf),
        "Using default metric_crs \\(EPSG:3857\\)"
    )
})

test_that("multiline_to_sorted_linestring stops when metric_crs is invalid NA", {
    mls <- st_multilinestring(list(
        matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)
    ))
    mls_sf <- st_sf(geometry = st_sfc(mls, crs = 4326))

    expect_error(
        GTFShift::multiline_to_sorted_linestring(mls_sf, metric_crs = NA),
        "metric_crs should be a valid CRS value"
    )
})

test_that("multiline_to_sorted_linestring parameter variation with guiding points", {
    # Line 1: (1, 0) to (2, 0), Line 2: (0, 0) to (1, 0)
    mls <- st_multilinestring(list(
        matrix(c(1, 0, 2, 0), ncol = 2, byrow = TRUE),
        matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)
    ))
    mls_sf <- st_sf(geometry = st_sfc(mls, crs = 4326))

    pts <- st_sfc(
        st_point(c(0, 0)),
        st_point(c(2, 0)),
        crs = 4326
    )

    res <- GTFShift::multiline_to_sorted_linestring(mls_sf, points = pts, metric_crs = 3857)
    coords <- unname(st_coordinates(res))
    
    # Should start near (0,0) and end near (2,0)
    expect_equal(coords[1, 1:2], c(0, 0))
    expect_equal(coords[nrow(coords), 1:2], c(2, 0))
})

test_that("multiline_to_sorted_linestring discards line segments farther than current + next length", {
    # Line 1: (0, 0) to (1, 0), Line 2: (100, 0) to (101, 0) (way too far, length=1, dist=99)
    mls <- st_multilinestring(list(
        matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
        matrix(c(100, 0, 101, 0), ncol = 2, byrow = TRUE)
    ))
    mls_sf <- st_sf(geometry = st_sfc(mls, crs = 4326))

    res <- GTFShift::multiline_to_sorted_linestring(mls_sf, metric_crs = 3857)
    coords <- unname(st_coordinates(res))

    # Discards the far segment and retains only the first segment
    expect_equal(nrow(coords), 2)
    expect_equal(coords[1, 1:2], c(0, 0))
    expect_equal(coords[2, 1:2], c(1, 0))
})

test_that("multiline_to_sorted_linestring orient start line when start_point is closer to remaining lines", {
    mls <- st_multilinestring(list(
        matrix(c(1, 0, 0, 0), ncol = 2, byrow = TRUE),
        matrix(c(1, 0, 2, 0), ncol = 2, byrow = TRUE)
    ))
    mls_sf <- st_sf(geometry = st_sfc(mls, crs = 4326))
    pts <- st_sfc(st_point(c(0, 0)), crs = 4326)

    res <- GTFShift::multiline_to_sorted_linestring(mls_sf, points = pts, metric_crs = 3857)
    coords <- unname(st_coordinates(res))
    expect_equal(coords[1, 1:2], c(0, 0))
    expect_equal(coords[nrow(coords), 1:2], c(2, 0))
})

test_that("multiline_to_sorted_linestring orient start line with second_point tie break", {
    mls <- st_multilinestring(list(
        matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE),
        matrix(c(-1, 0, 0, 0), ncol = 2, byrow = TRUE),
        matrix(c(2, 0, 3, 0), ncol = 2, byrow = TRUE)
    ))
    mls_sf <- st_sf(geometry = st_sfc(mls, crs = 4326))
    pts <- st_sfc(st_point(c(1, 0)), st_point(c(0, 0)), crs = 4326)

    res <- GTFShift::multiline_to_sorted_linestring(mls_sf, points = pts, metric_crs = 3857)
    coords <- unname(st_coordinates(res))
    expect_equal(coords[1, 1:2], c(2, 0))
})

test_that("multiline_to_sorted_linestring excludes identical duplicate remaining segments", {
    mls <- st_multilinestring(list(
        matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
        matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)
    ))
    mls_sf <- st_sf(geometry = st_sfc(mls, crs = 4326))
    pts <- st_sfc(st_point(c(0, 0)), crs = 4326)

    res <- GTFShift::multiline_to_sorted_linestring(mls_sf, points = pts, metric_crs = 3857)
    coords <- unname(st_coordinates(res))
    expect_equal(nrow(coords), 2)
})

test_that("multiline_to_sorted_linestring tie-breaks candidate selection using next_point proximity", {
    # Line 1: (0, 0) to (1, 0)
    # Candidate A: (1, 0) to (1, 1)
    # Candidate B: (1, 0) to (1, -2)
    mls <- st_multilinestring(list(
        matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
        matrix(c(1, 0, 1, -2), ncol = 2, byrow = TRUE),
        matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE)
    ))
    mls_sf <- st_sf(geometry = st_sfc(mls, crs = 4326))
    pts <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), crs = 4326)

    res <- GTFShift::multiline_to_sorted_linestring(mls_sf, points = pts, metric_crs = 3857)
    expect_s3_class(res, "sfc")
    coords <- unname(st_coordinates(res))
    expect_gte(nrow(coords), 3)
})

test_that("multiline_to_sorted_linestring handles circular ring line segment", {
    # Line 1: (0, 0) to (1, 0)
    # Line 2 (Circular ring): (1, 0) -> (2, 1) -> (2, -1) -> (1, 0)
    mls <- st_multilinestring(list(
        matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
        matrix(c(1, 0, 2, 1, 2, -1, 1, 0), ncol = 2, byrow = TRUE)
    ))
    mls_sf <- st_sf(geometry = st_sfc(mls, crs = 4326))

    res <- GTFShift::multiline_to_sorted_linestring(mls_sf, metric_crs = 3857)
    expect_s3_class(res, "sfc")
})
