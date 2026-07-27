library(testthat)
library(sf)

test_that("rt_extend_prioritization extends lane prioritization with speed metrics for multiple entries", {
    lanes_sf <- st_sf(
        way_osm_id = c("w1", "w2"),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(200, 0, 300, 0), ncol = 2, byrow = TRUE)),
            crs = 4326
        )
    )

    rt_points <- st_sf(
        speed = c(10, 20, 30, 40),
        current_status = c("IN_TRANSIT_TO", "IN_TRANSIT_TO", "STOPPED_AT", "IN_TRANSIT_TO"),
        geometry = st_sfc(
            st_point(c(20, 0)),
            st_point(c(50, 0)),
            st_point(c(80, 0)), # Filtered out due to status STOPPED_AT
            st_point(c(250, 0)),
            crs = 4326
        )
    )

    res <- GTFShift::rt_extend_prioritization(lanes_sf, rt_points, metric_crs = 3857)
    expect_s3_class(res, "sf")
    expect_contains(names(res), c("speed_avg", "speed_median", "speed_p25", "speed_p75", "speed_count"))

    # For w1: speeds 10 and 20 (point 3 at 80 is STOPPED_AT so filtered out)
    expect_equal(res$speed_avg[res$way_osm_id == "w1"], 15)
    expect_equal(res$speed_median[res$way_osm_id == "w1"], 15)
    expect_equal(unname(res$speed_p25[res$way_osm_id == "w1"]), 12.5)
    expect_equal(unname(res$speed_p75[res$way_osm_id == "w1"]), 17.5)
    expect_equal(res$speed_count[res$way_osm_id == "w1"], 2)

    # For w2: speed 40
    expect_equal(res$speed_avg[res$way_osm_id == "w2"], 40)
    expect_equal(res$speed_count[res$way_osm_id == "w2"], 1)
})

test_that("rt_extend_prioritization raises warning when metric_crs is default", {
    lanes_sf <- st_sf(
        way_osm_id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 4326)
    )

    rt_points <- st_sf(
        speed = 25,
        current_status = "IN_TRANSIT_TO",
        geometry = st_sfc(st_point(c(50, 0)), crs = 4326)
    )

    expect_warning(
        GTFShift::rt_extend_prioritization(lanes_sf, rt_points),
        "Using default metric_crs"
    )
})

test_that("rt_extend_prioritization stops when lane_prioritization is missing way_osm_id column", {
    invalid_lanes <- st_sf(
        id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 4326)
    )

    rt_points <- st_sf(
        speed = 25,
        geometry = st_sfc(st_point(c(50, 0)), crs = 4326)
    )

    expect_error(
        GTFShift::rt_extend_prioritization(invalid_lanes, rt_points, metric_crs = 3857),
        "lane_prioritization is missing required columns: way_osm_id"
    )
})

test_that("rt_extend_prioritization stops when rt_collection is missing speed column", {
    lanes_sf <- st_sf(
        way_osm_id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 4326)
    )

    invalid_rt <- st_sf(
        velocity = 25,
        geometry = st_sfc(st_point(c(50, 0)), crs = 4326)
    )

    expect_error(
        GTFShift::rt_extend_prioritization(lanes_sf, invalid_rt, metric_crs = 3857),
        "rt_collection is missing required columns: speed"
    )
})

test_that("rt_extend_prioritization stops when metric_crs is invalid", {
    lanes_sf <- st_sf(
        way_osm_id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 4326)
    )

    rt_points <- st_sf(
        speed = 25,
        geometry = st_sfc(st_point(c(50, 0)), crs = 4326)
    )

    expect_error(
        GTFShift::rt_extend_prioritization(lanes_sf, rt_points, metric_crs = NA)
    )
})
