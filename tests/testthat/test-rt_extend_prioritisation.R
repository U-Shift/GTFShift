library(testthat)
library(sf)

test_that("rt_extend_prioritisation extends lane prioritisation with speed metrics for multiple entries", {
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

    res <- GTFShift::rt_extend_prioritisation(lanes_sf, rt_points, metric_crs = 3857)
    expect_s3_class(res, "sf")
    expect_contains(names(res), c("speed_avg", "speed_median", "speed_p15", "speed_p25", "speed_p75", "speed_p85", "speed_count"))

    # For w1: speeds 10 and 20 (point 3 at 80 is STOPPED_AT so filtered out)
    expect_equal(res$speed_avg[res$way_osm_id == "w1"], 15)
    expect_equal(res$speed_median[res$way_osm_id == "w1"], 15)
    expect_equal(unname(res$speed_p15[res$way_osm_id == "w1"]), 11.5)
    expect_equal(unname(res$speed_p25[res$way_osm_id == "w1"]), 12.5)
    expect_equal(unname(res$speed_p75[res$way_osm_id == "w1"]), 17.5)
    expect_equal(unname(res$speed_p85[res$way_osm_id == "w1"]), 18.5)
    expect_equal(res$speed_count[res$way_osm_id == "w1"], 2)

    # For w2: speed 40
    expect_equal(res$speed_avg[res$way_osm_id == "w2"], 40)
    expect_equal(res$speed_median[res$way_osm_id == "w2"], 40)
    expect_equal(unname(res$speed_p15[res$way_osm_id == "w2"]), 40)
    expect_equal(unname(res$speed_p25[res$way_osm_id == "w2"]), 40)
    expect_equal(unname(res$speed_p75[res$way_osm_id == "w2"]), 40)
    expect_equal(unname(res$speed_p85[res$way_osm_id == "w2"]), 40)
    expect_equal(res$speed_count[res$way_osm_id == "w2"], 1)
})

test_that("rt_extend_prioritisation accurately computes speed_p15 and speed_p85 with multiple observations and NA values", {
    lanes_sf <- st_sf(
        way_osm_id = c("w1", "w2", "w_empty"),
        geometry = st_sfc(
            st_linestring(matrix(c(-9.150, 38.700, -9.140, 38.700), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(-9.130, 38.700, -9.120, 38.700), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(-9.100, 38.700, -9.090, 38.700), ncol = 2, byrow = TRUE)),
            crs = 4326
        )
    )

    # w1: 10 points with speeds 10, 20, ..., 100
    w1_lons <- seq(-9.149, -9.141, length.out = 10)
    w1_points <- lapply(w1_lons, function(x) st_point(c(x, 38.700)))
    w1_speeds <- seq(10, 100, by = 10)

    # w2: 4 points, one with NA speed: speeds 20, 40, NA, 60
    w2_lons <- seq(-9.128, -9.122, length.out = 4)
    w2_points <- lapply(w2_lons, function(x) st_point(c(x, 38.700)))
    w2_speeds <- c(20, 40, NA, 60)

    rt_points <- st_sf(
        speed = c(w1_speeds, w2_speeds),
        current_status = "IN_TRANSIT_TO",
        geometry = st_sfc(c(w1_points, w2_points), crs = 4326)
    )

    res <- GTFShift::rt_extend_prioritisation(lanes_sf, rt_points, metric_crs = 3763)

    # w1 percentiles: 15% is 23.5, 25% is 32.5, 75% is 77.5, 85% is 86.5
    expect_equal(unname(res$speed_p15[res$way_osm_id == "w1"]), 23.5)
    expect_equal(unname(res$speed_p25[res$way_osm_id == "w1"]), 32.5)
    expect_equal(unname(res$speed_p75[res$way_osm_id == "w1"]), 77.5)
    expect_equal(unname(res$speed_p85[res$way_osm_id == "w1"]), 86.5)
    expect_equal(res$speed_count[res$way_osm_id == "w1"], 10)

    # Verify monotonic percentile relationships: p15 <= p25 <= median <= p75 <= p85
    expect_true(res$speed_p15[res$way_osm_id == "w1"] <= res$speed_p25[res$way_osm_id == "w1"])
    expect_true(res$speed_p25[res$way_osm_id == "w1"] <= res$speed_median[res$way_osm_id == "w1"])
    expect_true(res$speed_median[res$way_osm_id == "w1"] <= res$speed_p75[res$way_osm_id == "w1"])
    expect_true(res$speed_p75[res$way_osm_id == "w1"] <= res$speed_p85[res$way_osm_id == "w1"])

    # w2 percentiles with NA: speeds c(20, 40, 60) -> 15% is 26, 85% is 54
    expect_equal(unname(res$speed_p15[res$way_osm_id == "w2"]), 26.0)
    expect_equal(unname(res$speed_p85[res$way_osm_id == "w2"]), 54.0)
    expect_equal(res$speed_count[res$way_osm_id == "w2"], 4)

    # w_empty: no points captured -> speed_p15 and speed_p85 are NA
    expect_true(is.na(res$speed_p15[res$way_osm_id == "w_empty"]))
    expect_true(is.na(res$speed_p85[res$way_osm_id == "w_empty"]))
    expect_true(is.na(res$speed_count[res$way_osm_id == "w_empty"]))
})

test_that("rt_extend_prioritisation raises warning when metric_crs is default", {
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
        GTFShift::rt_extend_prioritisation(lanes_sf, rt_points),
        "Using default metric_crs"
    )
})

test_that("rt_extend_prioritisation stops when lane_prioritisation is missing way_osm_id column", {
    invalid_lanes <- st_sf(
        id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 4326)
    )

    rt_points <- st_sf(
        speed = 25,
        geometry = st_sfc(st_point(c(50, 0)), crs = 4326)
    )

    expect_error(
        GTFShift::rt_extend_prioritisation(invalid_lanes, rt_points, metric_crs = 3857),
        "lane_prioritisation is missing required columns: way_osm_id"
    )
})

test_that("rt_extend_prioritisation stops when rt_collection is missing speed column", {
    lanes_sf <- st_sf(
        way_osm_id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 4326)
    )

    invalid_rt <- st_sf(
        velocity = 25,
        geometry = st_sfc(st_point(c(50, 0)), crs = 4326)
    )

    expect_error(
        GTFShift::rt_extend_prioritisation(lanes_sf, invalid_rt, metric_crs = 3857),
        "rt_collection is missing required columns: speed"
    )
})

test_that("rt_extend_prioritisation stops when metric_crs is invalid", {
    lanes_sf <- st_sf(
        way_osm_id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 4326)
    )

    rt_points <- st_sf(
        speed = 25,
        geometry = st_sfc(st_point(c(50, 0)), crs = 4326)
    )

    expect_error(
        GTFShift::rt_extend_prioritisation(lanes_sf, rt_points, metric_crs = NA)
    )
})
