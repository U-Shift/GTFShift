library(testthat)
library(sf)

test_that("get_prioritization_stats calculates summary statistics without speed_avg", {
    prioritization_df <- st_sf(
        is_bus_lane = c(TRUE, FALSE),
        frequency = c(10, 5),
        n_lanes_circulation = c(2, 4),
        n_lanes_parking = c(1, 3),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), # 100 m
            st_linestring(matrix(c(100, 0, 300, 0), ncol = 2, byrow = TRUE)), # 200 m
            crs = 3857
        )
    )

    stats <- GTFShift::get_prioritization_stats(prioritization_df, weight = "length", metric_crs = 3857)
    expect_type(stats, "list")
    expect_contains(names(stats), c("extension", "extension_bus_lane", "n_lanes_circulation_avg", "n_lanes_parking_avg"))

    # Internal length mutation: feature 1 = 100m, feature 2 = 200m
    # extension: total length = 300m
    expect_equal(stats$extension, 300)
    # extension_bus_lane: only feature 1 is bus lane -> 100m
    expect_equal(stats$extension_bus_lane, 100)

    # Weighted mean by length (weights: 100 and 200, total = 300):
    # n_lanes_circulation: (2*100 + 4*200) / 300 = (200 + 800) / 300 = 10/3
    expect_equal(stats$n_lanes_circulation_avg, 10 / 3)
    expect_equal(stats$n_lanes_circulation_min, 2)
    expect_equal(stats$n_lanes_circulation_max, 4)

    # n_lanes_parking: (1*100 + 3*200) / 300 = (100 + 600) / 300 = 7/3
    expect_equal(stats$n_lanes_parking_avg, 7 / 3)
    expect_equal(stats$n_lanes_parking_min, 1)
    expect_equal(stats$n_lanes_parking_max, 3)

    # Validate that speed metrics are NOT present when speed_avg column is missing
    expect_false(any(c("speed_avg", "speed_min", "speed_max") %in% names(stats)))
})

test_that("get_prioritization_stats calculates speed metrics when speed_avg column is present", {
    prioritization_df <- st_sf(
        is_bus_lane = c(TRUE, FALSE),
        frequency = c(10, 5),
        speed_avg = c(30, 50),
        n_lanes_circulation = c(2, 3),
        n_lanes_parking = c(1, 0),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(100, 0, 200, 0), ncol = 2, byrow = TRUE)),
            crs = 3857
        )
    )

    # Test with weight = "frequency" (weights: 10 and 5, total = 15)
    stats <- GTFShift::get_prioritization_stats(prioritization_df, weight = "frequency", metric_crs = 3857)
    expect_type(stats, "list")
    expect_contains(names(stats), c("speed_avg", "speed_min", "speed_max"))

    # speed_avg weighted by frequency: (30*10 + 50*5) / 15 = 550 / 15 = 36.66667
    expect_equal(stats$speed_avg, (30 * 10 + 50 * 5) / 15)
    expect_equal(stats$speed_min, 30)
    expect_equal(stats$speed_max, 50)

    # n_lanes_circulation weighted by frequency: (2*10 + 3*5) / 15 = 35 / 15 = 7/3
    expect_equal(stats$n_lanes_circulation_avg, 35 / 15)
    expect_equal(stats$n_lanes_circulation_min, 2)
    expect_equal(stats$n_lanes_circulation_max, 3)

    # n_lanes_parking weighted by frequency: (1*10 + 0*5) / 15 = 10 / 15 = 2/3
    expect_equal(stats$n_lanes_parking_avg, 10 / 15)
    expect_equal(stats$n_lanes_parking_min, 0)
    expect_equal(stats$n_lanes_parking_max, 1)
})

test_that("get_prioritization_stats raises warning when metric_crs is default", {
    prioritization_df <- st_sf(
        is_bus_lane = c(TRUE, FALSE),
        frequency = c(10, 5),
        n_lanes_circulation = c(2, 3),
        n_lanes_parking = c(1, 0),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(100, 0, 200, 0), ncol = 2, byrow = TRUE)),
            crs = 3857
        )
    )

    expect_warning(
        GTFShift::get_prioritization_stats(prioritization_df, weight = "length"),
        "Using default metric_crs"
    )
})

test_that("get_prioritization_stats stops on invalid weight or metric_crs", {
    prioritization_df <- st_sf(
        is_bus_lane = c(TRUE, FALSE),
        frequency = c(10, 5),
        n_lanes_circulation = c(2, 3),
        n_lanes_parking = c(1, 0),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(100, 0, 200, 0), ncol = 2, byrow = TRUE)),
            crs = 3857
        )
    )

    # Invalid weight choice
    expect_error(
        GTFShift::get_prioritization_stats(prioritization_df, weight = "invalid_weight", metric_crs = 3857)
    )

    # Invalid CRS
    expect_error(
        GTFShift::get_prioritization_stats(prioritization_df, weight = "length", metric_crs = NA),
        "metric_crs should be a valid CRS value"
    )
})

test_that("get_prioritization_stats calculates stats using weight = 'length' vs weight = 'frequency'", {
    # Feature 1: length = 100m, frequency = 30, speed_avg = 20, n_lanes_circulation = 2
    # Feature 2: length = 300m, frequency = 10, speed_avg = 60, n_lanes_circulation = 4
    prioritization_df <- st_sf(
        is_bus_lane = c(TRUE, FALSE),
        frequency = c(30, 10),
        speed_avg = c(20, 60),
        n_lanes_circulation = c(2, 4),
        n_lanes_parking = c(1, 3),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(100, 0, 400, 0), ncol = 2, byrow = TRUE)),
            crs = 3857
        )
    )

    # 1) Weight by length (weights: 100 and 300, total = 400)
    stats_length <- GTFShift::get_prioritization_stats(prioritization_df, weight = "length", metric_crs = 3857)
    # speed_avg: (20*100 + 60*300) / 400 = (2000 + 18000) / 400 = 50
    expect_equal(stats_length$speed_avg, 50)
    # n_lanes_circulation_avg: (2*100 + 4*300) / 400 = (200 + 1200) / 400 = 3.5
    expect_equal(stats_length$n_lanes_circulation_avg, 3.5)

    # 2) Weight by frequency (weights: 30 and 10, total = 40)
    stats_freq <- GTFShift::get_prioritization_stats(prioritization_df, weight = "frequency", metric_crs = 3857)
    # speed_avg: (20*30 + 60*10) / 40 = (600 + 600) / 40 = 30
    expect_equal(stats_freq$speed_avg, 30)
    # n_lanes_circulation_avg: (2*30 + 4*10) / 40 = (60 + 40) / 40 = 2.5
    expect_equal(stats_freq$n_lanes_circulation_avg, 2.5)
})
