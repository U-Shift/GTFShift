library(testthat)
library(sf)

test_that("rt_average_speed computes speeds along trip geometry", {
    trip_geom <- st_sf(
        trip_id = c("T1", "T2"),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(0, 100, 1000, 100), ncol = 2, byrow = TRUE)),
            crs = 3857
        )
    )

    rt_updates <- st_sf(
        trip_id = c("T1", "T1", "UNMATCHED_TRIP", "T2", "T2"),
        timestamp = c(1000, 1060, 1070, 1080, 1140),
        geometry = st_sfc(
            st_point(c(0, 0)),
            st_point(c(500, 10)), # Offset by 10m off line
            st_point(c(600, 0)),
            st_point(c(0, 100)),
            st_point(c(400, 100)),
            crs = 3857
        )
    )

    expect_warning(
        res <- GTFShift::rt_average_speed(rt_updates, trip_geom, metric_crs = 3857),
        "Trip UNMATCHED_TRIP has less than 2 updates. Ignoring it."
    )
    expect_s3_class(res, "sf")
    expect_equal(nrow(res), 4)
    expect_contains(unique(res$trip_id), c("T1", "T2"))
    expect_false("UNMATCHED_TRIP" %in% res$trip_id)

    expected_cols <- c(
        "trip_id",
        "timestamp",
        "geometry",
        "distance_along_geometry",
        "distance_along_geometry_reversed",
        "distance_to_closest_on_geometry",
        "time_since_prev_sec",
        "distance_since_prev_meters",
        "speed_kmh"
    )
    expect_contains(names(res), expected_cols)

    # Validate row 1 (first observation in T1: previous values and speed are NA)
    expect_equal(res$trip_id[1], "T1")
    expect_equal(res$timestamp[1], 1000)
    expect_equal(res$distance_along_geometry[1], 0)
    expect_equal(res$distance_along_geometry_reversed[1], 1000)
    expect_equal(res$distance_to_closest_on_geometry[1], 0)
    expect_true(is.na(res$time_since_prev_sec[1]))
    expect_true(is.na(res$distance_since_prev_meters[1]))
    expect_true(is.na(res$speed_kmh[1]))

    # Validate row 2 (second observation in T1 offset by 10m off line: distance_to_closest_on_geometry == 10)
    expect_equal(res$trip_id[2], "T1")
    expect_equal(res$timestamp[2], 1060)
    expect_equal(res$time_since_prev_sec[2], 60)
    expect_equal(res$distance_along_geometry[2], 494.95, tolerance = 1e-2)
    expect_equal(res$distance_along_geometry_reversed[2], 505.05, tolerance = 1e-2)
    expect_equal(res$distance_to_closest_on_geometry[2], 10, tolerance = 1e-2)
    expect_equal(res$distance_since_prev_meters[2], 494.95, tolerance = 1e-2)
    expect_equal(res$speed_kmh[2], 29.7, tolerance = 0.5)

    # Validate row 3 & 4 (trip T2: 2 updates, distance ~404m over 60s -> ~24.2 km/h)
    expect_equal(res$trip_id[3], "T2")
    expect_equal(res$timestamp[3], 1080)
    expect_true(is.na(res$speed_kmh[3]))

    expect_equal(res$trip_id[4], "T2")
    expect_equal(res$timestamp[4], 1140)
    expect_equal(res$time_since_prev_sec[4], 60)
    expect_equal(res$distance_since_prev_meters[4], 404.04, tolerance = 1e-2)
    expect_equal(res$speed_kmh[4], 24.24, tolerance = 0.5)
})

test_that("rt_average_speed issues warning and ignores trips with less than 2 updates", {
    trip_geom <- st_sf(
        trip_id = "T1",
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )

    rt_updates_single <- st_sf(
        trip_id = "T1",
        timestamp = 1000,
        geometry = st_sfc(st_point(c(0, 0)), crs = 3857)
    )

    expect_warning(
        res <- GTFShift::rt_average_speed(rt_updates_single, trip_geom, metric_crs = 3857),
        "Trip T1 has less than 2 updates. Ignoring it."
    )
    expect_equal(nrow(res), 0)
})

test_that("rt_average_speed stops on invalid inputs and mismatched columns", {
    trip_geom <- st_sf(
        trip_id = "T1",
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )

    rt_updates <- st_sf(
        trip_id = c("T1", "T1"),
        timestamp = c(1000, 1060),
        geometry = st_sfc(
            st_point(c(0, 0)),
            st_point(c(500, 0)),
            crs = 3857
        )
    )

    # Non-sf rt_collection
    expect_error(
        GTFShift::rt_average_speed(data.frame(trip_id = "T1"), trip_geom),
        "rt_collection must be an sf object"
    )

    # Non-sf trips_geometries
    expect_error(
        GTFShift::rt_average_speed(rt_updates, data.frame(trip_id = "T1")),
        "trips_geometries must be an sf object"
    )

    # MULTILINESTRING trip geometry
    multiline_geom <- st_sf(
        trip_id = "T1",
        geometry = st_sfc(st_multilinestring(list(matrix(c(0, 0, 500, 0), ncol = 2, byrow = TRUE))), crs = 3857)
    )
    expect_error(
        GTFShift::rt_average_speed(rt_updates, multiline_geom),
        "trips_geometries geometry must be LINESTRING"
    )

    # Missing match column in rt_collection
    expect_error(
        GTFShift::rt_average_speed(rt_updates, trip_geom, rt_collection_trips_geometries_match_col = "missing_col"),
        "rt_collection_trips_geometries_match_col must be one of the columns in rt_collection"
    )

    # Missing match column in trips_geometries
    trip_geom_other_col <- st_sf(
        other_id = "T1",
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )
    expect_error(
        GTFShift::rt_average_speed(rt_updates, trip_geom_other_col, rt_collection_trips_geometries_match_col = "trip_id"),
        "rt_collection_trips_geometries_match_col must be one of the columns in trips_geometries"
    )

    # Missing required timestamp column
    rt_no_time <- st_sf(
        trip_id = c("T1", "T1"),
        geometry = st_sfc(st_point(c(0, 0)), st_point(c(500, 0)), crs = 3857)
    )
    expect_error(
        GTFShift::rt_average_speed(rt_no_time, trip_geom),
        "rt_collection is missing required columns"
    )
})

