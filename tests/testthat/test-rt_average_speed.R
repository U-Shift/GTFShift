test_that("rt_average_speed computes speeds along trip geometry", {
    trip_geom <- sf::st_sf(
        trip_id = c("T1", "T2"),
        geometry = sf::st_sfc(
            sf::st_linestring(matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)),
            sf::st_linestring(matrix(c(0, 100, 1000, 100), ncol = 2, byrow = TRUE)),
            crs = 3857
        )
    )

    rt_updates <- sf::st_sf(
        trip_id = c("T1", "T1", "UNMATCHED_TRIP", "T2", "T2"),
        timestamp = c(1000, 1060, 1070, 1080, 1140),
        geometry = sf::st_sfc(
            sf::st_point(c(0, 0)),
            sf::st_point(c(500, 10)), # Offset by 10m off line
            sf::st_point(c(600, 0)),
            sf::st_point(c(0, 100)),
            sf::st_point(c(400, 100)),
            crs = 3857
        )
    )

    expect_warning(
        res <- GTFShift::rt_average_speed(rt_updates, trip_geom, metric_crs = 3857, geometry_sample_meters = 1),
        "Trip UNMATCHED_TRIP has less than 2 updates. Ignoring it."
    )
    expect_s3_class(res, "sf")
    expect_identical(nrow(res), 4L)
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

    # Validate first observation in T1 (timestamp == 1000: previous values and speed are NA)
    r_t1_1 <- res[res$trip_id == "T1" & res$timestamp == 1000, ]
    expect_identical(nrow(r_t1_1), 1L)
    expect_identical(r_t1_1$distance_along_geometry, 0)
    expect_identical(r_t1_1$distance_along_geometry_reversed, 1000)
    expect_identical(r_t1_1$distance_to_closest_on_geometry, 0)
    expect_true(is.na(r_t1_1$time_since_prev_sec))
    expect_true(is.na(r_t1_1$distance_since_prev_meters))
    expect_true(is.na(r_t1_1$speed_kmh))

    # Validate second observation in T1 (timestamp == 1060 offset by 10m off line: distance_to_closest_on_geometry == 10)
    r_t1_2 <- res[res$trip_id == "T1" & res$timestamp == 1060, ]
    expect_identical(nrow(r_t1_2), 1L)
    expect_identical(r_t1_2$time_since_prev_sec, 60)
    expect_equal(r_t1_2$distance_along_geometry, 500, tolerance = 1e-2)
    expect_equal(r_t1_2$distance_along_geometry_reversed, 500, tolerance = 1e-2)
    expect_equal(r_t1_2$distance_to_closest_on_geometry, 10, tolerance = 1e-2)
    expect_equal(r_t1_2$distance_since_prev_meters, 500, tolerance = 1e-2)
    expect_equal(r_t1_2$speed_kmh, 30, tolerance = 0.5)

    # Validate trip T2 (timestamp == 1080: first update, speed is NA)
    r_t2_1 <- res[res$trip_id == "T2" & res$timestamp == 1080, ]
    expect_identical(nrow(r_t2_1), 1L)
    expect_true(is.na(r_t2_1$speed_kmh))

    # Validate trip T2 (timestamp == 1140: second update, distance 400m over 60s -> 24 km/h)
    r_t2_2 <- res[res$trip_id == "T2" & res$timestamp == 1140, ]
    expect_identical(nrow(r_t2_2), 1L)
    expect_identical(r_t2_2$time_since_prev_sec, 60)
    expect_equal(r_t2_2$distance_since_prev_meters, 400, tolerance = 1e-2)
    expect_equal(r_t2_2$distance_along_geometry, 400, tolerance = 1e-2)
    expect_equal(r_t2_2$distance_along_geometry_reversed, 600, tolerance = 1e-2)
    expect_equal(r_t2_2$speed_kmh, 24, tolerance = 0.5)
})

test_that("rt_average_speed issues warning and ignores trips with less than 2 updates", {
    trip_geom <- sf::st_sf(
        trip_id = "T1",
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )

    rt_updates_single <- sf::st_sf(
        trip_id = "T1",
        timestamp = 1000,
        geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
    )

    expect_warning(
        res <- GTFShift::rt_average_speed(rt_updates_single, trip_geom, metric_crs = 3857),
        "Trip T1 has less than 2 updates. Ignoring it."
    )
    expect_identical(nrow(res), 0L)
})

test_that("rt_average_speed stops on invalid inputs and mismatched columns", {
    trip_geom <- sf::st_sf(
        trip_id = "T1",
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )

    rt_updates <- sf::st_sf(
        trip_id = c("T1", "T1"),
        timestamp = c(1000, 1060),
        geometry = sf::st_sfc(
            sf::st_point(c(0, 0)),
            sf::st_point(c(500, 0)),
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
    multiline_geom <- sf::st_sf(
        trip_id = "T1",
        geometry = sf::st_sfc(sf::st_multilinestring(list(matrix(c(0, 0, 500, 0), ncol = 2, byrow = TRUE))), crs = 3857)
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
    trip_geom_other_col <- sf::st_sf(
        other_id = "T1",
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )
    expect_error(
        GTFShift::rt_average_speed(rt_updates, trip_geom_other_col, rt_collection_trips_geometries_match_col = "trip_id"),
        "rt_collection_trips_geometries_match_col must be one of the columns in trips_geometries"
    )

    # Missing required timestamp column
    rt_no_time <- sf::st_sf(
        trip_id = c("T1", "T1"),
        geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(500, 0)), crs = 3857)
    )
    expect_error(
        GTFShift::rt_average_speed(rt_no_time, trip_geom),
        "rt_collection is missing required columns"
    )

    # Invalid metric_crs
    expect_error(
        GTFShift::rt_average_speed(rt_updates, trip_geom, metric_crs = NA),
        "metric_crs should be a valid CRS value"
    )
})

