library(testthat)
library(sf)

test_that("get_trip_speed_profile calculates speed profile with default parameters", {
    # 2 trips, same route, 5 updates each
    # Trip T1: 1500m over 240s -> commercial_speed = (1.5 km) / (240 / 3600 h) = 22.5 km/h
    # Trip T1 alt: (1200 - 300)m over (1180 - 1060)s = 900m / 120s -> 27.0 km/h
    # Trip T2: 1800m over 200s -> commercial_speed = (1.8 km) / (200 / 3600 h) = 32.4 km/h
    # Trip T2 alt: (1300 - 400)m over (2150 - 2050)s = 900m / 100s -> 32.4 km/h
    df <- data.frame(
        trip_id = rep(c("T1", "T2"), each = 5),
        route_id = "R1",
        timestamp = c(1000, 1060, 1120, 1180, 1240, 2000, 2050, 2100, 2150, 2200),
        distance_along_geometry = c(0, 300, 700, 1200, 1500, 0, 400, 800, 1300, 1800),
        speed_kmh = c(NA, 18, 24, 30, 18, NA, 28.8, 28.8, 36, 36)
    )

    res <- GTFShift::get_trip_speed_profile(df)

    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), 2)
    expected_cols <- c(
        "trip_id", "route_id", "day",
        "timestamp_min", "timestamp_max",
        "commercial_speed", "commercial_speed_alt",
        "speed_avg", "speed_median", "speed_sd", "speed_var",
        "speed_min", "speed_max",
        "speed_p15", "speed_p25", "speed_p75", "speed_p85",
        "speed_iqr", "speed_count", "n_updates"
    )
    expect_contains(names(res), expected_cols)

    # Trip T1 checks
    r_t1 <- res[res$trip_id == "T1", ]
    expect_equal(r_t1$route_id, "R1")
    expect_equal(r_t1$day, as.Date("1970-01-01"))
    expect_equal(r_t1$timestamp_min, 1000)
    expect_equal(r_t1$timestamp_max, 1240)
    expect_equal(r_t1$commercial_speed, 22.5)
    expect_equal(r_t1$commercial_speed_alt, 27.0)
    # T1 valid speeds: c(18, 24, 30, 18)
    expect_equal(r_t1$speed_avg, 22.5)
    expect_equal(r_t1$speed_median, 21.0)
    expect_equal(r_t1$speed_min, 18.0)
    expect_equal(r_t1$speed_max, 30.0)
    expect_equal(r_t1$speed_p15, 18.0)
    expect_equal(r_t1$speed_p25, 18.0)
    expect_equal(r_t1$speed_p75, 25.5)
    expect_equal(r_t1$speed_p85, 27.3)
    expect_equal(r_t1$speed_iqr, 7.5)
    expect_equal(r_t1$speed_count, 4)
    expect_equal(r_t1$n_updates, 5)

    # Monotonicity of percentiles
    expect_true(r_t1$speed_p15 <= r_t1$speed_p25)
    expect_true(r_t1$speed_p25 <= r_t1$speed_median)
    expect_true(r_t1$speed_median <= r_t1$speed_p75)
    expect_true(r_t1$speed_p75 <= r_t1$speed_p85)

    # Trip T2 checks
    r_t2 <- res[res$trip_id == "T2", ]
    expect_equal(r_t2$timestamp_min, 2000)
    expect_equal(r_t2$timestamp_max, 2200)
    expect_equal(r_t2$commercial_speed, 32.4)
    expect_equal(r_t2$commercial_speed_alt, 32.4)
    # T2 valid speeds: c(28.8, 28.8, 36, 36)
    expect_equal(r_t2$speed_avg, 32.4)
    expect_equal(r_t2$speed_median, 32.4)
    expect_equal(r_t2$speed_min, 28.8)
    expect_equal(r_t2$speed_max, 36.0)
    expect_equal(r_t2$speed_count, 4)
    expect_equal(r_t2$n_updates, 5)

    # Check global_summary attribute
    global_sum <- attr(res, "global_summary")
    expect_s3_class(global_sum, "data.frame")
    expect_equal(nrow(global_sum), 1)
    expect_equal(global_sum$timestamp_min, 1000)
    expect_equal(global_sum$timestamp_max, 2200)
    expect_equal(global_sum$commercial_speed, 27.45) # mean of 22.5 and 32.4
    expect_equal(global_sum$commercial_speed_alt, 29.7) # mean of 27.0 and 32.4
    expect_equal(global_sum$speed_count, 8)
    expect_equal(global_sum$n_updates, 10)
})

test_that("get_trip_speed_profile supports custom aggregation columns in 'by'", {
    df <- data.frame(
        trip_id = rep(c("T1", "T2"), each = 5),
        route_id = "R1",
        timestamp = c(1000, 1060, 1120, 1180, 1240, 2000, 2050, 2100, 2150, 2200),
        distance_along_geometry = c(0, 300, 700, 1200, 1500, 0, 400, 800, 1300, 1800),
        speed_kmh = c(NA, 18, 24, 30, 18, NA, 28.8, 28.8, 36, 36)
    )

    # 1. Grouping by route_id and day (collapsing multiple trips into one group)
    res_route <- GTFShift::get_trip_speed_profile(df, by = c("route_id", "day"))
    expect_equal(nrow(res_route), 1)
    expect_equal(res_route$route_id, "R1")
    # Commercial speeds should be averaged across the two trips
    expect_equal(res_route$commercial_speed, 27.45)
    expect_equal(res_route$commercial_speed_alt, 29.7)
    expect_equal(res_route$n_updates, 10)
    expect_equal(res_route$speed_count, 8)

    # 2. Grouping solely by trip_id
    res_trip <- GTFShift::get_trip_speed_profile(df, by = "trip_id")
    expect_equal(nrow(res_trip), 2)
    expect_false("route_id" %in% names(res_trip))
    expect_false("day" %in% names(res_trip))
    expect_equal(res_trip$commercial_speed, c(22.5, 32.4))

    # 3. by = NULL (whole dataset evaluated without grouping)
    res_null <- GTFShift::get_trip_speed_profile(df, by = NULL)
    expect_equal(nrow(res_null), 1)
    expect_equal(res_null$commercial_speed, 27.45)
    expect_equal(res_null$n_updates, 10)

    # 4. by = character(0)
    res_char0 <- GTFShift::get_trip_speed_profile(df, by = character(0))
    expect_equal(nrow(res_char0), 1)
    expect_equal(res_char0$commercial_speed, 27.45)

    # 5. Default by when route_id is missing: keeps available columns (trip_id and derived day)
    df_no_route <- df[, c("trip_id", "timestamp", "distance_along_geometry", "speed_kmh")]
    res_default_partial <- GTFShift::get_trip_speed_profile(df_no_route)
    expect_equal(nrow(res_default_partial), 2)
    expect_contains(names(res_default_partial), c("trip_id", "day"))
    expect_false("route_id" %in% names(res_default_partial))
})

test_that("get_trip_speed_profile derives day column from different timestamp types", {
    # 1. POSIXct timestamp
    t_posix <- as.POSIXct(c("2024-05-01 10:00:00", "2024-05-01 10:02:00", "2024-05-01 10:04:00", "2024-05-01 10:06:00"), tz = "UTC")
    df_posix <- data.frame(
        trip_id = "T1",
        route_id = "R1",
        timestamp = t_posix,
        distance_along_geometry = c(0, 500, 1000, 1500),
        speed_kmh = c(NA, 15, 15, 15)
    )
    res_posix <- GTFShift::get_trip_speed_profile(df_posix)
    expect_equal(res_posix$day, as.Date("2024-05-01"))
    expect_equal(res_posix$commercial_speed, 15.0)
    expect_equal(res_posix$commercial_speed_alt, 15.0)

    # 2. Date timestamp
    df_date <- data.frame(
        trip_id = "T1",
        route_id = "R1",
        timestamp = as.Date(c("2024-06-01", "2024-06-02", "2024-06-03", "2024-06-04")),
        distance_along_geometry = c(0, 100000, 200000, 300000),
        speed_kmh = c(NA, 4.17, 4.17, 4.17)
    )
    res_date <- GTFShift::get_trip_speed_profile(df_date)
    expect_equal(res_date$day, as.Date(c("2024-06-01", "2024-06-02", "2024-06-03", "2024-06-04")))

    res_date_trip <- GTFShift::get_trip_speed_profile(df_date, by = "trip_id")
    expect_equal(res_date_trip$commercial_speed, 4.17)

    # 3. Pre-existing day column is preserved
    df_custom_day <- df_posix
    df_custom_day$day <- as.Date("2020-01-01")
    res_custom_day <- GTFShift::get_trip_speed_profile(df_custom_day)
    expect_equal(res_custom_day$day, as.Date("2020-01-01"))
})

test_that("get_trip_speed_profile drops geometry from sf objects", {
    df_sf <- sf::st_sf(
        trip_id = rep("T1", 5),
        route_id = "R1",
        timestamp = c(1000, 1060, 1120, 1180, 1240),
        distance_along_geometry = c(0, 300, 700, 1200, 1500),
        speed_kmh = c(NA, 18, 24, 30, 18),
        geometry = sf::st_sfc(lapply(1:5, function(i) sf::st_point(c(i, i))))
    )

    res_sf <- GTFShift::get_trip_speed_profile(df_sf)
    expect_false(inherits(res_sf, "sf"))
    expect_false("geometry" %in% names(res_sf))
    expect_equal(res_sf$commercial_speed, 22.5)
})

test_that("get_trip_speed_profile handles update count edge cases (n < 2, 2 <= n < 4, n >= 4)", {
    # 1 update: both commercial speeds NA, variance/sd NA
    df_n1 <- data.frame(
        trip_id = "T1",
        timestamp = 1000,
        distance_along_geometry = 0,
        speed_kmh = 25
    )
    res_n1 <- GTFShift::get_trip_speed_profile(df_n1, by = "trip_id")
    expect_true(is.na(res_n1$commercial_speed))
    expect_true(is.na(res_n1$commercial_speed_alt))
    expect_true(is.na(res_n1$speed_sd))
    expect_true(is.na(res_n1$speed_var))
    expect_equal(res_n1$speed_avg, 25.0)
    expect_equal(res_n1$speed_count, 1)
    expect_equal(res_n1$n_updates, 1)

    # 2 updates: commercial_speed valid, commercial_speed_alt NA
    df_n2 <- data.frame(
        trip_id = "T1",
        timestamp = c(1000, 1060),
        distance_along_geometry = c(0, 500),
        speed_kmh = c(NA, 30)
    )
    res_n2 <- GTFShift::get_trip_speed_profile(df_n2, by = "trip_id")
    expect_equal(res_n2$commercial_speed, 30.0)
    expect_true(is.na(res_n2$commercial_speed_alt))

    # 3 updates: commercial_speed valid, commercial_speed_alt NA
    df_n3 <- data.frame(
        trip_id = "T1",
        timestamp = c(1000, 1060, 1120),
        distance_along_geometry = c(0, 500, 1000),
        speed_kmh = c(NA, 30, 30)
    )
    res_n3 <- GTFShift::get_trip_speed_profile(df_n3, by = "trip_id")
    expect_equal(res_n3$commercial_speed, 30.0)
    expect_true(is.na(res_n3$commercial_speed_alt))

    # 4 updates: both valid
    df_n4 <- data.frame(
        trip_id = "T1",
        timestamp = c(1000, 1060, 1120, 1180),
        distance_along_geometry = c(0, 500, 1000, 1500),
        speed_kmh = c(NA, 30, 30, 30)
    )
    res_n4 <- GTFShift::get_trip_speed_profile(df_n4, by = "trip_id")
    expect_equal(res_n4$commercial_speed, 30.0)
    expect_equal(res_n4$commercial_speed_alt, 30.0)

    # Multi-trip group where all trips have n < 2
    df_multi_n1 <- data.frame(
        route_id = "R1",
        trip_id = c("T1", "T2"),
        timestamp = c(1000, 2000),
        distance_along_geometry = c(0, 0),
        speed_kmh = c(20, 30)
    )
    res_multi_n1 <- GTFShift::get_trip_speed_profile(df_multi_n1, by = "route_id")
    expect_true(is.na(res_multi_n1$commercial_speed))
    expect_true(is.na(res_multi_n1$commercial_speed_alt))

    # Multi-trip group where all trips have n < 4
    df_multi_n2 <- data.frame(
        route_id = "R1",
        trip_id = c("T1", "T1", "T2", "T2"),
        timestamp = c(1000, 1060, 2000, 2060),
        distance_along_geometry = c(0, 500, 0, 600),
        speed_kmh = c(NA, 30, NA, 36)
    )
    res_multi_n2 <- GTFShift::get_trip_speed_profile(df_multi_n2, by = "route_id")
    expect_equal(res_multi_n2$commercial_speed, 33.0) # mean of 30 and 36
    expect_true(is.na(res_multi_n2$commercial_speed_alt))

    # Multi-trip group where one trip has 4 updates and one has 1 update
    df_multi_mixed <- data.frame(
        route_id = "R1",
        trip_id = c("T1", "T1", "T1", "T1", "T2"),
        timestamp = c(1000, 1060, 1120, 1180, 2000),
        distance_along_geometry = c(0, 500, 1000, 1500, 0),
        speed_kmh = c(NA, 30, 30, 30, 20)
    )
    res_multi_mixed <- GTFShift::get_trip_speed_profile(df_multi_mixed, by = "route_id")
    expect_equal(res_multi_mixed$commercial_speed, 30.0) # only T1 has valid commercial speed
    expect_equal(res_multi_mixed$commercial_speed_alt, 30.0)
})

test_that("get_trip_speed_profile handles unordered updates, zero/negative elapsed time, and missing values", {
    # 1. Unordered updates are sorted chronologically
    df_sorted <- data.frame(
        trip_id = "T1",
        timestamp = c(1000, 1060, 1120, 1180),
        distance_along_geometry = c(0, 500, 1000, 1500),
        speed_kmh = c(NA, 30, 30, 30)
    )
    df_scrambled <- df_sorted[c(3, 1, 4, 2), ]

    res_sorted <- GTFShift::get_trip_speed_profile(df_sorted, by = "trip_id")
    res_scrambled <- GTFShift::get_trip_speed_profile(df_scrambled, by = "trip_id")
    expect_equal(res_scrambled$timestamp_min, res_sorted$timestamp_min)
    expect_equal(res_scrambled$timestamp_max, res_sorted$timestamp_max)
    expect_equal(res_scrambled$commercial_speed, res_sorted$commercial_speed)
    expect_equal(res_scrambled$commercial_speed_alt, res_sorted$commercial_speed_alt)

    # 2. Zero or negative time difference yields NA commercial speed
    df_zero_time <- data.frame(
        trip_id = "T1",
        timestamp = c(1000, 1000),
        distance_along_geometry = c(0, 500),
        speed_kmh = c(NA, 30)
    )
    res_zero <- GTFShift::get_trip_speed_profile(df_zero_time, by = "trip_id")
    expect_true(is.na(res_zero$commercial_speed))

    # 3. NA timestamp or NA distance yields NA commercial speed
    df_na_time <- data.frame(
        trip_id = "T1",
        timestamp = c(NA, 1060),
        distance_along_geometry = c(0, 500),
        speed_kmh = c(NA, 30)
    )
    res_na_time <- GTFShift::get_trip_speed_profile(df_na_time, by = "trip_id")
    expect_true(is.na(res_na_time$commercial_speed))

    df_na_dist <- data.frame(
        trip_id = "T1",
        timestamp = c(1000, 1060),
        distance_along_geometry = c(NA, 500),
        speed_kmh = c(NA, 30)
    )
    res_na_dist <- GTFShift::get_trip_speed_profile(df_na_dist, by = "trip_id")
    expect_true(is.na(res_na_dist$commercial_speed))

    # 4. Backward distance progression uses absolute difference
    df_backward <- data.frame(
        trip_id = "T1",
        timestamp = c(1000, 1060, 1120, 1180),
        distance_along_geometry = c(1500, 1000, 500, 0),
        speed_kmh = c(NA, 30, 30, 30)
    )
    res_backward <- GTFShift::get_trip_speed_profile(df_backward, by = "trip_id")
    expect_equal(res_backward$commercial_speed, 30.0)
    expect_equal(res_backward$commercial_speed_alt, 30.0)

    # 5. Character timestamps parseable as POSIXct
    df_char <- data.frame(
        trip_id = "T1",
        timestamp = c("2024-05-01 10:00:00", "2024-05-01 10:02:00", "2024-05-01 10:04:00", "2024-05-01 10:06:00"),
        distance_along_geometry = c(0, 500, 1000, 1500),
        speed_kmh = c(NA, 15, 15, 15)
    )
    res_char <- GTFShift::get_trip_speed_profile(df_char, by = "trip_id")
    expect_equal(res_char$commercial_speed, 15.0)
    expect_equal(res_char$commercial_speed_alt, 15.0)
})

test_that("get_trip_speed_profile handles speed column variations and edge cases", {
    # 1. Non-finite speed values (Inf, -Inf) are ignored
    df_inf <- data.frame(
        trip_id = "T1",
        timestamp = c(1000, 1060, 1120, 1180),
        distance_along_geometry = c(0, 500, 1000, 1500),
        speed_kmh = c(20, Inf, -Inf, 40)
    )
    res_inf <- GTFShift::get_trip_speed_profile(df_inf, by = "trip_id")
    expect_equal(res_inf$speed_avg, 30.0)
    expect_equal(res_inf$speed_min, 20.0)
    expect_equal(res_inf$speed_max, 40.0)
    expect_equal(res_inf$speed_count, 2)

    # 2. All speeds NA
    df_all_na <- data.frame(
        trip_id = "T1",
        timestamp = c(1000, 1060),
        distance_along_geometry = c(0, 500),
        speed_kmh = c(NA_real_, NA_real_)
    )
    res_all_na <- GTFShift::get_trip_speed_profile(df_all_na, by = "trip_id")
    expect_equal(res_all_na$speed_count, 0)
    expect_true(is.na(res_all_na$speed_avg))
    expect_true(is.na(res_all_na$speed_median))
    expect_true(is.na(res_all_na$speed_sd))
    expect_true(is.na(res_all_na$speed_var))
    expect_true(is.na(res_all_na$speed_min))
    expect_true(is.na(res_all_na$speed_max))
    expect_true(is.na(res_all_na$speed_p15))
    expect_true(is.na(res_all_na$speed_p25))
    expect_true(is.na(res_all_na$speed_p75))
    expect_true(is.na(res_all_na$speed_p85))
    expect_true(is.na(res_all_na$speed_iqr))
    expect_equal(res_all_na$commercial_speed, 30.0)

    # 3. Missing speed_col completely
    df_no_speed <- data.frame(
        trip_id = "T1",
        timestamp = c(1000, 1060),
        distance_along_geometry = c(0, 500)
    )
    res_no_speed <- GTFShift::get_trip_speed_profile(df_no_speed, by = "trip_id")
    expect_equal(res_no_speed$speed_count, 0)
    expect_true(is.na(res_no_speed$speed_avg))
    expect_equal(res_no_speed$commercial_speed, 30.0)

    # 4. Custom column names
    df_custom <- data.frame(
        my_trip = "T1",
        my_time = c(1000, 1060, 1120, 1180),
        my_dist = c(0, 500, 1000, 1500),
        my_spd = c(NA, 30, 30, 30)
    )
    res_custom <- GTFShift::get_trip_speed_profile(
        df_custom,
        by = "my_trip",
        dist_col = "my_dist",
        speed_col = "my_spd",
        time_col = "my_time"
    )
    expect_equal(res_custom$commercial_speed, 30.0)
    expect_equal(res_custom$speed_avg, 30.0)
    expect_equal(res_custom$speed_count, 3)

    # 5. Grouped data.frame input is ungrouped properly
    df_grouped <- dplyr::group_by(df_inf, trip_id)
    res_grouped <- GTFShift::get_trip_speed_profile(df_grouped, by = "trip_id")
    expect_equal(res_grouped$commercial_speed, 30.0)
})

test_that("get_trip_speed_profile stops with informative error on invalid inputs", {
    # 1. Non-dataframe input
    expect_error(
        GTFShift::get_trip_speed_profile(123),
        "rt_speed must be a data.frame or sf object"
    )
    expect_error(
        GTFShift::get_trip_speed_profile(list(a = 1)),
        "rt_speed must be a data.frame or sf object"
    )

    # 2. Missing required columns
    expect_error(
        GTFShift::get_trip_speed_profile(data.frame(x = 1)),
        "rt_speed is missing required column\\(s\\): distance_along_geometry, timestamp"
    )
    expect_error(
        GTFShift::get_trip_speed_profile(
            data.frame(x = 1),
            dist_col = "my_dist",
            time_col = "my_time"
        ),
        "rt_speed is missing required column\\(s\\): my_dist, my_time"
    )

    # 3. Specified grouping column in by is missing
    df_valid <- data.frame(
        trip_id = "T1",
        timestamp = 1000,
        distance_along_geometry = 0
    )
    expect_error(
        GTFShift::get_trip_speed_profile(df_valid, by = c("non_existent_col")),
        "The following grouping columns specified in 'by' were not found in rt_speed: non_existent_col"
    )
})

test_that("get_trip_speed_profile integrates seamlessly with rt_average_speed output", {
    trip_geom <- sf::st_sf(
        trip_id = c("T1", "T2"),
        geometry = sf::st_sfc(
            sf::st_linestring(matrix(c(0, 0, 1000, 0), ncol = 2, byrow = TRUE)),
            sf::st_linestring(matrix(c(0, 100, 1000, 100), ncol = 2, byrow = TRUE)),
            crs = 3857
        )
    )

    rt_updates <- sf::st_sf(
        trip_id = c("T1", "T1", "T1", "T1", "T2", "T2", "T2", "T2"),
        route_id = "R1",
        timestamp = c(1000, 1060, 1120, 1180, 2000, 2060, 2120, 2180),
        geometry = sf::st_sfc(
            sf::st_point(c(0, 0)),
            sf::st_point(c(250, 0)),
            sf::st_point(c(600, 0)),
            sf::st_point(c(900, 0)),
            sf::st_point(c(0, 100)),
            sf::st_point(c(300, 100)),
            sf::st_point(c(650, 100)),
            sf::st_point(c(1000, 100)),
            crs = 3857
        )
    )

    speed_sf <- GTFShift::rt_average_speed(rt_updates, trip_geom, metric_crs = 3857)
    profile <- GTFShift::get_trip_speed_profile(speed_sf)

    expect_s3_class(profile, "data.frame")
    expect_false(inherits(profile, "sf"))
    expect_equal(nrow(profile), 2)
    expect_equal(profile$trip_id, c("T1", "T2"))
    expect_equal(profile$route_id, c("R1", "R1"))
    expect_true(all(!is.na(profile$commercial_speed)))
    expect_true(all(!is.na(profile$commercial_speed_alt)))
    expect_true(all(!is.na(profile$speed_avg)))
    expect_true(all(!is.na(profile$speed_p15)))
    expect_true(all(!is.na(profile$speed_p85)))

    # Global summary check
    global_summary <- attr(profile, "global_summary")
    expect_s3_class(global_summary, "data.frame")
    expect_equal(nrow(global_summary), 1)
    expect_equal(global_summary$n_updates, 8)
})
