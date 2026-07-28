library(testthat)

test_that("get_stop_frequency_hourly calculates stop departures per hour", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    ref_date <- gtfs$calendar$start_date[1]

    res <- GTFShift::get_stop_frequency_hourly(gtfs, date = ref_date)
    expect_s3_class(res, "sf")
    expect_contains(names(res), c("stop_id", "hour", "frequency", "geometry"))

    # Validate frequency calculation for a selected stop against filtered stop_times
    target_stop_id <- res$stop_id[1]
    calculated_stop_freq <- res |>
        sf::st_drop_geometry() |>
        dplyr::filter(stop_id == target_stop_id) |>
        dplyr::arrange(hour)

    gtfs_date <- tidytransit::filter_feed_by_date(gtfs, extract_date = ref_date)
    pattern_gtfs <- tidytransit::set_servicepattern(gtfs_date)
    service_pattern_ids <- pattern_gtfs$.$dates_servicepatterns |>
        dplyr::filter(date == ref_date)
    service_ids <- pattern_gtfs$.$servicepattern |>
        dplyr::filter(servicepattern_id %in% service_pattern_ids$servicepattern_id) |>
        dplyr::pull(service_id)

    # Filter trips matching active service_ids for the date
    active_trips <- gtfs_date$trips |>
        dplyr::filter(service_id %in% service_ids)

    expected_stop_freq <- gtfs_date$stop_times |>
        dplyr::filter(stop_id == target_stop_id, trip_id %in% active_trips$trip_id) |>
        dplyr::mutate(hour = lubridate::hour(departure_time)) |>
        dplyr::group_by(hour) |>
        dplyr::summarise(expected_freq = dplyr::n()) |>
        dplyr::filter(hour %in% calculated_stop_freq$hour) |>
        dplyr::arrange(hour)

    expect_equal(calculated_stop_freq$frequency, expected_stop_freq$expected_freq)
})
