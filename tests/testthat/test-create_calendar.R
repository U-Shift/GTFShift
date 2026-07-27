library(testthat)

test_that("create_calendar generates calendar table from calendar_dates", {
    sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    cal <- GTFShift::create_calendar(gtfs)
    expect_true(is.data.frame(cal))
    expect_contains(names(cal), c("service_id", "monday", "tuesday", "start_date", "end_date"))

    service_ids <- gtfs$calendar_dates |> filter(exception_type == 1) |> pull(service_id) |> unique()
    expect_equal(sort(unique(cal$service_id)), sort(service_ids))
})
