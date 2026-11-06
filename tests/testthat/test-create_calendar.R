library(testthat)

#' @srrstats {EA6.0} Tests return object class, dimensions, column names, and column types of `create_calendar()`.
#' @srrstats {EA6.0a} Tests `data.frame` class of returned calendar table.
#' @srrstats {EA6.0b} Tests `nrow()` and `ncol()` dimensions of returned calendar table.
#' @srrstats {EA6.0c} Tests expected GTFS `calendar.txt` column names.
#' @srrstats {EA6.0d} Tests integer weekday indicator types and `Date` start/end date column classes.
test_that("create_calendar generates calendar table from calendar_dates", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    cal <- GTFShift::create_calendar(gtfs)
    expect_s3_class(cal, "data.frame")
    expect_equal(ncol(cal), 10)
    expect_contains(
        names(cal),
        c("service_id", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "start_date", "end_date")
    )
    expect_type(cal$monday, "integer")
    expect_s3_class(cal$start_date, "Date")
    expect_s3_class(cal$end_date, "Date")

    service_ids <- gtfs$calendar_dates |> filter(exception_type == 1) |> pull(service_id) |> unique()
    expect_equal(nrow(cal), length(service_ids))
    expect_equal(sort(unique(cal$service_id)), sort(service_ids))
})
