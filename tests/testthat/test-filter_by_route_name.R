library(testthat)

test_that("filter_by_route_name filters by short name exact match", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    short_target <- gtfs$routes$route_short_name[1]
    gtfs_short_exact <- GTFShift::filter_by_route_name(gtfs, values = list(short_target), short_name = TRUE, exact_match = TRUE)
    expect_contains(class(gtfs_short_exact), "tidygtfs")
    expect_equal(unique(gtfs_short_exact$routes$route_short_name), short_target)
})

test_that("filter_by_route_name filters by short name partial match", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    short_target <- gtfs$routes$route_short_name[1]
    short_partial <- substr(short_target, 1, 1)
    gtfs_short_partial <- GTFShift::filter_by_route_name(gtfs, values = list(short_partial), short_name = TRUE, exact_match = FALSE)
    expect_contains(class(gtfs_short_partial), "tidygtfs")
    expect_true(all(grepl(short_partial, gtfs_short_partial$routes$route_short_name, ignore.case = TRUE)))
})

test_that("filter_by_route_name filters by long name exact match", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    long_target <- gtfs$routes$route_long_name[1]
    gtfs_long_exact <- GTFShift::filter_by_route_name(gtfs, values = list(long_target), short_name = FALSE, exact_match = TRUE)
    expect_contains(class(gtfs_long_exact), "tidygtfs")
    expect_equal(unique(gtfs_long_exact$routes$route_long_name), long_target)
})

test_that("filter_by_route_name filters by long name partial match", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    long_target <- gtfs$routes$route_long_name[1]
    long_words <- unlist(strsplit(long_target, "\\s+"))
    long_partial <- long_words[1]
    gtfs_long_partial <- GTFShift::filter_by_route_name(gtfs, values = list(long_partial), short_name = FALSE, exact_match = FALSE)
    expect_contains(class(gtfs_long_partial), "tidygtfs")
    expect_true(all(grepl(long_partial, gtfs_long_partial$routes$route_long_name, ignore.case = TRUE)))
})
