library(testthat)

test_that("filter_by_modes filters routes by mode code", {
    sample_file <- system.file("extdata", "gtfs_merged_sample.zip", package = "GTFShift")
    # agency_id 8 (TCB) has bus routes (route_type 3) and agency 4 (TTSL) has ferry routes (route_type 4)
    gtfs <- GTFShift::load_feed(sample_file)

    # Filter for bus routes (mode 3)
    gtfs_bus <- GTFShift::filter_by_modes(gtfs, modes = list(3))
    expect_contains(class(gtfs_bus), "tidygtfs")
    expect_true(all(gtfs_bus$routes$route_type == 3))
    expect_gt(nrow(gtfs_bus$routes), 0)

    # Filter for ferry routes (mode 4)
    gtfs_ferry <- GTFShift::filter_by_modes(gtfs, modes = list(4))
    expect_contains(class(gtfs_ferry), "tidygtfs")
    expect_true(all(gtfs_ferry$routes$route_type == 4))
    expect_gt(nrow(gtfs_ferry$routes), 0)

    # Filter for multiple modes (3 and 4)
    gtfs_multi <- GTFShift::filter_by_modes(gtfs, modes = list(3, 4))
    expect_contains(class(gtfs_multi), "tidygtfs")
    expect_true(all(gtfs_multi$routes$route_type %in% c(3, 4)))
    expect_equal(nrow(gtfs_multi$routes), nrow(gtfs_bus$routes) + nrow(gtfs_ferry$routes))
})

test_that("filter_by_modes returns empty feed when no routes match mode code", {
    sample_file <- system.file("extdata", "gtfs_merged_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    # Mode 999 does not exist in sample
    gtfs_empty <- GTFShift::filter_by_modes(gtfs, modes = list(999))
    expect_contains(class(gtfs_empty), "tidygtfs")
    expect_equal(nrow(gtfs_empty$routes), 0)
    expect_equal(nrow(gtfs_empty$trips), 0)
})
