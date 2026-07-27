library(testthat)

test_that("filter_by_agency filters by id and name using merged feed sample", {
    sample_file <- system.file("extdata", "gtfs_merged_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    agencies <- gtfs$agency$agency_name
    agency_id_target <- gtfs$agency$agency_id[1]
    agency_name_target <- gtfs$agency$agency_name[1]

    # Filter by ID
    gtfs_by_id <- GTFShift::filter_by_agency(gtfs, id = agency_id_target)
    testthat::expect_contains(class(gtfs_by_id), "tidygtfs")
    testthat::expect_equal(unique(gtfs_by_id$agency$agency_id), agency_id_target)

    # Filter by Name
    gtfs_by_name <- GTFShift::filter_by_agency(gtfs, name = agency_name_target)
    testthat::expect_equal(unique(gtfs_by_name$agency$agency_name), agency_name_target)
})


test_that("filter_by_agency returns empty result when query does not match", {
    sample_file <- system.file("extdata", "gtfs_merged_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    gtfs_empty <- GTFShift::filter_by_agency(gtfs, id = "non_existent_agency_id_9999")
    testthat::expect_equal(nrow(gtfs_empty$agency), 0)
})
