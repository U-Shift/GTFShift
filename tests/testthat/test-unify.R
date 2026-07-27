library(testthat)

test_that("unify merges two GTFS feeds with default parameters", {
    sample_file <- system.file("extdata", "gtfs_merged_sample.zip", package = "GTFShift")
    gtfs1 <- GTFShift::load_feed(sample_file) |> GTFShift::filter_by_agency(8)
    gtfs2 <- GTFShift::load_feed(sample_file) |> GTFShift::filter_by_agency(4)

    merge_gtfs_called <- FALSE
    real_merge <- gtfstools::merge_gtfs

    testthat::with_mocked_bindings(
        merge_gtfs = function(...) {
            merge_gtfs_called <<- TRUE
            real_merge(...)
        },
        .package = "gtfstools",
        code = {
            unified <- GTFShift::unify(gtfs1, gtfs2)
            expect_true(merge_gtfs_called)
            expect_s3_class(unified, "tidygtfs")
            expect_contains(names(unified), c("agency", "routes", "stops", "trips"))
            expect_equal(length(unique(unified$agency$agency_id)), 2)
            expect_contains(unique(unified$agency$agency_id), c("8", "4"))
        }
    )
})

test_that("unify supports prefix = TRUE", {
    sample_file <- system.file("extdata", "gtfs_merged_sample.zip", package = "GTFShift")
    gtfs1 <- GTFShift::load_feed(sample_file) |> GTFShift::filter_by_agency(8)
    gtfs2 <- GTFShift::load_feed(sample_file) |> GTFShift::filter_by_agency(4)

    unified_prefix <- GTFShift::unify(gtfs1, gtfs2, prefix = TRUE)
    expect_s3_class(unified_prefix, "tidygtfs")
    expect_true(any(grepl("^8_", unified_prefix$routes$route_id)))
    expect_true(any(grepl("^4_", unified_prefix$routes$route_id)))

    expect_true(any(grepl("^8_", unified_prefix$stops$stop_id)))
    expect_true(any(grepl("^4_", unified_prefix$stops$stop_id)))

    expect_true(any(grepl("^8_", unified_prefix$trips$trip_id)))
    expect_true(any(grepl("^4_", unified_prefix$trips$trip_id)))

    expect_true(any(grepl("^8_", unified_prefix$shapes$shape_id)))
    expect_true(any(grepl("^4_", unified_prefix$shapes$shape_id)))

    expect_true(any(grepl("^8_", unified_prefix$calendar$service_id)))
    expect_true(any(grepl("^4_", unified_prefix$calendar$service_id)))
})

test_that("unify supports create_transfers = TRUE with custom transfer_distance and transfer_time", {
    sample_file <- system.file("extdata", "gtfs_merged_sample.zip", package = "GTFShift")
    gtfs1 <- GTFShift::load_feed(sample_file) |> GTFShift::filter_by_agency(8)
    gtfs2 <- GTFShift::load_feed(sample_file) |> GTFShift::filter_by_agency(4)

    unified_transfers <- GTFShift::unify(
        gtfs1,
        gtfs2,
        create_transfers = TRUE,
        transfer_distance = 500,
        transfer_time = 180,
        transfer_street_routing = FALSE
    )

    expect_s3_class(unified_transfers, "tidygtfs")
    expect_true("transfers" %in% names(unified_transfers))
    expect_gt(nrow(unified_transfers$transfers), 0)
})

test_that("unify stores feed to store_path", {
    sample_file <- system.file("extdata", "gtfs_merged_sample.zip", package = "GTFShift")
    gtfs1 <- GTFShift::load_feed(sample_file) |> GTFShift::filter_by_agency(8)
    gtfs2 <- GTFShift::load_feed(sample_file) |> GTFShift::filter_by_agency(4)

    tmp_dir <- tempfile()
    tmp_zip <- file.path(tmp_dir, "nested", "unified_out.zip")

    unified <- GTFShift::unify(gtfs1, gtfs2, store_path = tmp_zip)
    expect_true(file.exists(tmp_zip))
    testthat::expect_true(file.size(tmp_zip) > 0)
    zip::zip_list(tmp_zip) |>
        dplyr::pull(filename) |>
        testthat::expect_contains(c("agency.txt", "routes.txt", "trips.txt", "stops.txt", "stop_times.txt", "shapes.txt"))

    # Clean up
    unlink(tmp_dir, recursive = TRUE)
})

test_that("unify stops on invalid input feeds", {
    expect_error(
        GTFShift::unify("not_a_gtfs_object"),
        "Must inherit from class 'gtfs'"
    )
})
