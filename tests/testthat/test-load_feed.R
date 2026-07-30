library(testthat)

test_that("gtfs simple load", {
    gtfs <- GTFShift::load_feed(system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"))
    testthat::expect_contains(class(gtfs), "tidygtfs")
    testthat::expect_contains(class(gtfs), "gtfs")
    testthat::expect_contains(class(gtfs), "list")
    testthat::expect_contains(names(gtfs), "agency")
    testthat::expect_contains(names(gtfs), "routes")
    testthat::expect_contains(names(gtfs), "trips")
    testthat::expect_contains(names(gtfs), "stops")
    testthat::expect_contains(names(gtfs), "stop_times")
    testthat::expect_contains(names(gtfs), "shapes")
})

test_that("stores file at defined location", {
    tempfolder <- withr::local_tempdir()
    location <- paste0(tempfolder, "/new_dir/gtfs_tcb_sample.zip")
    gtfs <- GTFShift::load_feed(
        system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"),
        store_path = location
    )
    testthat::expect_true(file.exists(location))
    testthat::expect_true(file.size(location) > 0)
    testthat::expect_contains(class(gtfs), "tidygtfs")
    zip::zip_list(location) |>
        dplyr::pull(filename) |>
        testthat::expect_contains(c("agency.txt", "routes.txt", "trips.txt", "stops.txt", "stop_times.txt", "shapes.txt"))
})

test_that("creates transfers", {
    gtfs <- GTFShift::load_feed(system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"), create_transfers = TRUE)
    testthat::expect_contains(names(gtfs), "transfers")
    testthat::expect_gte(nrow(gtfs$transfers), 1)
})

test_that("clean empty stop_times", {
    gtfs <- GTFShift::load_feed(system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"), create_transfers = TRUE)
    random_trip = gtfs$trips |> sample_n(1) |> pull(trip_id)
    gtfs$stop_times[random_trip == gtfs$stop_times$trip_id, ][1, ]$arrival_time <- NA 
    location = withr::local_tempfile(fileext = ".zip")
    tidytransit::write_gtfs(gtfs, location)
    testthat::expect_warning(gtfs_new <- GTFShift::load_feed(location), "without arrival time")
})

test_that("creates shapes when missing", {
    gtfs <- GTFShift::load_feed(system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"))
    gtfs_manipulated <- gtfs[!names(gtfs) %in% "shapes"]
    gtfs_manipulated <- tidytransit::as_tidygtfs(gtfs_manipulated)
    gtfs_manipulated$trips <- gtfs_manipulated$trips[, !names(gtfs_manipulated$trips) %in% "shape_id"]
    location <- withr::local_tempfile(fileext = ".zip")
    tidytransit::write_gtfs(gtfs_manipulated, location)
    testthat::expect_warning(gtfs_new <- GTFShift::load_feed(location), "CREATED shapes.txt")
    testthat::expect_contains(names(gtfs_new), "shapes")
    testthat::expect_gte(nrow(gtfs_new$shapes), 1)
    testthat::expect_true(all(names(gtfs_new$shapes) %in% names(gtfs$shapes)))
})

test_that("headers set when calling remote gtfs url", {
    API_KEY <- "ash84r"
    headers <- c("X-App-Id" = API_KEY)
    URL <- "http://example.com/gtfs.zip"

    captured_url <- NULL
    captured_headers <- NULL

    sample_gtfs <- structure(
        list(
            agency = data.frame(),
            routes = data.frame(),
            trips = data.frame(shape_id = character(0)),
            stops = data.frame(),
            stop_times = data.frame(trip_id = character(0), arrival_time = character(0)),
            shapes = data.frame()
        ),
        class = c("tidygtfs", "gtfs", "list")
    )

    testthat::with_mocked_bindings(
        GET = function(url, config, ...) {
            captured_url <<- url
            captured_headers <<- config$headers
            structure(list(status_code = 200), class = "response")
        },
        stop_for_status = function(res) {
            NULL
        },
        .package = "httr",
        code = {
            testthat::with_mocked_bindings(
                read_gtfs = function(path, ...) {
                    sample_gtfs
                },
                .package = "tidytransit",
                code = {
                    gtfs <- GTFShift::load_feed(URL, headers = headers)
                    expect_s3_class(gtfs, "tidygtfs")
                }
            )
        }
    )

    expect_equal(captured_url, URL)
    expect_equal(captured_headers, headers)
})
