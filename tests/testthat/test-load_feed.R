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

test_that("creates transfers with custom transfer_distance and transfer_time", {
    gtfs <- GTFShift::load_feed(
        system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"),
        create_transfers = TRUE,
        transfer_distance = 500,
        transfer_time = 180,
        transfer_street_routing = FALSE
    )
    testthat::expect_contains(names(gtfs), "transfers")
    testthat::expect_gte(nrow(gtfs$transfers), 1)
    testthat::expect_contains(names(gtfs$transfers), c("from_stop_id", "to_stop_id", "transfer_type", "min_transfer_time"))
    testthat::expect_true(all(gtfs$transfers$min_transfer_time >= 180))
})

test_that("load_feed passes custom parameters to gtfsrouter::gtfs_transfer_table", {
    transfer_table_called <- FALSE
    captured_d_limit <- NULL
    captured_min_transfer_time <- NULL
    captured_network_times <- NULL

    testthat::with_mocked_bindings(
        gtfs_transfer_table = function(gtfs_transfers, d_limit, min_transfer_time, network_times, ...) {
            transfer_table_called <<- TRUE
            captured_d_limit <<- d_limit
            captured_min_transfer_time <<- min_transfer_time
            captured_network_times <<- network_times
            list(transfers = data.frame(
                from_stop_id = "s1",
                to_stop_id = "s2",
                transfer_type = 2,
                min_transfer_time = min_transfer_time
            ))
        },
        .package = "gtfsrouter",
        code = {
            res <- GTFShift::load_feed(
                system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"),
                create_transfers = TRUE,
                transfer_distance = 600,
                transfer_time = 240,
                transfer_street_routing = FALSE
            )
            expect_true(transfer_table_called)
            expect_equal(captured_d_limit, 600)
            expect_equal(captured_min_transfer_time, 240)
            expect_false(captured_network_times)
            expect_contains(names(res), "transfers")
            expect_equal(nrow(res$transfers), 1)
        }
    )
})

test_that("clean empty stop_times", {
    gtfs <- GTFShift::load_feed(system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"), create_transfers = TRUE)
    random_trip <- gtfs$trips |> sample_n(1) |> pull(trip_id)
    gtfs$stop_times[random_trip == gtfs$stop_times$trip_id, ][1, ]$arrival_time <- NA
    location <- withr::local_tempfile(fileext = ".zip")
    tidytransit::write_gtfs(gtfs, location)
    testthat::expect_warning(gtfs_new <- GTFShift::load_feed(location), "interpolated .* stop times with missing times")
    testthat::expect_equal(nrow(gtfs_new$stop_times), nrow(gtfs$stop_times))
    testthat::expect_false(anyNA(gtfs_new$stop_times$arrival_time))
})

test_that("load_feed interpolates missing intermediate stop times", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- tidytransit::read_gtfs(sample_file)
    t_id <- gtfs$stop_times$trip_id[1]
    rows <- which(gtfs$stop_times$trip_id == t_id)
    if (length(rows) > 2) {
        gtfs$stop_times$arrival_time[rows[2]] <- NA
        gtfs$stop_times$departure_time[rows[2]] <- NA
    }
    location <- withr::local_tempfile(fileext = ".zip")
    tidytransit::write_gtfs(gtfs, location)

    testthat::expect_warning(gtfs_new <- GTFShift::load_feed(location), "interpolated .* stop times with missing times")
    testthat::expect_equal(nrow(gtfs_new$stop_times), nrow(gtfs$stop_times))
    testthat::expect_false(anyNA(gtfs_new$stop_times$arrival_time))
    testthat::expect_false(anyNA(gtfs_new$stop_times$departure_time))
})

test_that("load_feed removes duplicates across tables", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- tidytransit::read_gtfs(sample_file)
    gtfs$agency <- rbind(gtfs$agency, gtfs$agency[1, ])
    location <- withr::local_tempfile(fileext = ".zip")
    tidytransit::write_gtfs(gtfs, location)

    gtfs_new <- GTFShift::load_feed(location)
    testthat::expect_equal(nrow(gtfs_new$agency), 1)
})

test_that("load_feed removes unused IDs across tables", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- tidytransit::read_gtfs(sample_file)
    unused_stop <- gtfs$stops[1, ]
    unused_stop$stop_id <- "unused_stop_id_test"
    gtfs$stops <- rbind(gtfs$stops, unused_stop)
    location <- withr::local_tempfile(fileext = ".zip")
    tidytransit::write_gtfs(gtfs, location)

    gtfs_new <- GTFShift::load_feed(location)
    testthat::expect_false("unused_stop_id_test" %in% gtfs_new$stops$stop_id)
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
