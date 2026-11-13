test_that("rt_collect_json extracts JSON feed data and appends to CSV destination", {
    dest_file <- withr::local_tempfile(fileext = ".csv")
    parsed_json <- list(
        header = list(timestamp = 1700000000),
        entity = list(list(id = "1", vehicle = list(trip = list(trip_id = "t1"), position = list(latitude = 41.1, longitude = -8.6))))
    )

    testthat::with_mocked_bindings(
        fromJSON = function(...) parsed_json,
        .package = "jsonlite",
        code = {
            GTFShift::rt_collect_json(
                gtfs_rt_url = "http://example.com/rt.json",
                destination_file = dest_file,
                scrape_interval = -1
            )
        }
    )

    expect_true(file.exists(dest_file))
    res_df <- read.csv(dest_file)
    expect_identical(nrow(res_df), 1L)
})

test_that("headers are passed and httr is mocked", {
    dest_file <- withr::local_tempfile(fileext = ".csv")
    URL <- "http://example.com/rt.json"
    headers <- c("Authorization" = "Bearer token123")

    state <- new.env(parent = emptyenv())
    state$captured_url <- NULL
    state$captured_headers <- NULL

    parsed_json <- list(
        header = list(timestamp = 1700000000),
        entity = list(list(id = "1", vehicle = list(trip = list(trip_id = "t1"), position = list(latitude = 41.1, longitude = -8.6))))
    )

    testthat::with_mocked_bindings(
        GET = function(url, config, ...) {
            state$captured_url <- url
            state$captured_headers <- config$headers
            structure(list(status_code = 200), class = "response")
        },
        stop_for_status = function(res) {
            NULL
        },
        content = function(res, as = "text", encoding = "UTF-8") {
            '{"mock":"json"}'
        },
        .package = "httr",
        code = {
            testthat::with_mocked_bindings(
                fromJSON = function(...) parsed_json,
                .package = "jsonlite",
                code = {
                    GTFShift::rt_collect_json(
                        gtfs_rt_url = URL,
                        destination_file = dest_file,
                        scrape_interval = -1,
                        headers = headers
                    )
                }
            )
        }
    )

    expect_identical(state$captured_url, URL)
    expect_identical(state$captured_headers, headers)
    expect_true(file.exists(dest_file))
    res_df <- read.csv(dest_file)
    expect_identical(nrow(res_df), 1L)
})

test_that("parameter variation: log_file", {
    dest_file <- withr::local_tempfile(fileext = ".csv")
    log_file <- withr::local_tempfile(fileext = ".log")
    parsed_json <- list(
        header = list(timestamp = 1700000000),
        entity = list(list(id = "1", vehicle = list(trip = list(trip_id = "t1"), position = list(latitude = 41.1, longitude = -8.6))))
    )

    testthat::with_mocked_bindings(
        fromJSON = function(...) parsed_json,
        .package = "jsonlite",
        code = {
            GTFShift::rt_collect_json(
                gtfs_rt_url = "http://example.com/rt.json",
                destination_file = dest_file,
                scrape_interval = -1,
                log_file = log_file
            )
        }
    )

    expect_true(file.exists(log_file))
    log_content <- readLines(log_file)
    expect_true(any(grepl("Starting GTFS-RT data collection", log_content, fixed = TRUE)))
    expect_true(any(grepl("Iteration 1 completed", log_content, fixed = TRUE)))
})

test_that("parameter variation: entity_key custom and NA", {
    # Custom entity key
    dest_file1 <- withr::local_tempfile(fileext = ".csv")
    parsed_json_custom <- list(
        header = list(timestamp = 1700000000),
        custom_entities = list(list(id = "99", vehicle = list(trip = list(trip_id = "t99"), position = list(latitude = 40.0, longitude = -8.0))))
    )

    testthat::with_mocked_bindings(
        fromJSON = function(...) parsed_json_custom,
        .package = "jsonlite",
        code = {
            GTFShift::rt_collect_json(
                gtfs_rt_url = "http://example.com/rt.json",
                destination_file = dest_file1,
                entity_key = "custom_entities",
                scrape_interval = -1
            )
        }
    )

    res_df1 <- read.csv(dest_file1)
    expect_identical(nrow(res_df1), 1L)
    expect_identical(res_df1$id, 99)

    # NA entity key (flat list)
    dest_file2 <- withr::local_tempfile(fileext = ".csv")
    parsed_json_flat <- data.frame(id = "100", vehicle.trip.trip_id = "t100", vehicle.position.latitude = 42.0, vehicle.position.longitude = -8.5)

    testthat::with_mocked_bindings(
        fromJSON = function(...) parsed_json_flat,
        .package = "jsonlite",
        code = {
            GTFShift::rt_collect_json(
                gtfs_rt_url = "http://example.com/rt.json",
                destination_file = dest_file2,
                header_key = NA,
                entity_key = NA,
                scrape_interval = -1
            )
        }
    )

    res_df2 <- read.csv(dest_file2)
    expect_identical(nrow(res_df2), 1L)
    expect_identical(res_df2$id, 100)
})

test_that("test incrementality in response", {
    dest_file <- withr::local_tempfile(fileext = ".csv")
    parsed_json <- list(
        header = list(timestamp = 1700000000, incrementality = "FULL_DATASET"),
        entity = list(list(id = "1", vehicle = list(trip = list(trip_id = "t1"), position = list(latitude = 41.1, longitude = -8.6))))
    )

    testthat::with_mocked_bindings(
        fromJSON = function(...) parsed_json,
        .package = "jsonlite",
        code = {
            GTFShift::rt_collect_json(
                gtfs_rt_url = "http://example.com/rt.json",
                destination_file = dest_file,
                scrape_interval = -1
            )
        }
    )

    res_df <- read.csv(dest_file)
    expect_true("feed_incrementality" %in% names(res_df))
    expect_identical(res_df$feed_incrementality, "FULL_DATASET")
})

test_that("scrape_interval performs at least 3 requests", {
    dest_file <- withr::local_tempfile(fileext = ".csv")
    parsed_json <- list(
        header = list(timestamp = 1700000000),
        entity = list(list(id = "1", vehicle = list(trip = list(trip_id = "t1"), position = list(latitude = 41.1, longitude = -8.6))))
    )

    state <- new.env(parent = emptyenv())
    state$request_count <- 0
    start_time <- Sys.time()

    testthat::with_mocked_bindings(
        fromJSON = function(...) {
            state$request_count <- state$request_count + 1
            if (state$request_count > 3) {
                stop("stop_loop_after_3_requests")
            }
            parsed_json
        },
        .package = "jsonlite",
        code = {
            expect_error(
                GTFShift::rt_collect_json(
                    gtfs_rt_url = "http://example.com/rt.json",
                    destination_file = dest_file,
                    scrape_interval = 1
                ),
                "stop_loop_after_3_requests"
            )
        }
    )

    elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    expect_identical(state$request_count, 4)
    res_df <- read.csv(dest_file)
    expect_identical(nrow(res_df), 3L)
    expect_gte(elapsed_time, 3)
})

test_that("rt_collect_json fails gracefully with informative message when internet resource is unavailable", {
    dest_file <- withr::local_tempfile(fileext = ".csv")
    log_file <- withr::local_tempfile(fileext = ".log")
    URL <- "http://example.com/unavailable.json"

    # Connection error without headers (with log_file)
    testthat::with_mocked_bindings(
        fromJSON = function(...) {
            stop("cannot open the connection to 'http://example.com/unavailable.json'")
        },
        .package = "jsonlite",
        code = {
            expect_message(
                res <- GTFShift::rt_collect_json(
                    gtfs_rt_url = URL,
                    destination_file = dest_file,
                    scrape_interval = -1,
                    log_file = log_file
                ),
                "Internet resource unavailable"
            )
            expect_null(res)
            expect_false(file.exists(dest_file))
            expect_true(file.exists(log_file))
            expect_true(any(grepl("Internet resource unavailable", readLines(log_file), fixed = TRUE)))
        }
    )

    # Connection error with headers (GET throws error, with log_file)
    log_file2 <- withr::local_tempfile(fileext = ".log")
    testthat::with_mocked_bindings(
        GET = function(...) {
            stop("Failed to connect to host")
        },
        .package = "httr",
        code = {
            expect_message(
                res <- GTFShift::rt_collect_json(
                    gtfs_rt_url = URL,
                    destination_file = dest_file,
                    scrape_interval = -1,
                    headers = c("Authorization" = "token"),
                    log_file = log_file2
                ),
                "Internet resource unavailable"
            )
            expect_null(res)
            expect_true(any(grepl("Internet resource unavailable", readLines(log_file2), fixed = TRUE)))
        }
    )

    # HTTP error with headers (with log_file)
    log_file3 <- withr::local_tempfile(fileext = ".log")
    testthat::with_mocked_bindings(
        GET = function(...) {
            structure(list(status_code = 404), class = "response")
        },
        stop_for_status = function(...) {
            stop("Not Found (HTTP 404)")
        },
        .package = "httr",
        code = {
            expect_message(
                res <- GTFShift::rt_collect_json(
                    gtfs_rt_url = URL,
                    destination_file = dest_file,
                    scrape_interval = -1,
                    headers = c("Authorization" = "token"),
                    log_file = log_file3
                ),
                "Internet resource unavailable"
            )
            expect_null(res)
            expect_false(file.exists(dest_file))
            expect_true(any(grepl("Internet resource unavailable", readLines(log_file3), fixed = TRUE)))
        }
    )

    # JSON parse error with headers
    log_file4 <- withr::local_tempfile(fileext = ".log")
    testthat::with_mocked_bindings(
        GET = function(...) {
            structure(list(status_code = 200), class = "response")
        },
        stop_for_status = function(...) {
            NULL
        },
        content = function(...) "invalid json",
        .package = "httr",
        code = {
            testthat::with_mocked_bindings(
                fromJSON = function(...) {
                    stop("lexical error: invalid char in json text.")
                },
                .package = "jsonlite",
                code = {
                    expect_message(
                        res <- GTFShift::rt_collect_json(
                            gtfs_rt_url = URL,
                            destination_file = dest_file,
                            scrape_interval = -1,
                            headers = c("Authorization" = "token"),
                            log_file = log_file4
                        ),
                        "Failed to parse JSON"
                    )
                    expect_null(res)
                    expect_true(any(grepl("Failed to parse JSON", readLines(log_file4), fixed = TRUE)))
                }
            )
        }
    )
})

test_that("rt_collect_json retries when resource temporarily fails in loop mode", {
    dest_file <- withr::local_tempfile(fileext = ".csv")
    URL <- "http://example.com/retry.json"
    state <- new.env(parent = emptyenv())
    state$req_count <- 0
    parsed_json <- list(
        header = list(timestamp = 1700000000),
        entity = list(list(id = "1", vehicle = list(trip = list(trip_id = "t1"), position = list(latitude = 41.1, longitude = -8.6))))
    )

    testthat::with_mocked_bindings(
        fromJSON = function(...) {
            state$req_count <- state$req_count + 1
            if (state$req_count == 1) {
                stop("cannot open the connection to 'http://example.com/retry.json'")
            }
            if (state$req_count == 2) {
                return(parsed_json)
            }
            stop("stop_after_second_iteration")
        },
        .package = "jsonlite",
        code = {
            expect_message(
                expect_error(
                    GTFShift::rt_collect_json(
                        gtfs_rt_url = URL,
                        destination_file = dest_file,
                        scrape_interval = 0.01
                    ),
                    "stop_after_second_iteration"
                ),
                "Internet resource unavailable"
            )
        }
    )
    expect_gte(state$req_count, 2)
    expect_true(file.exists(dest_file))
})
