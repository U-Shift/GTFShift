library(testthat)

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
    expect_equal(nrow(res_df), 1)
})

test_that("headers are passed and httr is mocked", {
    dest_file <- withr::local_tempfile(fileext = ".csv")
    URL <- "http://example.com/rt.json"
    headers <- c("Authorization" = "Bearer token123")

    captured_url <- NULL
    captured_headers <- NULL

    parsed_json <- list(
        header = list(timestamp = 1700000000),
        entity = list(list(id = "1", vehicle = list(trip = list(trip_id = "t1"), position = list(latitude = 41.1, longitude = -8.6))))
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

    expect_equal(captured_url, URL)
    expect_equal(captured_headers, headers)
    expect_true(file.exists(dest_file))
    res_df <- read.csv(dest_file)
    expect_equal(nrow(res_df), 1)
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
    expect_true(any(grepl("Starting GTFS-RT data collection", log_content)))
    expect_true(any(grepl("Iteration 1 completed", log_content)))
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
    expect_equal(nrow(res_df1), 1)
    expect_equal(res_df1$id, 99)

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
    expect_equal(nrow(res_df2), 1)
    expect_equal(res_df2$id, 100)
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
    expect_equal(res_df$feed_incrementality, "FULL_DATASET")
})

test_that("scrape_interval performs at least 3 requests", {
    dest_file <- withr::local_tempfile(fileext = ".csv")
    parsed_json <- list(
        header = list(timestamp = 1700000000),
        entity = list(list(id = "1", vehicle = list(trip = list(trip_id = "t1"), position = list(latitude = 41.1, longitude = -8.6))))
    )

    request_count <- 0
    start_time <- Sys.time()

    testthat::with_mocked_bindings(
        fromJSON = function(...) {
            request_count <<- request_count + 1
            if (request_count > 3) {
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
    expect_equal(request_count, 4)
    res_df <- read.csv(dest_file)
    expect_equal(nrow(res_df), 3)
    expect_gte(elapsed_time, 3)
})
