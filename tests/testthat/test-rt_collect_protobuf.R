library(testthat)

test_that("rt_collect_protobuf decodes protobuf and passes to rt_collect_json via mocks", {
    dest_file <- tempfile(fileext = ".csv")
    pb_file <- tempfile(fileext = ".pb")
    file.create(pb_file)

    testthat::with_mocked_bindings(
        readProtoFiles = function(...) TRUE,
        P = function(...) TRUE,
        read = function(...) structure(list(id = "1"), class = "Message"),
        .package = "RProtoBuf",
        code = {
            testthat::with_mocked_bindings(
                rt_collect_json = function(...) {
                    write.table(data.frame(id = "1"), file = dest_file, sep = ",", row.names = FALSE)
                },
                .package = "GTFShift",
                code = {
                    GTFShift::rt_collect_protobuf(
                        gtfs_rt_url = pb_file,
                        destination_file = dest_file,
                        scrape_interval = -1
                    )
                }
            )
        }
    )

    expect_true(file.exists(dest_file))
})

test_that("headers are passed and httr is mocked", {
    dest_file <- tempfile(fileext = ".csv")
    URL <- "http://example.com/rt.pb"
    headers <- c("Authorization" = "Bearer token123")

    captured_url <- NULL
    captured_headers <- NULL

    testthat::with_mocked_bindings(
        GET = function(url, config = list(), ...) {
            captured_url <<- url
            captured_headers <<- config$headers
            dots <- list(config, ...)
            for (arg in dots) {
                if (inherits(arg, "write_disk") && !is.null(arg$path)) {
                    file.create(arg$path)
                }
                if (is.list(arg) && !is.null(arg$file)) {
                    file.create(arg$file)
                }
            }
            structure(list(status_code = 200), class = "response")
        },
        stop_for_status = function(res) {
            NULL
        },
        write_disk = function(path, overwrite = TRUE) {
            structure(list(path = path), class = "write_disk")
        },
        .package = "httr",
        code = {
            testthat::with_mocked_bindings(
                readProtoFiles = function(...) TRUE,
                P = function(...) TRUE,
                read = function(...) structure(list(id = "1"), class = "Message"),
                .package = "RProtoBuf",
                code = {
                    testthat::with_mocked_bindings(
                        rt_collect_json = function(...) {
                            write.table(data.frame(id = "1"), file = dest_file, sep = ",", row.names = FALSE)
                        },
                        .package = "GTFShift",
                        code = {
                            GTFShift::rt_collect_protobuf(
                                gtfs_rt_url = URL,
                                destination_file = dest_file,
                                scrape_interval = -1,
                                headers = headers
                            )
                        }
                    )
                }
            )
        }
    )

    expect_equal(captured_url, URL)
    expect_equal(captured_headers, headers)
    expect_true(file.exists(dest_file))
})

test_that("parameter variation: log_file", {
    dest_file <- tempfile(fileext = ".csv")
    pb_file <- tempfile(fileext = ".pb")
    file.create(pb_file)
    log_file <- tempfile(fileext = ".log")

    testthat::with_mocked_bindings(
        readProtoFiles = function(...) TRUE,
        P = function(...) TRUE,
        read = function(...) structure(list(id = "1"), class = "Message"),
        .package = "RProtoBuf",
        code = {
            testthat::with_mocked_bindings(
                rt_collect_json = function(...) {
                    write.table(data.frame(id = "1"), file = dest_file, sep = ",", row.names = FALSE)
                },
                .package = "GTFShift",
                code = {
                    GTFShift::rt_collect_protobuf(
                        gtfs_rt_url = pb_file,
                        destination_file = dest_file,
                        scrape_interval = -1,
                        log_file = log_file
                    )
                }
            )
        }
    )

    expect_true(file.exists(log_file))
    log_content <- readLines(log_file)
    expect_true(any(grepl("Starting GTFS-RT data collection", log_content)))
    expect_true(any(grepl("Iteration 1 completed", log_content)))
})

test_that("test incrementality in response", {
    dest_file <- tempfile(fileext = ".csv")
    pb_file <- tempfile(fileext = ".pb")
    file.create(pb_file)

    msg_header <- structure(list(timestamp = 1700000000, incrementality = "FULL_DATASET"), class = "Message")
    msg_entity <- list(structure(list(id = "1"), class = "Message"))
    feed_msg <- structure(list(header = msg_header, entity = msg_entity), class = "Message")

    testthat::with_mocked_bindings(
        readProtoFiles = function(...) TRUE,
        P = function(...) TRUE,
        read = function(...) feed_msg,
        .package = "RProtoBuf",
        code = {
            GTFShift::rt_collect_protobuf(
                gtfs_rt_url = pb_file,
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
    dest_file <- tempfile(fileext = ".csv")
    pb_file <- tempfile(fileext = ".pb")
    file.create(pb_file)

    request_count <- 0
    start_time <- Sys.time()

    testthat::with_mocked_bindings(
        readProtoFiles = function(...) TRUE,
        P = function(...) TRUE,
        read = function(...) {
            request_count <<- request_count + 1
            if (request_count > 3) {
                stop("stop_loop_after_3_requests")
            }
            structure(list(id = "1"), class = "Message")
        },
        .package = "RProtoBuf",
        code = {
            expect_error(
                GTFShift::rt_collect_protobuf(
                    gtfs_rt_url = pb_file,
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
