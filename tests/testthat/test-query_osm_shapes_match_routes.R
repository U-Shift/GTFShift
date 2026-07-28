library(testthat)
library(sf)

# Helper fixture setup for osm_shapes_match_routes
setup_match_fixtures <- function(gtfs) {
    target_route_id <- gtfs$routes$route_id[1]
    target_route <- gtfs$routes$route_short_name[1]
    target_shape_id <- gtfs$trips$shape_id[gtfs$trips$route_id == target_route_id][1]

    mock_rel_df <- data.frame(
        relation_osm_id = "rel_1",
        type = c("way", "node", "node"),
        osm_id = c("w1", "n1", "n2"),
        role = c("forward", "stop_entry_only", "stop_exit_only"),
        ref = target_route,
        name = paste("Line", target_route),
        gtfs_shape_id = NA_character_,
        gtfs_route_id = NA_character_,
        roundtrip = "no",
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    mock_ways <- st_sf(
        osm_id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    mock_stops <- st_sf(
        osm_id = c("n1", "n2"),
        public_transport = c("stop_position", "stop_position"),
        geometry = st_sfc(st_point(c(-8.6, 41.1)), st_point(c(-8.61, 41.11)), crs = 4326)
    )

    list(
        target_route_id = target_route_id,
        target_route = target_route,
        target_shape_id = target_shape_id,
        rel_df = mock_rel_df,
        ways = mock_ways,
        stops = mock_stops
    )
}

test_that("osm_shapes_match_routes validates input parameters and throws errors", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    # Invalid gtfs_match
    expect_error(
        GTFShift::osm_shapes_match_routes(gtfs, q = NA, gtfs_match = "invalid_col"),
        "gtfs_match should be one of"
    )

    # Invalid osm_match
    expect_error(
        GTFShift::osm_shapes_match_routes(gtfs, q = NA, osm_match = "invalid_col"),
        "osm_match should be one of"
    )

    # Invalid metric_crs
    expect_error(
        GTFShift::osm_shapes_match_routes(gtfs, q = NA, metric_crs = "invalid_crs_string"),
    )

    # Invalid metric_crs
    expect_error(
        GTFShift::osm_shapes_match_routes(gtfs, q = NA, metric_crs = NA)
    )
})

test_that("osm_shapes_match_routes executes matching flow with osm_file provided", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    fx <- setup_match_fixtures(gtfs)

    log_tmp <- tempfile(fileext = ".log")
    on.exit(unlink(log_tmp), add = TRUE)

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) fx$rel_df,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") fx$stops else fx$ways
                },
                .package = "osmextract",
                code = {
                    res <- suppressWarnings(GTFShift::osm_shapes_match_routes(
                        gtfs = gtfs,
                        q = NA,
                        log_file = log_tmp,
                        osm_file = "dummy.pbf",
                        metric_crs = 3857
                    ))

                    expect_s3_class(res, "sf")
                    expect_contains(names(res), c("route_id", "shape_id", "osm_id", "distance_diff", "points_diff", "stops_diff", "geometry"))
                    expect_equal(res$osm_id[1], "rel_1")
                    expect_equal(res$shape_id[1], fx$target_shape_id)

                    expect_true(file.exists(log_tmp))
                    log_lines <- readLines(log_tmp)
                    expect_true(any(grepl("Found \\d+ GTFS shapes and \\d+ stops", log_lines)))
                    expect_true(any(grepl("Found \\d+ OSM route relations and \\d+ stops/platforms", log_lines)))
                    expect_true(any(grepl("Associated \\d+ shapes", log_lines)))
                }
            )
        }
    )
})

test_that("osm_shapes_match_routes executes matching flow when osm_file is NULL (Overpass API path)", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    fx <- setup_match_fixtures(gtfs)

    mock_multilines <- st_sf(
        osm_id = "rel_1",
        ref = fx$target_route,
        name = paste("Line", fx$target_route),
        geometry = st_sfc(st_multilinestring(list(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2))), crs = 4326)
    )

    mock_osm_data <- list(
        osm_multilines = mock_multilines,
        osm_points = fx$stops
    )

    mock_bg_job_xml <- list(is_alive = function() FALSE, get_result = function() NULL)
    mock_bg_job_sf <- list(is_alive = function() FALSE, get_result = function() mock_osm_data)
    mock_bg_job_points <- list(is_alive = function() FALSE, get_result = function() fx$stops)
    mock_bg_job_rel <- list(is_alive = function() FALSE, get_result = function() fx$rel_df)

    r_bg_call_count <- 0

    log_tmp <- tempfile(fileext = ".log")
    on.exit(unlink(log_tmp), add = TRUE)

    testthat::with_mocked_bindings(
        r_bg = function(...) {
            r_bg_call_count <<- r_bg_call_count + 1
            if (r_bg_call_count == 1) {
                return(mock_bg_job_xml)
            } else if (r_bg_call_count == 2) {
                return(mock_bg_job_sf)
            } else if (r_bg_call_count == 3) {
                return(mock_bg_job_points)
            } else {
                return(mock_bg_job_rel)
            }
        },
        .package = "callr",
        code = {
            res <- suppressWarnings(GTFShift::osm_shapes_match_routes(
                gtfs = gtfs,
                q = "dummy_q",
                log_file = log_tmp,
                osm_file = NULL,
                metric_crs = 3857
            ))

            expect_s3_class(res, "sf")
            expect_contains(names(res), c("route_id", "shape_id", "osm_id", "geometry"))
            expect_equal(res$osm_id[1], "rel_1")

            expect_true(file.exists(log_tmp))
            log_lines <- readLines(log_tmp)
            expect_true(any(grepl("Found \\d+ OSM route relations and \\d+ stops/platforms", log_lines)))
        }
    )
})

test_that("osm_shapes_match_routes supports non-exact string matching (gtfs_osm_match_exact = FALSE)", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    fx <- setup_match_fixtures(gtfs)

    fx$rel_df$ref <- paste("Route", fx$target_route, "Express")

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) fx$rel_df,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") fx$stops else fx$ways
                },
                .package = "osmextract",
                code = {
                    res <- suppressWarnings(GTFShift::osm_shapes_match_routes(
                        gtfs = gtfs,
                        q = NA,
                        gtfs_osm_match_exact = FALSE,
                        osm_file = "dummy.pbf",
                        metric_crs = 3857
                    ))

                    expect_s3_class(res, "sf")
                    expect_gt(nrow(res), 0)
                }
            )
        }
    )
})

test_that("osm_shapes_match_routes writes logs to log_file when provided", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    fx <- setup_match_fixtures(gtfs)

    log_tmp <- tempfile(fileext = ".log")
    on.exit(unlink(log_tmp), add = TRUE)

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) fx$rel_df,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") fx$stops else fx$ways
                },
                .package = "osmextract",
                code = {
                    suppressWarnings(GTFShift::osm_shapes_match_routes(
                        gtfs = gtfs,
                        q = NA,
                        log_file = log_tmp,
                        osm_file = "dummy.pbf",
                        metric_crs = 3857
                    ))

                    expect_true(file.exists(log_tmp))
                    log_lines <- readLines(log_tmp)
                    expect_true(any(grepl("Running osm_shapes_match_routes", log_lines)))
                }
            )
        }
    )
})

test_that("osm_shapes_match_routes returns plain data.frame when geometry = FALSE", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    fx <- setup_match_fixtures(gtfs)

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) fx$rel_df,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") fx$stops else fx$ways
                },
                .package = "osmextract",
                code = {
                    res <- suppressWarnings(GTFShift::osm_shapes_match_routes(
                        gtfs = gtfs,
                        q = NA,
                        geometry = FALSE,
                        osm_file = "dummy.pbf",
                        metric_crs = 3857
                    ))

                    expect_false(inherits(res, "sf"))
                    expect_s3_class(res, "data.frame")
                    expect_false("geometry" %in% names(res))
                }
            )
        }
    )
})

test_that("osm_shapes_match_routes issues warnings when routes or stops are missing or unsorted", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    mock_rel_empty <- data.frame(
        relation_osm_id = "rel_1",
        type = "way",
        osm_id = "w1",
        role = "forward",
        ref = "NON_EXISTENT_REF",
        name = "Unknown Route",
        gtfs_shape_id = NA_character_,
        gtfs_route_id = NA_character_,
        roundtrip = "no",
        stringsAsFactors = FALSE
    )

    mock_ways_empty <- st_sf(
        osm_id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    mock_stops <- st_sf(
        osm_id = "n1",
        public_transport = "stop_position",
        geometry = st_sfc(st_point(c(-8.6, 41.1)), crs = 4326)
    )

    log_tmp <- tempfile(fileext = ".log")
    on.exit(unlink(log_tmp), add = TRUE)

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) mock_rel_empty,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") mock_stops else mock_ways_empty
                },
                .package = "osmextract",
                code = {
                    warn_pattern <- "did not match any OSM route"
                    expect_warning(
                        GTFShift::osm_shapes_match_routes(
                            gtfs = gtfs,
                            q = NA,
                            log_file = log_tmp,
                            osm_file = "dummy.pbf",
                            metric_crs = 3857
                        ),
                        warn_pattern
                    )
                    expect_true(file.exists(log_tmp))
                    log_lines <- readLines(log_tmp)
                    expect_true(any(grepl("WARNING!", log_lines) & grepl(warn_pattern, log_lines)))
                }
            )
        }
    )
})

test_that("osm_shapes_match_routes issues warning when metric_crs is default / missing", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    fx <- setup_match_fixtures(gtfs)

    log_tmp <- tempfile(fileext = ".log")
    on.exit(unlink(log_tmp), add = TRUE)

    warn_pattern <- "Using default metric_crs"
    testthat::with_mocked_bindings(
        get_osm_relations = function(...) fx$rel_df,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") fx$stops else fx$ways
                },
                .package = "osmextract",
                code = {
                    res <- expect_warning(
                        GTFShift::osm_shapes_match_routes(
                            gtfs = gtfs,
                            q = NA,
                            log_file = log_tmp,
                            osm_file = "dummy.pbf"
                        ),
                        warn_pattern
                    )
                    expect_true(file.exists(log_tmp))
                    log_lines <- readLines(log_tmp)
                    expect_true(any(grepl("WARNING!", log_lines) & grepl(warn_pattern, log_lines)))
                }
            )
        }
    )
})

test_that("osm_shapes_match_routes handles unsorted stops and osm_stop_order_relaxed parameter", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    fx <- setup_match_fixtures(gtfs)

    # Unsorted relation: entry stop is at position 2 instead of position 1
    mock_rel_unsorted <- data.frame(
        relation_osm_id = "rel_1",
        type = c("node", "node", "way"),
        osm_id = c("n2", "n1", "w1"),
        role = c("stop_exit_only", "stop_entry_only", "forward"),
        ref = fx$target_route,
        name = paste("Line", fx$target_route),
        gtfs_shape_id = NA_character_,
        gtfs_route_id = NA_character_,
        roundtrip = "no",
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    log_tmp <- tempfile(fileext = ".log")
    on.exit(unlink(log_tmp), add = TRUE)

    # With osm_stop_order_relaxed = FALSE (default), unsorted stops trigger warning and result in no match
    testthat::with_mocked_bindings(
        get_osm_relations = function(...) mock_rel_unsorted,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") fx$stops else fx$ways
                },
                .package = "osmextract",
                code = {
                    warn_pattern <- "entry/exit stops not respecting the right order"
                    expect_warning(
                        GTFShift::osm_shapes_match_routes(
                            gtfs = gtfs,
                            q = NA,
                            log_file = log_tmp,
                            osm_file = "dummy.pbf",
                            metric_crs = 3857,
                            osm_stop_order_relaxed = FALSE
                        ),
                        warn_pattern
                    )
                    expect_true(file.exists(log_tmp))
                    log_lines <- readLines(log_tmp)
                    expect_true(any(grepl("WARNING!", log_lines) & grepl(warn_pattern, log_lines)))

                    # With osm_stop_order_relaxed = TRUE, relation is matched despite unsorted stops
                    res <- suppressWarnings(GTFShift::osm_shapes_match_routes(
                        gtfs = gtfs,
                        q = NA,
                        osm_file = "dummy.pbf",
                        metric_crs = 3857,
                        osm_stop_order_relaxed = TRUE
                    ))
                    expect_s3_class(res, "sf")
                    expect_equal(nrow(res), 1)
                }
            )
        }
    )
})

test_that("osm_shapes_match_routes returns empty result when !osm_stop_order_relaxed and unsorted stops", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    fx <- setup_match_fixtures(gtfs)

    mock_rel_unsorted <- data.frame(
        relation_osm_id = "rel_1",
        type = c("node", "node", "way"),
        osm_id = c("n2", "n1", "w1"),
        role = c("stop_exit_only", "stop_entry_only", "forward"),
        ref = fx$target_route,
        name = paste("Line", fx$target_route),
        gtfs_shape_id = NA_character_,
        gtfs_route_id = NA_character_,
        roundtrip = "no",
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) mock_rel_unsorted,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") fx$stops else fx$ways
                },
                .package = "osmextract",
                code = {
                    res <- suppressWarnings(GTFShift::osm_shapes_match_routes(
                        gtfs = gtfs,
                        q = NA,
                        osm_file = "dummy.pbf",
                        metric_crs = 3857,
                        osm_stop_order_relaxed = FALSE
                    ))
                    expect_s3_class(res, "sf")
                    expect_equal(nrow(res), 0)
                }
            )
        }
    )
})

test_that("osm_shapes_match_routes handles empty gtfs_route_name or error in stop evaluation (warn_osm_stops_missing)", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    fx <- setup_match_fixtures(gtfs)

    mock_rel_bad_stops <- data.frame(
        relation_osm_id = "rel_1",
        type = c("way", "node", "node"),
        osm_id = c("w1", "n1", "n2"),
        role = c("forward", "stop_entry_only", "stop_exit_only"),
        ref = fx$target_route,
        name = paste("Line", fx$target_route),
        gtfs_shape_id = NA_character_,
        gtfs_route_id = NA_character_,
        roundtrip = "no",
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    log_tmp <- tempfile(fileext = ".log")
    on.exit(unlink(log_tmp), add = TRUE)

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) mock_rel_bad_stops,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") fx$stops else fx$ways
                },
                .package = "osmextract",
                code = {
                    testthat::with_mocked_bindings(
                        drop_units = function(x) {
                            if (inherits(x, "sgbp") || is.numeric(x) || inherits(x, "units")) {
                                stop("Simulated stop distance calculation error")
                            }
                            x
                        },
                        .package = "units",
                        code = {
                            warn_pattern <- "There were \\d+ error\\(s\\) during the algorithm execution"
                            expect_warning(
                                GTFShift::osm_shapes_match_routes(
                                    gtfs = gtfs,
                                    q = NA,
                                    log_file = log_tmp,
                                    osm_file = "dummy.pbf",
                                    metric_crs = 3857
                                ),
                                warn_pattern
                            )
                            expect_true(file.exists(log_tmp))
                            log_lines <- readLines(log_tmp)
                            expect_true(any(grepl("WARNING!", log_lines) & grepl("error\\(s\\) during the algorithm execution", log_lines)))
                        }
                    )
                }
            )
        }
    )
})

test_that("osm_shapes_match_routes supports parallel execution (num_cores > 1)", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    fx <- setup_match_fixtures(gtfs)

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) fx$rel_df,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") fx$stops else fx$ways
                },
                .package = "osmextract",
                code = {
                    res <- suppressWarnings(GTFShift::osm_shapes_match_routes(
                        gtfs = gtfs,
                        q = NA,
                        osm_file = "dummy.pbf",
                        metric_crs = 3857,
                        num_cores = 2
                    ))

                    expect_s3_class(res, "sf")
                    expect_gt(nrow(res), 0)
                }
            )
        }
    )
})

test_that("osm_shapes_match_routes prints warning messages to console when errors occur", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    mock_rel_empty <- data.frame(
        relation_osm_id = "rel_1",
        type = "way",
        osm_id = "w1",
        role = "forward",
        ref = "NON_EXISTENT_REF",
        name = "Unknown Route",
        gtfs_shape_id = NA_character_,
        gtfs_route_id = NA_character_,
        roundtrip = "no",
        stringsAsFactors = FALSE
    )

    mock_ways_empty <- st_sf(
        osm_id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    mock_stops <- st_sf(
        osm_id = "n1",
        public_transport = "stop_position",
        geometry = st_sfc(st_point(c(-8.6, 41.1)), crs = 4326)
    )

    log_tmp <- tempfile(fileext = ".log")
    on.exit(unlink(log_tmp), add = TRUE)

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) mock_rel_empty,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") mock_stops else mock_ways_empty
                },
                .package = "osmextract",
                code = {
                    warn_pattern <- "There were \\d+ error\\(s\\) during the algorithm execution"
                    expect_warning(
                        GTFShift::osm_shapes_match_routes(
                            gtfs = gtfs,
                            q = NA,
                            log_file = log_tmp,
                            osm_file = "dummy.pbf",
                            metric_crs = 3857
                        ),
                        warn_pattern
                    )
                    expect_true(file.exists(log_tmp))
                    log_lines <- readLines(log_tmp)
                    expect_true(any(grepl("WARNING!", log_lines) & grepl("error\\(s\\) during the algorithm execution", log_lines)))
                }
            )
        }
    )
})

test_that("osm_shapes_match_routes handles GTFS route with no trips or shapes (nrow(gtfs_route_name) == 0)", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)
    fx <- setup_match_fixtures(gtfs)

    # Modify GTFS trips to remove all trips for the target route, resulting in nrow(gtfs_route_name) == 0
    gtfs_no_trips <- gtfs
    gtfs_no_trips$trips <- gtfs_no_trips$trips[0, ]

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) fx$rel_df,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, layer = "lines", ...) {
                    if (!missing(layer) && layer == "points") fx$stops else fx$ways
                },
                .package = "osmextract",
                code = {
                    res <- suppressWarnings(GTFShift::osm_shapes_match_routes(
                        gtfs = gtfs_no_trips,
                        q = NA,
                        osm_file = "dummy.pbf",
                        metric_crs = 3857
                    ))
                    expect_equal(nrow(res), 0)
                }
            )
        }
    )
})

