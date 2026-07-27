library(testthat)
library(sf)

test_that("osm_shapes_to_routes matches shapes by gtfs:shape_id using mocks", {
    sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]

    mock_rel_df <- data.frame(
        relation_osm_id = "rel_1",
        type = "way",
        osm_id = "w1",
        role = "forward",
        `gtfs:shape_id` = target_shape_id,
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    mock_ways <- st_sf(
        osm_id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) mock_rel_df,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(...) mock_ways,
                .package = "osmextract",
                code = {
                    res <- GTFShift::osm_shapes_to_routes(gtfs, q = NA, osm_file = "dummy.pbf")
                    expect_s3_class(res, "sf")
                    expect_contains(names(res), c("shape_id", "osm_id", "geometry"))
                }
            )
        }
    )
})

test_that("osm_shapes_to_routes works when osm_file is not provided (calls Overpass API)", {
    sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]

    mock_multilines <- st_sf(
        osm_id = "rel_1",
        `gtfs:shape_id` = target_shape_id,
        geometry = st_sfc(st_multilinestring(list(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2))), crs = 4326)
    )

    mock_osm_data <- list(
        osm_multilines = mock_multilines
    )

    mock_bg_job <- list(
        is_alive = function() FALSE,
        get_result = function() mock_osm_data
    )

    mock_bg_job_xml <- list(
        is_alive = function() FALSE,
        get_result = function() NULL
    )

    r_bg_call_count <- 0

    testthat::with_mocked_bindings(
        r_bg = function(...) {
            r_bg_call_count <<- r_bg_call_count + 1
            if (r_bg_call_count == 1) {
                return(mock_bg_job_xml)
            } else {
                return(mock_bg_job)
            }
        },
        .package = "callr",
        code = {
            res <- GTFShift::osm_shapes_to_routes(gtfs, q = "dummy_q", osm_file = NULL)
            expect_s3_class(res, "sf")
            expect_contains(names(res), c("shape_id", "osm_id", "geometry"))
            expect_equal(nrow(res), 1)
            expect_equal(res$shape_id, target_shape_id)
            expect_equal(res$osm_id, "rel_1")
        }
    )
})

test_that("osm_shapes_to_routes with ways = TRUE and osm_file provided extracts way tags", {
    sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]

    mock_rel_df <- data.frame(
        relation_osm_id = "rel_1",
        type = "way",
        osm_id = "w1",
        role = "forward",
        `gtfs:shape_id` = target_shape_id,
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    mock_ways <- st_sf(
        osm_id = "w1",
        geometry = st_sfc(st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    mock_extra_tags <- data.frame(
        osm_id = "w1",
        lanes = "2",
        maxspeed = "50",
        other_tags = "dummy",
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    testthat::with_mocked_bindings(
        get_osm_relations = function(...) mock_rel_df,
        .package = "GTFShift",
        code = {
            testthat::with_mocked_bindings(
                oe_read = function(file, extra_tags = NULL, ...) {
                    if (is.null(extra_tags)) {
                        return(mock_ways)
                    } else {
                        return(st_sf(mock_extra_tags, geometry = st_sfc(st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)))
                    }
                },
                oe_get_keys = function(...) c("lanes", "maxspeed"),
                .package = "osmextract",
                code = {
                    res <- GTFShift::osm_shapes_to_routes(
                        gtfs, q = NA, ways = TRUE,
                        ways_tags = c("lanes", "maxspeed"),
                        osm_file = "dummy.pbf"
                    )
                    expect_s3_class(res, "sf")
                    expect_contains(names(res), c("shape_id", "osm_id", "way_osm_id", "lanes", "maxspeed", "geometry"))
                    expect_equal(res$way_osm_id, "w1")
                    expect_equal(res$lanes, "2")
                    expect_equal(res$maxspeed, "50")
                }
            )
        }
    )
})



test_that("osm_shapes_to_routes with ways = TRUE and osm_file = NULL extracts ways and ways_tags", {
    sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]

    mock_multilines <- st_sf(
        osm_id = "rel_1",
        `gtfs:shape_id` = target_shape_id,
        geometry = st_sfc(st_multilinestring(list(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2))), crs = 4326)
    )

    mock_lines <- st_sf(
        osm_id = "w1",
        lanes = "3",
        maxspeed = "60",
        geometry = st_sfc(st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    mock_osm_data <- list(
        osm_multilines = mock_multilines,
        osm_lines = mock_lines
    )

    mock_bg_job_sf <- list(
        is_alive = function() FALSE,
        get_result = function() mock_osm_data
    )

    mock_bg_job_xml <- list(
        is_alive = function() FALSE,
        get_result = function() NULL
    )

    mock_relations_df <- data.frame(
        type = "way",
        ref = "w1",
        role = "forward",
        relation_osm_id = "rel_1",
        stringsAsFactors = FALSE
    )

    r_bg_call_count <- 0

    testthat::with_mocked_bindings(
        r_bg = function(func, args, ...) {
            r_bg_call_count <<- r_bg_call_count + 1
            if (r_bg_call_count == 1) {
                return(mock_bg_job_xml)
            } else if (r_bg_call_count == 2) {
                return(mock_bg_job_sf)
            } else {
                return(list(
                    is_alive = function() FALSE,
                    get_result = function() mock_relations_df
                ))
            }
        },
        .package = "callr",
        code = {
            res <- GTFShift::osm_shapes_to_routes(
                gtfs, q = "dummy_q", ways = TRUE,
                ways_tags = c("lanes", "maxspeed"),
                osm_file = NULL
            )
            expect_s3_class(res, "sf")
            expect_contains(names(res), c("shape_id", "osm_id", "way_osm_id", "lanes", "maxspeed", "geometry"))
            expect_equal(res$way_osm_id, "w1")
            expect_equal(res$lanes, "3")
            expect_equal(res$maxspeed, "60")
        }
    )
})






