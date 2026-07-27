library(testthat)
library(sf)

test_that("prioritize_lanes analyzes lane prioritization with mocked dependencies", {
    sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    ref_date <- gtfs$calendar$start_date[1]

    mock_way_freq <- st_sf(
        way_osm_id = c("1001", "1002"),
        hour = c(8, 8),
        frequency = c(12, 15),
        routes = I(list(c("R1"), c("R2"))),
        shapes = I(list(c("S1"), c("S2"))),
        lanes = c("2", "4"),
        oneway = c("yes", "no"),
        psv = c(NA, "designated"),
        `parking:lane:both` = c("parallel", "no"),
        `parking:lane:left` = c(NA, "diagonal"),
        `parking:lane:right` = c(NA, "no"),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(100, 0, 200, 0), ncol = 2, byrow = TRUE)),
            crs = 4326
        )
    )

    testthat::with_mocked_bindings(
        get_way_frequency_hourly = function(...) {
            mock_way_freq
        },
        .package = "GTFShift",
        code = {
            res <- GTFShift::prioritize_lanes(gtfs, q = NA, date = ref_date)
            expect_s3_class(res, "sf")
            expect_equal(nrow(res), 2)
            
            # Default keep_osm_attributes = FALSE: OSM columns should not be in the output
            expect_false(any(c("lanes", "oneway", "psv", "parking:lane:both") %in% names(res)))

            # Check is_bus_lane
            expect_equal(res$is_bus_lane, c(FALSE, TRUE))

            # Check n_lanes_parking
            expect_equal(res$n_lanes_parking, c(2L, 1L))

            # Check n_lanes_circulation
            expect_equal(res$n_lanes_circulation, c(2, 4))

            # Check n_directions
            expect_equal(res$n_directions, c(1, 2))

            # Check n_lanes_circulation_direction
            expect_equal(res$n_lanes_circulation_direction, c(2, 2))
        }
    )
})

test_that("prioritize_lanes retains extra OSM attributes when keep_osm_attributes = TRUE", {
    sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    ref_date <- gtfs$calendar$start_date[1]

    mock_way_freq <- st_sf(
        way_osm_id = c("1001", "1002"),
        hour = c(8, 8),
        frequency = c(12, 15),
        routes = I(list(c("R1"), c("R2"))),
        shapes = I(list(c("S1"), c("S2"))),
        lanes = c("2", "4"),
        oneway = c("yes", "no"),
        psv = c(NA, "designated"),
        `parking:lane:both` = c("parallel", "no"),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(100, 0, 200, 0), ncol = 2, byrow = TRUE)),
            crs = 4326
        )
    )

    testthat::with_mocked_bindings(
        get_way_frequency_hourly = function(...) {
            mock_way_freq
        },
        .package = "GTFShift",
        code = {
            res <- GTFShift::prioritize_lanes(gtfs, q = NA, date = ref_date, keep_osm_attributes = TRUE)
            expect_s3_class(res, "sf")
            expect_contains(names(res), c("lanes", "oneway", "psv", "parking:lane:both"))
        }
    )
})
