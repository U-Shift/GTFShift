library(testthat)
library(sf)

test_that("osm_bus_lanes queries and filters bus lanes using mocked osmextract", {
    mock_lines <- st_sf(
        osm_id = c("1", "2"),
        psv = c("designated", "no"),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)),
            st_linestring(matrix(c(1, 1, 2, 2), ncol = 2)),
            crs = 4326
        )
    )

    testthat::with_mocked_bindings(
        oe_read = function(...) {
            mock_lines
        },
        oe_get_keys = function(...) {
            c("psv")
        },
        .package = "osmextract",
        code = {
            bbox <- st_bbox(st_sfc(st_point(c(0, 0)), crs = 4326))
            res <- GTFShift::osm_bus_lanes(bbox, osm_file = "dummy.pbf")
            expect_s3_class(res, "sf")
            expect_equal(nrow(res), 1)
            expect_equal(res[["osm:id"]][1], "1")
            expect_false("2" %in% res[["osm:id"]])
        }
    )
})

test_that("osm_bus_lanes queries bus lanes when osm_file = NULL using mocked osmdata", {
    mock_lines <- st_sf(
        osm_id = c("1", "2"),
        psv = c("designated", "no"),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)),
            st_linestring(matrix(c(1, 1, 2, 2), ncol = 2)),
            crs = 4326
        )
    )

    mock_osmdata <- list(osm_lines = mock_lines)

    testthat::with_mocked_bindings(
        opq = function(...) "mock_opq",
        add_osm_feature = function(...) "mock_opq",
        osmdata_sf = function(...) mock_osmdata,
        osm_poly2line = function(...) mock_osmdata,
        .package = "GTFShift",
        code = {
            bbox <- st_bbox(st_sfc(st_point(c(0, 0)), crs = 4326))
            res <- GTFShift::osm_bus_lanes(bbox, osm_file = NULL)
            expect_s3_class(res, "sf")
            expect_equal(nrow(res), 1)
            expect_equal(res$osm_id[1], "1")
            expect_false("2" %in% res$osm_id)
        }
    )
})
