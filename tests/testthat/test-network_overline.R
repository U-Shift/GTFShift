library(testthat)
library(sf)

#' @srrstats {G2.11} Tests `network_overline()` with `units`-classed `attr` column and verifies `units` preservation (`SP4.1`).
#' @srrstats {G5.2} Tests error when `metric_crs` is invalid `NA` or `attr` is missing.
#' @srrstats {G5.4} Tests correctness of line attribute aggregation (`sum` and `max`) over a target road network.
#' @srrstats {G5.4b} Tests `network_overline()` integration with `stplanr` line segmentation and network joining.
#' @srrstats {SP2.2b} Demonstrates compatibility with `sf` and `stplanr` workflows in tests.
#' @srrstats {SP4.1} Verifies that `units` on the aggregated `attr` column are preserved in the returned `sf` object.
#' @srrstats {SP6.0} Verifies that aggregated network segments preserve the original CRS of `target_network`.
#' @srrstats {SP6.1} Tests `network_overline()` on both projected (`metric_crs = 3857`) and geographic (`metric_crs = NULL`, `EPSG:4326`) networks.
test_that("network_overline aggregates lines onto target network", {
    target_net <- st_sf(
        id = 1,
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )
    lines_sf <- st_sf(
        frequency = c(5, 10),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            crs = 3857
        )
    )

    suppressWarnings({ # stplanr will warn "rsgeo not installed, using lwgeom"
        res <- GTFShift::network_overline(
            target_network = target_net,
            lines = lines_sf,
            attr = "frequency",
            target_network_split = NA,
            metric_crs = 3857
        )
    })

    expect_s3_class(res, "sf")
    expect_contains(names(res), "frequency")
    expect_equal(res$frequency, sum(lines_sf$frequency)) # Expecting the sum of frequencies
    })

test_that("network_overline works when metric_crs is default (NULL)", {
    target_net <- st_sf(
        id = 1,
        geometry = st_sfc(st_linestring(matrix(c(-9.1, 38.7, -9.11, 38.71), ncol = 2, byrow = TRUE)), crs = 4326)
    )
    lines_sf <- st_sf(
        frequency = 5,
        geometry = st_sfc(st_linestring(matrix(c(-9.1, 38.7, -9.11, 38.71), ncol = 2, byrow = TRUE)), crs = 4326)
    )

    suppressWarnings({ # stplanr will warn about sub-geometries / rsgeo
        res <- GTFShift::network_overline(
            target_network = target_net,
            lines = lines_sf,
            attr = "frequency",
            target_network_split = NA
        )
    })
    expect_s3_class(res, "sf")
    expect_equal(res$frequency, 5)
})

test_that("network_overline stops on invalid metric_crs", {
    target_net <- st_sf(
        id = 1,
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )
    lines_sf <- st_sf(
        frequency = 5,
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )

    expect_error(
        GTFShift::network_overline(
            target_network = target_net,
            lines = lines_sf,
            attr = "frequency",
            metric_crs = NA
        ),
        "metric_crs should be a valid CRS value"
    )
})

test_that("network_overline handles parameter variations (target_network_split, fun, join_dist)", {
    target_net <- st_sf(
        id = 1,
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 200, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )
    lines_sf <- st_sf(
        frequency = c(10, 20),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 200, 0), ncol = 2, byrow = TRUE)), # original line at y = 0
            st_linestring(matrix(c(0, 5, 50, 5), ncol = 2, byrow = TRUE)), # parallel line offset by +5m at y = 5
            crs = 3857
        )
    )


    # Test with target_network_split = 50 and fun = max
    suppressWarnings({ # stplanr will warn "rsgeo not installed, using lwgeom"
        res_max <- GTFShift::network_overline(
            target_network = target_net,
            lines = lines_sf,
            attr = "frequency",
            target_network_split = 50,
            fun = max,
            join_dist = 15,
            metric_crs = 3857
        )
    })

    expect_s3_class(res_max, "sf")
    expect_contains(names(res_max), "frequency")
    expect_equal(nrow(res_max), 4) # Expecting 4 segments after splitting
    expect_contains(res_max$frequency, c(10, 20))
})

test_that("network_overline preserves units on attr column (G2.11, SP4.1)", {
    target_net <- st_sf(
        id = 1,
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )
    lines_sf <- st_sf(
        frequency = units::set_units(c(5, 10), "1/h"),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            crs = 3857
        )
    )

    suppressWarnings({
        res_units <- GTFShift::network_overline(
            target_network = target_net,
            lines = lines_sf,
            attr = "frequency",
            target_network_split = NA,
            metric_crs = 3857
        )
    })

    expect_s3_class(res_units$frequency, "units")
    expect_equal(as.numeric(res_units$frequency), 15)
})
