library(testthat)
library(sf)

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

test_that("network_overline raises warning when metric_crs is default", {
    target_net <- st_sf(
        id = 1,
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )
    lines_sf <- st_sf(
        frequency = 5,
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )

    expect_warning(
        GTFShift::network_overline(
            target_network = target_net,
            lines = lines_sf,
            attr = "frequency",
            target_network_split = NA
        ),
        "Using default metric_crs"
    )
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
