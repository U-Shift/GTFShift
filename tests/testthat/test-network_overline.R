test_that("network_overline aggregates lines onto target network", {
    target_net <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )
    lines_sf <- sf::st_sf(
        frequency = c(5, 10),
        geometry = sf::st_sfc(
            sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
            sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)),
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
    expect_identical(res$frequency, sum(lines_sf$frequency)) # Expecting the sum of frequencies
    })

test_that("network_overline works when metric_crs is default (NULL)", {
    target_net <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(-9.1, 38.7, -9.11, 38.71), ncol = 2, byrow = TRUE)), crs = 4326)
    )
    lines_sf <- sf::st_sf(
        frequency = 5,
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(-9.1, 38.7, -9.11, 38.71), ncol = 2, byrow = TRUE)), crs = 4326)
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
    expect_identical(res$frequency, 5)
})

test_that("network_overline stops on invalid metric_crs", {
    target_net <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )
    lines_sf <- sf::st_sf(
        frequency = 5,
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE)), crs = 3857)
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
    target_net <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 200, 0), ncol = 2, byrow = TRUE)), crs = 3857)
    )
    lines_sf <- sf::st_sf(
        frequency = c(10, 20),
        geometry = sf::st_sfc(
            sf::st_linestring(matrix(c(0, 0, 200, 0), ncol = 2, byrow = TRUE)), # original line at y = 0
            sf::st_linestring(matrix(c(0, 5, 50, 5), ncol = 2, byrow = TRUE)), # parallel line offset by +5m at y = 5
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
    expect_identical(nrow(res_max), 4L) # Expecting 4 segments after splitting
    expect_contains(res_max$frequency, c(10, 20))
})
