test_that("network_overline aggregates lines onto target network", {
  l_100 <- sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE))
  target_net <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(l_100, crs = 3857)
  )
  lines_sf <- sf::st_sf(
    frequency = c(5, 10),
    geometry = sf::st_sfc(l_100, l_100, crs = 3857)
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
  # Expecting the sum of frequencies
  expect_identical(res$frequency, sum(lines_sf$frequency))
})

test_that("network_overline works when metric_crs is default (NULL)", {
  l_geo <- sf::st_linestring(
    matrix(c(-9.1, 38.7, -9.11, 38.71), ncol = 2, byrow = TRUE)
  )
  target_net <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(l_geo, crs = 4326)
  )
  lines_sf <- sf::st_sf(
    frequency = 5,
    geometry = sf::st_sfc(l_geo, crs = 4326)
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
  l_100 <- sf::st_linestring(matrix(c(0, 0, 100, 0), ncol = 2, byrow = TRUE))
  target_net <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(l_100, crs = 3857)
  )
  lines_sf <- sf::st_sf(
    frequency = 5,
    geometry = sf::st_sfc(l_100, crs = 3857)
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

test_that(
  paste0(
    "network_overline handles parameter variations ",
    "(target_network_split, fun, join_dist)"
  ),
  {
    l_200 <- sf::st_linestring(matrix(c(0, 0, 200, 0), ncol = 2, byrow = TRUE))
    l_par <- sf::st_linestring(matrix(c(0, 5, 50, 5), ncol = 2, byrow = TRUE))
    target_net <- sf::st_sf(
      id = 1,
      geometry = sf::st_sfc(l_200, crs = 3857)
    )
    lines_sf <- sf::st_sf(
      frequency = c(10, 20),
      geometry = sf::st_sfc(
        l_200, # original line at y = 0
        l_par, # parallel line offset by +5m at y = 5
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
  }
)
