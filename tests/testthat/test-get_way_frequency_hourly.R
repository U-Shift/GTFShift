test_that(
  paste0(
    "get_way_frequency_hourly calculates way frequencies ",
    "with mocked osm_shapes_to_routes"
  ),
  {
    sample_file <- system.file(
      "extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"
    )
    gtfs <- GTFShift::load_feed(sample_file)

    ref_date <- gtfs$calendar$start_date[1]

    # Calculate route frequencies to select an active shape_id with departures
    route_freq <- GTFShift::get_route_frequency_hourly(gtfs, date = ref_date)
    target_shape_id <- route_freq$shape_id[1]
    expected_freq_by_hour <- route_freq |>
      sf::st_drop_geometry() |>
      dplyr::filter(shape_id == target_shape_id) |>
      dplyr::group_by(hour) |>
      dplyr::summarise(total_freq = sum(frequency))

    l1 <- sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2))
    mock_line <- sf::st_sf(
      shape_id = target_shape_id,
      way_osm_id = "12345",
      geometry = sf::st_sfc(l1, crs = 4326)
    )

    testthat::with_mocked_bindings(
      osm_shapes_to_routes = function(...) {
        mock_line
      },
      .package = "GTFShift",
      code = {
        res <- GTFShift::get_way_frequency_hourly(
          gtfs, q = NA, date = ref_date
        )
        expect_s3_class(res, "sf")
        expect_contains(
          names(res),
          c("way_osm_id", "frequency", "geometry", "routes", "shapes")
        )

        # Validate that way frequency matches route frequency for selected shape
        way_freq_by_hour <- res |>
          sf::st_drop_geometry() |>
          dplyr::filter(way_osm_id == "12345") |>
          dplyr::group_by(hour) |>
          dplyr::summarise(total_freq = sum(frequency))

        expect_identical(
          way_freq_by_hour$total_freq, expected_freq_by_hour$total_freq
        )
        expect_identical(way_freq_by_hour$hour, expected_freq_by_hour$hour)
      }
    )
  }
)
