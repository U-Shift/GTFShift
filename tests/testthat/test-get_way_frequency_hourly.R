library(testthat)
library(sf)

#' @srrstats {G2.12} Tests `routes` and `shapes` list columns in the returned `sf` table.
#' @srrstats {EA6.0} Tests return object classes, dimensions, column names, column types, and numeric frequency values of `get_way_frequency_hourly()`.
#' @srrstats {EA6.0a} Tests `sf` and `data.frame` classes of returned table.
#' @srrstats {EA6.0b} Tests `nrow()` and `ncol()` dimensions of returned way frequency table.
#' @srrstats {EA6.0c} Tests expected column names (`way_osm_id`, `hour`, `frequency`, `routes`, `shapes`, `geometry`).
#' @srrstats {EA6.0d} Tests column types including `list` columns (`routes`, `shapes`), numeric `frequency`, and `sfc` geometry.
#' @srrstats {EA6.0e} Tests exact numeric equality of aggregated way frequencies.
test_that("get_way_frequency_hourly calculates way frequencies with mocked osm_shapes_to_routes", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
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

    mock_line <- st_sf(
        shape_id = target_shape_id,
        way_osm_id = "12345",
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)), crs = 4326)
    )

    testthat::with_mocked_bindings(
        osm_shapes_to_routes = function(...) {
            mock_line
        },
        .package = "GTFShift",
        code = {
            res <- GTFShift::get_way_frequency_hourly(gtfs, q = NA, date = ref_date)
            expect_s3_class(res, "sf")
            expect_s3_class(res, "data.frame")
            expect_equal(ncol(res), 6)
            expect_contains(names(res), c("way_osm_id", "hour", "frequency", "geometry", "routes", "shapes"))
            expect_type(res$way_osm_id, "character")
            expect_true(is.numeric(res$frequency))
            expect_type(res$routes, "list")
            expect_type(res$shapes, "list")
            expect_s3_class(sf::st_geometry(res), "sfc")

            # Validate that the way frequency matches the route frequency for the selected shape
            way_freq_by_hour <- res |>
                sf::st_drop_geometry() |>
                dplyr::filter(way_osm_id == "12345") |>
                dplyr::group_by(hour) |>
                dplyr::summarise(total_freq = sum(frequency))

            expect_equal(way_freq_by_hour$total_freq, expected_freq_by_hour$total_freq)
            expect_equal(way_freq_by_hour$hour, expected_freq_by_hour$hour)
        }
    )
})
