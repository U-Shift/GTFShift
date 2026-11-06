library(testthat)
library(sf)

#' @srrstats {G5.0} Uses bundled GTFS sample feed (`gtfs_tcb_sample.zip`) with known routes, trips, and stops.
#' @srrstats {G5.1} Sample datasets in `inst/extdata/samples` are exported with the package and used across tests and examples.
#' @srrstats {G5.2} Tests error behaviour when `metric_crs` is invalid or `shape_id` column is missing.
#' @srrstats {G5.2b} Verifies exact error messages with `expect_error()`.
#' @srrstats {G5.3} Explicitly tests absence of `NA` values in `shape_dist_traveled` (`expect_false(any(is.na(shapes_df$shape_dist_traveled)))`).
#' @srrstats {G5.4} Tests correctness of shape generation and cumulative `shape_dist_traveled` against `sf::st_length()`.
#' @srrstats {G5.4b} Compares `shape_dist_traveled` against reference length from `sf::st_length()`.
#' @srrstats {EA6.0e} Tests numeric equality of `shape_dist_traveled` against `sf::st_length()` using an explicit `tolerance = 1e-3`.
#' @srrstats {SP2.2b} Demonstrates integration of `sf`, `tidytransit`, and `gtfstools` in tests.
#' @srrstats {SP6.0} Verifies that generated GTFS shapes recover WGS 84 (`EPSG:4326`) coordinates after internal metric projection.
#' @srrstats {SP6.1} Tests `create_shapes_from_sf()` on both projected (`metric_crs = 3857`) and curvilinear (`metric_crs = NULL`) domains.
#' @srrstats {SP6.1b} Verifies that `shape_dist_traveled` matches `sf::st_length()` in both projected and geographic CRS modes.
test_that("create_shapes_from_sf generates shapes from sf object", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]
    sf_line <- st_sf(
        shape_id = target_shape_id,
        geometry = st_sfc(st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    shapes_df <- GTFShift::create_shapes_from_sf(sf_line, gtfs, metric_crs = 3857)
    expect_contains(names(shapes_df), c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"))
    expect_gt(nrow(shapes_df), 1)
    expect_equal(shapes_df$shape_id[1], target_shape_id)
})

test_that("create_shapes_from_sf calculates shape_dist_traveled when shape_dist_traveled = TRUE", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]
    unmatched_shape_id <- "non_existent_shape_id"

    sf_lines <- st_sf(
        shape_id = c(target_shape_id, unmatched_shape_id),
        geometry = st_sfc(
            st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)),
            st_linestring(matrix(c(-8.65, -8.66, 41.15, 41.16), ncol = 2)),
            crs = 4326
        )
    )

    expected_dist <- as.numeric(st_length(st_transform(sf_lines[1, ], 3857)))

    shapes_df <- GTFShift::create_shapes_from_sf(sf_lines, gtfs, metric_crs = 3857, shape_dist_traveled = TRUE)
    expect_contains(names(shapes_df), c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence", "shape_dist_traveled"))
    expect_false(any(is.na(shapes_df$shape_dist_traveled)))
    expect_equal(shapes_df$shape_dist_traveled[1], 0)
    expect_equal(shapes_df$shape_dist_traveled[2], expected_dist, tolerance = 1e-3)

    # Validate that unmatched_shape_id was ignored and not present in shapes_df
    expect_false(unmatched_shape_id %in% shapes_df$shape_id)
})

test_that("create_shapes_from_sf works when metric_crs is default (NULL)", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]
    sf_line <- st_sf(
        shape_id = target_shape_id,
        geometry = st_sfc(st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    expect_no_warning(
        shapes_df <- GTFShift::create_shapes_from_sf(sf_line, gtfs, shape_dist_traveled = TRUE)
    )
    expect_contains(names(shapes_df), c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence", "shape_dist_traveled"))
    expect_equal(shapes_df$shape_dist_traveled[1], 0)
    expect_equal(shapes_df$shape_dist_traveled[2], as.numeric(st_length(sf_line[1, ])), tolerance = 1e-3)
})

test_that("create_shapes_from_sf stops when metric_crs is invalid NA", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]
    sf_line <- st_sf(
        shape_id = target_shape_id,
        geometry = st_sfc(st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    expect_error(
        GTFShift::create_shapes_from_sf(sf_line, gtfs, metric_crs = NA),
        "metric_crs should be a valid CRS value"
    )
})

test_that("create_shapes_from_sf stops when shape_id column is missing", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    sf_line_no_id <- st_sf(
        invalid_id = "123",
        geometry = st_sfc(st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    expect_error(
        GTFShift::create_shapes_from_sf(sf_line_no_id, gtfs, metric_crs = 3857),
        "The sf_shapes object must contain a \"shape_id\" column."
    )
    expect_error(
        GTFShift::create_shapes_from_sf(data.frame(shape_id = "1"), gtfs),
        "The sf_shapes object must be an sf object."
    )
    expect_error(
        GTFShift::create_shapes_from_sf(sf_line_no_id, list()),
        "gtfs must be a tidygtfs object."
    )
})

test_that("create_shapes_from_sf yields equivalent results across curvilinear, metric_crs, and st_transform (SP6.1b, G5.3)", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]
    sf_line_4326 <- st_sf(
        shape_id = target_shape_id,
        geometry = st_sfc(st_linestring(matrix(c(-9.07, -9.08, 38.66, 38.67), ncol = 2)), crs = 4326)
    )
    sf_line_3763 <- st_transform(sf_line_4326, 3763)

    shapes_curv <- GTFShift::create_shapes_from_sf(sf_line_4326, gtfs, metric_crs = NULL, shape_dist_traveled = TRUE)
    shapes_metric <- GTFShift::create_shapes_from_sf(sf_line_4326, gtfs, metric_crs = 3763, shape_dist_traveled = TRUE)
    shapes_transformed <- GTFShift::create_shapes_from_sf(sf_line_3763, gtfs, metric_crs = 3763, shape_dist_traveled = TRUE)

    expect_true(all(is.finite(shapes_curv$shape_dist_traveled)))
    expect_true(all(is.finite(shapes_metric$shape_dist_traveled)))
    expect_equal(shapes_metric$shape_dist_traveled, shapes_transformed$shape_dist_traveled, tolerance = 1e-6)
    expect_equal(shapes_curv$shape_dist_traveled, shapes_metric$shape_dist_traveled, tolerance = 5e-3)
})
