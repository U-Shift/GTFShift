test_that("create_shapes_from_sf generates shapes from sf object", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]
    sf_line <- sf::st_sf(
        shape_id = target_shape_id,
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    shapes_df <- GTFShift::create_shapes_from_sf(sf_line, gtfs, metric_crs = 3857)
    expect_contains(names(shapes_df), c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"))
    expect_gt(nrow(shapes_df), 1)
    expect_identical(shapes_df$shape_id[1], target_shape_id)
})

test_that("create_shapes_from_sf calculates shape_dist_traveled when shape_dist_traveled = TRUE", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]
    unmatched_shape_id <- "non_existent_shape_id"

    sf_lines <- sf::st_sf(
        shape_id = c(target_shape_id, unmatched_shape_id),
        geometry = sf::st_sfc(
            sf::st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)),
            sf::st_linestring(matrix(c(-8.65, -8.66, 41.15, 41.16), ncol = 2)),
            crs = 4326
        )
    )

    expected_dist <- as.numeric(sf::st_length(sf::st_transform(sf_lines[1, ], 3857)))

    shapes_df <- GTFShift::create_shapes_from_sf(sf_lines, gtfs, metric_crs = 3857, shape_dist_traveled = TRUE)
    expect_contains(names(shapes_df), c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence", "shape_dist_traveled"))
    expect_false(anyNA(shapes_df$shape_dist_traveled))
    expect_identical(shapes_df$shape_dist_traveled[1], 0)
    expect_equal(shapes_df$shape_dist_traveled[2], expected_dist, tolerance = 1e-3)

    # Validate that unmatched_shape_id was ignored and not present in shapes_df
    expect_false(unmatched_shape_id %in% shapes_df$shape_id)
})

test_that("create_shapes_from_sf works when metric_crs is default (NULL)", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]
    sf_line <- sf::st_sf(
        shape_id = target_shape_id,
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    expect_no_warning(
        shapes_df <- GTFShift::create_shapes_from_sf(sf_line, gtfs, shape_dist_traveled = TRUE)
    )
    expect_contains(names(shapes_df), c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence", "shape_dist_traveled"))
    expect_identical(shapes_df$shape_dist_traveled[1], 0)
    expect_equal(shapes_df$shape_dist_traveled[2], as.numeric(sf::st_length(sf_line[1, ])), tolerance = 1e-3)
})

test_that("create_shapes_from_sf stops when metric_crs is invalid NA", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    target_shape_id <- gtfs$trips$shape_id[1]
    sf_line <- sf::st_sf(
        shape_id = target_shape_id,
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    expect_error(
        GTFShift::create_shapes_from_sf(sf_line, gtfs, metric_crs = NA),
        "metric_crs should be a valid CRS value"
    )
})

test_that("create_shapes_from_sf stops when shape_id column is missing", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    sf_line_no_id <- sf::st_sf(
        invalid_id = "123",
        geometry = sf::st_sfc(sf::st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2)), crs = 4326)
    )

    expect_error(
        GTFShift::create_shapes_from_sf(sf_line_no_id, gtfs, metric_crs = 3857),
        "The sf_shapes object must contain a \"shape_id\" column."
    )
})
