library(testthat)

#' @srrstats {EA6.0} Tests return object classes, dimensions, column names, and column types of `create_shapes_from_stops()`.
#' @srrstats {EA6.0a} Tests `tidygtfs` and `data.frame` classes of returned objects.
#' @srrstats {EA6.0b} Tests `nrow()` and `ncol()` dimensions of generated `shapes` and `trips` tables.
#' @srrstats {EA6.0c} Tests expected column names in `res_gtfs$shapes` and `res_gtfs$trips`.
#' @srrstats {EA6.0d} Tests character/numeric column types in `res_gtfs$shapes`.
test_that("create_shapes_from_stops constructs shape geometries from stop sequences", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    expect_warning(
        res_gtfs <- GTFShift::create_shapes_from_stops(gtfs),
        "The GTFS feed already has shapes defined"
    )

    expect_s3_class(res_gtfs, "tidygtfs")
    expect_s3_class(res_gtfs$shapes, "data.frame")
    expect_equal(ncol(res_gtfs$shapes), 4)
    expect_contains(names(res_gtfs), "shapes")
    expect_contains(names(res_gtfs$shapes), c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"))
    expect_type(res_gtfs$shapes$shape_id, "character")
    expect_type(res_gtfs$shapes$shape_pt_lat, "double")
    expect_type(res_gtfs$shapes$shape_pt_lon, "double")

    # Select a target shape_id and validate that its shape point count equals the stop count of a trip sharing that shape
    target_shape_id <- res_gtfs$shapes$shape_id[1]
    matching_trip_id <- res_gtfs$trips$trip_id[res_gtfs$trips$shape_id == target_shape_id][1]

    shape_points_count <- sum(res_gtfs$shapes$shape_id == target_shape_id)
    trip_stops_count <- sum(gtfs$stop_times$trip_id == matching_trip_id)

    expect_equal(shape_points_count, trip_stops_count)

    # Make sure trips table has shape_id, but keeps other columns
    expect_equal(sort(unique(res_gtfs$trips$shape_id)), sort(unique(res_gtfs$shapes$shape_id)))
    expect_equivalent(sort(colnames(res_gtfs$trips)), sort(unique(colnames(gtfs$trips), "shape_id")))
    expect_equal(nrow(gtfs$trips), nrow(res_gtfs$trips))
})
