library(testthat)

test_that("create_shapes_from_stops constructs shape geometries from stop sequences", {
    sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    expect_warning(
        res_gtfs <- GTFShift::create_shapes_from_stops(gtfs),
        "The GTFS feed already has shapes defined"
    )

    expect_contains(names(res_gtfs), "shapes")
    expect_contains(names(res_gtfs$shapes), c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"))

    # Select a target shape_id and validate that its shape point count equals the stop count of a trip sharing that shape
    target_shape_id <- res_gtfs$shapes$shape_id[1]
    matching_trip_id <- res_gtfs$trips$trip_id[res_gtfs$trips$shape_id == target_shape_id][1]

    shape_points_count <- sum(res_gtfs$shapes$shape_id == target_shape_id)
    trip_stops_count <- sum(gtfs$stop_times$trip_id == matching_trip_id)

    expect_equal(shape_points_count, trip_stops_count)
})
