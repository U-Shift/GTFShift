library(testthat)

test_that("get_route_frequency_hourly calculates route frequencies", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    ref_date <- gtfs$calendar$start_date[1]

    res <- GTFShift::get_route_frequency_hourly(gtfs, date = ref_date)
    expect_s3_class(res, "sf")
    expect_contains(names(res), c("frequency", "hour", "geometry", "route_id", "route_short_name", "shape_id"))
})

test_that("get_route_frequency_hourly supports overline = TRUE", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    ref_date <- gtfs$calendar$start_date[1]

    res <- GTFShift::get_route_frequency_hourly(gtfs, date = ref_date, overline = TRUE)
    expect_s3_class(res, "sf")
    expect_contains(names(res), c("frequency", "hour", "geometry"))

    res_no_overline <- GTFShift::get_route_frequency_hourly(gtfs, date = ref_date, overline = FALSE) 
    expect_gt(nrow(res), nrow(res_no_overline))
})

test_that("get_route_frequency_hourly supports use_osm_routes != NA", {
    sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
    gtfs <- GTFShift::load_feed(sample_file)

    ref_date <- gtfs$calendar$start_date[1]

    mock_shapes <- sf::st_sf(
        shape_id = unique(gtfs$trips$shape_id[!is.na(gtfs$trips$shape_id)]),
        geometry = sf::st_sfc(
            lapply(
                seq_along(unique(gtfs$trips$shape_id[!is.na(gtfs$trips$shape_id)])),
                function(i) sf::st_linestring(matrix(c(-8.6, -8.61, 41.1, 41.11), ncol = 2))
            ),
            crs = 4326
        )
    )

    testthat::with_mocked_bindings(
        osm_shapes_to_routes = function(...) mock_shapes,
        .package = "GTFShift",
        code = {
            res <- GTFShift::get_route_frequency_hourly(gtfs, date = ref_date, use_osm_routes = "mock_opq")
            expect_s3_class(res, "sf")
            expect_contains(names(res), c("frequency", "hour"))
        }
    )
})

