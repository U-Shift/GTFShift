library(testthat)

#' @srrstats {G2.11} Tests `units` and numeric output handling from `get_network_extension()`.
#' @srrstats {G5.0} Uses bundled sample GTFS feed (`gtfs_tcb_sample.zip`).
#' @srrstats {G5.1} Confirms tests run on exported sample datasets in `inst/extdata/samples`.
#' @srrstats {G5.2} Tests error messages on invalid `route_identifier` and invalid `metric_crs`.
#' @srrstats {G5.2b} Verifies error messages via `expect_error()`.
#' @srrstats {SP6.1} Tests `get_network_extension()` on both projected (`metric_crs = 3857`) and geographic (`metric_crs = NULL`) CRS.
#' @srrstats {SP6.1b} Verifies equivalent results across numeric (`3857`) and character (`"EPSG:3857"`) CRS specifications.
test_that("get_network_extension calculates network route extension in meters", {
  sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
  gtfs <- GTFShift::load_feed(sample_file)

  ref_date <- gtfs$calendar$start_date[1]

  ext <- GTFShift::get_network_extension(gtfs, date = ref_date, metric_crs = 3857)
  expect_true(is.numeric(ext) || inherits(ext, "units"))
  expect_gt(as.numeric(ext), 0)
})

test_that("get_network_extension throws error for invalid route_identifier", {
  sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
  gtfs <- GTFShift::load_feed(sample_file)
  ref_date <- gtfs$calendar$start_date[1]

  expect_error(
    GTFShift::get_network_extension(gtfs, route_identifier = "invalid_id", date = ref_date, metric_crs = 3857),
    "route_identifier should be one of: route_id, route_short_name or route_long_name"
  )
})

test_that("get_network_extension throws error for invalid metric_crs", {
  sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
  gtfs <- GTFShift::load_feed(sample_file)
  ref_date <- gtfs$calendar$start_date[1]

  expect_error(
    GTFShift::get_network_extension(gtfs, date = ref_date, metric_crs = NA),
    "metric_crs should be a valid CRS value"
  )
})

test_that("get_network_extension works when metric_crs is default (NULL)", {
  sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
  gtfs <- GTFShift::load_feed(sample_file)
  ref_date <- gtfs$calendar$start_date[1]

  expect_no_warning(
    ext <- GTFShift::get_network_extension(gtfs, date = ref_date)
  )
  expect_true(is.numeric(ext) || inherits(ext, "units"))
  expect_gt(as.numeric(ext), 0)
})

test_that("get_network_extension works with parameter variations", {
  sample_file <- system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift")
  gtfs <- GTFShift::load_feed(sample_file)
  ref_date <- gtfs$calendar$start_date[1]

  # Variation 1: direction_wise = FALSE
  ext_dir_false <- GTFShift::get_network_extension(
    gtfs,
    date = ref_date,
    direction_wise = FALSE,
    metric_crs = 3857
  )
  expect_gt(as.numeric(ext_dir_false), 0)

  # Variation 2: unified = TRUE
  ext_unified <- GTFShift::get_network_extension(
    gtfs,
    date = ref_date,
    unified = TRUE,
    metric_crs = 3857
  )
  expect_gt(as.numeric(ext_unified), 0)

  # Unified extension should be less than or equal to non-unified extension
  ext_non_unified <- GTFShift::get_network_extension(
    gtfs,
    date = ref_date,
    unified = FALSE,
    metric_crs = 3857
  )
  expect_lte(as.numeric(ext_unified), as.numeric(ext_non_unified))

  # Variation 3: alternative valid route_identifier
  ext_short_name <- GTFShift::get_network_extension(
    gtfs,
    route_identifier = "route_short_name",
    date = ref_date,
    metric_crs = 3857
  )
  expect_gt(as.numeric(ext_short_name), 0)

  # Variation 4: alternative character metric_crs
  ext_epsg_str <- GTFShift::get_network_extension(
    gtfs,
    date = ref_date,
    metric_crs = "EPSG:3857"
  )
  expect_equal(as.numeric(ext_epsg_str), as.numeric(ext_non_unified))
})

