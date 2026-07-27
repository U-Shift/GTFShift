library(testthat)

test_that("get_network_extension calculates network route extension in meters", {
  sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
  gtfs <- GTFShift::load_feed(sample_file)

  ref_date <- gtfs$calendar$start_date[1]

  ext <- GTFShift::get_network_extension(gtfs, date = ref_date, metric_crs = 3857)
  expect_true(is.numeric(ext) || inherits(ext, "units"))
  expect_gt(as.numeric(ext), 0)
})

test_that("get_network_extension throws error for invalid route_identifier", {
  sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
  gtfs <- GTFShift::load_feed(sample_file)
  ref_date <- gtfs$calendar$start_date[1]

  expect_error(
    GTFShift::get_network_extension(gtfs, route_identifier = "invalid_id", date = ref_date, metric_crs = 3857),
    "route_identifier should be one of: route_id, route_short_name or route_long_name"
  )
})

test_that("get_network_extension throws error for invalid metric_crs", {
  sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
  gtfs <- GTFShift::load_feed(sample_file)
  ref_date <- gtfs$calendar$start_date[1]

  expect_error(
    GTFShift::get_network_extension(gtfs, date = ref_date, metric_crs = NA),
    "metric_crs should be a valid CRS value"
  )
})

test_that("get_network_extension issues warning when metric_crs is missing", {
  sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
  gtfs <- GTFShift::load_feed(sample_file)
  ref_date <- gtfs$calendar$start_date[1]

  expect_warning(
    GTFShift::get_network_extension(gtfs, date = ref_date),
    "Using default metric_crs"
  )
})

test_that("get_network_extension works with parameter variations", {
  sample_file <- system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift")
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

