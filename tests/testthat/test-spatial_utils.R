library(testthat)
library(sf)

#' @srrstats {G5.2} Explicitly tests all error conditions in `validate_metric_crs()` and `preprocess_spatial_crs()`.
#' @srrstats {G5.2a} Verifies unique error messages for each validation branch in `R/spatial_utils.R`.
#' @srrstats {G5.2b} Verifies error messages via `expect_error()`.
#' @srrstats {SP2.4a} Tests rejection of PROJ4-strings in both `metric_crs` and spatial object CRS metadata.
#' @srrstats {SP2.8} Tests single spatial pre-processing routine `preprocess_spatial_crs()`.
#' @srrstats {SP2.9} Verifies that `preprocess_spatial_crs()` preserves all non-spatial attributes and classes during CRS transformation.
test_that("validate_metric_crs handles NULL, valid CRS inputs, and invalid inputs", {
  # NULL returns NULL
  expect_null(validate_metric_crs(NULL))

  # Valid integer, authority:code string, and crs object
  crs_int <- validate_metric_crs(3857)
  expect_s3_class(crs_int, "crs")
  expect_equal(crs_int, st_crs(3857))

  crs_str <- validate_metric_crs("EPSG:3763")
  expect_s3_class(crs_str, "crs")
  expect_equal(crs_str, st_crs(3763))

  crs_obj <- validate_metric_crs(st_crs(4326))
  expect_s3_class(crs_obj, "crs")
  expect_equal(crs_obj, st_crs(4326))

  # Multivariate input (length != 1)
  expect_error(
    validate_metric_crs(c(3857, 4326)),
    "metric_crs should be a valid single CRS value"
  )

  # PROJ4-string input
  expect_error(
    validate_metric_crs("+proj=longlat +datum=WGS84 +no_defs"),
    "PROJ4-strings are not permitted for metric_crs"
  )

  # NA or unparseable CRS input
  expect_error(
    validate_metric_crs(NA),
    "metric_crs should be a valid CRS value"
  )
  expect_error(
    validate_metric_crs("NOT_A_VALID_CRS"),
    "metric_crs should be a valid CRS value"
  )
})

test_that("preprocess_spatial_crs validates classes, rejects PROJ4 CRS, and transforms CRS while preserving attributes", {
  pt_sfc_4326 <- st_sfc(st_point(c(-9.1, 38.7)), crs = 4326)
  pt_sf_4326 <- st_sf(id = "p1", extra = 42, geometry = pt_sfc_4326)

  # 1. Non-spatial input throws informative error with arg_name
  expect_error(
    preprocess_spatial_crs(data.frame(x = 1), arg_name = "custom_arg"),
    "custom_arg must be an sf object or sfc geometry"
  )

  # 2. Input spatial object whose CRS input is a PROJ4-string is rejected
  pt_proj4 <- pt_sfc_4326
  crs_proj4 <- st_crs(4326)
  crs_proj4$input <- "+proj=longlat +datum=WGS84 +no_defs"
  attr(pt_proj4, "crs") <- crs_proj4
  expect_error(
    preprocess_spatial_crs(pt_proj4, arg_name = "geom_input"),
    "PROJ4-strings are not permitted for geom_input CRS"
  )

  # 3. Transformation to metric_crs preserves sf class and attributes
  pt_sf_3857 <- preprocess_spatial_crs(pt_sf_4326, metric_crs = 3857, arg_name = "pt_sf")
  expect_s3_class(pt_sf_3857, "sf")
  expect_equal(st_crs(pt_sf_3857), st_crs(3857))
  expect_equal(pt_sf_3857$id, "p1")
  expect_equal(pt_sf_3857$extra, 42)

  # 4. Already in metric_crs: returns unchanged without re-transforming
  pt_sf_same <- preprocess_spatial_crs(pt_sf_3857, metric_crs = 3857, arg_name = "pt_sf")
  expect_equal(st_crs(pt_sf_same), st_crs(3857))

  # 5. Missing CRS (NA_crs_) when metric_crs is provided:
  pt_na_crs <- st_sfc(st_point(c(0, 0)), crs = NA_crs_)
  # a) require_valid_crs = FALSE returns object as-is
  res_na_allowed <- preprocess_spatial_crs(
    pt_na_crs,
    metric_crs = 3857,
    arg_name = "pt_na",
    require_valid_crs = FALSE
  )
  expect_true(is.na(st_crs(res_na_allowed)))
  # b) require_valid_crs = TRUE throws informative error
  expect_error(
    preprocess_spatial_crs(
      pt_na_crs,
      metric_crs = 3857,
      arg_name = "pt_na",
      require_valid_crs = TRUE
    ),
    "pt_na must have a valid CRS to use metric_crs"
  )

  # 6. When metric_crs is NULL and reference_crs differs from x_crs, transforms x to reference_crs
  res_ref <- preprocess_spatial_crs(
    pt_sf_3857,
    metric_crs = NULL,
    reference_crs = st_crs(4326),
    arg_name = "pt_sf"
  )
  expect_equal(st_crs(res_ref), st_crs(4326))
  expect_equal(res_ref$id, "p1")
})
