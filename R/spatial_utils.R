#' Validate and parse `metric_crs` parameter
#'
#' Ensures `metric_crs` is either `NULL` or a valid single coordinate reference
#' system compliant with `PROJ >= 6` and `WKT2`, explicitly rejecting legacy
#' PROJ4-strings.
#'
#' @param metric_crs Integer, character, or `crs` object (Default `NULL`).
#'
#' @returns `NULL` or an `sf` `crs` object.
#'
#' @srrstats {G1.4a} Internal non-exported helper function documented with `roxygen2` and `@noRd`.
#' @srrstats {G2.0} Asserts that `metric_crs` has length 1 when non-NULL (`! inherits(metric_crs, "crs") && length(metric_crs) != 1`).
#' @srrstats {G2.2} Prohibits multivariate inputs for `metric_crs`.
#' @srrstats {SP2.4} Validates coordinate reference systems using `sf::st_crs()` (`PROJ >= 6`).
#' @srrstats {SP2.4a} Explicitly rejects legacy PROJ4-strings (`grepl("^\\+", ...)`), requiring `WKT2` or `authority:code` representations.
#' @srrstats {SP2.7} Confirms that `metric_crs` resolves to a valid non-`NA` `crs` object.
#' @noRd
validate_metric_crs <- function(metric_crs = NULL) {
  if (is.null(metric_crs)) {
    return(NULL)
  }
  if (!inherits(metric_crs, "crs") && length(metric_crs) != 1) {
    stop("metric_crs should be a valid single CRS value (e.g., 3857 or 'EPSG:3857')")
  }
  if (is.character(metric_crs) && !is.na(metric_crs) && grepl("^\\+", trimws(metric_crs))) {
    stop("PROJ4-strings are not permitted for metric_crs; please use WKT2 or an authority:code CRS value (e.g., 3857 or 'EPSG:3857')")
  }
  parsed_crs <- tryCatch(
    suppressWarnings(sf::st_crs(metric_crs)),
    error = function(e) sf::NA_crs_
  )
  if (is.na(parsed_crs)) {
    stop("metric_crs should be a valid CRS value (e.g., 3857 or 'EPSG:3857')")
  }
  parsed_crs
}

#' Pre-process and harmonize spatial input data and CRS
#'
#' Single pre-processing routine to validate spatial input objects (`sf` or
#' `sfc`), verify `WKT2` CRS compliance, and transform coordinates to a uniform
#' CRS while preserving all non-spatial attributes and metadata.
#'
#' @param x An `sf` or `sfc` spatial object.
#' @param metric_crs Optional target projected CRS validated via `validate_metric_crs()`.
#' @param reference_crs Optional fallback `crs` object to align `x` with when `metric_crs` is `NULL`.
#' @param arg_name Character string naming the argument for informative error messages.
#' @param require_valid_crs Logical. If `TRUE` and `metric_crs` is non-NULL, errors when `sf::st_crs(x)` is `NA`.
#'
#' @returns The validated and (optionally) CRS-transformed `sf` or `sfc` object, preserving its input class and attributes.
#'
#' @srrstats {G1.4a} Internal non-exported helper function documented with `roxygen2` and `@noRd`.
#' @srrstats {G2.1} Validates that spatial input `x` inherits from `sf` or `sfc`.
#' @srrstats {G2.8} Pre-processes spatial inputs into a uniform CRS before downstream geometric operations.
#' @srrstats {SP2.0} Accepts only dedicated spatial classes (`sf` and `sfc`).
#' @srrstats {SP2.0b} Errors appropriately on non-spatial inputs or missing CRS when `metric_crs` is requested.
#' @srrstats {SP2.4} Uses `sf::st_crs()` and `sf::st_transform()` compliant with `PROJ >= 6`.
#' @srrstats {SP2.4a} Rejects inputs whose CRS is specified only as a PROJ4-string without `WKT2`.
#' @srrstats {SP2.5} Verifies CRS metadata on input spatial objects.
#' @srrstats {SP2.7} Validates acceptable spatial classes (`sf` or `sfc`).
#' @srrstats {SP2.8} Single pre-processing routine validating spatial inputs and transforming to a uniform CRS.
#' @srrstats {SP2.9} Maintains all attributes, columns, and metadata of input `sf`/`sfc` objects during transformation.
#' @noRd
preprocess_spatial_crs <- function(
  x,
  metric_crs = NULL,
  reference_crs = NULL,
  arg_name = "x",
  require_valid_crs = FALSE
) {
  if (!inherits(x, c("sf", "sfc"))) {
    stop(sprintf("%s must be an sf object or sfc geometry", arg_name))
  }
  x_crs <- sf::st_crs(x)
  if (!is.na(x_crs) && !is.null(x_crs$input) && grepl("^\\+", trimws(x_crs$input))) {
    stop(sprintf("PROJ4-strings are not permitted for %s CRS; please use WKT2 or an authority:code CRS representation.", arg_name))
  }
  metric_crs <- validate_metric_crs(metric_crs)
  if (!is.null(metric_crs)) {
    if (is.na(x_crs)) {
      if (require_valid_crs) {
        stop(sprintf("%s must have a valid CRS to use metric_crs", arg_name))
      }
      return(x)
    }
    if (x_crs != metric_crs) {
      x <- sf::st_transform(x, crs = metric_crs)
    }
  } else if (!is.null(reference_crs) && !is.na(reference_crs) && !is.na(x_crs) && x_crs != reference_crs) {
    x <- sf::st_transform(x, crs = reference_crs)
  }
  x
}

