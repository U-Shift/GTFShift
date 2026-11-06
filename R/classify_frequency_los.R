#' Classify bus frequency level of service based on HCM
#'
#' @param frequencies data.frame. Data frame with frequency information.
#' @param frequency_col String (Default \code{"frequency"}). Case-sensitive name of the column with frequency values.
#'
#' @details
#' Classifies bus frequency level of service (LOS) based on the Highway Capacity Manual (HCM) 2000 guidelines
#' on "Service Frequency LOS for Urban Scheduled Transit Service" (Exhibit 27-1).
#' 
#' Refer to \code{vignette("classify")} for more details on this classification.
#'
#' @returns data.frame. Input data frame with an additional column \code{frequency_los} indicating the LOS classification.
#'
#' @examples
#' # Subset GTFS for one route only, for demo purposes
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_tcb_sample.zip", package = "GTFShift")
#' )
#' gtfs <- GTFShift::filter_by_route_name(gtfs, c("1", "2", "3", "4"))
#' 
#' # Get route frequency 
#' frequency_analysis <- GTFShift::get_route_frequency_hourly(
#'   gtfs, 
#'   date = gtfs$calendar$start_date[1]
#' ) 
#' 
#' # Compute LOS
#' frequency_los = GTFShift::classify_frequency_los(frequency_analysis)
#' 
#' frequency_los |> 
#'   sf::st_drop_geometry() |>
#'   dplyr::select(route_id, frequency_los)
#'
#' @import dplyr
#' @srrstats {G1.0} Cites the Highway Capacity Manual (HCM 2000, Exhibit 27-1) reference for transit frequency Level of Service thresholds.
#' @srrstats {G1.1} Documents that this function is an R implementation of the transit frequency Level of Service classification from the Highway Capacity Manual (HCM 2000).
#' @srrstats {G1.3} Defines Level of Service (LOS A through F) thresholds unambiguously.
#' @srrstats {G2.0} Validates that `frequency_col` has length 1.
#' @srrstats {G2.1} Validates that `frequencies` is a `data.frame` and `frequency_col` is character.
#' @srrstats {G2.2} Prohibits multivariate `frequency_col` inputs.
#' @srrstats {G2.3} Validates `frequency_col` against column names of `frequencies`.
#' @srrstats {G2.3b} Explicitly documents case sensitivity for `frequency_col`.
#' @srrstats {G2.11} Supports `units`-classed frequency columns via `as.numeric()`.
#' @srrstats {G2.12} Preserves any `list` columns in `frequencies`.
#' @srrstats {EA2.3} Allows user-specified `frequency_col` rather than hardcoding a single column name.
#' @srrstats {EA4.0} Returns a `data.frame` (or `sf` data frame) preserving the input class and columns.
#' @srrstats {SP4.1} Preserves `units` on `frequencies[[frequency_col]]` in the returned table.
#'
#' @export
classify_frequency_los <- function(frequencies, frequency_col = "frequency") {
  if (!is.data.frame(frequencies)) {
    stop("frequencies must be a data.frame or sf object")
  }
  if (!is.character(frequency_col) || length(frequency_col) != 1 || !(frequency_col %in% colnames(frequencies))) {
    stop("frequency_col must be a single column name present in frequencies")
  }

  freq_vals <- as.numeric(frequencies[[frequency_col]])
  los = frequencies |>
    mutate(
      frequency_los = case_when(
        freq_vals == 0 ~ "F",
        freq_vals == 1 ~ "E",
        freq_vals == 2 ~ "D",
        freq_vals >= 3 & freq_vals <= 4 ~ "C",
        freq_vals >= 5 & freq_vals <= 6 ~ "B",
        freq_vals >= 7 ~ "A",
        TRUE ~ NA_character_
      )
    )

  return(los)
}
