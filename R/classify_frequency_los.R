#' Classify bus frequency level of service based on HCM
#'
#' @param frequencies data.frame. Data frame with frequency information.
#' @param frequency_col String (Default "frequency"). Name of the column with frequency values.
#'
#' @details
#' Classifies bus frequency level of service (LOS) based on the Highway Capacity Manual (HCM) 2000 guidelines
#' on "Service Frequency LOS for Urban Scheduled Transit Service" (Exhibit 27-1).
#'
#' @returns data.frame. Input data frame with an additional column \code{frequency_los} indicating the LOS classification.
#'
#' @examples
#' \dontrun{
#' gtfs = GTFShift::load_feed("gtfs.zip")
#' frequency_analysis = GTFShift::get_route_frequency_hourly(gtfs)
#' frequency_los = GTFShift::classify_frequency_los(frequency_analysis)
#' }
#'
#' @import dplyr
#'
#' @export
classify_frequency_los <- function(frequencies, frequency_col = "frequency") {

  los = frequencies |>
    mutate(
      frequency_los = case_when(
        .data[[frequency_col]] == 0 ~ "F",
        .data[[frequency_col]] == 1 ~ "E",
        .data[[frequency_col]] == 2 ~ "D",
        .data[[frequency_col]] >= 3 & .data[[frequency_col]] <= 4 ~ "C",
        .data[[frequency_col]] >= 5 & .data[[frequency_col]] <= 6 ~ "B",
        .data[[frequency_col]] >= 7 ~ "A",
        TRUE ~ NA_character_
      )
    )

  return(los)
}
