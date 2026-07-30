#' Create calendar.txt from calendar_dates.txt
#'
#' @param gtfs tidygtfs. GTFS feed.
#'
#' @details
#' When \code{calendar_dates.txt} declares all service dates, \code{calendar.txt} becomes optional in the
#' \href{https://gtfs.org/documentation/schedule/reference/#dataset-files}{GTFS feed specification}.
#' However, to perform some operations, this table might be necessary.
#'
#' This method allows to create a \code{calendar.txt} table, based on the \code{calendar_dates.txt}.
#' It performs an approximation, considering, for each \code{service_id}, the
#' minimum and maximum dates and setting each week day to true if it has any date that matches that date. The results
#' might not be 100% accurate, as it captures the whole time span and exceptions in the week days along it are ignored.
#'
#' @returns data.frame. Table for calendar.txt.
#'
#' @examples
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_ttsl_sample_no_shapes.zip", package = "GTFShift")
#' )
#' 
#' head(gtfs$calendar_dates |> dplyr::filter(exception_type == 1))
#' 
#' gtfs_calendar <- GTFShift::create_calendar(gtfs)
#' 
#' gtfs_calendar
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
create_calendar <- function(gtfs) {

  dates = gtfs$calendar_dates |>
    filter(.data$exception_type==1)  |> # Get dates for service inclusion (not removal, which corresponds to exception_type 2)
    mutate(weekday = tolower(weekdays(.data$date))) # Get week day from date

  # Aggregate values in calendar.txt structure
  calendar = dates |>
    group_by(.data$service_id) |>
    summarise(
      monday = as.integer(any(.data$weekday == "monday")),
      tuesday = as.integer(any(.data$weekday == "tuesday")),
      wednesday = as.integer(any(.data$weekday == "wednesday")),
      thursday = as.integer(any(.data$weekday == "thursday")),
      friday = as.integer(any(.data$weekday == "friday")),
      saturday = as.integer(any(.data$weekday == "saturday")),
      sunday = as.integer(any(.data$weekday == "sunday")),
      start_date = min(.data$date),
      end_date = max(.data$date)
    )

  return(calendar)
}
