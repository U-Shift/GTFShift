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
#' @returns A data.frame for calendar.txt.
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::load_feed("gtfs.zip")
#' gtfs$calendar <- GTFShift::create_calendar(gtfs)
#' }
#'
#' @import dplyr
#'
#' @export
create_calendar <- function(gtfs) {

  dates = gtfs$calendar_dates %>%
    filter(exception_type==1)  %>% # Get dates for service inclusion (not removal, which corresponds to exception_type 2)
    mutate(weekday = tolower(weekdays(date))) # Get week day from date

  # Aggregate values in calendar.txt structure
  calendar = dates %>%
    group_by(service_id) %>%
    summarise(
      monday = as.integer(any(weekday == "monday")),
      tuesday = as.integer(any(weekday == "tuesday")),
      wednesday = as.integer(any(weekday == "wednesday")),
      thursday = as.integer(any(weekday == "thursday")),
      friday = as.integer(any(weekday == "friday")),
      saturday = as.integer(any(weekday == "saturday")),
      sunday = as.integer(any(weekday == "sunday")),
      start_date = min(date),
      end_date = max(date)
    )

  return(calendar)
}
