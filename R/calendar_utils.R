#' Get next business Wednesday
#'
#' Find the next Wednesday that is not a holiday.
#' @param start_date String. Reference date.
#' @import lubridate
#' @keywords internal
#' @noRd
calendar_nextBusinessWednesday <- function(start_date = Sys.Date()) {
  year <- lubridate::year(start_date)
  holidays <- calendar_get_pt_holidays(year)

  # Find the next Wednesday
  next_wed <- start_date + (4 - lubridate::wday(start_date) + 7) %% 7

  # If next Wednesday is a holiday, keep searching
  while (next_wed %in% holidays) {
    next_wed <- next_wed + 7  # Move to the next Wednesday

    # If we cross into a new year, update holidays
    if (year(next_wed) != year) {
      year <- lubridate::year(next_wed)
      holidays <- calendar_get_pt_holidays(year)
    }
  }

  return(next_wed)
}

#' Get Portugal holidays
#'
#' Get public holidays for Portugal for a given year.
#' @param year Integer. Year to get holidays for.
#' @import httr
#' @import jsonlite
#' @noRd
calendar_get_pt_holidays <- function(year) {
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", year, "/PT")
  response <- httr::GET(url)

  if (status_code(response) == 200) {
    holidays <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"))
    return(as.Date(holidays$date))
  } else {
    stop("Failed to retrieve holidays. Please check your internet connection or API availability.")
  }
}

time_convert_seconds_to_hms <- function(seconds) {
  h <- seconds %/% 3600
  m <- (seconds %% 3600) %/% 60
  s <- seconds %% 60
  sprintf("%02d:%02d:%02d", h, m, s)
}
