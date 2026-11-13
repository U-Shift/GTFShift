#' Get aggregated frequency per hour for each bus stop
#'
#' For each stop, returns the number of departures aggregated per hour.
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param date Date (Default \code{GTFShift::calendar_nextBusinessWednesday()}).
#'   Reference date to consider when analyzing the GTFS file.
#'
#' @details
#' This method analyses the GTFS feed for a representative day, generating for
#' each stop the number of services aggregated per hour.
#' For a detailed example, see the \code{vignette("analyse")}.
#'
#' @returns sf data.frame. Hourly stop frequencies, with the following columns:
#' \describe{
#'   \item{stop_id}{The \code{stop_id} attribute from \code{stops.txt} file.}
#'   \item{hour}{The hour for which the frequency applies (24 hour format).}
#'   \item{frequency}{The number of services provided at the stop for the
#'     corresponding 60 minutes period.}
#'   \item{geometry}{The stop coordinates.}
#' }
#'
#' @examples
#' # Subset GTFS for one route only, for demo purposes
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_tcb_sample.zip", package = "GTFShift")
#' )
#' gtfs <- GTFShift::filter_by_route_name(gtfs, c("1", "2", "3", "4"))
#'
#' # Get frequency
#' frequency_analysis <- GTFShift::get_stop_frequency_hourly(
#'   gtfs,
#'   date = gtfs$calendar$start_date[1]
#' )
#'
#' head(frequency_analysis)
#'
#' @seealso \code{GTFShift::calendar_nextBusinessWednesday()}
#'
#' @import sf
#' @import lubridate
#' @import tidytransit
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
get_stop_frequency_hourly <- function(
  gtfs,
  date = GTFShift::calendar_nextBusinessWednesday()
) {
  message(sprintf("Analysing GTFS for %s...", date))

  ## Consider transit data for one day only
  message(sprintf("> Filtering by reference date %s...", date))
  suppressWarnings({ # Ignore missing transfers warnings
    gtfs_date <- tidytransit::filter_feed_by_date(
      gtfs, extract_date = date
    )
  })

  message(sprintf("> Found %d routes operating %d trips on %d stops...",
    length(gtfs_date$trips$trip_id),
    length(gtfs_date$routes$route_id),
    length(gtfs_date$stops$stop_id)
  ))

  # PROCESS GTFS, generating table calculating the frequencies per bus stop

  ## Service pattern

  ### Building meta data on the service patterns
  ### See tidytransit servicepatterns vignette:
  ### https://cran.r-project.org/web/packages/tidytransit/tidytransit.pdf
  ### Creates $.$servicepatterns with unique id per pattern
  ### Creates $.$dates_servicepatterns matching each individual date covered by
  ### the GTFS with the corresponding id
  pattern_gtfs <- tidytransit::set_servicepattern(gtfs_date)
  n_patterns <- length(pattern_gtfs$.$servicepatterns$servicepattern_id)
  svc_ids_str <- toString(pattern_gtfs$.$servicepatterns$service_id)
  message(sprintf(
    "> Identified %d service patterns matching date: %s",
    n_patterns,
    svc_ids_str
  ))
  ### WARNING: every time we run this, random ids are generated for patterns

  ## Convert stops and shapes to simple features
  pattern_gtfs <- tidytransit::gtfs_as_sf(pattern_gtfs)
  # Compute length for each shape
  pattern_gtfs$shapes$length <- st_length(pattern_gtfs$shapes)

  shape_lengths <- pattern_gtfs$shapes |>
    as.data.frame() |>
    select("shape_id", "length", -"geometry")

  ## Get statistics: for each service pattern, get nr of trips, routes, total
  ## and avg distance and number of stops covered
  service_pattern_summary <- pattern_gtfs$trips |>
    left_join(pattern_gtfs$.$servicepatterns, by = "service_id") |>
    left_join(shape_lengths, by = "shape_id") |>
    left_join(pattern_gtfs$stop_times, by = "trip_id") |>
    group_by(.data$servicepattern_id) |>
    summarise(
      trips = n(),
      routes = n_distinct(.data$route_id),
      # divide by 1e3 to convert meters to kms
      total_distance_per_day_km =
        sum(as.numeric(.data$length), na.rm = TRUE) / 1e3,
      route_avg_distance_km =
        (sum(as.numeric(.data$length), na.rm = TRUE) / 1e3) /
        (.data$trips * .data$routes),
      # divided by two because usually there is one stop per direction
      stops = (n_distinct(.data$stop_id) / 2)
    )

  ## Add the number of days that each service is in operation
  service_pattern_summary <- pattern_gtfs$.$dates_servicepatterns |>
    group_by(.data$servicepattern_id) |>
    summarise(days_in_service = n()) |>
    left_join(service_pattern_summary, by = "servicepattern_id")

  ## Get service patterns that run on the date selected
  service_pattern_ids <- pattern_gtfs$.$dates_servicepatterns |>
    filter(.data$date == date)

  service_ids <- pattern_gtfs$.$servicepattern |>
    filter(
      .data$servicepattern_id %in% service_pattern_ids$servicepattern_id
    ) |>
    pull(.data$service_id)

  #### Filter by date

  # Get stop frequency (missing data)

  frame <- data.frame()

  stop_times <- gtfs_date$stop_times |>
    mutate(hour = lubridate::hour(.data$departure_time))
  min_hour <- min(stop_times$hour, na.rm = TRUE)
  max_hour <- max(stop_times$hour, na.rm = TRUE)

  message(sprintf(
    "> Calculating stop frequencies for hours %d to %d...",
    min_hour,
    max_hour
  ))

  for (i in min_hour:max_hour) {
    stop_frequency <- tidytransit::get_stop_frequency(
      gtfs_date,
      start_time = sprintf("%.2d:00:00", i),
      end_time = sprintf("%.2d:59:59", i),
      service_ids = service_ids,
      by_route = TRUE
    )

    stop_frequency <- stop_frequency |>
      group_by(.data$stop_id) |>
      summarise(frequency = sum(.data$n_departures)) |>
      mutate(hour = i)

    frame <- rbind(frame, stop_frequency)
  }

  frequency <- frame |>
    ungroup() |>
    group_by(.data$stop_id, .data$hour) |>
    summarise(frequency = sum(.data$frequency)) |>
    ungroup()

  stops_coords <- gtfs_date$stops |>
    select("stop_id", "stop_lon", "stop_lat")
  table <- frequency |>
    left_join(stops_coords, by = "stop_id") |>
    st_as_sf(crs = 4326, coords = c("stop_lon", "stop_lat"))

  message("Finished GTFS analysis!")

  return(table)
}
