#' Get aggregated frequency per hour for each bus stop
#'
#' For each stop, returns the number of departures aggregated per hour.
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param date Date. Reference date to consider when analyzing the GTFS file. Defaults to next business Wednesday in Portugal.
#'
#' @details
#' This method analyses the GTFS feed for a representative day, generating for each stop the number of services aggregated per hour.
#' For a detailed example, see the \code{vignette("analyse")}.
#'
#' @returns An `sf` `data.frame` object with the following columns:
#' \itemize{
#'  \item `stop_id`, the `stop_id` attribute from `stops.txt` file.
#'  \item `hour`, the hour for which the frequency applies (24 hour format).
#'  \item `frequency`, the number of services provided at the stop for the corresponding 60 minutes period.
#'  \item `geometry`, the stop coordinates.
#' }
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::load_feed("gtfs.zip")
#' frequency_analysis <- GTFShift::get_stop_frequency_hourly(gtfs)
#' }
#'
#' @import sf
#' @import tidyverse
#' @import lubridate
#' @import tidytransit
#' @import dplyr
#'
#' @export
get_stop_frequency_hourly <- function(gtfs, date=NULL) {
  message(sprintf("Analysing GTFS..."))

  ## Consider transit data for one day only
  if (is.null(date)) {
    date = calendar_nextBusinessWednesday()
    message(sprintf("> Reference date not provided, considering next business wednesday: %s...", date))
  }
  message(sprintf("> Filtering by reference date %s...", date))
  gtfs_date <- tidytransit::filter_feed_by_date(
    gtfs, extract_date = date
  )

  message(sprintf("> Found %d routes operating %d trips on %d stops...",
    length(gtfs_date$trips$trip_id),
    length(gtfs_date$routes$route_id),
    length(gtfs_date$stops$stop_id)
  ))

  # PROCESS GTFS, generating table calculating the frequencies per bus stop

  ## Service pattern

  ### Building meta data on the service patterns
  ### https://cran.r-project.org/web/packages/tidytransit/vignettes/servicepatterns.html
  ### Alternative docs: https://cran.r-project.org/web/packages/tidytransit/tidytransit.pdf, page 20
  ### Creates $.$servicepatterns with unique id per pattern
  ### Creates $.$dates_servicepatterns matching each individual date covered by the GTFS with the corresponding id
  pattern_gtfs <- tidytransit::set_servicepattern(gtfs_date)
  message(sprintf("> Identified %d service patterns matching date: %s", length(pattern_gtfs$.$servicepatterns$servicepattern_id), paste(pattern_gtfs$.$servicepatterns$service_id, collapse=", ")))
  ### WARNING: every time we run this, random ids will be generated for the service patterns

  ## Convert stops and shapes to simple features
  pattern_gtfs <- tidytransit::gtfs_as_sf(pattern_gtfs)
  pattern_gtfs$shapes$length <- st_length(pattern_gtfs$shapes) # Compute length for each shape

  shape_lengths <- pattern_gtfs$shapes |>
    as.data.frame() |>
    select(shape_id, length, -geometry)


  ## Get statistics: for each service pattern, get nr of trips, routes, total and avg distance and number of stops covered
  service_pattern_summary <- pattern_gtfs$trips |> # Join trips
    left_join(pattern_gtfs$.$servicepatterns, by="service_id") |> # with service pattern
    left_join(shape_lengths, by="shape_id") |> # with shape length
    left_join(pattern_gtfs$stop_times, by="trip_id") |> # with planned route (stops and times)
    group_by(servicepattern_id) |> # group by service pattern
    summarise(
      trips = n(),
      routes = n_distinct(route_id),
      total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3, # divide by 1e3 to convert meters to kms
      route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
      stops=(n_distinct(stop_id)/2) # divided by two because usually there is one stop per direction
    )

  ## Add the number of days that each service is in operation (by join with $.$dates_servicepatterns)
  service_pattern_summary <- pattern_gtfs$.$dates_servicepatterns |>
    group_by(servicepattern_id) |>
    summarise(days_in_service = n()) |>
    left_join(service_pattern_summary, by = "servicepattern_id")

  ## Get service patterns that run on the date selected
  service_pattern_ids = pattern_gtfs$.$dates_servicepatterns |>
    filter(date==date)

  service_ids = pattern_gtfs$.$servicepattern |>
    filter(servicepattern_id %in% service_pattern_ids$servicepattern_id) |>
    pull(service_id)

  #### Filter by date

  # Get stop frequency (missing data)

  frame = data.frame()

  for (i in 6:23) {
    stop_frequency <- tidytransit::get_stop_frequency(
      gtfs_date,
      start_time = sprintf("%.2d:00:00", i),
      end_time = sprintf("%.2d:59:59", i),
      service_ids = service_ids,
      by_route = TRUE
    )

    stop_frequency <- stop_frequency |>
      group_by(stop_id) |>
      summarise(frequency = sum(n_departures)) |>
      mutate(hour = i)

    frame <- rbind(frame, stop_frequency)
  }

  frequency <- frame |>
    ungroup() |>
    group_by(stop_id, hour) |>
    summarise(frequency = sum(frequency)) |>
    ungroup()

  table <- frequency |>
    left_join(gtfs_date$stops |>
                select(stop_id, stop_lon, stop_lat), by = "stop_id") |>
    st_as_sf(crs = 4326, coords = c("stop_lon", "stop_lat"))

  message("Finished GTFS analysis!")

  return(table)
}
