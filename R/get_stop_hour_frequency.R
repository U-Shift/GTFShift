#' Get aggregated frequency per hour for each bus stop
#'
#' For each stop, returns the number of departures aggregated per hour.
#'
#' @param gtfs tidygtfs. GTFS loaded using tidytransit::read_gtfs.
#' @param date Reference date to consider when analysing the GTFS file. Defaults to next business wednesday (in Portugal).
#' @param route_types Restricts analysis to defined route_types, defaults to those that have conflicts on urban environments: tram and bus.
#' @param prepend_agency Boolean. When true, stop_id is prepended with agency_id to avoid duplicate stop ids from multiple GTFS merging.
#'
#' @details
#' This method analyses the GTFS feed for a representative day, generating for each stop the number of services aggregated per hour.
#'
#' @returns A `data.frame` object with the following columns:
#' - `stop_id`, the `stop_id` attribute from `stops.txt` file;
#' - `hour`, the hour for which the frequency applies (24 hour format);
#' - `frequency`, the number of services provided at the stop for the corresponding 60 minutes period;
#' - `geometry`, the stop coordinates.
#'
#' @examples
#' \dontrun{
#' gtfs <- tidytransit::read_gtfs("gtfs.zip")
#' frequency_analysis <- get_stop_hour_frequency(gtfs)
#' }
#'
#' @seealso [tidytransit::read_gtfs()]
#'
#' @import sf
#' @import tidyverse
#' @import lubridate
#' @import tidytransit
#' @import dplyr
#'
#' @export
get_stop_hour_frequency <- function(gtfs, date=NULL, route_types=list(0,3,5,11), prepend_agency=TRUE) {
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

  # Consider trips for defined modes only
  if (!is.null(route_types)) {
    message(sprintf("> Filtering by route types %s...", toString(route_types)))
    routesNBefore <- length(gtfs_date$routes$route_id)
    tripsNBefore <- length(gtfs_date$trips$trip_id)

    routes_ids <- gtfs_date$routes[gtfs_date$routes$route_type %in% route_types, ]$route_id
    trips_ids <- gtfs_date$trips[gtfs_date$trips$route_id %in% routes_ids, ]$trip_id
    gtfs_date <- tidytransit::filter_feed_by_trips(gtfs_date, trips_ids)

    routesNAfter = length(gtfs_date$routes$route_id)
    tripsNAfter = length(gtfs_date$trips$trip_id)
    message(sprintf("> Removed %d routes, representing %d trips, proceding analysis...", routesNBefore-routesNAfter, tripsNBefore-tripsNAfter))
  }

  message(sprintf("> Found %d routes operating %d trips on %d stops...",
                  length(gtfs_date$trips$trip_id),
                  length(gtfs_date$routes$route_id),
                  length(gtfs_date$stops$stop_id)
  ))

  if (length(gtfs_date$trips$trip_id)==0) {
    stop("No trips found after filtering! Make sure you have a valid GTFS!")
  }

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

  ## Prepend stop_id with GTFS.agency.agency_id to avoid duplicate stop ids from multiple GTFS merging
  if (prepend_agency) {
    table$stop_id <- paste0(sprintf("%s_", gtfs$agency$agency_id), table$stop_id)
  }

  message("Finished GTFS analysis!")

  return(table)
}
