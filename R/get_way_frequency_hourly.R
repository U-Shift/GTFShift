#' Get aggregated frequency per hour for each OSM way
#'
#' For each OSM way with GTFS service, returns the number of departures aggregated per hour and direction.
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param q osmdata::opq. Overpass query for transit network, to obtain OSM route ways, using \code{GTFShift::osm_shapes_to_routes()}.
#' @param date Date (Default \code{GTFShift::calendar_nextBusinessWednesday()}). Reference date to consider when analyzing the GTFS file.
#' @param keep_osm_attributes Boolean (Default FALSE). Whether to keep all OSM way attributes in the output \code{sf} object.
#' @param osm_file character (Optional). Location of OSM extract file with \code{osm.pbf} format. Refer to \code{osmextract::oe_download()} for more details. If not provided OSM Overpass API is called through \code{osmdata::osmdata_sf()}.
#'
#' @details
#' This method analyses the GTFS feed for a representative day, finding for each route the corresponding OSM ways using \code{GTFShift::osm_shapes_to_routes()}
#' (routes not on OSM are ignored), aggregating the number of services per hour and direction for each.
#'
#' For a detailed example, see the \code{vignette("analyse")}.
#'
#' @returns An \code{sf} \code{data.frame} object with the following columns:
#' \describe{
#'   \item{way_osm_id}{The \code{osm_id} attribute from OSM way.}
#'   \item{hour}{The hour for which the frequency applies (24 hour format).}
#'   \item{frequency}{The number of services for the route that depart from the first stop for the corresponding 60 minutes period.}
#'   \item{routes}{The list of route_ids that use the way.}
#'   \item{shapes}{The list of shape_ids that use the way.}
#'   \item{geometry}{The route shape.}
#'   \item{(if \code{keep_osm_attributes = TRUE})}{All OSM way attributes.}
#' }
#'
#' @examples
#' # Subset GTFS for one route only, for demo purposes
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"))
#' gtfs <- GTFShift::filter_by_route_name(gtfs, c("1", "2", "3", "4"))
#' 
#' # Build query and prepare osm extract (possible to use API as alternative)
#' q <- osmdata::opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |> 
#'   osmdata::add_osm_feature(key = "route", value = "bus") |> 
#'   osmdata::add_osm_feature(key = "operator", value = "Transportes Colectivos do Barreiro")
#' osm_file <- system.file("extdata/samples", "osmextract_tcb_network.pbf", package = "GTFShift")
#' 
#' # Get frequency
#' frequency_analysis <- GTFShift::get_way_frequency_hourly(
#'   gtfs, q, 
#'   date = gtfs$calendar$start_date[1],
#'   osm_file = osm_file
#' )
#' 
#' head(frequency_analysis |> sf::st_drop_geometry())
#'
#' @seealso \code{GTFShift::calendar_nextBusinessWednesday()}
#' @seealso \code{GTFShift::osm_shapes_to_routes()}
#'
#' @import tidytransit
#' @import dplyr
#' @import sf
#' @import lubridate
#' @importFrom tidyselect any_of
#'
#' @export
get_way_frequency_hourly <- function(
  gtfs,
  q,
  date = GTFShift::calendar_nextBusinessWednesday(),
  keep_osm_attributes = FALSE,
  osm_file = NULL
) {
  message(sprintf("Analysing GTFS for %s...", date))

  ## Consider transit data for one day only
  message(sprintf("> Filtering by reference date %s...", date))
  suppressWarnings({ # Ignore missing transfers warnings
    gtfs_date <- tidytransit::filter_feed_by_date(gtfs, extract_date = date)
  })

  # PROCESS GTFS, generating table calculating the frequencies per route
  trips <- gtfs_date$trips
  stops <- gtfs_date$stops
  ways <- GTFShift::osm_shapes_to_routes(gtfs, q, TRUE, osm_file = osm_file)

  routes <- gtfs_date$routes
  stop_times <- gtfs_date$stop_times

  stop_times <- stop_times |>
    left_join(trips, by = "trip_id") |>
    left_join(routes, by = "route_id") |>
    select(any_of(c(
      "route_id",
      "route_short_name",
      "trip_id",
      "stop_id",
      "service_id",
      "arrival_time",
      "departure_time",
      "direction_id",
      "shape_id",
      "stop_sequence"
    )))

  stop_times <- stop_times |>
    arrange(stop_sequence) |>
    group_by(trip_id) |>
    slice(1) |> # Only departures from origin (first stop)
    ungroup() |>
    mutate(hour = lubridate::hour(departure_time))

  freq_data <- stop_times |>
    group_by(across(any_of(c("route_id", "route_short_name", "direction_id", "hour")))) |>
    summarize(frequency = n()) |>
    ungroup()

  routes_freq <-
    freq_data |>
    left_join(
      trips |>
        select(any_of(c("route_id", "direction_id", "shape_id"))) |>
        distinct(),
      by = c("route_id", "direction_id"),
      relationship = "many-to-many"
    ) |>
    as.data.frame()

  # Join with ways
  ways_unique_geometry <- ways |>
    distinct(way_osm_id, .keep_all = TRUE)

  if (!keep_osm_attributes) {
    ways_unique_geometry <- ways_unique_geometry |>
      select(way_osm_id, geometry)
  }

  ways_freq <- routes_freq |>
    inner_join(ways |> sf::st_drop_geometry() |> select(shape_id, way_osm_id), by = "shape_id", relationship = "many-to-many") |>
    group_by(way_osm_id, hour) |>
    summarize(
      frequency = sum(frequency),
      routes = list(unique(route_id)),
      shapes = list(unique(shape_id))
    ) |>
    ungroup() |>
    inner_join(ways_unique_geometry, by = "way_osm_id") |>
    st_as_sf()

  return(ways_freq)
}
