#' Prioritize road network lanes for bus lane implementation
#'
#' For each OSM way with GTFS service, aggregates its characteristics to assist in the bus lane implementation prioritization
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param q osmdata::opq. Overpass query for transit network, to obtain OSM route ways, using \code{GTFShift::osm_shapes_to_routes()}.
#' @param date Date (Default \code{GTFShift::calendar_nextBusinessWednesday()}). Reference date to consider when analyzing the GTFS file.
#' @param keep_osm_attributes Boolean (Default FALSE). Whether to keep all OSM way attributes in the output \code{sf} object.
#' @param osm_file character (Optional). Location of OSM extract file with \code{osm.pbf} format. Refer to \code{osmextract::oe_download()} for more details. If not provided OSM Overpass API is called through \code{osmdata::osmdata_sf()}.
#'
#' @details
#' This method analyses the GTFS feed for a representative day, returning a data.frame with the road segments where transit routes
#' run and for each, a set of parameters that can be used to prioritize bus lane implementations.
#'
#' Its functionality is a bundle that encapsulates the logic of several methods from the package,
#' including \code{GTFShift::get_way_frequency_hourly()} and \code{GTFShift::osm_bus_lanes()}, that can be used separately if needed.
#'
#' Mind that this method uses \code{GTFShift::get_way_frequency_hourly()} to match routes with OSM ways, which requires that the
#' OSM relation mapping is well defined for the transit routes. Routes that do not have an OSM match are ignored.
#'
#' @returns An \code{sf} \code{data.frame} object with the following columns:
#' \describe{
#'   \item{way_osm_id}{The \code{osm_id} attribute from OSM way.}
#'   \item{hour}{The hour for which the frequency applies (24 hour format).}
#'   \item{frequency}{The number of services for the route that depart from the first stop for the corresponding 60 minutes period.}
#'   \item{is_bus_lane}{Whether the way has a bus lane.}
#'   \item{n_lanes_parking}{The number of parking lanes.}
#'   \item{n_lanes_circulation}{The number of circulation lanes.}
#'   \item{n_directions}{The number of travel directions.}
#'   \item{n_lanes_circulation_direction}{The number of circulation lanes per direction.}
#'   \item{routes}{The list of route_id that use the way.}
#'   \item{shapes}{The list of shape_id that use the way.}
#'   \item{geometry}{The route shape.}
#'   \item{(if \code{keep_osm_attributes = TRUE})}{All OSM way attributes.}
#' }
#'
#' @examples
#' # Subset GTFS for one route only, for demo purposes
#' gtfs <- GTFShift::load_feed(system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift"))
#' gtfs <- GTFShift::filter_by_route_name(gtfs, c("4"))
#' 
#' # Build query and prepare osm extract (possible to use API as alternative)
#' q <- osmdata::opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |> 
#'   osmdata::add_osm_feature(key = "route", value = "bus") |> 
#'   osmdata::add_osm_feature(key = "operator", value = "Transportes Colectivos do Barreiro")
#' osm_file <- system.file("extdata", "osmextract_tcb_network.pbf", package = "GTFShift")
#' 
#' lane_prioritization <- GTFShift::prioritize_lanes(
#'   gtfs, q, 
#'   osm_file = osm_file, 
#'   date = gtfs$calendar$start_date[1]
#' )
#' 
#' head(lane_prioritization |> dplyr::select(way_osm_id, hour, frequency, is_bus_lane, n_lanes_circulation, routes))
#' 
#' @import dplyr
#' @import tidytransit
#'
#' @export
prioritize_lanes <- function(
  gtfs,
  q,
  date = GTFShift::calendar_nextBusinessWednesday(),
  keep_osm_attributes = FALSE,
  osm_file = NULL
) {
  # Get way frequency hourly
  way_frequency <- GTFShift::get_way_frequency_hourly(gtfs, q, date, TRUE, osm_file = osm_file)

  # Get bus lanes
  bus_lanes <- filter_osm_bus_lanes(way_frequency |> distinct(way_osm_id, .keep_all = TRUE))

  # Aggregate data
  # > Add missing lanes columns, to prevent errors
  lane_cols <- c("lanes", "lanes:forward", "lanes:backward", "lanes:both_ways", "oneway")
  for (col in lane_cols) {
    if (!(col %in% colnames(way_frequency))) {
      way_frequency[[col]] <- NA_character_
    }
  }
  parse_lanes <- function(x) {
    suppressWarnings(as.numeric(sub(";.*$", "", x)))
  }

  # > Compute aggregation
  lanes <- way_frequency |>
    left_join(bus_lanes |> st_drop_geometry() |> select(way_osm_id) |> mutate(is_bus_lane = TRUE), by = "way_osm_id") |>
    mutate(
      is_bus_lane = ifelse(is.na(is_bus_lane), FALSE, is_bus_lane),
      n_lanes_parking = dplyr::case_when(
        # Any 'parking:both' or 'parking:lane:both' column present with value different from 'no'
        if_any(matches("^parking(:lane)?:both"), ~ !is.na(.) & . != "no") ~ 2L,
        # Otherwise, count left and right sides separately based on specific tags (parking:lane:left/right or parking:left/right)
        TRUE ~ (
          as.integer(
            # grepl "no" to account for parking:left:restriction=no_stopping
            if_any(matches("^parking(:lane)?:left"), ~ !is.na(.) & !grepl("\\bno\\b|\\bno_", ., ignore.case = TRUE))
          ) +
            as.integer(
              if_any(matches("^parking(:lane)?:right"), ~ !is.na(.) & !grepl("\\bno\\b|\\bno_", ., ignore.case = TRUE))
            )
        )
      ),
      n_lanes_circulation = coalesce(
        # Global count
        parse_lanes(lanes),
        # Directional count (sum existing ones; returns NA if all are missing)
        na_if(
          rowSums(across(matches("^lanes(:[^:]+)*:forward$"), ~ coalesce(parse_lanes(.), 0)), na.rm = TRUE) +
            rowSums(across(matches("^lanes(:[^:]+)*:backward$"), ~ coalesce(parse_lanes(.), 0)), na.rm = TRUE) +
            rowSums(across(matches("^lanes(:[^:]+)*:both_ways$"), ~ coalesce(parse_lanes(.), 0)), na.rm = TRUE),
          0
        ),
        # If oneway=="yes", then 1
        ifelse(oneway == "yes", 1, NA_integer_),
        # Else, assume 2 lanes, one per direction
        2 # NA_integer_
      ),
      n_directions = case_when(
        n_lanes_circulation == 1 ~ 1, # When only one lane, assume one direction
        # any oneway:* tag indicating "no"
        if_any(matches("oneway"), ~ tolower(.x) %in% c("no", "0", "false")) ~ 2,
        # any oneway:* tag indicating "yes"
        if_any(matches("oneway"), ~ tolower(.x) %in% c("yes", "1", "-1", "true")) ~ 1,
        TRUE ~ 2
      ),
      n_lanes_circulation_direction = case_when(
        n_lanes_circulation / n_directions < 1 ~ 1,
        !is.na(n_lanes_circulation) & !is.na(n_directions) ~ n_lanes_circulation / n_directions,
        TRUE ~ NA_real_
      )
    )

  if (!keep_osm_attributes) {
    lanes <- lanes |>
      select(way_osm_id, hour, frequency, is_bus_lane, n_lanes_parking, n_lanes_circulation, n_directions, n_lanes_circulation_direction, routes, shapes, geometry)
  }

  return(lanes)
}
