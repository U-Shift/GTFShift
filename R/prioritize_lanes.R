#' Prioritize road network lanes for bus lane implementation
#'
#' For each OSM way with GTFS service, aggregates its characteristics to assist in the bus lane implementation prioritization
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param q osmdata::opq. Overpass query for transit network, to obtain OSM route ways, using \code{GTFShift::osm_shapes_to_routes()}.
#' @param date Date (Default \code{GTFShift::calendar_nextBusinessWednesday()}). Reference date to consider when analyzing the GTFS file.
#' @param keep_osm_attributes Boolean (Default FALSE). Whether to keep all OSM way attributes in the output \code{sf} object.
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
#' \itemize{
#'  \item \code{way_osm_id}, the \code{osm_id} attribute from OSM way.
#'  \item \code{hour}, the hour for which the frequency applies (24 hour format).
#'  \item \code{frequency}, the number of services for the route that depart from the first stop for the corresponding 60 minutes period.
#'  \item \code{is_bus_lane}, whether the way has a bus lane.
#'  \item \code{n_lanes}, the total number of lanes.
#'  \item \code{n_directions}, the number of travel directions.
#'  \item \code{n_lanes_direction}, the number of lanes per direction.
#'  \item \code{routes}, the list of route_ids that use the way, separated by semicolon.
#'  \item \code{geometry}, the route shape.
#'  \item (if \code{keep_osm_attributes = TRUE}) all OSM way attributes.
#' }
#'
#' @examples
#' \dontrun{
#' gtfs = GTFShift::load_feed("gtfs.zip")
#' q = opq(bbox=sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |> add_osm_feature(key = "route", value = "bus")
#' lanes_analysis = GTFShift::prioritize_lanes(gtfs, q)
#' }
#'
#' @import dplyr
#' @import tidytransit
#'
#' @export
prioritize_lanes <- function(
    gtfs,
    q,
    date = GTFShift::calendar_nextBusinessWednesday(),
    keep_osm_attributes = FALSE
) {

  # Get way frequency hourly
  way_frequency = GTFShift::get_way_frequency_hourly(gtfs, q, date, TRUE)

  # Get bus lanes
  shapes_sf = tidytransit::shapes_as_sf(gtfs$shapes)
  shapes_bbox = sf::st_bbox(shapes_sf)
  bus_lanes = GTFShift::osm_bus_lanes(shapes_bbox)

  # Aggregate data
  # > Add missing lanes columns, to prevent errors
  lane_cols = c("lanes", "lanes:forward", "lanes:backward", "lanes:both_ways", "oneway")
  for (col in lane_cols) {
    if (!(col %in% colnames(way_frequency))) {
      way_frequency[[col]] = NA_character_
    }
  }
  parse_lanes <- function(x) {
    suppressWarnings(as.numeric(sub(";.*$", "", x)))
  }

  # > Compute aggregation
  lanes = way_frequency |>
    left_join(bus_lanes |> sf::st_drop_geometry() |> select(osm_id) |> mutate(is_bus_lane = TRUE), by = c("way_osm_id" = "osm_id")) |>
    mutate(
      is_bus_lane = ifelse(is.na(is_bus_lane), FALSE, is_bus_lane),
      n_lanes = coalesce(
        # Global count
        parse_lanes(lanes),
        # Directional count
        parse_lanes(`lanes:forward`) + parse_lanes(`lanes:backward`) + parse_lanes(`lanes:both_ways`),
        # If oneway=="yes", then 1
        ifelse(oneway == "yes", 1, NA_integer_),
        # Else, assume 2 lanes, one per direction
        2 # NA_integer_
      ),
      n_directions = case_when(
        oneway %in% c("yes", "1", "-1", "true") ~ 1,
        oneway %in% c("no", "0", "false") ~ 2,
        TRUE ~ 2
      ),
      n_lanes_direction = case_when(
        !is.na(n_lanes) & !is.na(n_directions) ~ n_lanes / n_directions,
        TRUE ~ NA_real_
      )
    )

  if (!keep_osm_attributes) {
    lanes = lanes |>
      select(way_osm_id, hour, frequency, is_bus_lane, n_lanes, n_directions, n_lanes_direction, routes, geometry)
  }

  return(lanes)
}
