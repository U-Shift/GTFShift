#' Export designated bus lanes from OpenStreetMaps
#
#'
#' @param bbox bbox. Area from which to export bus lanes.
#'
#' @details
#' Exports roads tagged as designated bus lanes on OpenStreetMaps for given area.
#'
#' @returns osm_lines in sf format
#'
#'
#' @examples
#' \dontrun{
#' BBOX = sf::st_bbox(city_limit)
#' bus_lanes <- GTFShift::osm_bus_lanes(BBOX)
#' }
#'
#' @import osmdata
#' @import sf
#' @import dplyr
#'
#' @export
osm_bus_lanes <- function(bbox) {

  road_osm = road_osm = opq(bbox) |> # uses osmdata package, to extract only with BB
    add_osm_feature(key = "highway") |>
    osmdata_sf() |>
    osm_poly2line() # makes roundabouts into lines

  road_osm = road_osm$osm_lines

  cols_to_check_access <- grep("psv:lanes|bus:lanes", names(road_osm), value = TRUE)
  cols_to_check_count <- grep("lanes:psv|lanes:bus", names(road_osm), value = TRUE)

  osm_lanes = road_osm |> filter(
    # Based on https://wiki.openstreetmap.org/wiki/Bus_lanes
    psv == "designated"
    | highway == "busway"
    | if_any(all_of(cols_to_check_access), ~ grepl("designated", .x))
    | if_any(all_of(cols_to_check_count), ~ is.numeric(.x) & .x >= 1)
  )

  return(osm_lanes)
}
