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
#' BBOX <- sf::st_bbox(city_limit)
#' bus_lanes <- GTFShift::osm_bus_lanes(BBOX)
#' }
#'
#' @import osmdata
#' @import sf
#' @import dplyr
#'
#' @export
osm_bus_lanes <- function(bbox) {
  road_osm <- opq(bbox) |> # uses osmdata package, to extract only with BB
    add_osm_feature(key = "highway") |>
    osmdata_sf() |>
    osm_poly2line() # makes roundabouts into lines

  road_osm <- road_osm$osm_lines

  osm_lanes <- filter_osm_bus_lanes(road_osm)

  return(osm_lanes)
}
