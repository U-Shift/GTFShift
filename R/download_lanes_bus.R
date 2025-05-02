#' Download bus lanes from Open Street Maps
#
#'
#' @param bbox bbox. Area from which to export bus lanes.
#'
#' @details
#' Exports roads tagged as bus lanes on Open Street Map for given area.
#'
#' @returns osm_lines in sf format
#'
#'
#' @examples
#' \dontrun{
#' BBOX = sf::st_bbox(city_limit)
#' bus_lanes <- GTFShift::download_bus_lanes(BBOX)
#' }
#'
#' @import osmdata
#' @import sf
#' @import dplyr
#'
#' @export
download_bus_lanes <- function(bbox) {

  road_osm = road_osm = opq(bbox) |> # uses osmdata package, to extract only with BB
    add_osm_feature(key = "highway") |>
    osmdata_sf() |>
    osm_poly2line() # makes roundabouts into lines

  road_osm = road_osm$osm_lines

  osm_lanes = road_osm |> select(contains("psv"))
  osm_lanes = osm_lanes |> filter(psv == "designated" |
                                    `lanes:psv` == 1 |
                                    `lanes:psv:forward` == 1 |
                                    `lanes:psv:backward` == 1 |
                                    `psv:lanes:backward` == "designated" |
                                    `psv:lanes:forward` == "designated" |
                                    !is.na(`psv:lanes`)
  )

  return(osm_lanes)
}
