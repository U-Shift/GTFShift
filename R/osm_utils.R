#' Filter OpenStreetMap ways by bus lane tags
#'
#' @param road_osm sf object. Road OSM data.
#'
#' @return sf object. Filtered road OSM data.
#'
#' @import dplyr
#'
#' @noRd
filter_osm_bus_lanes <- function(road_osm) {
  cols_to_check_access <- grep("psv:lanes|bus:lanes", names(road_osm), value = TRUE)
  cols_to_check_count <- grep("lanes:psv|lanes:bus", names(road_osm), value = TRUE)

  osm_lanes <- road_osm |> filter(
    # Based on https://wiki.openstreetmap.org/wiki/Bus_lanes
    psv == "designated" |
      highway == "busway" |
      (length(cols_to_check_access) & if_any(all_of(cols_to_check_access), ~ grepl("designated", .x))) |
      (length(cols_to_check_count) & if_any(all_of(cols_to_check_count), ~ is.numeric(.x) & .x >= 1))
  )

  return(osm_lanes)
}
