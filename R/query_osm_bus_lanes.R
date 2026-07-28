#' Export designated bus lanes from OpenStreetMaps
#
#'
#' @param bbox bbox. Area from which to export bus lanes.
#' @param osm_file character (Optional). Location of OSM extract file with \code{osm.pbf} format. Refer to \code{osmextract::oe_download()} for more details. If not provided OSM Overpass API is called through \code{osmdata::osmdata_sf()}.
#'
#' @details
#' Exports roads tagged as designated bus lanes on OpenStreetMaps for given area.
#'
#' @returns osm_lines in sf format
#'
#'
#' @examples
#' # Create bbox for Lisbon
#' bbox <- st_as_sfc(st_bbox(c(
#'   xmin = -9.229836, ymin = 38.691399, 
#'   xmax = -9.087387, ymax = 38.796760
#' ), crs = 4326))
#'
#' # Use sample osmextract for Lisbon highways
#' osm_file <- system.file("extdata/samples", "osmextract_lisbon_highways_sample.pbf", package = "GTFShift")
#' 
#' # Export bus lanes
#' bus_lanes <- GTFShift::osm_bus_lanes(bbox, osm_file = osm_file)
#'
#' names(bus_lanes)
#' 
#' head(bus_lanes |> dplyr::select(`osm:id`, name))
#'
#' @import osmdata
#' @import sf
#' @import dplyr
#'
#' @export
osm_bus_lanes <- function(bbox, osm_file = NULL) {
  if (!is.null(osm_file)) {
    highways_base <- osmextract::oe_read(osm_file, boundary = bbox, quiet = TRUE)
    highways_cols <- osmextract::oe_get_keys(highways_base)
    cols_to_check <- c(
      grep("psv:lanes|bus:lanes", highways_cols, value = TRUE),
      grep("lanes:psv|lanes:bus", highways_cols, value = TRUE),
      "psv"
    )
    road_osm <- osmextract::oe_read(osm_file, boundary = bbox, quiet = TRUE, extra_tags = cols_to_check)
    names(road_osm) <- gsub("_", ":", names(road_osm))
  } else {
    road_osm <- opq(bbox) |> # uses osmdata package, to extract only with BB
      add_osm_feature(key = "highway") |>
      osmdata_sf() |>
      osm_poly2line() # makes roundabouts into lines
    road_osm <- road_osm$osm_lines
  }


  osm_lanes <- filter_osm_bus_lanes(road_osm)

  return(osm_lanes)
}
