#' Get OSM routes geometry considering gtfs:shape_id match
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param q osmdata::opq. Overpass query for transit network.
#'
#' @details
#' For each route, matches its trips' shapes with OSM route relations, considering the
#' OSM \code{gtfs:shape_id} attribute.
#'
#' @returns A \code{sf} \code{data.frame} with the following columns:
#' \itemize{
#'  \item \code{shape_id}, the \code{shape_id} attribute from \code{shapes.txt} file.
#'  \item \code{osm_id}, the \code{osm_id} attribute from OSM route relation.
#'  \item \code{geometry}, the geometrical data for the OSM route relation.
#' }
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::load_feed("gtfs.zip")
#'
#' q = opq("Lisbon")  |>
#'   add_osm_feature(key = "route", value = c("bus", "tram")) |>
#'   add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)
#'
#' shapes_geometry_osm = GTFShift::osm_shapes_to_routes(gtfs, q)
#' }
#'
#' @import osmdata
#' @import sf
#' @import dplyr
#'
#' @export
osm_shapes_to_routes <- function(gtfs, q) {

  # 1. Get OSM routes
  osm = q |> osmdata_sf()
  osm_multilines = osm$osm_multilines
  osm_multilines_redux = osm_multilines |>
    select(any_of(c("osm_id", "gtfs:shape_id")))

  # 2. Merge with GTFS
  result = gtfs$trips |> select(trip_id, shape_id) |>
    distinct(shape_id, .keep_all=TRUE) |>
    left_join(osm_multilines_redux |> select("osm_id", "gtfs:shape_id", "geometry"), by=c("shape_id" = "gtfs:shape_id")) |>
    select(-trip_id) |>
    st_as_sf()

}
