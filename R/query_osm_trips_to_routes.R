#' Get OSM routes geometry considering gtfs:trip_id match
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param q osmdata::opq. Overpass query for transit network.
#'
#' @details
#' For each route, matches its trips with OSM route relations, considering the
#' OSM \code{gtfs:trip_id} attribute.
#'
#' @returns A \code{sf} \code{data.frame} with the following columns:
#' \itemize{
#'  \item \code{trip_id}, the \code{trip_id} attribute from \code{trips.txt} file.
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
#' trips_geometry_osm = GTFShift::osm_trips_to_routes(gtfs, q)
#' }
#'
#' @import osmdata
#' @import sf
#' @import dplyr
#'
#' @export
osm_trips_to_routes <- function(gtfs, q) {

  # 1. Get OSM routes
  osm = q |> osmdata_sf()
  osm_multilines = osm$osm_multilines
  osm_multilines_redux = osm_multilines |>
    select(any_of(c("osm_id", "gtfs:trip_id")))

  # 2. Merge with GTFS
  result = gtfs$trips |> select(trip_id) |>
    inner_join(osm_multilines_redux |> select("osm_id", "gtfs:trip_id", "geometry"), by=c("trip_id" = "gtfs:trip_id")) |>
    st_as_sf()

}
