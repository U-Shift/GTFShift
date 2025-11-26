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
#' Shapes that do not have a match on OSM are ignored.
#' If that occurs, a warning is displayed during the method execution, informing about the missing geometries.
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

  pb <- progress::progress_bar$new( # Track progress
    format = "1/2: Fetching OSM data [:bar] :percent :spin elapsed=:elapsed",
    clear = FALSE, show_after=0
  )
  pb$update(0)

  # 1. Get OSM routes
  job <- callr::r_bg(function(q) { # update spinner while blocking method call
    return(q |> osmdata::osmdata_sf())
  }, args=list(q))
  while (job$is_alive()) { pb$tick(0); Sys.sleep(0.1) }
  osm <- job$get_result()

  pb$update(0.5)
  osm_multilines = osm$osm_multilines
  osm_multilines_redux = osm_multilines |>
    select(any_of(c("osm_id", "gtfs:shape_id")))

  pb$update(1)

  # 2. Merge with GTFS
  shape_ids = gtfs$trips |> select(shape_id) |>
    distinct()
  message(sprintf("> Trying to match %d shapes with %s osm routes...", nrow(shape_ids), nrow(osm_multilines_redux)))
  pb <- progress::progress_bar$new( # Track progress
    format = "2/2: Matching shapes with OSM routes [:bar] :percent :spin elapsed=:elapsed",
    clear = FALSE, show_after=0
  )
  pb$update(0)
  result = shape_ids |>
    inner_join(osm_multilines_redux |> select("osm_id", "gtfs:shape_id", "geometry"), by=c("shape_id" = "gtfs:shape_id")) |>
    st_as_sf()
  pb$update(1)
  message(sprintf("> Matched %d shapes with OSM routes!", nrow(result)))

  # 3. Log missing shapes
  shapes_missing = shape_ids |> filter(!(shape_id %in% result$shape_id)) |> left_join(gtfs$trips, by="shape_id") |> left_join(gtfs$routes, by="route_id") |> distinct(shape_id, .keep_all = TRUE)
  if (nrow(shapes_missing)>0) {
    row_strings <- with(shapes_missing, sprintf("%s (%s)", shape_id, route_short_name))
    warning(sprintf("Shapes missing (ignored in the result): %s", paste(row_strings, collapse = " ")))
  }

  return(result)
}
