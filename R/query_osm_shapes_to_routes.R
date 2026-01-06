#' Get OSM routes geometry considering gtfs:shape_id match
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param q osmdata::opq. Overpass query for transit network.
#' @param ways boolean (Default False). If true, relation is dissagregated in ways.
#' @param ways_tags character vector (Default \code{c("lanes", "psv", "bus", "way")}). List of OSM way tags to extract when \code{ways} parameter is set to true. Match is done using \code{tidyselect::contains()}.
#'
#' @details
#' For each route, matches its trips' shapes with OSM route relations, considering the
#' OSM \code{gtfs:shape_id} attribute.
#'
#' @returns A \code{sf} \code{data.frame} with the following columns:
#' \itemize{
#'  \item \code{shape_id}, the \code{shape_id} attribute from \code{shapes.txt} file.
#'  \item \code{osm_id}, the \code{osm_id} attribute from OSM route relation.
#'  \item \code{way_osm_id}, the \code{osm_id} attribute from OSM way (if \code{ways} parameter is set to true).
#'  \item \code{*}, any column that matches \code{ways_tags} parameter.
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
#' @import progress
#' @import callr
#'
#' @export
osm_shapes_to_routes <- function(gtfs, q, ways = FALSE, ways_tags = c("lanes", "psv", "bus", "way")) {

  total_steps = ifelse(ways, 3, 2)

  # 1. Get OSM routes
  pb <- progress::progress_bar$new( # Track progress
    format = sprintf("1/%d: Fetching OSM routes [:bar] :percent :spin elapsed=:elapsed", total_steps),
    clear = FALSE, show_after=0
  )
  pb$update(0)
  job <- callr::r_bg(function(q) { # update spinner while blocking method call
    return(q |> osmdata::osmdata_sf())
  }, args=list(q))
  while (job$is_alive()) { pb$tick(0); Sys.sleep(0.1) }
  osm <- job$get_result()

  pb$update(0.5)
  osm_multilines <- osm$osm_multilines
  osm_multilines_redux = osm_multilines |>
    select(any_of(c("osm_id", "gtfs:shape_id")))
  pb$update(1)
  pb$terminate()

  # 2. Merge with GTFS
  shape_ids = gtfs$trips |> select(shape_id) |> distinct()
  pb <- progress::progress_bar$new( # Track progress
    format = sprintf("2/%d: Matching %d shapes with %s routes [:bar] :percent :spin elapsed=:elapsed", total_steps, nrow(shape_ids), nrow(osm_multilines_redux)),
    clear = FALSE, show_after=0
  )
  pb$update(0)

  result = shape_ids |>
    inner_join(osm_multilines_redux |> select("osm_id", "gtfs:shape_id", "geometry"), by=c("shape_id" = "gtfs:shape_id")) |>
    st_as_sf()

  pb$update(1)
  pb$terminate()

  # If relation disaggregation
  if (ways) {
    pb <- progress::progress_bar$new( # Track progress
      format = sprintf("3/%d: Matching OSM routes with ways  [:bar] :percent :spin elapsed=:elapsed", total_steps),
      clear = FALSE, show_after=0
    )
    pb$update(0)

    # 3.1. Get OSM relations (to associate relations and ways)
    osm_file <- tempfile(fileext = ".osm")
    job <- callr::r_bg(function(q, osm_file) { # update spinner while blocking method call
      osmdata::osmdata_xml(q, filename = osm_file)
    }, args=list(q, osm_file))
    while (job$is_alive()) { pb$tick(0); Sys.sleep(0.1) }
    pb$update(0.33)

    job <- callr::r_bg(function(osm_file) { # update spinner while blocking method call
      library(xml2)
      library(dplyr)

      doc <- read_xml(osm_file)
      relations <- xml_find_all(doc, ".//relation")
      relations_df <- lapply(relations, function(relation) { # blocking
        relation_id <- xml_attr(relation, "id")
        members <- xml_find_all(relation, "member")

        members_df <- lapply(members, function(member) {
          c(
            type = xml_attr(member, "type"),
            ref = xml_attr(member, "ref"),
            role = xml_attr(member, "role")
          )
        })
        df <- data.frame(do.call(rbind, members_df))
        df$relation_osm_id <- relation_id
        return(df)
      })
      return(bind_rows(relations_df))
    }, args=list(osm_file))
    while (job$is_alive()) { pb$tick(0); Sys.sleep(0.1) }
    pb$update(0.66)

    relations_df <- job$get_result()
    ways_relations <- relations_df |> filter(type=="way") |> select(ref, relation_osm_id) # ref is way osm_id

    # 3.2. Disaggregate relations in ways
    result = result |>
      sf::st_drop_geometry() |>
      left_join(ways_relations |> rename(way_osm_id = ref) |> rename(osm_id = relation_osm_id), by = "osm_id") |>
      left_join(as_tibble(osm$osm_lines) |> select(osm_id, contains(ways_tags)), by = c("way_osm_id" = "osm_id"))

    geom <- osm$osm_lines$geometry
    names(geom) <- NULL
    result$geometry <- geom[match(result$way_osm_id, osm$osm_lines$osm_id)]
    result <- sf::st_as_sf(result, sf_column_name = "geometry", crs = st_crs(osm$osm_lines))

    pb$update(1)
    pb$terminate()
  }

  # 4. Log missing shapes
  message(sprintf("Matched %d shapes with OSM routes!", length(unique(result$shape_id))))
  shapes_missing = shape_ids |> filter(!(shape_id %in% result$shape_id)) |> left_join(gtfs$trips, by="shape_id") |> left_join(gtfs$routes, by="route_id") |> distinct(shape_id, .keep_all = TRUE)
  if (nrow(shapes_missing)>0) {
    row_strings <- with(shapes_missing, sprintf("%s (%s)", shape_id, route_short_name))
    warning(sprintf("Shapes missing (ignored in the result): %s", paste(row_strings, collapse = " ")))
  }

  return(result)
}
