#' Get OSM routes geometry considering gtfs:shape_id match
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param q osmdata::opq. Overpass query for transit network.
#' @param ways boolean (Default False). If true, relation is disaggregated in ways.
#' @param ways_tags character vector (Default \code{c("lanes", "psv", "bus", "way", "parking", "name")}). List of OSM way tags to extract when \code{ways} parameter is set to true. Match is done using \code{tidyselect::contains()}.
#' @param osm_file character (Optional). Location of OSM extract file with \code{osm.pbf} format. Refer to \code{osmextract::oe_download()} for more details. If not provided OSM Overpass API is called through \code{osmdata::osmdata_sf()}.
#' @param osm_route_type character (Default "bus"). OSM route type. Used to query OSM network (e.g., 'bus', 'train').
#'
#' @details
#' For each route, matches its trips' shapes with OSM route relations, considering the
#' OSM \code{gtfs:shape_id} attribute.
#'
#' @returns A \code{sf} \code{data.frame} with the following columns:
#' \describe{
#'   \item{shape_id}{The \code{shape_id} attribute from \code{shapes.txt} file.}
#'   \item{osm_id}{The \code{osm_id} attribute from OSM route relation.}
#'   \item{way_osm_id}{The \code{osm_id} attribute from OSM way (if \code{ways} parameter is set to true).}
#'   \item{*}{Any column that matches \code{ways_tags} parameter.}
#'   \item{geometry}{The geometrical data for the OSM route relation.}
#' }
#'
#' Shapes that do not have a match on OSM are ignored.
#' If that occurs, a warning is displayed during the method execution, informing about the missing geometries.
#'
#' @examples
#' # Subset GTFS for one route only, for demo purposes
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples", "gtfs_tcb_sample.zip", package = "GTFShift"))
#' gtfs <- GTFShift::filter_by_route_name(gtfs, c("1", "2", "3", "4"))
#'
#' # Build query and prepare osm extract (possible to use API as alternative)
#' q <- osmdata::opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |>
#'   osmdata::add_osm_feature(key = "route", value = "bus") |>
#'   osmdata::add_osm_feature(key = "operator", value = "Transportes Colectivos do Barreiro")
#' osm_file <- system.file("extdata/samples", "osmextract_tcb_network.pbf", package = "GTFShift")
#'
#' # Get OSM route geometries based on gtfs:shape_id match
#' shapes_osm_routes <- GTFShift::osm_shapes_to_routes(
#'   gtfs, q,
#'   osm_file = osm_file
#' )
#'
#' head(shapes_osm_routes |> dplyr::select(shape_id, osm_id))
#' 
#' nrow(shapes_osm_routes)
#' 
#' # Get OSM ways instead
#' shapes_osm_ways <- GTFShift::osm_shapes_to_routes(
#'   gtfs, q,
#'   osm_file = osm_file,
#'   ways = TRUE
#' )
#' 
#' head(shapes_osm_ways |> dplyr::select(way_osm_id, shape_id, osm_id))
#' 
#' nrow(shapes_osm_ways)
#' 
#' @import osmdata
#' @import sf
#' @import dplyr
#' @import progress
#' @import callr
#'
#' @export
osm_shapes_to_routes <- function(
  gtfs, q,
  ways = FALSE, ways_tags = c("lanes", "psv", "bus", "way", "parking", "name"),
  osm_file = NULL,
  osm_route_type = "bus"
) {
  total_steps <- 2 + ways
  if (!is.null(osm_file)) {
    total_steps <- total_steps - 1
  }
  relations_df <- NULL
  osm_ways <- NULL

  # 1. Fetch OSM data as XML
  pb <- progress::progress_bar$new( # Track progress
    format = sprintf(
      ifelse(
        is.null(osm_file),
        "1/%d: Fetching OSM data [:bar] :percent :spin elapsed=:elapsed",
        "1/%d: Getting and parsing OSM file [:bar] :percent :spin elapsed=:elapsed"
      ),
      total_steps
    ),
    clear = FALSE, show_after = 0
  )
  pb$update(0)

  initial_osm_file <- osm_file
  if (!is.null(osm_file)) {
    # 1.1. Get relations
    relations_df <- get_osm_relations(osm_file, q, pb, osm_route_type, 0.1, 0.2, 0.3, 0.94) |>
      filter(type == "way") |>
      rename(way_osm_id = osm_id, osm_id = relation_osm_id)

    # 1.3. Get geometries and filter by matched relations
    bbox <- st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))
    osm_ways <- osmextract::oe_read(osm_file, boundary = bbox, quiet = TRUE)
    pb$update(0.95)
    osm_multilines_redux <- relations_df |>
      select(osm_id, way_osm_id, `gtfs:shape_id`) |>
      # Join with osm_ways to get geometries back
      left_join(osm_ways |> select(osm_id), by = c("way_osm_id" = "osm_id")) |>
      st_as_sf()

    if (!ways) {
      # Group by osm_id, gtfs:shape_id, generating multilinestring with geometries
      pb$update(0.99)
      osm_multilines_redux <- osm_multilines_redux |>
        dplyr::group_by(osm_id, `gtfs:shape_id`) |>
        dplyr::summarise(do_union = FALSE, .groups = "drop") |>
        sf::st_cast("MULTILINESTRING")
    }
  } else {
    osm_file <- tempfile(fileext = ".osm", tmpdir = tempdir(check = TRUE))

    job <- callr::r_bg(function(q, osm_file) { # update spinner while blocking method call
      osmdata::osmdata_xml(q, filename = osm_file, quiet = FALSE)
    }, args = list(q, osm_file))
    while (job$is_alive()) {
      pb$tick(0)
      Sys.sleep(0.1)
    }
    job$get_result() # This will throw any error that occurred in the subprocess (e.g., timeout)

    pb$update(0.5)

    # 2. Convert to SF and Extract routes
    job <- callr::r_bg(function(q, osm_file) { # update spinner while blocking method call
      return(osmdata::osmdata_sf(q, osm_file))
    }, args = list(q, osm_file))
    while (job$is_alive()) {
      pb$tick(0)
      Sys.sleep(0.1)
    }
    osm <- job$get_result()

    pb$update(0.75)

    osm_multilines <- osm$osm_multilines
    osm_multilines_redux <- osm_multilines |>
      select(any_of(c("osm_id", "gtfs:shape_id")))
  }

  pb$update(1)
  pb$terminate()

  # 3. Merge with GTFS
  shape_ids <- gtfs$trips |>
    select(shape_id) |>
    distinct()
  pb <- progress::progress_bar$new( # Track progress
    format = sprintf("2/%d: Matching %d shapes with %s routes [:bar] :percent :spin elapsed=:elapsed", total_steps, nrow(shape_ids), nrow(osm_multilines_redux)),
    clear = FALSE, show_after = 0
  )
  pb$update(0)

  result <- shape_ids |>
    inner_join(osm_multilines_redux |> select(any_of(c("osm_id", "way_osm_id", "gtfs:shape_id", "geometry"))), by = c("shape_id" = "gtfs:shape_id")) |>
    st_as_sf()

  pb$update(1)
  pb$terminate()

  # If relation disaggregation
  if (ways && is.null(initial_osm_file)) {
    # 4. Processing OSM relations (already have the file!)
    pb <- progress::progress_bar$new( # Track progress
      format = sprintf("3/%d: Matching OSM routes with ways [:bar] :percent :spin elapsed=:elapsed", total_steps),
      clear = FALSE, show_after = 0
    )
    pb$update(0)

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
    }, args = list(osm_file))
    while (job$is_alive()) {
      pb$tick(0)
      Sys.sleep(0.1)
    }
    relations_df <- job$get_result()
    ways_relations <- relations_df |>
      filter(type == "way") |>
      select(ref, relation_osm_id) # ref is way osm_id

    # 4.2. Disaggregate relations in ways
    result <- result |>
      sf::st_drop_geometry() |>
      left_join(ways_relations |> rename(way_osm_id = ref, osm_id = relation_osm_id), by = "osm_id") |>
      left_join(as_tibble(osm$osm_lines) |> select(osm_id, contains(ways_tags)), by = c("way_osm_id" = "osm_id"))

    geom <- osm$osm_lines$geometry
    names(geom) <- NULL
    result$geometry <- geom[match(result$way_osm_id, osm$osm_lines$osm_id)]
    result <- sf::st_as_sf(result, sf_column_name = "geometry", crs = st_crs(osm$osm_lines))

    pb$update(1)
    pb$terminate()
  } else if (ways && !is.null(initial_osm_file) && !is.null(ways_tags) && length(ways_tags) > 0) {
    ways_other_tags <- osmextract::oe_get_keys(osm_ways)
    # Filter ways_other_tags for elements that contain any of strings in ways_tags
    tags_to_extract <- ways_other_tags[Reduce(`|`, lapply(ways_tags, function(t) grepl(t, ways_other_tags)))]
    osm_extra_tags <- osmextract::oe_read(osm_file, boundary = bbox, quiet = TRUE, extra_tags = tags_to_extract) |> st_drop_geometry()
    names(osm_extra_tags)[names(osm_extra_tags) != "osm_id"] <- gsub("_", ":", names(osm_extra_tags)[names(osm_extra_tags) != "osm_id"])
    result <- result |>
      left_join(osm_extra_tags |> select(-`other:tags`), by = c("way_osm_id" = "osm_id"))
    # Remove columns that only have empty values
    result <- result |>
      dplyr::select(dplyr::where(~ {
        if (inherits(.x, "sfc")) {
          return(TRUE)
        }
        any(!is.na(.x) & (if (is.character(.x)) .x != "" else TRUE))
      }))
  }

  # 4. Log missing shapes/routes
  routes_shapes <- gtfs$routes |>
    select(route_id, route_short_name, route_long_name) |>
    right_join(gtfs$trips |> select(trip_id, route_id, shape_id), by = "route_id") |>
    distinct(route_id, shape_id, .keep_all = TRUE)

  shapes_matched_n <- result |>
    distinct(shape_id) |>
    nrow()
  shapes_gtfs_n <- gtfs$shapes |>
    distinct(shape_id) |>
    nrow()
  routes_matched_n <- routes_shapes |>
    filter(shape_id %in% result$shape_id) |>
    distinct(route_id) |>
    nrow()
  routes_gtfs_n <- gtfs$routes |>
    distinct(route_id) |>
    nrow()

  message(sprintf(
    "Matched %d shapes (%.2f%% of %d in GTFS) of %d routes (%.2f%% of %d in GTFS) with OSM routes!",
    shapes_matched_n, shapes_matched_n / shapes_gtfs_n * 100, shapes_gtfs_n,
    routes_matched_n, routes_matched_n / routes_gtfs_n * 100, routes_gtfs_n
  ))
  routes_shapes_missing <- routes_shapes |> filter(!(shape_id %in% result$shape_id))
  if (nrow(routes_shapes_missing) > 0) {
    row_strings <- with(routes_shapes_missing, sprintf("| %s | %s | %s | %s |", route_id, shape_id, route_short_name, route_long_name))
    warning(sprintf("Shapes missing (ignored in the result):\n| route_id | shape_id | route_short_name | route_long_name |\n%s", paste(row_strings, collapse = "\n")))
  }

  return(result)
}
