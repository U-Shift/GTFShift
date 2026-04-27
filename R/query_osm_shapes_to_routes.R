#' Get OSM routes geometry considering gtfs:shape_id match
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param q osmdata::opq. Overpass query for transit network.
#' @param ways boolean (Default False). If true, relation is disaggregated in ways.
#' @param ways_tags character vector (Default \code{c("lanes", "psv", "bus", "way", "parking", "name")}). List of OSM way tags to extract when \code{ways} parameter is set to true. Match is done using \code{tidyselect::contains()}.
#' @param sleep_duration Numeric (Default 30). Time to sleep, in seconds, before fetching OSM data to avoid overloading the server.
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
#' q <- opq("Lisbon") |>
#'   add_osm_feature(key = "route", value = c("bus", "tram")) |>
#'   add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)
#'
#' shapes_geometry_osm <- GTFShift::osm_shapes_to_routes(gtfs, q)
#' }
#'
#' @import osmdata
#' @import sf
#' @import dplyr
#' @import progress
#' @import callr
#'
#' @export
osm_shapes_to_routes <- function(gtfs, q, ways = FALSE, ways_tags = c("lanes", "psv", "bus", "way", "parking", "name"), osm_file = NULL) {
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

  if (!is.null(osm_file)) {
    # 1.1. Get relations XML
    bus_relations_pbf <- tempfile(fileext = ".osm.pbf")

    job <- callr::r_bg(function(bus_relations_pbf, osm_file) { # update spinner while blocking method call
      return(rosmium::tags_filter(
        osm_file,
        "nwr/route=bus",
        output = bus_relations_pbf,
        overwrite = TRUE
      ))
    }, args = list(bus_relations_pbf, osm_file))
    while (job$is_alive()) {
      pb$tick(0)
      Sys.sleep(0.1)
    }
    job$get_result()
    pb$update(0.1)

    bus_relations_xml <- rosmium::show_content(
      bus_relations_pbf,
      object_type = c("relation"),
      output_format = "xml",
      preview = FALSE,
      spinner = FALSE
    )
    pb$update(0.2)

    # 1.2. Filter relations using q$features and extract way members
    doc <- xml2::read_xml(bus_relations_xml)
    relations <- xml2::xml_find_all(doc, ".//relation")

    # > Extract filter criteria from q$features
    features_str <- q$features
    feature_regex <- '\\["([^"]+)"([=~])"([^"]+)"\\]'
    feature_matches <- regmatches(features_str, gregexpr(feature_regex, features_str))[[1]]
    parsed_features <- lapply(feature_matches, function(f) {
      m <- regexec(feature_regex, f)
      parts <- regmatches(f, m)[[1]]
      list(key = parts[2], op = parts[3], val = parts[4])
    })

    pb$update(0.3)
    rel_n <- 0
    relations_data <- lapply(relations, function(rel) {
      rel_n <<- rel_n + 1
      pb$update(min(round(0.3 + (0.6 * rel_n / length(relations)), digits = 2), 1))
      tags <- xml2::xml_find_all(rel, ".//tag")
      tag_keys <- xml2::xml_attr(tags, "k")
      tag_vals <- xml2::xml_attr(tags, "v")
      names(tag_vals) <- tag_keys

      # Check if relation matches all features in q
      matches_all <- all(sapply(parsed_features, function(feat) {
        val <- tag_vals[feat$key]
        if (is.na(val)) {
          return(FALSE)
        }
        if (feat$op == "=") {
          return(val == feat$val)
        }
        if (feat$op == "~") {
          return(grepl(feat$val, val))
        }
        return(FALSE)
      }))

      if (!matches_all) {
        return(NULL)
      }

      way_members <- xml2::xml_find_all(rel, ".//member[@type='way']")
      if (length(way_members) == 0) {
        return(NULL)
      }

      data.frame(
        relation_osm_id = xml2::xml_attr(rel, "id"),
        type = "way",
        ref = xml2::xml_attr(way_members, "ref"),
        role = xml2::xml_attr(way_members, "role"),
        `gtfs:shape_id` = tag_vals["gtfs:shape_id"],
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    relations_df <- dplyr::bind_rows(relations_data) |>
      rename(osm_id = relation_osm_id, way_osm_id = ref)

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
  if (ways && is.null(osm_file)) {
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
  } else if (ways && !is.null(ways_tags) && length(ways_tags) > 0) {
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
