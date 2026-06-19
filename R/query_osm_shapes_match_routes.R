#' Get OSM routes that match shapes, based on geometrical match
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param q osmdata::opq. Overpass query for transit network
#' @param geometry Boolean (Default TRUE). If TRUE, returns sf object with geometry, otherwise, a simple data.frame.
#' @param gtfs_match String (Default route_short_name). routes.txt attribute that identifies routes. Accepted values: route_id, route_short_name, route_long_name.
#' @param osm_match String (Default ref). OSM attribute that identifies routes by matching with gtfs_match. Accepted values: ref, name, gtfs:route_id.
#' @param gtfs_osm_match_exact Boolean (Default TRUE). If TRUE, gtfs and route names are matched strictly. Otherwise, partial string match is considered (all words in gtfs_match must be in osm_match, ignoring case).
#' @param log_file String (Optional). If provided, will log warnings to this file, in addition to the console.
#' @param osm_file character (Optional). Location of OSM extract file with \code{osm.pbf} format. Refer to \code{osmextract::oe_download()} for more details. If not provided OSM Overpass API is called through \code{osmdata::osmdata_sf()}.
#' @param num_cores Integer (Default 1). Number of cores to use for parallel computation. Only supported on Unix-like systems (Linux, macOS).
#' @param osm_stop_order_relaxed Boolean (Default FALSE). If TRUE, OSM routes with entry/exit stops not respecting the right order will still be matched (this may indicate OSM data integrity problems). If FALSE, these routes will be ignored.
#' @param osm_route_type character (Default "bus"). OSM route type. Used to query OSM network (e.g., 'bus', 'train').
#'
#' @details
#' For each route, matches its trips' shapes with OSM route relations.
#'
#' The matching algorithm is formulated as follows:
#' Let \eqn{R} be a GTFS route identifier.
#'
#' \bold{1. Filtering and Base Data Selection:}
#' Let \eqn{\mathcal{O}_R = \{O_1, \dots, O_m\}} be the set of candidate OSM route relations matching the identifier \eqn{R}
#' (based on \code{gtfs_match} and \code{osm_match}).
#' If \eqn{\mathcal{O}_R} is empty, route \eqn{R} is skipped.
#' Unless \code{osm_stop_order_relaxed = TRUE}, any relation in \eqn{\mathcal{O}_R} with entry/exit stops not in the correct order is discarded.
#' We also retrieve the set of GTFS shapes associated with route \eqn{R}, denoted as \eqn{\mathcal{S}_R = \{S_1, \dots, S_n\}}.
#'
#' \bold{2. Feature Extraction:}
#' For each GTFS shape \eqn{S_i \in \mathcal{S}_R}:
#' \itemize{
#'   \item Extract the start and end coordinates of its trips' first and last stops: \eqn{\text{init}_{GTFS, i}} and \eqn{\text{fin}_{GTFS, i}}.
#'   \item Compute the shape's total length \eqn{L_{GTFS, i}} and the number of stop times \eqn{N_{stops, i}}.
#' }
#' For each candidate OSM route relation \eqn{O_j \in \mathcal{O}_R}:
#' \itemize{
#'   \item Extract the coordinates of the first and last stops/platforms: \eqn{\text{init}_{OSM, j}} and \eqn{\text{fin}_{OSM, j}}.
#'   \item Compute the relation's geometry length \eqn{L_{OSM, j}} and the number of stop/platform nodes \eqn{N_{stops, j}}.
#' }
#'
#' \bold{3. Closeness Metric Evaluation:}
#' For each GTFS shape \eqn{S_i}, we calculate the closeness metric \eqn{C(i, j)} for all candidate OSM routes \eqn{O_j \in \mathcal{O}_R}:
#' \deqn{C(i, j) = d(\text{init}_{GTFS, i}, \text{init}_{OSM, j}) + d(\text{fin}_{GTFS, i}, \text{fin}_{OSM, j}) + |L_{GTFS, i} - L_{OSM, j}| + \frac{L_{GTFS, i}}{N_{stops, i}} \cdot |N_{stops, i} - N_{stops, j}|}{C(i, j) = d(init_GTFS,i, init_OSM,j) + d(fin_GTFS,i, fin_OSM,j) + |L_GTFS,i - L_OSM,j| + (L_GTFS,i / N_stops,i) * |N_stops,i - N_stops,j|}
#'
#' where:
#' \itemize{
#'   \item \eqn{d(\cdot)} is the Euclidean distance.
#'   \item The term \eqn{\frac{L_{GTFS, i}}{N_{stops, i}}}{(L_GTFS,i / N_stops,i)} represents the average distance between stops on the GTFS shape, serving as a scale factor for the difference in the number of stops.
#' }
#' Shape \eqn{S_i} is associated with the OSM route \eqn{O_{j^*}} that minimizes the closeness metric:
#' \deqn{j^* = \operatorname{argmin}_{j} C(i, j)}{j* = argmin_j C(i, j)}
#'
#' \bold{4. Conflict Resolution:}
#' If multiple GTFS shapes are associated with the same OSM route \eqn{O_j}, only the shape \eqn{S_i} that minimizes the closeness metric is retained. The other conflicting shapes are ignored and a warning is triggered.
#'
#'
#' Be aware that the result might ignore some GTFS routes, in the following cases:
#' \itemize{
#'  \item If there is no OSM route relation that matches the GTFS route identifier;
#'  \item If, for a GTFS route, there is any OSM route relation that has entry/exit stops not respecting the right order;
#'  \item If, for the same route, distinct shapes are associated to the same OSM route.
#' }
#' If any of these errors occurs, warnings will be thrown at end of the method execution, and those GTFS route will be ignored in the results.
#'
#' Nevertheless, provided there are enough OSM routes, all the GTFS shapes for each route will necessarily be associated with
#' an OSM one. This might generate wrong results if the topology of routes on OSM does not match the GTFS shapes for that route.
#' Refer to  \code{distance_diff}, \code{points_diff} and \code{stops_diff} on the results table to validate the results and identify misassociations.
#'
#' @returns A \code{data.frame} (\code{sf} if \code{geometry=TRUE}) with the following columns:
#' \describe{
#'   \item{route_id}{The \code{route_id} attribute from \code{routes.txt} file.}
#'   \item{shape_id}{The \code{shape_id} attribute from \code{shapes.txt} file.}
#'   \item{osm_id}{The \code{osm_id} attribute from OSM route relation.}
#'   \item{distance_diff}{The difference, in meters, between GTFS shape and OSM route lengths.}
#'   \item{points_diff}{The sum of the difference, in meters, between GTFS shape and OSM route start and end points.}
#'   \item{stops_diff}{The difference between GTFS and OSM routes number of stops.}
#'   \item{route_short_name}{The \code{route_short_name} attribute from \code{routes.txt} file.}
#'   \item{route_long_name}{The \code{route_long_name} attribute from \code{routes.txt} file.}
#'   \item{osm_ref}{The \code{ref} attribute from OSM route relation.}
#'   \item{osm_name}{The \code{name} attribute from OSM route relation.}
#'   \item{geometry}{The geometrical data for the OSM route relation.}
#' }
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::load_feed("gtfs.zip")
#'
#' q <- opq("Lisbon") |>
#'   add_osm_feature(key = "route", value = c("bus", "tram")) |>
#'   add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)
#'
#' # To use OSM API:
#' shapes_match_routes <- GTFShift::osm_shapes_match_routes(gtfs, q)
#'
#' # To use a local OSM file:
#' osm_file <- oe_download("https://download.geofabrik.de/europe/portugal-latest.osm.pbf")
#' shapes_match_routes <- GTFShift::osm_shapes_match_routes(gtfs, q, osm_file = osm_file)
#' }
#'
#' @import osmdata
#' @import sf
#' @import dplyr
#' @import stplanr
#' @import xml2
#' @import progress
#' @import callr
#' @import stringi
#' @import parallel
#'
#' @export
osm_shapes_match_routes <- function(
  gtfs, q,
  geometry = TRUE,
  gtfs_match = "route_short_name", osm_match = "ref", gtfs_osm_match_exact = TRUE,
  log_file = NA,
  osm_file = NULL,
  num_cores = 1,
  osm_stop_order_relaxed = FALSE,
  osm_route_type = "bus"
) {
  total_steps <- 4
  if (!is.null(osm_file)) {
    total_steps <- 3
  }

  if (!is.na(log_file)) {
    cat(
      sprintf("-----------------------------\n%s: Running osm_shapes_match_routes() for %s...\n\n", Sys.time(), paste(gtfs$agency$agency_name, collapse = ", ")),
      file = log_file, append = TRUE
    )
  }

  # 0. Validations
  if (!(gtfs_match %in% c("route_id", "route_short_name", "route_long_name"))) {
    stop("gtfs_match should be one of: route_id, route_short_name or route_long_name")
  }
  if (!(osm_match %in% c("ref", "name", "gtfs:route_id"))) {
    stop("osm_match should be one of: ref, name, gtfs:route_id")
  }

  # 1. Get geometry for shapes and stops
  pb <- progress::progress_bar$new( # Track progress
    format = sprintf("1/%d: Preparing GTFS data [:bar] :percent :spin elapsed=:elapsed", total_steps),
    clear = FALSE, show_after = 0
  )
  pb$update(0)
  shapes_sf <- tidytransit::shapes_as_sf(gtfs$shapes)
  stops_sf <- tidytransit::stops_as_sf(gtfs$stops)
  pb$update(1)
  pb$terminate()

  m <- sprintf("> Found %d GTFS shapes and %d stops\n", nrow(shapes_sf), nrow(stops_sf))
  message(m)
  if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)

  # 2. Get OSM data as XML
  pb <- progress::progress_bar$new( # Track progress
    format = sprintf(
      ifelse(
        is.null(osm_file),
        "2/%d: Fetching OSM data [:bar] :percent :spin elapsed=:elapsed",
        "2/%d: Getting and parsing OSM file [:bar] :percent :spin elapsed=:elapsed"
      ),
      total_steps
    ),
    clear = FALSE, show_after = 0
  )
  pb$update(0)

  relations_df <- NULL
  if (!is.null(osm_file)) {
    # 2.1. Get relations
    relations_df <- get_osm_relations(osm_file, q, pb, osm_route_type, 0.12, 0.25, 0.37, 0.49)
    pb$update(0.5)

    # 2.2. Get geometries and filter by matched relations
    # Consider 500 meters outside of shapes to avoid loosing stops on the edge
    bbox <- st_bbox(tidytransit::shapes_as_sf(gtfs$shapes) |> st_transform(3857) |> st_buffer(500))

    osm_ways <- osmextract::oe_read(osm_file, boundary = bbox, quiet = TRUE)
    pb$update(0.75)
    osm_multilines_redux <- relations_df |>
      filter(type == "way") |>
      select(relation_osm_id, osm_id, ref, name, `gtfs:shape_id`, `gtfs:route_id`) |>
      # Join with osm_ways to get geometries back
      left_join(osm_ways |> select(osm_id), by = "osm_id") |>
      st_as_sf() |>
      # Group by osm_id, gtfs:shape_id, generating multilinestring with geometries
      dplyr::group_by(relation_osm_id, ref, name, `gtfs:shape_id`, `gtfs:route_id`) |>
      dplyr::summarise(do_union = FALSE, .groups = "drop") |>
      sf::st_cast("MULTILINESTRING") |>
      rename(osm_id = relation_osm_id)

    # 2.3 Get stop locations
    osm_stops <- osmextract::oe_read(osm_file, layer = "points", boundary = bbox, quiet = TRUE, extra_tags = c("public_transport")) |>
      dplyr::filter(public_transport == "stop_position" | public_transport == "platform") |>
      select(osm_id)
    pb$update(0.99)

    # Remove type==node that has osm_id not in osm_stops
    relations_df <- relations_df |>
      filter(!(type == "node" & !(osm_id %in% osm_stops$osm_id)))

    osm_stoppositions <- relations_df |>
      filter(type == "node") |>
      # Join with osm_stops to get geometries back
      left_join(osm_stops |> select(osm_id), by = "osm_id") |>
      st_as_sf()
    pb$update(1)
    pb$terminate()

    m <- sprintf("> Found %d OSM route relations and %d bus stops/platforms\n", length(unique(relations_df$relation_osm_id)), length(unique(osm_stoppositions$osm_id)))
    message(m)
    if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)
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
    pb$update(0.33)

    # 3. Convert to SF and Extract routes/points
    job <- callr::r_bg(function(q, osm_file) { # update spinner while blocking method call
      return(osmdata::osmdata_sf(q, osm_file))
    }, args = list(q, osm_file))
    while (job$is_alive()) {
      pb$tick(0)
      Sys.sleep(0.1)
    }
    osm <- job$get_result()

    osm_multilines <- osm$osm_multilines
    osm_multilines_redux <- osm_multilines |>
      select(any_of(c("osm_id", "ref", "from", "to", "via", "name", "roundtrip", "gtfs:route_id"))) |>
      distinct(osm_id, .keep_all = TRUE)
    pb$update(0.66)

    st_agr(osm$osm_points) <- "constant" # https://github.com/r-spatial/sf/issues/406
    job <- callr::r_bg(function(osm, osm_multilines_redux) { # update spinner while blocking method call
      return(
        osm$osm_points |>
          sf::st_crop(sf::st_bbox(stplanr::geo_buffer(osm_multilines_redux, dist = 100))) |>
          dplyr::filter(public_transport == "stop_position" | public_transport == "platform") |>
          dplyr::select_if(~ !all(is.na(.)))
      )
    }, args = list(osm, osm_multilines_redux))
    while (job$is_alive()) {
      pb$tick(0)
      Sys.sleep(0.1)
    }
    osm_stoppositions <- job$get_result()
    pb$update(1)
    pb$terminate()
  }


  # 4. Processing OSM relations (already have the file!)
  if (is.null(osm_file)) {
    pb <- progress::progress_bar$new(
      format = sprintf("3/%d: Processing OSM relations [:bar] :percent :spin elapsed=:elapsed", total_steps),
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
            osm_id = xml_attr(member, "ref"),
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

    pb$update(1)
    pb$terminate()
    m <- sprintf("> Found %d OSM route relations and %d bus stops/platforms\n", nrow(osm_multilines_redux), nrow(osm_stoppositions))
    message(m)
    if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)
  }

  # 5. For each gtfs route, match shapes with OSM routes
  routes_names <- unique(gtfs$routes |> pull(!!gtfs_match)) # !! to use variable value and not its literal name

  pb <- progress::progress_bar$new( # Track progress
    format = sprintf("%d/%d: Matching GTFS shapes with OSM routes [:bar] :percent :spin elapsed=:elapsed", total_steps, total_steps),
    clear = FALSE, show_after = 0
  )

  warning_routes_missing <- list() # Warning records
  warning_osm_repeated <- list()
  warning_osm_unsorted_stops <- list()
  warning_osm_stops_missing <- list()

  match_route_worker <- function(route_name) {
    # message("route_name = " %>% paste(route_name))
    # Warning records for this specific route
    warn_routes_missing <- list()
    warn_osm_repeated <- list()
    warn_osm_unsorted_stops <- list()
    warn_osm_stops_missing <- list()

    # 1. Get base data
    # > Filter OSM network
    if (gtfs_osm_match_exact) {
      osm_route_name <- osm_multilines_redux |>
        dplyr::filter(.data[[osm_match]] == route_name)
    } else {
      words <- tolower(strsplit(route_name, "\\s+")[[1]])
      words_norm <- stringi::stri_trans_general(words, "Latin-ASCII")
      osm_route_name <- osm_multilines_redux |>
        dplyr::filter(
          vapply(
            stringi::stri_trans_general(tolower(.data[[osm_match]]), "Latin-ASCII"),
            function(x) all(vapply(words_norm, grepl, logical(1), x = x, fixed = TRUE)),
            logical(1)
          )
        )
    }

    # >> Validate OSM data
    if (nrow(osm_route_name) == 0) { # Validate that there is an OSM match for GTFS route
      warn_routes_missing <- append(warn_routes_missing, route_name)
      return(list(
        res = data.frame(route_name = route_name),
        warn_missing = warn_routes_missing,
        warn_repeated = warn_osm_repeated,
        warn_unsorted = warn_osm_unsorted_stops,
        warn_stops_missing = warn_osm_stops_missing
      ))
    }
    osm_route_error <- FALSE
    for (i in 1:nrow(osm_route_name)) { # Validate that if OSM route has entry/exit stops, they respect the right order
      route <- osm_route_name[i, ]
      relation_df <- relations_df |> dplyr::filter(type == "node" & relation_osm_id == route$osm_id)
      entry_rows <- grep("entry", relation_df$role, ignore.case = TRUE)
      exit_rows <- grep("exit", relation_df$role, ignore.case = TRUE)
      if (length(entry_rows) > 0) { # If entry row exists, validate that is first
        first_entry_row <- head(entry_rows, 1)
        if (first_entry_row != 1) {
          warn_osm_unsorted_stops <- append(warn_osm_unsorted_stops, sprintf("`osm_id` %s (`%s` %s)", route$osm_id, gtfs_match, route_name))
          osm_route_error <- TRUE
        }
      }
      if (length(exit_rows) > 0) { # If exit row exists, validate that is last
        last_exit_row <- tail(exit_rows, 1)
        if (last_exit_row != nrow(relation_df)) {
          warn_osm_unsorted_stops <- append(warn_osm_unsorted_stops, sprintf("`osm_id` %s (`%s` %s)", route$osm_id, gtfs_match, route_name))
          osm_route_error <- TRUE
        }
      }
    }
    if (osm_route_error && !osm_stop_order_relaxed) {
      return(list(
        res = data.frame(route_name = route_name),
        warn_missing = warn_routes_missing,
        warn_repeated = warn_osm_repeated,
        warn_unsorted = warn_osm_unsorted_stops,
        warn_stops_missing = warn_osm_stops_missing
      ))
    }

    # > Filter GTFS
    gtfs_route_name <- gtfs$routes |> # Start on routes.txt to match line number with route_name
      dplyr::select(route_id, route_short_name, route_long_name) |>
      dplyr::filter(.data[[gtfs_match]] == route_name) |>
      dplyr::left_join(gtfs$trips |> dplyr::select(route_id, trip_id, shape_id, direction_id), by = "route_id") |>
      dplyr::filter(!is.na(trip_id)) |>
      dplyr::left_join(shapes_sf, by = "shape_id") |>
      dplyr::distinct(shape_id, .keep_all = TRUE) |>
      sf::st_as_sf()
    if (nrow(gtfs_route_name) == 0) { # In case route does not have trips, nor shapes (no need to log error, as it had no geometries anyway)
      return(list(
        res = data.frame(route_name = route_name),
        warn_missing = warn_routes_missing,
        warn_repeated = warn_osm_repeated,
        warn_unsorted = warn_osm_unsorted_stops,
        warn_stops_missing = warn_osm_stops_missing
      ))
    }

    # 2. Match based on initial and final points
    # > Compute osm final and initial points
    geom_col <- sf::st_geometry(osm_route_name)
    osm_route_name <- tryCatch(
      {
        osm_route_name |>
          dplyr::mutate(
            roundtrip = if (!"roundtrip" %in% names(osm_route_name)) NA else roundtrip,
            route_dist = sf::st_length(geom_col) |> units::drop_units()
          ) |>
          dplyr::rowwise() |>
          dplyr::mutate(
            # Geographical data
            # Other relevant parameters
            nr_stops = { # Consider the number of stops to be the maximum of stops or platforms, because some routes use them mixed and miss some
              nr_s <- nrow(relations_df |> dplyr::filter(relation_osm_id == osm_id & grepl("stop", role)))
              nr_p <- nrow(relations_df |> dplyr::filter(relation_osm_id == osm_id & grepl("platform", role)))
              max(nr_s, nr_p)
            },
            first_stop_osm_id = relations_df |>
              dplyr::filter(type == "node") |>
              dplyr::select(relation_osm_id, stop_osm_id = osm_id, role) |>
              # Consider both stop_entry/exit_only and stop, because circular lines do not have entry/exit, only stop
              dplyr::filter(relation_osm_id == osm_id & role %in% c("stop_entry_only", "stop", "platform_entry_only", "platform")) |>
              # Use sorting to give priority to entry/exit, when they exist
              dplyr::arrange(
                match(role, c("stop_entry_only", "platform_entry_only", "stop", "platform")),
                role
              ) |>
              dplyr::slice(1) |>
              dplyr::pull(stop_osm_id),
            last_stop_osm_id = relations_df |>
              dplyr::filter(type == "node") |>
              dplyr::select(relation_osm_id, stop_osm_id = osm_id, role) |>
              dplyr::filter(relation_osm_id == osm_id & role %in% c("stop_exit_only", "stop", "platform_exit_only", "platform")) |>
              dplyr::mutate(role_group = dplyr::case_when(
                # When roundtrip (circular), keep normal order
                roundtrip == "yes" ~ 1,
                # Otherwise, consider first stop_exit_only or last stop (if no exit_only)
                role == "stop_exit_only" ~ 1, role == "platform_exit_only" ~ 2, role == "stop" ~ 4, role == "platform" ~ 4, TRUE ~ 5
              )) |>
              dplyr::arrange(
                role_group,
                dplyr::case_when(
                  roundtrip == "yes" ~ dplyr::row_number(), # When roundtrip (circular), keep normal order
                  role == "stop_exit_only" ~ dplyr::row_number(), # keep natural order
                  role == "platform_exit_only" ~ dplyr::row_number(), # keep natural order
                  role == "stop" ~ dplyr::desc(dplyr::row_number()), # reverse order
                  role == "platform" ~ dplyr::desc(dplyr::row_number()), # reverse order
                  TRUE ~ dplyr::row_number() # fallback order for others
                )
              ) |>
              dplyr::slice(1) |>
              dplyr::pull(stop_osm_id),
            initial = osm_stoppositions |> dplyr::filter(osm_id == first_stop_osm_id) |> dplyr::slice(1) |> dplyr::pull(geometry) |> dplyr::first(default = NA),
            final = osm_stoppositions |> dplyr::filter(osm_id == last_stop_osm_id) |> dplyr::slice(1) |> dplyr::pull(geometry) |> dplyr::first(default = NA)
          ) |>
          dplyr::ungroup() |>
          dplyr::select(osm_id, ref, name, route_dist, nr_stops, first_stop_osm_id, last_stop_osm_id, initial, final, geometry) |>
          dplyr::arrange(route_dist)
      },
      error = function(e) {
        warn_osm_stops_missing <- append(warn_osm_stops_missing, sprintf("`osm_id` %s (`%s` %s)", paste(osm_route_name$osm_id, collapse = ", "), gtfs_match, route_name))
        return(NULL)
      }
    )
    if (is.null(osm_route_name)) {
      return(list(
        res = data.frame(route_name = route_name),
        warn_missing = warn_routes_missing,
        warn_repeated = warn_osm_repeated,
        warn_unsorted = warn_osm_unsorted_stops,
        warn_stops_missing = warn_osm_stops_missing
      ))
    }

    # > Same for GTFS shapes
    geom_col <- sf::st_geometry(gtfs_route_name)
    gtfs_route_name <- gtfs_route_name |>
      dplyr::mutate(route_dist = sf::st_length(geom_col) |> units::drop_units()) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        # Geographical data
        trip_id_copy = trip_id,
        first_stop_id = gtfs$stop_times |> dplyr::filter(trip_id == trip_id_copy) |> dplyr::arrange(stop_sequence) |> dplyr::slice(1) |> dplyr::pull(stop_id),
        last_stop_id = gtfs$stop_times |> dplyr::filter(trip_id == trip_id_copy) |> dplyr::arrange(dplyr::desc(stop_sequence)) |> dplyr::slice(1) |> dplyr::pull(stop_id),
        initial = stops_sf |> dplyr::filter(stop_id == first_stop_id) |> dplyr::slice(1) |> dplyr::pull(geometry),
        final = stops_sf |> dplyr::filter(stop_id == last_stop_id) |> dplyr::slice(1) |> dplyr::pull(geometry),

        # Other relevant parameters
        nr_stops = nrow(gtfs$stop_times |> dplyr::filter(trip_id == trip_id_copy))
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-trip_id_copy) |>
      dplyr::arrange(route_dist, initial, final)

    # 3. Match gtfs shapes and osm routes, by choosing the one that share the closest start and end points
    # >  Compute distances between init and final points for both
    init <- units::drop_units(sf::st_distance(osm_route_name$initial, gtfs_route_name$initial))
    fin <- units::drop_units(sf::st_distance(osm_route_name$final, gtfs_route_name$final))
    length_diff <- sapply(gtfs_route_name$route_dist, function(y) abs(osm_route_name$route_dist - y))
    # Proxy for number of stops distance: average distance between stops on GTFS, times the difference between osm and gtfs stops
    stops_diff <- sapply(
      seq_along(gtfs_route_name$nr_stops),
      function(i) {
        (gtfs_route_name$route_dist[i] / gtfs_route_name$nr_stops[i]) *
          abs(osm_route_name$nr_stops - gtfs_route_name$nr_stops[i])
      }
    )

    # > Match OSM network and GTFS shapes considering the match with min aggregated distance (init + fin)
    closeness <- abs(init + fin + length_diff + stops_diff)

    gtfs_route_name_minimos <- gtfs_route_name |>
      dplyr::mutate(osm_id = NA)

    for (i in 1:nrow(gtfs_route_name_minimos)) {
      gtfs_route_name_minimos[i, ]$osm_id <- osm_route_name[which.min(closeness[, i]), ]$osm_id
    }

    gtfs_route_name_result <- gtfs_route_name_minimos |>
      sf::st_drop_geometry() |>
      dplyr::left_join(
        osm_route_name |> dplyr::select(osm_id, name, ref, route_dist, nr_stops, geometry, initial, final) |> dplyr::rename(osm_name = name, osm_ref = ref),
        by = "osm_id",
        suffix = c("_gtfs", "_osm")
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        distance_diff = abs(route_dist_gtfs - route_dist_osm),
        points_diff = as.numeric(units::drop_units(sf::st_distance(initial_osm, initial_gtfs)) + units::drop_units(sf::st_distance(final_osm, final_gtfs))),
        stops_diff = abs(nr_stops_gtfs - nr_stops_osm)
      ) |> # absolute difference
      dplyr::ungroup() |>
      dplyr::select(-initial_gtfs, -final_gtfs) |>
      sf::st_as_sf(sf_column_name = "geometry")

    # When multiple osm_id, return those with min distance_diff + points_diff + then stops_diff
    if (length(unique(gtfs_route_name_result$osm_id)) < nrow(gtfs_route_name_result)) {
      gtfs_route_name_result_unique <- gtfs_route_name_result |>
        dplyr::group_by(osm_id) |>
        dplyr::slice_min(order_by = distance_diff + points_diff + stops_diff, with_ties = FALSE) |>
        dplyr::ungroup()

      warn_osm_repeated <- append(warn_osm_repeated, sprintf(
        "`%s` %s has %d shapes, but the geometrical match returned only %d (out of %d) OSM routes\n>> `osm_id` for route: %s\n>> The ignored ones were: %s\n>> The duplicated ones were: %s\n>> Returning shapes that have greatest geometrical match: %s\n>> Shapes ignored: %s",
        gtfs_match, route_name, nrow(gtfs_route_name),
        length(unique(gtfs_route_name_result$osm_id)), nrow(osm_route_name),
        paste(osm_route_name$osm_id, collapse = ", "),
        # osm ignored
        paste(setdiff(
          union(gtfs_route_name_result$osm_id, osm_route_name$osm_id),
          intersect(gtfs_route_name_result$osm_id, osm_route_name$osm_id)
        ), collapse = ", "),
        # osm duplicated
        paste(unique(gtfs_route_name_result$osm_id[duplicated(gtfs_route_name_result$osm_id)]), collapse = ", "),
        # shapes returned
        paste(gtfs_route_name_result_unique$shape_id, collapse = ", "),
        # shapes ignored
        paste(setdiff(gtfs_route_name$shape_id, gtfs_route_name_result_unique$shape_id), collapse = ", ")
      ))
      gtfs_route_name_result <- gtfs_route_name_result_unique
    }

    # Memory optimization: drop heavy columns before returning to the main process
    # Geometries and auxiliary points will be re-joined later
    gtfs_route_name_result <- gtfs_route_name_result |>
      sf::st_drop_geometry() |>
      dplyr::select(-any_of(c("initial_osm", "final_osm", "initial_gtfs", "final_gtfs", "osm_name", "osm_ref")))

    return(list(
      res = gtfs_route_name_result,
      warn_missing = warn_routes_missing,
      warn_repeated = warn_osm_repeated,
      warn_unsorted = warn_osm_unsorted_stops,
      warn_stops_missing = warn_osm_stops_missing
    ))
  }

  if (num_cores > 1 && .Platform$OS.type != "windows") {
    m <- sprintf("> Matching routes in parallel using %d cores...\n", num_cores)
    message(m)
    if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)

    results_list <- parallel::mclapply(routes_names, match_route_worker, mc.cores = num_cores)
  } else {
    results_list <- lapply(routes_names, function(route_name) {
      pb$update(min(head(match(route_name, routes_names), 1) / length(routes_names), 0.99)) # update progress
      match_route_worker(route_name)
    })
  }

  # 6. Unpack results
  warning_routes_missing <- do.call(c, lapply(results_list, `[[`, "warn_missing"))
  warning_osm_repeated <- do.call(c, lapply(results_list, `[[`, "warn_repeated"))
  warning_osm_unsorted_stops <- do.call(c, lapply(results_list, `[[`, "warn_unsorted"))
  warning_osm_stops_missing <- do.call(c, lapply(results_list, `[[`, "warn_stops_missing"))

  message("> Unpacking results\n")
  result <- lapply(results_list, `[[`, "res")
  rm(results_list)
  gc()

  message("> Combining results\n")
  if (length(result[lengths(result) > 1]) == 1) {
    result_success <- result[[which(lengths(result) > 1)]]
  } else {
    result_success <- bind_rows(result[lengths(result) > 1])
  }
  pb$update(1)
  pb$terminate()

  # 7. Re-attach OSM metadata and geometries (dropped in workers to save RAM)
  if (nrow(result_success) > 0) {
    message("> Re-attaching OSM metadata and geometries\n")
    result_success <- result_success |>
      dplyr::left_join(
        osm_multilines_redux |>
          sf::st_drop_geometry() |>
          dplyr::select(osm_id, osm_name = name, osm_ref = ref) |>
          dplyr::distinct(osm_id, .keep_all = TRUE),
        by = "osm_id"
      ) |>
      dplyr::left_join(
        osm_multilines_redux |> dplyr::select(osm_id, geometry),
        by = "osm_id"
      ) |>
      sf::st_as_sf()
  }

  # 5. Give user feedback on processing

  # > Get route metadata
  message("> Getting route metadata\n")
  route_shapes <- gtfs$routes |>
    left_join(gtfs$trips, by = "route_id") |>
    group_by(across(any_of(c("route_id", "route_short_name", "trip_headsign", "trip_short_name", "direction_id", "shape_id")))) |>
    summarise(
      n_trips = n(),
      .groups = "drop_last"
    ) |>
    ungroup()


  # > Output success message
  if (nrow(result_success) > 0) {
    result_success <- result_success |> left_join(route_shapes |> select(
      -any_of(names(result_success)), shape_id # Avoid duplicate columns
    ), by = "shape_id")
    m <- sprintf(
      "> Associated %d shapes (%.2f%% of %d total) of %d routes (%.2f%% of %d total) with OSM routes, corresponding to %d trips (%.2f%% of %d total), with a mean distance of %.2f meters for points, %.2f meters for route length and a mean difference of %.2f stops\n",
      # shapes
      nrow(result_success),
      nrow(result_success) / nrow(shapes_sf) * 100,
      nrow(shapes_sf),
      # routes
      nrow(result_success |> distinct(route_id)),
      nrow(result_success |> distinct(route_id)) / length(unique(gtfs$routes$route_id)) * 100,
      length(unique(gtfs$routes$route_id)),
      # trips
      sum(result_success$n_trips),
      sum(result_success$n_trips) / sum(route_shapes$n_trips) * 100,
      sum(route_shapes$n_trips),
      # heuristics
      mean(result_success$points_diff),
      mean(result_success$distance_diff),
      mean(result_success$stops_diff)
    )
    message(m)
    if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)

    m <- sprintf(
      "> Of those, %d shapes (%.2f%% of %d matched) have a distance difference below 1000 meters AND a points difference below 500 meters\n",
      nrow(result_success |> filter(distance_diff < 1000 & points_diff < 500)),
      nrow(result_success |> filter(distance_diff < 1000 & points_diff < 500)) / nrow(result_success) * 100,
      nrow(result_success)
    )
    message(m)
    if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)
  }

  # > Output error messages
  not_found <- bind_rows(result[lengths(result) <= 1])
  routes_shapes_n <- gtfs$routes |> # Start on routes.txt to match line number with route_name
    select(route_id, !!gtfs_match) |>
    left_join(gtfs$trips |> select(route_id, trip_id, shape_id, direction_id), by = "route_id") |>
    filter(!is.na(trip_id)) |> # Ignore routes without trips, because they do not have shapes, so they are not expected to be matched with OSM routes (and thus, not expected to be in the results, so no need to log them as errors)
    left_join(shapes_sf, by = "shape_id") |>
    distinct(.data[[gtfs_match]], shape_id) |>
    group_by(.data[[gtfs_match]]) |>
    summarise(shapes_n = n())
  partial_match <- result_success |>
    st_drop_geometry()
  if (nrow(partial_match)) {
    partial_match <- partial_match |>
      group_by(.data[[gtfs_match]]) |>
      summarise(shapes_n = n()) |>
      left_join(routes_shapes_n, by = gtfs_match) |>
      rename(matched = shapes_n.x, gtfs = shapes_n.y) |>
      filter(matched < gtfs)
  }

  warning_osm_unsorted_stops <- unique(warning_osm_unsorted_stops) # This warning list can have duplicates, ignore
  errors <- length(warning_routes_missing) + length(warning_osm_repeated) + length(warning_osm_unsorted_stops) + length(warning_osm_stops_missing)
  if (errors > 0) {
    w <- sprintf(
      "There were %d error(s) during the algorithm execution",
      errors
    )
    warning(w)
    if (!is.na(log_file)) cat(paste("WARNING! ", w, "\n"), file = log_file, append = TRUE)
  }
  if (nrow(not_found)) {
    w <- sprintf(
      "These led to %d route(s) without a match (route(s) ignored), with the following `%s`:\n\n> %s",
      nrow(not_found),
      gtfs_match,
      paste(not_found$route_name, collapse = "\n> ")
    )
    warning(w)
    if (!is.na(log_file)) cat(paste("WARNING! ", w, "\n"), file = log_file, append = TRUE)
  }
  if (nrow(partial_match)) {
    w <- sprintf(
      "%d routes had partial matches (only some of its shapes had a match):\n\n> %s",
      nrow(partial_match),
      paste(partial_match[[gtfs_match]], " (matched ", partial_match[["matched"]], " of ", partial_match[["gtfs"]], " shapes)", collapse = "\n> ", sep = "")
    )
    warning(w)
    if (!is.na(log_file)) cat(paste("WARNING! ", w, "\n"), file = log_file, append = TRUE)
  }
  if (length(warning_routes_missing) > 0) {
    w <- sprintf("%d error(s) were GTFS routes `%s` that did not match any OSM route `%s`:\n\n> %s\n", length(warning_routes_missing), gtfs_match, osm_match, paste(
      warning_routes_missing,
      collapse = "\n> "
    ))
    warning(w)
    if (!is.na(log_file)) cat(paste("WARNING! ", w, "\n"), file = log_file, append = TRUE)
  }
  if (length(warning_osm_repeated) > 0) {
    w <- sprintf("%d error(s) were GTFS routes that had multiple shapes associated to the same osm route (routes ignored):\n(This might indicate a mismatch between GTFS and OSM data)\n\n> %s\n", length(warning_osm_repeated), paste(
      warning_osm_repeated,
      collapse = "\n>\n "
    ))
    warning(w)
    if (!is.na(log_file)) cat(paste("WARNING! ", w, "\n"), file = log_file, append = TRUE)
  }
  if (length(warning_osm_unsorted_stops) > 0) {
    w <- sprintf(
      ifelse(osm_stop_order_relaxed,
        "%d error(s) were OSM routes that had entry/exit stops not respecting the right order (routes were still matched, but this might indicate OSM data integrity problems)\n\n> %s",
        "%d error(s) were OSM routes that had entry/exit stops not respecting the right order (routes ignored):\n(This might indicate OSM data integrity problems)\n\n> %s"
      ),
      length(warning_osm_unsorted_stops),
      paste(
        warning_osm_unsorted_stops,
        collapse = "\n> "
      )
    )
    warning(w)
    if (!is.na(log_file)) cat(paste("WARNING! ", w, "\n"), file = log_file, append = TRUE)
  }
  if (length(warning_osm_stops_missing) > 0) {
    w <- sprintf("%d error(s) were GTFS routes that matched OSM routes with inconsistent stops  (routes ignored):\n(This might indicate OSM data integrity problems)\n\n> %s", length(warning_osm_stops_missing), paste(
      warning_osm_stops_missing,
      collapse = "\n> "
    ))
    warning(w)
    if (!is.na(log_file)) cat(paste("WARNING! ", w, "\n"), file = log_file, append = TRUE)
  }

  if (nrow(result_success) == 0) {
    m <- "> No shapes were matched with OSM routes!\n"
    message(m)
    if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)
    return(if (geometry) st_sf(st_sfc()) else data.frame())
  }

  result_success <- result_success |> select(
    any_of(c(
      "route_id", "route_short_name", "route_long_name",
      "shape_id", "trip_headsign", "trip_short_name", "direction_id",
      "osm_id", "osm_name", "osm_ref",
      "distance_diff", "points_diff", "stops_diff",
      "geometry"
    ))
  )

  if (!geometry) {
    return(result_success |> st_drop_geometry())
  }
  return(result_success)
}
