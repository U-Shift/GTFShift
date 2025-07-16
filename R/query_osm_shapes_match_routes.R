#' Get OSM routes that match shapes, based on geometrical match
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param q osmdata::opq. Overpass query for transit network
#' @param geometry Boolean (Default TRUE). If TRUE, returns sf object with geometry, otherwise, a simple data.frame.
#' @param gtfs_match String (Default route_short_name). routes.txt attribute that identifies routes. Accepted values: route_id, route_short_name, route_long_name.
#' @param osm_match String (Default ref). OSM attribute that identifies routes by matching with gtfs_match. Accepted values: ref, name, gtfs:route_id.
#'
#' @details
#' For each route, matches its trips' shapes with OSM route relations.
#'
#' The match is performed considering, for each shape, the closest OSM route, based on
#' the start and end points, total length and average distance between stops.
#'
#' @returns A \code{data.frame} (\code{sf} if \code{geometry=TRUE}) with the following columns:
#' \itemize{
#'  \item \code{shape_id}, the \code{shape_id} attribute from \code{shapes.txt} file.
#'  \item \code{osm_id}, the \code{osm_id} attribute from OSM route relation.
#'  \item \code{distance_diff}, the difference, in meters, between GTFS shape and OSM route lengths.
#'  \item \code{points_diff}, the sum of the difference, in meters, between GTFS shape and OSM route start and end points.
#'  \item \code{stops_diff}, the difference between GTFS and OSM routes number of stops.
#'  \item \code{route_short_name}, the \code{route_short_name} attribute from \code{routes.txt} file.
#'  \item \code{route_long_name}, the \code{route_long_name} attribute from \code{routes.txt} file.
#'  \item \code{geometry}, the geometrical data for the OSM route relation.
#' }
#'
#' Attention! For each GTFS route, provided there is at least one OSM route, all the GTFS shapes for that route will necessarily be associated with
#' an OSM one. This might generate wrong results if the number/topology of routes on OSM does not match the GTFS shapes for that route.
#' Refer to  \code{distance_diff}, \code{points_diff} and \{stops_diff} on the results table to validate the results and identify misassociations.
#'
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::load_feed("gtfs.zip")
#'
#' q = opq("Lisbon")  |>
#'   add_osm_feature(key = "route", value = c("bus", "tram")) |>
#'   add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)
#'
#' shapes_match_routes = GTFShift::osm_shapes_match_routes(gtfs, q)
#' }
#'
#' @import osmdata
#' @import sf
#' @import dplyr
#' @import stplanr
#' @import xml2
#'
#' @export
osm_shapes_match_routes <- function(gtfs, q, geometry=TRUE, gtfs_match="route_short_name", osm_match="ref") {

  # 0. Validations
  if (!(gtfs_match %in% c("route_id", "route_short_name", "route_long_name"))) {
    stop("gtfs_match should be one of: route_id, route_short_name or route_long_name")
  }
  if (!(osm_match %in% c("ref", "name", "gtfs:route_id"))) {
    stop("osm_match should be one of: ref, name")
  }

  message("Preparing OSM and GTFS data...")

  # 1. Get geometry for shapes and stops
  shapes_sf = tidytransit::shapes_as_sf(gtfs$shapes)
  stops_sf = tidytransit::stops_as_sf(gtfs$stops)
  message(sprintf("Found %d GTFS shapes and %d stops...", nrow(shapes_sf), nrow(stops_sf)))

  # 2. Get OSM routes and stops
  osm = q |> osmdata_sf()
  osm_multilines = osm$osm_multilines
  osm_multilines_redux = osm_multilines |>
    select(any_of(c("osm_id", "ref", "from", "to", "via", "name", "roundtrip", "gtfs:route_id")))

  osm_stoppositions = osm$osm_points |>
    st_crop(st_bbox(stplanr::geo_buffer(osm_multilines_redux, dist=100) )) |>
    filter(public_transport == "stop_position" | public_transport == "platform") |>
    select_if(~!all(is.na(.)))
  message(sprintf("Found %d OSM route relations and %d bus stops/platforms...", nrow(osm_multilines_redux), nrow(osm_stoppositions)))

  # 3. Get OSM relations (to associate routes and stops)
  osm_file <- tempfile(fileext = ".osm")
  osmdata_xml(q, filename = osm_file)
  doc <- read_xml(osm_file)
  relations <- xml_find_all(doc, ".//relation")
  relations_df = lapply(relations, function(relation) {
    relation_id <- xml_attr(relation, "id")
    members <- xml_find_all(relation, "member")

    members_df = lapply(members, function(member) {
      c(
        type = xml_attr(member, "type"),
        ref = xml_attr(member, "ref"),
        role = xml_attr(member, "role")
      )
    })
    df <- data.frame(do.call(rbind, members_df))
    df$relation_osm_id = relation_id
    return(df)
  })
  relations_df = bind_rows(relations_df)

  message("Done! Starting match algorithm...")

  # 4. For each gtfs route, match shapes with OSM routes
  routes_names = unique( gtfs$routes |> pull( !!gtfs_match ) ) # !! to use variable value and not its literal name
  counter_no_osm = 0
  counter_osm_duplicate_match = 0
  result <- lapply(routes_names, function(route_name) {

    message(sprintf("Running for route %s...", route_name))

    # 1. Get base data
    # > Filter osm network
    osm_route_name = osm_multilines_redux |>
      filter(.data[[osm_match]] == route_name)

    if (nrow(osm_route_name) == 0) {
      counter_no_osm = counter_no_osm + 1
      warning("No OSM routes found for GTFS ", gtfs_match, " ", route_name)
      return(data.frame(
        route_name=route_name
      ))  # Return NULL for failed elements
    }

    # > Filter GTFS
    gtfs_route_name = gtfs$routes |>  # Start on routes.txt to match line number with route_name
      select(route_id, route_short_name, route_long_name) |>
      filter(.data[[gtfs_match]] == route_name) |>
      left_join(gtfs$trips |> select(route_id, trip_id, shape_id, direction_id), by="route_id") |>
      left_join(shapes_sf, by="shape_id") |>
      distinct(shape_id, .keep_all = TRUE) |>
      sf::st_as_sf()

    # 2. Match based on initial and final points
    # > Compute osm final and initial points
    osm_route_name = tryCatch ({
      osm_route_name |>
        mutate(roundtrip = if (!"roundtrip" %in% names(osm_route_name)) NA else roundtrip) |>
        rowwise() |>
        mutate(
          # Geographical data
          route_dist = st_length(geometry) |> units::drop_units(),
          # Other relevant parameters
          nr_stops = { # Consider the number of stops to be the maximum of stops or platforms, because some routes use them mixed and miss some
            nr_s <- nrow(relations_df |> filter(relation_osm_id==osm_id & grepl("stop", role)))
            nr_p <- nrow(relations_df |> filter(relation_osm_id == osm_id & grepl("platform", role)))
            max(nr_s, nr_p)
          },
          first_stop_osm_id = relations_df |>
            # Consider both stop_entry/exit_only and stop, because circular lines do not have entry/exit, only stop
            filter(relation_osm_id==osm_id & role %in% c("stop_entry_only", "stop", "platform_entry_only", "platform")) |>
            # Use sorting to give priority to entry/exit, when they exist
            arrange(
              match(role, c("stop_entry_only", "platform_entry_only", "stop", "platform")),
              role
            ) |>
            slice(1)  |>
            pull(ref),
          last_stop_osm_id = relations_df |>
            filter(relation_osm_id==osm_id & role %in% c("stop_exit_only", "stop", "platform_exit_only", "platform")) |>
            mutate( role_group = case_when(
              # When roundtrip (circular), keep normal order
              roundtrip == "yes" ~ 1,
              # Otherwise, consider first stop_exit_only or last stop (if no exit_only)
              role == "stop_exit_only" ~ 1,role == "platform_exit_only" ~ 2,role == "stop" ~ 4,role == "platform" ~ 4,TRUE ~ 5
            ) ) |>
            arrange(
              role_group,
              case_when(
                roundtrip == "yes" ~ row_number(),             # When roundtrip (circular), keep normal order
                role == "stop_exit_only" ~ row_number(),       # keep natural order
                role == "platform_exit_only" ~ row_number(),   # keep natural order
                role == "stop" ~ desc(row_number()),           # reverse order
                role == "platform" ~ desc(row_number()),       # reverse order
                TRUE ~ row_number()                            # fallback order for others
              )
            ) |>
            slice(1) |>
            pull(ref),
          initial = osm_stoppositions |> filter(osm_id==first_stop_osm_id) |> slice(1) |> pull(geometry),
          final = osm_stoppositions |> filter(osm_id==last_stop_osm_id) |> slice(1) |> pull(geometry)
        ) |>
        ungroup() |>
        select(osm_id, name, route_dist, nr_stops, first_stop_osm_id, last_stop_osm_id, initial, final, geometry) |>
        arrange(route_dist)
    }, error = function(e) {
      warning("Error determining start/end points for OSM route (", gtfs_match, " ", route_name, "):",e)
      return(NULL)
    })
    if (is.null(osm_route_name)) {
      return(data.frame(
        route_name=route_name
      ))  # Return NULL for failed elements
    }

    # > Same for GTFS shapes
    gtfs_route_name = gtfs_route_name |>
      rowwise() |>
      mutate(
        # Geographical data
        route_dist = st_length(geometry) |> units::drop_units(),
        trip_id_copy = trip_id,

        first_stop_id = gtfs$stop_times |> filter(trip_id == trip_id_copy & stop_sequence == 1) |> slice(1) |> pull(stop_id),
        last_stop_id = gtfs$stop_times |> filter(trip_id == trip_id_copy) |> arrange(desc(stop_sequence)) |> slice(1) |> pull(stop_id),
        initial = stops_sf |> filter(stop_id == first_stop_id) |> slice(1) |> pull(geometry),
        final = stops_sf |> filter(stop_id == last_stop_id) |> slice(1) |> pull(geometry),

        # Other relevant parameters
        nr_stops = nrow(gtfs$stop_times |> filter(trip_id == trip_id_copy))
      ) |>
      ungroup() |>
      select(-trip_id_copy) |>
      arrange(route_dist, initial, final)

    # 3. Match gtfs shapes and osm routes, by choosing the one that share the closest start and end points
    # >  Compute distances between init and final points for both
    init = units::drop_units( st_distance(osm_route_name$initial, gtfs_route_name$initial) )
    fin = units::drop_units( st_distance(osm_route_name$final, gtfs_route_name$final) )
    length_diff = sapply(gtfs_route_name$route_dist, function(y) abs(osm_route_name$route_dist - y))
    # Proxy for number of stops distance: average distance between stops on GTFS, times the difference between osm and gtfs stops
    stops_diff = sapply(
      seq_along(gtfs_route_name$nr_stops),
      function(i) {
        (gtfs_route_name$route_dist[i] / gtfs_route_name$nr_stops[i]) *
          abs(osm_route_name$nr_stops - gtfs_route_name$nr_stops[i])
      }
    )


    # > Match OSM network and GTFS shapes considering the match with min aggregated distance (init + fin)
    closeness = abs(init + fin + length_diff + stops_diff)

    gtfs_route_name_minimos = gtfs_route_name |>
      mutate(osm_id = NA)

    for (i in 1:nrow(gtfs_route_name_minimos)) {
      gtfs_route_name_minimos[i,]$osm_id = osm_route_name[which.min(closeness[,i]),]$osm_id
    }

    gtfs_route_name_result = gtfs_route_name_minimos |>
      st_drop_geometry() |>
      left_join(
        osm_route_name |> select(osm_id, name, route_dist, nr_stops, geometry, initial, final),
        by = "osm_id",
        suffix = c("_gtfs", "_osm")
      ) |>
      rowwise() |>
      mutate(
        distance_diff = abs(route_dist_gtfs - route_dist_osm),
        points_diff = as.numeric( units::drop_units( st_distance(initial_osm, initial_gtfs) ) + units::drop_units( st_distance(final_osm, final_gtfs) ) ),
        stops_diff = abs(nr_stops_gtfs - nr_stops_osm)
      ) |> # absolute difference
      ungroup() |>
      select(-initial_gtfs, -final_gtfs) |>
      st_as_sf(sf_column_name="geometry")

    if (length(unique(gtfs_route_name_result$osm_id)) < nrow(gtfs_route_name_result)) {
      counter_osm_duplicate_match = counter_osm_duplicate_match + 1
      warning(sprintf(
        "GTFS route %s has %d shapes, but they were matched with only %d (out of %d) OSM routes. This might indicate a mismatch between GTFS and OSM data.\nosm_id for route: %s (the ignored ones were %s) (the duplicated ones were %s)",
        route_name, nrow(gtfs_route_name),
        length(unique(gtfs_route_name_result$osm_id)),
        nrow(osm_route_name),
        paste(osm_route_name$osm_id, collapse=", "),
        paste(setdiff(
          union(gtfs_route_name_result$osm_id, osm_route_name$osm_id),
          intersect(gtfs_route_name_result$osm_id, osm_route_name$osm_id)
        ), collapse=", "),
        paste(unique(gtfs_route_name_result$osm_id[duplicated(gtfs_route_name_result$osm_id)]), collapse=", ")
      ))
      return(data.frame(
        route_name=route_name
      ))  # Return NULL for failed elements
    }

    return(gtfs_route_name_result)
  })
  result_success = bind_rows( result[lengths(result)>1] )

  message(sprintf(
    "DONE! Associated %d shapes with OSM routes, with a mean distance of %.2f meters for points, %.2f meters for route length and a mean difference of %.2f stops.",
    nrow(result_success),
    mean(result_success$points_diff),
    mean(result_success$distance_diff),
    mean(result_success$stops_diff)
  ))
  if (counter_osm_duplicate_match>0) {
    warning(sprintf(
      "%d GTFS routes had shapes matched with duplicate OSM routes. Check the logs for more details.",
      counter_osm_duplicate_match
    ))
  }
  if (counter_no_osm>0) {
    warning(sprintf(
      "%d GTFS routes had no OSM routes found. Check the logs for more details.",
      counter_no_osm
    ))
  }

  not_found = bind_rows( result[lengths(result)<=1] )
  if (nrow(not_found)>0) {
    warning(sprintf("%d missing matches for %s: %s", nrow(not_found), gtfs_match, paste(not_found$route_name, collapse=", ")))
  }

  result_success = result_success |> select(shape_id, osm_id, distance_diff, points_diff, stops_diff, route_short_name, route_long_name)

  if (!geometry) {
    return (result_success |> st_drop_geometry())
  }
  return (result_success)
}
