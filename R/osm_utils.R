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
    if_any(any_of("psv"), ~ .x == "designated") |
      if_any(any_of("highway"), ~ .x == "busway") |
      if_any(any_of(cols_to_check_access), ~ grepl("designated", .x)) |
      if_any(any_of(cols_to_check_count), ~ is.numeric(.x) & .x >= 1)
  )

  return(osm_lanes)
}


#' Get OSM relations and elements (ways and nodes) tagged as bus networks
#'
#' @param osm_file character. Path to OSM file.
#' @param pb progress bar object.
#' @param pb_update_1 numeric. Value to add to progress bar when progress at 1/4.
#' @param pb_update_2 numeric. Value to add to progress bar when progress at 2/4.
#' @param pb_update_3 numeric. Value to add to progress bar when progress at 3/4.
#' @param pb_update_4 numeric. Value to add to progress bar when progress at 4/4.
#'
#' @return data frame. OSM relations ways and nodes (with relation attributes) data frame with columns: `relation_osm_id`, `type`, `osm_id`, `role`, `gtfs:shape_id`, `gtfs:route_id`, `name`, `ref`, `roundtrip`
#'
#' @noRd
get_osm_relations <- function(osm_file, q, pb, osm_route_type = "bus", pb_update_1 = 0.25, pb_update_2 = 0.5, pb_update_3 = 0.75, pb_update_4 = 1) {
  bus_relations_pbf <- tempfile(fileext = ".osm.pbf")

  job <- callr::r_bg(function(bus_relations_pbf, osm_file, osm_route_type) { # update spinner while blocking method call
    return(rosmium::tags_filter(
      osm_file,
      sprintf("nwr/route=%s", osm_route_type),
      output = bus_relations_pbf,
      overwrite = TRUE
    ))
  }, args = list(bus_relations_pbf, osm_file, osm_route_type))
  while (job$is_alive()) {
    pb$tick(0)
    Sys.sleep(0.1)
  }
  job$get_result()
  pb$update(pb_update_1)

  bus_relations_xml <- rosmium::show_content(
    bus_relations_pbf,
    object_type = c("relation"),
    output_format = "xml",
    preview = FALSE,
    spinner = FALSE
  )

  pb$update(pb_update_2)

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

  pb$update(pb_update_3)
  rel_n <- 0
  relations_data <- lapply(relations, function(rel) {
    rel_n <<- rel_n + 1
    pb$update(min(round(pb_update_3 + ((pb_update_4 - pb_update_3) * rel_n / length(relations)), digits = 2), 1))
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

    members <- xml2::xml_find_all(rel, ".//member[@type='way' or @type='node']")
    members_type <- xml2::xml_attr(members, "type")
    if (length(members) == 0 | !any(members_type == "way") | !any(members_type == "node")) {
      return(NULL)
    }

    data.frame(
      # <relation>
      relation_osm_id = xml2::xml_attr(rel, "id"),
      # <member>
      type = xml2::xml_attr(members, "type"),
      osm_id = xml2::xml_attr(members, "ref"),
      role = xml2::xml_attr(members, "role"),
      # <tag>
      `gtfs:shape_id` = tag_vals["gtfs:shape_id"],
      `gtfs:route_id` = tag_vals["gtfs:route_id"],
      name = tag_vals["name"],
      ref = tag_vals["ref"],
      roundtrip = tag_vals["roundtrip"],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  relations_df <- dplyr::bind_rows(relations_data)

  return(relations_df)
}
