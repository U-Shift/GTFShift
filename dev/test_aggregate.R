mutate(
  # Consider lanes, or lanes:forward + lanes:backward if not available
  way_osm_lanes = dplyr::coalesce(
    as.numeric(get("lanes")),
    rowSums(dplyr::across(dplyr::any_of(c("lanes:forward", "lanes:backward")), ~ as.numeric(.x)), na.rm = TRUE),
    NA_real_
  ),
  # Consider lanes:psv or lanes:bus, or lanes:psv:forward + lanes:psv:backward or lanes:bus:forward + lanes:bus:backward if not available
  way_osm_bus_lanes = dplyr::coalesce(
    as.numeric(get("lanes:psv")),
    as.numeric(get("lanes:bus")),
    rowSums(dplyr::across(dplyr::any_of(c("lanes:psv:forward", "lanes:psv:backward")), ~ as.numeric(.x)), na.rm = TRUE),
    rowSums(dplyr::across(dplyr::any_of(c("lanes:bus:forward", "lanes:bus:backward")), ~ as.numeric(.x)), na.rm = TRUE),
    NA_real_
  )
)
