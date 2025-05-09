library(pak)
pak::pak("JosiahParry/anime/r")
library(anime)
sl  <- frequencies_route
mapview::mapview(sl)
attr="frequency"
library(osmdata)
library(sf)
library(mapview)

osm_overline <- function(sl,attr) {

  # Get bounding box for object
<<<<<<< Updated upstream
  bbox <- sf::st_bbox(routes_moraissoares)

  # Download OSM road network for bbox
  road_osm = opq(bbox) |> # uses osmdata package, to extract only with BB
    add_osm_feature(key = "highway", value = c(
      "motorway", "trunk", "primary", "secondary", "tertiary", "residential",
      "motorway_link", "trunk_link", "primary_link",
      "secondary_link", "tertiary_link","service", "bus_guideway", "busway"
    )) |>
=======
  bbox <- sf::st_bbox(sl)

  # Download OSM road network for bbox
  road_osm = opq(bbox) |> # uses osmdata package, to extract only with BB
    add_osm_feature(key = "highway") |>
>>>>>>> Stashed changes
    osmdata_sf() |>
    osm_poly2line() # makes roundabouts into lines

  road_osm = road_osm$osm_lines
<<<<<<< Updated upstream
  View(road_osm)
=======
>>>>>>> Stashed changes
  mapview::mapview(road_osm)

  # Overlap
  sl = sl |>
    mutate(
      match_id = row_number()
    )
  View(sl)
  join = stplanr::rnet_join(
    rnet_y = road_osm |> select(osm_id),
    rnet_x = sl |> select(match_id),
    key_column = "match_id",
    length_y=FALSE
  )
  View(join)
  mapview(join, layer.name="Join", lwd=5, col.regions=list("black")) + mapview(sl, layer.name="Routes", col.regions=("green"))

  # Overlap routes with osm
  matches <- anime(
    source = sl,
    target = road_osm,
    distance_tolerance = 10,
    angle_tolerance = 5
  )
  matches

  ## TEST
  # https://github.com/U-Shift/biclar/blob/master/code/test-code/TESTS_hackathon_join_rnet_osm.R
  rnet_buffer = stplanr::geo_projected(shp = rnet_small, fun = sf::st_buffer, crs = local_crs, dist = 1)
  rnet_union_buffer = stplanr::geo_projected(shp = rnet_union, fun = sf::st_buffer, crs = local_crs, dist = 1)
  qtm(rnet_union_buffer)
  # rnet_union_buffer = stplanr::geo_buffer(rnet_union, 1)
  # Removes relevant network segments:
  transport_network_in_buffer = transport_network_small[rnet_union_buffer, , op = sf::st_within]
  # transport_network_in_buffer = transport_network_small[rnet_union_buffer, ]

}
