routes_706 = routes_706[c(1),]
View(routes_706)
mapview(routes_706)
mapview(road_osm)

routes_706
route_mercator = st_transform(routes_706, crs=3857)
route_segmented = stplanr::line_segment(route_mercator, segment_length = 10)  |>
  mutate(
    match_id = row_number(),
    id=match_id
  )
View(route_segmented)
mapview::mapview(route_segmented)

# rnet_join
route_segmented_osm = rnet_join(rnet_x = road_osm |> select(osm_id),
    rnet_y = st_transform(route_segmented, crs=4326),
    # contains = FALSE, # error
    max_angle_diff = 15,
    # dist_subset = 3,
    # subset_x = TRUE, # muito lento
    #dist = 5
) %>% filter(!is.na(route_id))
View(route_segmented_osm)
mapview(route_segmented_osm)

# anime
library(anime)
st_crs(road_osm)
source_geometry = st_transform(road_osm, crs = 3857) %>% mutate(id=as.integer(osm_id))
View(source_geometry)
target = route_segmented
matches <- anime::anime(
  source = target,
  target = route_segmented,
  distance_tolerance = 0.5,
  angle_tolerance = 5
)

matches_tbl <- get_matches(matches)
View(matches_tbl)

interpolated_from_source <- source_geometry |>
  reframe(value = interpolate_intensive(id, matches))

interpolated_target <- bind_cols(target, interpolated_from_source)

