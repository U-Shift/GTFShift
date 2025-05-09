View(frequencies_route)

routes_742 = routes_freq %>%
  filter(route_short_name == "742") |>
  filter(arrival_hour == 8)
View(routes_742)
mapview::mapview(routes_742, zcol = "frequency")


routes_moraissoares
mapview::mapview(routes_moraissoares, zcol = "frequency")

routes_moraissoares_segments = stplanr::line_segment(
  st_transform(routes_moraissoares %>% select(arrival_hour,frequency), crs=3857),
  segment_length = 1
)
View(routes_moraissoares_segments)

st_crs(routes_moraissoares)
st_bbox(routes_moraissoares)

routes_moraissoares_segments_overline_100 = routes_moraissoares_segments %>%
  filter(arrival_hour == 8) |>
  overline2(attrib = "frequency") %>%
  arrange(frequency) %>%
  mutate(hour = 8)

mapview::mapview(routes_moraissoares_segments_overline_100, zcol = "frequency")


plot(routes_moraissoares)


# https://gis.stackexchange.com/questions/358887/how-to-merge-groups-of-touching-lines-to-multilinestring
library(sf)
library(igraph)
routes_742_segments <- stplanr::line_segment(
  st_transform(routes_742 %>% select(arrival_hour,frequency), crs=3857),
  segment_length = 1
)
mapview::mapview(routes_742_segments, zcol = "frequency")
lines <- routes_moraissoares_segments

touch <- st_touches(lines)
adj <- graph.adjlist(touch)
com <- components(adj)

# generate a simple features collection and add the group number
lines = st_sf(geometry = lines)
lines$group = com$membership

# summarise by group
multi_lines = lines |> group_by(group) |>
  summarise()
multi_lines
View(multi_lines)
mapview::mapview(multi_lines, zcol = "group")


#### Aggregate
intersection = stplanr::overline_intersection(routes_moraissoares_segments, "frequency")
mapview::mapview(intersection)
View(intersection)
