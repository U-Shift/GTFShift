routes = frequencies_route
View(routes)
# 1. Create buffer and export for ArcGis
layer <- st_transform(routes, crs=3857)

buffer = sf::st_buffer(layer, 8)
mapview(buffer)

buffer = sf::st_union(buffer)
mapview(buffer)

sf::st_write(buffer, "buffer_8.gpkg", append=FALSE)

# 2. Open centerlines from ArcGIS
# COnvert to line and segment
library(dplyr)
library(mapview)
centerline <- sf::st_read("centerline_carris.gpkg") %>% mutate(row=row_number())
mapview(centerline)
View(centerline)

library(stplanr)
library(sf)
sf::st_crs(centerline)
centerline_line = stplanr::line_cast(centerline)
View(centerline_line)

centerline_segmented = stplanr::line_segment(centerline_line, segment_length=100) %>% mutate(row=row_number())
mapview(centerline_segmented, zcol="row")
View(centerline_segmented)

network = centerline_segmented

# 3. Assign routes to centerline network
routes = routes |>
  mutate(
    match_id = row_number()
  )
View(routes)

routes_network_match = rnet_join(rnet_x = st_transform(routes, crs=3857),
  rnet_y = network |> select(row),
  length_y = FALSE,
  # contains = FALSE, # error
  # max_angle_diff = 25,
  # dist_subset = 3,
  # subset_x = TRUE, # muito lento
  key_column = "match_id",
  dist = 10
) |> st_drop_geometry()
View(routes_network_match)

routes_network = network |>
  select(row) |>
  filter(row %in% routes_network_match$row) %>%
  left_join(routes_network_match, by="row") %>%
  left_join(routes %>% select(match_id, route_short_name, frequency, arrival_hour) %>% st_drop_geometry(), by="match_id")
View(routes_network)
mapview::mapview(routes_network, zcol="route_short_name")
st_write(routes_network, "routes_network.gpkg", append=FALSE)


# 4. Manual overline
manual_overline = routes_network %>% filter(arrival_hour==8)%>% select(row, frequency) %>% group_by(row) %>% summarize(frequency=sum(frequency))
View(manual_overline)
mapview::mapview(st_transform(manual_overline, crs=4326), zcol="frequency")
st_write(manual_overline  , "manual_overline_carris.gpkg", append=FALSE)


# Compare with overline
routes_network_overline = overline2(routes_network, attrib = "frequency")
st_write(routes_network_overline, "routes_network_overline.gpkg")
