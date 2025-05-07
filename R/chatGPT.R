library(dplyr)
library(mapview)
library(sf)
View(frequencies_route)
summary(frequencies_route)

routes_708 = frequencies_route %>%
  filter(route_short_name == "708")|>
  filter(arrival_hour == 8)

routes <- st_transform(routes_708, 3857)
summary(routes)
View(routes)
mapview::mapview(routes,zcol = "shape_id")

routes_708_overline = routes_708 %>% overline2(attrib = "frequency") %>%
  arrange(frequency) %>%
  mutate(hour = 8)
mapview::mapview(routes_708_overline,zcol = "frequency")
st_write(routes_708_overline, "routes_708_overline.gpkg")


buffers <- sf::st_buffer(routes, dist = 5)
mapview(buffers)
View(buffers)

unioned <- sf::st_union(buffers)
unioned_sf <- st_as_sf(unioned)
View(unioned_sf)
mapview(unioned_sf)
class(unioned_sf)

st_write(unioned_sf, "unioned_sf.gpkg")



network_lines <- st_boundary(unioned)
network_lines <- st_cast(network_lines, "LINESTRING")
View(network_lines)
mapview(st_as_sf(network_lines))

mapview(st_geometry(network_lines))
network_lines


library(centerline)
lake_skeleton_s <-cnt_skeleton(unioned_sf, keep = 2)
mapview(lake_skeleton_s)

lake_centerline <- cnt_path_guess(unioned_sf, keep = 2)
mapview(lake_centerline)


devtools::install_github("RichardPatterson/midlines")
library(midlines)
library(sf)
library(units)

m1 = midlines_draw(unioned_sf)
m1_m = midlines_draw(unioned_sf, dfMaxLength = set_units(5,"m"))
mapview(m1)
mapview(m1_m)

m2 = midlines_clean(m1, n_removed = 1)
mapview(m2[m2$removed_flag==0,])
