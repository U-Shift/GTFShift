library(dplyr)
library(mapview)
library(purrr)
library(sf)

View(routes_742)
routes = frequencies_route_overline %>% filter(arrival_hour==8)
routes = routes_742[c(1),]


View(routes)
routes <- st_transform(routes, 3857)
mapview::mapview(
  routes,
  zcol = "frequency",
  layer.name = "Frequency (hour)"
)


buffers <- sf::st_buffer(routes, dist = 10)
mapview(buffers)
View(buffers)


## Centerline
library(cmgo)

###########
buffers_no_overlap <- map(1:nrow(buffers), function(i) {
  other <- buffers[-i, ]
  this <- buffers[i, ]

  diff <- st_difference(this, st_union(other))

  diff
}) %>% bind_rows()

View(buffers_no_overlap)
mapview(buffers_no_overlap)
