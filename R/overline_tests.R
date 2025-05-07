# overline tests

library(dplyr)
routes_freq <- frequencies_route
library(mapview)
library(stplanr)
mapview(routes_freq)

routes_742 = routes_freq %>%
  filter(route_short_name == "742") |>
  filter(arrival_hour == 8)

routes_706 = routes_freq %>%
  filter(route_short_name == "706")|>
  filter(arrival_hour == 8)

routes_730 = routes_freq %>%
  filter(route_short_name == "730")|>
  filter(arrival_hour == 8)

routes_718 = routes_freq %>%
  filter(route_short_name == "718")|>
  filter(arrival_hour == 8)

mapview::mapview(routes_742) +
  mapview::mapview(routes_730, color = "red") +
  mapview::mapview(routes_718, color = "blue") +
  mapview::mapview(routes_706, color = "green")

routes_moraissoares = rbind(routes_742, routes_730, routes_718, routes_706)
routes_moraissoares_overline = routes_moraissoares %>%
  filter(arrival_hour == 8) |>
  overline2(attrib = "frequency", regionalise = 1e4) %>%
  arrange(frequency) %>%
  mutate(hour = 8)

mapview::mapview(routes_moraissoares_overline, zcol = "frequency")
# , , lwd = 5, layer.name = "Frequência",




# só 706
routes_706_overline = routes_706 %>%
  # st_simplify(dTolerance = 1, preserveTopology = TRUE) |> # engraçado brincar com isto, mas não resolve
  filter(arrival_hour == 8) |>
  overline2(attrib = "frequency") %>%
  arrange(frequency) %>%
  mutate(hour = 8) |>
  overline_intersection(attrib = "frequency") # sugested here https://github.com/ropensci/stplanr/issues/420 -> does not solve!

mapview::mapview(routes_706_overline, zcol = "frequency", lwd = "frequency", lwd.multiplier = 2)

# match com OSM


# create unique sequential id
library(dplyr)
library(stplanr)
library(mapview)
library(sf)
routes_706 = routes_706 |>
  mutate(
    match_id = row_number(),
    id=match_id
  )
View(routes_706)
mapview::mapview(routes_706)

osm_706 = rnet_join(rnet_x = routes_706,
                    rnet_y = road_osm |> select(osm_id),
                    length_y = TRUE,
                    # contains = FALSE, # error
                    # max_angle_diff = 25,
                    # dist_subset = 3,
                    # subset_x = TRUE, # muito lento
                    key_column = "match_id",
                    dist = 10
) |> st_drop_geometry()
View(osm_706)
st_write(osm_706, "osm_706.gpkg")

mapview::mapview(osm_706)


osm_706_line = road_osm |>
  select(osm_id) |>
  filter(osm_id %in% osm_706$osm_id) |>
  mutate(
    id = row_number()
  ) |>
  left_join(osm_706 |> select(osm_id, length_y), by="osm_id")
osm_706_line
# |>
#   left_join(osm_706) |>
#   left_join(routes_706 |>
#               st_drop_geometry() |>
#               select(frequency, match_id)) |>
#   group_by(osm_id) |>
#   summarise(frequency = sum(frequency))

mapview::mapview(routes_706, color="gray", lwd=5) + mapview::mapview(osm_706_line, color="green", lwd=1)

# Part 2: Try to clear
library(anime)
matches <- anime::anime(
  source=st_transform(osm_706_line, crs = 3857),
  target=st_transform(routes_706, crs=3857)
)
matches_tbl <- get_matches(matches)
matches_tbl

osm_706_line_join <- left_join(osm_706_line, matches_tbl, by=c("id"="source_id")) %>% mutate(shared_ratio = shared_len/length_y * 100)
View(osm_706_line_join)
mapview::mapview(routes_706, color="gray", lwd=5) + mapview::mapview(osm_706_line_join, color="green", lwd=1)


st_write(routes_moraissoares_overline, paste0("data/", bus_operator, "_routes_moraissoares_freq.gpkg"))
st_write(routes_706, paste0("data/", bus_operator, "_routes_706.gpkg"))
st_write(routes_718, paste0("data/", bus_operator, "_routes_718.gpkg"))
st_write(routes_730, paste0("data/", bus_operator, "_routes_730.gpkg"))
st_write(routes_742, paste0("data/", bus_operator, "_routes_742.gpkg"))

# inspect in qgis


