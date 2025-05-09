# overline tests

library(dplyr)
routes_freq <- frequencies_route
library(mapview)
library(stplanr)
mapview(routes_freq)
View(routes_freq)
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
st_write(routes_moraissoares_overline, "routes_moraissoares_overline.gpkg")

View(routes_moraissoares_overline)
mapview::mapview(routes_moraissoares_overline)
mapview::mapview(routes_moraissoares_overline, zcol = "frequency")
mapview::mapview(routes_moraissoares_overline, zcol = "frequency", lwd = "frequency", lwd.multiplier = 2)

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
    match_id = row_number()
  )
View(routes_706)
mapview::mapview(routes_706)

road_osm_segments = line_segment(st_transform(road_osm, crs = 3857) |> select(osm_id), segment_length=10)
road_osm_segments = road_osm_segments %>% mutate(osm_id_2=row_number())

library(dplyr)
osm_706 = rnet_join(rnet_x = routes_706,
                    rnet_y = road_osm |> select(osm_id),
                    length_y = FALSE,
                    # contains = FALSE, # error
                    # max_angle_diff = 25,
                    # dist_subset = 3,
                    # subset_x = TRUE, # muito lento
                    key_column = "match_id",
                    dist = 10
        ) |> st_drop_geometry()

View(osm_706)

mapview(osm_706, zcol="match_id")

osm_706_line = road_osm |>
  select(osm_id) |>
  filter(osm_id %in% osm_706$osm_id)

  # left_join(osm_706 |> select(osm_id, length_y), by="osm_id")
osm_706_line

st_write(st_transform(osm_706_line, crs = 3857), "osm_706_line_3857_5.gpkg")

aggregated_line <- st_union(osm_706_line)
mapview::mapview(aggregated_line)
View(aggregated_line)
st_write(aggregated_line, "aggregated_line.gpkg")

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
  target=st_transform(routes_706, crs=3857),
  angle_tolerance=45
)
matches_tbl <- get_matches(matches)
matches_tbl

osm_706_line_join <- left_join(osm_706_line, matches_tbl, by=c("id"="source_id")) %>% mutate(shared_ratio = shared_len/length_y * 100)
View(osm_706_line_join)
mapview::mapview(routes_706, color="gray", lwd=5) + mapview::mapview(
  osm_706_line_join,
  color="green", lwd=1
)

# interpolate values
interpolated_from_source <- st_transform(osm_706_line, crs = 3857) |>
  reframe(value = interpolate_intensive(osm_id, matches))

# bind them together
interpolated_target <- bind_cols(target, interpolated_from_source)


  left_join(osm_706 |> st_drop_geometry()) |>
  left_join(routes_706 |>
              st_drop_geometry() |>
              select(frequency, match_id)) |>
  group_by(osm_id) |>
  summarise(frequency = sum(frequency))

mapview(
  osm_706_line,
  zcol = "frequency",
  lwd.multiplier = 2 # acho que não faz nada
)

# não resolve. fica com "buracos"  e apanha partes que não devia.


>>>>>>> Stashed changes


st_write(routes_moraissoares_overline, paste0("data/", bus_operator, "_routes_moraissoares_freq.gpkg"))
st_write(routes_706, paste0("data/", bus_operator, "_routes_706.gpkg"))
st_write(routes_718, paste0("data/", bus_operator, "_routes_718.gpkg"))
st_write(routes_730, paste0("data/", bus_operator, "_routes_730.gpkg"))
st_write(routes_742, paste0("data/", bus_operator, "_routes_742.gpkg"))

# inspect in qgis


<<<<<<< Updated upstream
## APPROACH EDGES
library(sf)
osm_706_line_3857 = st_transform(osm_706_line, crs=3857)
start_points <- st_line_sample(osm_706_line_3857, sample = 0) %>% st_cast("POINT")
end_points <- st_line_sample(osm_706_line_3857, sample = 1) %>% st_cast("POINT")
mapview::mapview(start_points, layer.name="Start points", color="yellow") + mapview::mapview(end_points, layer.name="End points", color="black")
=======
>>>>>>> Stashed changes
