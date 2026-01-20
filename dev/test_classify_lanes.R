library(GTFShift)
library(osmdata)
library(dplyr)

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_id = "lisboa"
gtfs = GTFShift::load_feed(data$URL[data$ID == gtfs_id], create_transfers=FALSE)

q = opq(bbox=sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes)))  |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

lanes_global = classify_lanes(gtfs, q)
nrow(lanes_global)

lanes = lanes_global |> filter(hour == 8)

nrow(lanes)
nrow(lanes |> filter(is.na(n_lanes)))
mapview::mapview(lanes |> filter(is.na(n_lanes)))
nrow(lanes |> filter(!is.na(n_lanes)))
nrow(lanes |> filter(!is.na(n_lanes) & n_lanes==1))
nrow(lanes |> filter(!is.na(n_lanes) & n_lanes>1))
nrow(lanes |> filter(!is.na(n_lanes) & n_lanes>2))
nrow(lanes |> filter(!is.na(n_lanes) & n_lanes>3))
nrow(lanes |> filter(!is.na(n_lanes) & n_lanes>3 & is_bus_lane))
nrow(lanes |> filter(!is.na(n_lanes) & n_lanes>3 & !is_bus_lane))

nrow(lanes |> filter(n_directions==1))
nrow(lanes |> filter(n_directions==2))

table(lanes$frequency)
nrow(lanes |> filter(frequency>=3)) # 82%
nrow(lanes |> filter(frequency>=5)) # 62%

# Classify

# View (debug only)
mapviewOptions(basemaps = "CatroDB.Positron")

mapview::mapview(way_frequency)
mapview::mapview(lanes, zcol="is_bus_lane")
mapview::mapview(lanes, zcol="n_directions")
mapview::mapview(lanes, zcol="n_lanes_direction")

# Prioritization
# Color pallete from https://colorhunt.co/palette/f63049d027528a244b111f35
map_needs = mapview::mapview(
  lanes |> filter(frequency>5 & !is.na(n_lanes) & n_lanes_direction>1 & !is_bus_lane),
  layer.name="+5 bus/h + 1 lane/dir, NO bus lane",
  color="#F63049"
) +
  mapview::mapview(
    lanes |> filter(frequency>5 & !is.na(n_lanes) & n_lanes_direction>2 & !is_bus_lane),
    layer.name="+5 bus/h + 2 lane/dir, NO bus lane",
    color="#8A244B"
  ) +
  mapview::mapview(
    lanes |> filter(frequency>5 & !is.na(n_lanes) & n_lanes_direction>3 & !is_bus_lane),
    layer.name="+5 bus/h + 3 lane/dir, NO bus lane",
    color="#111F35"
  )
map_needs

# Color pallete from https://colorhunt.co/palette/0054610c7779249e943bc1a8
map_current = mapview::mapview(
  lanes |> filter((frequency<5 | (is.na(n_lanes) | n_lanes_direction<=1)) & is_bus_lane),
  layer.name="Bus lane with -6 bus/h OR - 1 lane/dir",
  color="#DAD887"
) +
  mapview::mapview(
    lanes |> filter(frequency>=5 & !is.na(n_lanes) & n_lanes_direction>1 & is_bus_lane),
    layer.name="+5 bus/h + 1 lane/dir, with bus lane",
    color="#3BC1A8"
  ) +
  mapview::mapview(
    lanes |> filter(frequency>=5 & !is.na(n_lanes) & n_lanes_direction>2 & is_bus_lane),
    layer.name="+5 bus/h + 2 lane/dir, with bus lane",
    color="#0C7779"
  ) +
  mapview::mapview(
    lanes |> filter(frequency>=5 & !is.na(n_lanes) & n_lanes_direction>3 & is_bus_lane),
    layer.name="+5 bus/h + 3 lane/dir, with bus lane",
    color="#005461"
  )
map_current

map_current + map_needs

map_aggregated_simplified = mapview::mapview(
  lanes |> filter((frequency<5 | (is.na(n_lanes) | n_lanes_direction<=1)) & is_bus_lane),
  layer.name="Bus lane with -6 bus/h OR - 1 lane/dir",
  color="#DAD887"
) + mapview::mapview(
  lanes |> filter(frequency>=5 & !is.na(n_lanes) & n_lanes_direction>1 & is_bus_lane),
  layer.name="+5 bus/h + 1 lane/dir, with bus lane",
  color="#3BC1A8"
) + mapview::mapview(
  lanes |> filter(frequency>5 & !is.na(n_lanes) & n_lanes_direction>1 & !is_bus_lane),
  layer.name="+5 bus/h + 1 lane/dir, NO bus lane",
  color="#F63049"
)
map_aggregated_simplified

# Save to html
library(mapview)
# mapshot(map_needs, "releases/v0_8/lane_classification_needs.html")
# mapshot(map_current, "releases/v0_8/lane_classification_current.html")
# mapshot(map_current + map_needs, "releases/v0_8/lane_classification_combined.html")
# mapshot(map_aggregated_simplified, "releases/v0_8/lane_classification_aggregated_simplified.html")
