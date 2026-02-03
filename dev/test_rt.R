# Carris Metropolitana (plain JSON)
rt_collect_json(
  gtfs_rt_url = "https://api.carrismetropolitana.pt/v1/vehicles",
  header_key = NA,
  entity_key = NA,
  fields_collect = c("block_id", "trip_id", "lat", "lon", "current_status", "stop_id", "speed", "timestamp"),
  destination_file = "releases/gtfs_rt_data/cm.csv",
  scrape_interval = 5,
  log_file = "releases/gtfs_rt_data/cm_collect_log.txt"
)

# Carris Lisbon (Protocol Buffers)
rt_collect_protobuf(
  gtfs_rt_url="https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS/realtime/vehiclepositions",
  destination_file = "releases/gtfs_rt_data/carris.csv",
  scrape_interval = 5,
  log_file = "releases/gtfs_rt_data/carris_collect_log.txt"
)


# Extend prioritization with rt data
# lane_prioritization <- readRDS("releases/lane_prioritization/lisbon_lane_prioritization.rds")
lane_prioritization <- lanes_global
rt_collection_cm <- sf::st_read("releases/gtfs_rt_data/carris_updates_more15MBusStop.csv") |>
  mutate(
    lon = str_replace(lon, "c\\(", ""),
    lat = str_replace(lat, "\\)", ""),
    speed = as.numeric(speed)
  ) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

View(rt_collection_cm|>sf::st_drop_geometry())
mapview::mapview(rt_collection_cm[sample(nrow(rt_collection_cm), 1000), ], zcol="speed", layer.title="RT points sample")

lane_prioritization_extended = rt_extend_prioritization(
  lane_prioritization = lane_prioritization,
  rt_collection = rt_collection_cm
)

summary(lane_prioritization_extended$speed_avg)
summary(lane_prioritization_extended$speed_count)

mapview::mapview(lane_prioritization_extended, zcol="speed_avg", layer.title="Avg speed")

lanes_extended = lane_prioritization_extended |> filter(hour == 8)

map_aggregated_simplified_extended = mapview::mapview(
  lanes_extended |> filter((frequency<5 | (is.na(n_lanes) | n_lanes_direction<=1)) & is_bus_lane),
  layer.name="Bus lane with - 5 bus/h OR - 1 lane/dir",
  color="#DAD887"
) + mapview::mapview(
  lanes_extended |> filter(frequency>=5 & !is.na(n_lanes) & n_lanes_direction>1 & speed_avg<10 & is_bus_lane),
  layer.name="Bus lane with 5 or + bus/h + 1 lane/dir",
  color="#3BC1A8"
) + mapview::mapview(
  lanes_extended |> filter(frequency>=5 & !is.na(n_lanes) & n_lanes_direction>1 & speed_avg<10 & !is_bus_lane),
  layer.name="NO bus lane with 5 or + bus/h + 1 lane/dir avg_speed < 10km/h",
  color="#F63049"
)
map_aggregated_simplified_extended

output = "releases/web"
library(mapview)
mapshot(
  map_aggregated_simplified_extended,
  file = file.path(output, "map_rt_extended_prioritization.html"),
  selfcontained = TRUE
)
