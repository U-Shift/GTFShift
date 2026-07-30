devtools::load_all()
library(dplyr)

usethis::edit_r_environ()
readRenviron("~/.Renviron")

# load_feed()

# > With credentials
# STCP (Docs at https://api.stcp.pt:8082/Doc)
gtfs_stcp <- GTFShift::load_feed(
  path = "https://api.stcp.pt:8443/v1/ficheiros/estatico/ficheirozip",
  create_transfers = FALSE,
  headers = c(
    "X-App-Id" = Sys.getenv("GTFS_STCP_KEY"),
    "X-Api-Key" = Sys.getenv("GTFS_STCP_SECRET")
  )
)
summary(gtfs_stcp)

# rt_collect_*()

# > Carris Metropolitana (plain JSON)
rt_collect_json(
  gtfs_rt_url = "https://api.carrismetropolitana.pt/v1/vehicles",
  header_key = NA,
  entity_key = NA,
  fields_collect = c("block_id", "trip_id", "lat", "lon", "current_status", "stop_id", "speed", "timestamp"),
  destination_file = "releases/gtfs_rt_data/cm.csv",
  scrape_interval = 5,
  log_file = "releases/gtfs_rt_data/cm_collect_log.txt"
)

# > Carris Lisbon (Protocol Buffers)
rt_collect_protobuf(
  gtfs_rt_url = "https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS/realtime/vehiclepositions",
  destination_file = "releases/gtfs_rt_data/carris.csv",
  scrape_interval = 5,
  log_file = "releases/gtfs_rt_data/carris_collect_log.txt"
)

# > STCP (Protocol Buffers, with credentials)
rt_collect_protobuf(
  gtfs_rt_url = "https://api.stcp.pt:8443/v1/ficheiros/real/stream/vehiclepositions",
  destination_file = "releases/gtfs_rt_data/stcp.csv",
  scrape_interval = 5,
  log_file = "releases/gtfs_rt_data/stcp_collect_log.txt",
  headers = c(
    "X-App-Id" = Sys.getenv("GTFS_STCP_KEY"),
    "X-Api-Key" = Sys.getenv("GTFS_STCP_SECRET")
  )
)
stcp_data <- read.csv("releases/gtfs_rt_data/stcp.csv")
summary(stcp_data)


# Extend prioritisation with rt data
# lane_prioritisation <- readRDS("releases/lane_prioritisation/lisbon_lane_prioritisation.rds")
lane_prioritisation <- lanes_global
rt_collection_cm <- sf::st_read("releases/gtfs_rt_data/carris_updates_more15MBusStop.csv") |>
  mutate(
    lon = str_replace(lon, "c\\(", ""),
    lat = str_replace(lat, "\\)", ""),
    speed = as.numeric(speed)
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

View(rt_collection_cm |> sf::st_drop_geometry())
mapview::mapview(rt_collection_cm[sample(nrow(rt_collection_cm), 1000), ], zcol = "speed", layer.title = "RT points sample")

lane_prioritisation_extended <- rt_extend_prioritisation(
  lane_prioritisation = lane_prioritisation,
  rt_collection = rt_collection_cm
)

summary(lane_prioritisation_extended$speed_avg)
summary(lane_prioritisation_extended$speed_count)

mapview::mapview(lane_prioritisation_extended, zcol = "speed_avg", layer.title = "Avg speed")

lanes_extended <- lane_prioritisation_extended |> filter(hour == 8)

map_aggregated_simplified_extended <- mapview::mapview(
  lanes_extended |> filter((frequency < 5 | (is.na(n_lanes) | n_lanes_direction <= 1)) & is_bus_lane),
  layer.name = "Bus lane with - 5 bus/h OR - 1 lane/dir",
  color = "#DAD887"
) + mapview::mapview(
  lanes_extended |> filter(frequency >= 5 & !is.na(n_lanes) & n_lanes_direction > 1 & speed_avg < 10 & is_bus_lane),
  layer.name = "Bus lane with 5 or + bus/h + 1 lane/dir",
  color = "#3BC1A8"
) + mapview::mapview(
  lanes_extended |> filter(frequency >= 5 & !is.na(n_lanes) & n_lanes_direction > 1 & speed_avg < 10 & !is_bus_lane),
  layer.name = "NO bus lane with 5 or + bus/h + 1 lane/dir avg_speed < 10km/h",
  color = "#F63049"
)
map_aggregated_simplified_extended

output <- "releases/web"
library(mapview)
mapshot(
  map_aggregated_simplified_extended,
  file = file.path(output, "map_rt_extended_prioritisation.html"),
  selfcontained = TRUE
)
