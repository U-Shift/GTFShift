# Carris Metropolitana (plain JSON)
rt_collect(
  gtfs_rt_url = "https://api.carrismetropolitana.pt/v1/vehicles",
  header_key = NA,
  entity_key = NA,
  fields_collect = c("block_id", "trip_id", "lat", "lon", "current_status", "stop_id", "speed", "timestamp"),
  destination_file = "releases/gtfs_rt_data/cm.csv",
  scrap_interval = 5,
  log_file = "releases/gtfs_rt_data/cm_collect_log.txt"
)

# Carris Lisbon (Protocol Buffers)
rt_collect_protobuf(
  gtfs_rt_url="https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS/realtime/vehiclepositions",
  destination_file = "releases/gtfs_rt_data/carris.csv",
  scrap_interval = 5,
  log_file = "releases/gtfs_rt_data/carris_collect_log.txt"
)
