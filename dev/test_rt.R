# Carris Metropolitana (plain JSON)
rt_collect(
  "https://api.carrismetropolitana.pt/v1/vehicles",
  "releases/gtfs_rt_data/cm",
  scrap_interval = 5,
  log_file = "releases/gtfs_rt_data/cm_collect_log.txt"
)

# Carris Lisbon (Protocol Buffers)
rt_collect_protobuf(
  gtfs_rt_url="https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS/realtime/vehiclepositions",
  destination_folder="releases/gtfs_rt_data/carris",
  scrap_interval = 5,
  log_file = "releases/gtfs_rt_data/carris_collect_log.txt"
)
