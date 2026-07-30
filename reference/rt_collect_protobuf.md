# Collect GTFS-RT data from a Protocol Buffers feed at regular intervals

Collect GTFS-RT data from a Protocol Buffers feed at regular intervals

## Usage

``` r
rt_collect_protobuf(
  gtfs_rt_url,
  destination_file,
  fields_collect = c("id", "vehicle.trip.trip_id", "vehicle.position.latitude",
    "vehicle.position.longitude", "vehicle.position.speed", "vehicle.timestamp",
    "vehicle.current_status", "vehicle.current_stop_sequence", "vehicle.stop_id"),
  scrape_interval = 60,
  log_file = NA,
  headers = NULL
)
```

## Arguments

- gtfs_rt_url:

  String. URL of the Protocol Buffers GTFS-RT feed.

- destination_file:

  String. File to save the downloaded GTFS-RT data. Content is appended
  in each iteration.

- fields_collect:

  Character vector. Fields to extract from each entity in the feed.

- scrape_interval:

  Integer (Default 60). Interval in seconds between each download.
  Negative to run only once.

- log_file:

  String (Optional). Path to a log file to save download logs.

- headers:

  Named list or character vector (Optional). Custom HTTP headers for
  credentials when accessing the GTFS-RT feed URL.

## Value

String. The location of the file where data was collected.

## Details

Downloads GTFS-RT data from the specified URL at regular intervals and
saves them to the destination file.

This function will run indefinitely until manually stopped (CTRL + C).

## Examples

``` r
# Create file
destination_file <- withr::local_tempfile(fileext = ".csv")

# Collect data
GTFShift::rt_collect_protobuf(
  gtfs_rt_url = "https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs.pb",
  destination_file = destination_file,
  scrape_interval = -1 # Negative to run only once
)
#> [2026-07-30 14:52:36] Starting GTFS-RT data collection from https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs.pb
#> [20260730_145236] Iteration 1 completed
#> [1] "/tmp/RtmpX8K2XZ/file1ec29c8625b.csv"

# Read data
collection <- read.csv(destination_file)

names(collection)
#>  [1] "id"                            "vehicle.trip.trip_id"         
#>  [3] "vehicle.position.latitude"     "vehicle.position.longitude"   
#>  [5] "vehicle.position.speed"        "vehicle.timestamp"            
#>  [7] "vehicle.current_status"        "vehicle.current_stop_sequence"
#>  [9] "vehicle.stop_id"               "feed_timestamp"               
#> [11] "feed_incrementality"          

head(
  collection |>
    dplyr::select("vehicle.trip.trip_id", "vehicle.position.latitude", "vehicle.position.longitude")
)
#>                vehicle.trip.trip_id vehicle.position.latitude
#> 1 [Y8LCX][BNA17]2106_0_2|140|3|1455                   38.9080
#> 2   [Y8LCX][BNA17]2727_0_1|1|3|1530                   38.8155
#> 3   [Y8LCX][BNA17]2781_0_2|1|3|1545                   38.7826
#> 4 [Y8LCX][BNA17]2740_0_2|140|3|1510                   38.9262
#> 5 [Y8LCX][BNA17]2756_0_1|140|3|1500                   38.9035
#> 6   [Y8LCX][BNA17]2303_0_1|1|3|1540                   38.9031
#>   vehicle.position.longitude
#> 1                    -9.2470
#> 2                    -9.1218
#> 3                    -9.1880
#> 4                    -9.2970
#> 5                    -9.1902
#> 6                    -9.0485
```
