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
#> [2026-07-30 14:43:28] Starting GTFS-RT data collection from https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs.pb
#> [20260730_144328] Iteration 1 completed
#> [1] "/tmp/RtmpJMmoOT/file1edb4dc6330c.csv"

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
#>                        vehicle.trip.trip_id vehicle.position.latitude
#> 1          [8TCHD][A2L1N]4720_0_1|3000|1515                   38.5642
#> 2          [8TCHD][A2L1N]4420_0_1|3000|1530                   38.5302
#> 3           [Y8LCX][BNA17]2328_0_1|1|3|1540                   38.8623
#> 4          [8TCHD][A2L1N]4600_0_2|3000|1500                   38.6718
#> 5 [Q5VCH][YA15B]3523_1_1_1530_1559_0_VER_DU                   38.5822
#> 6          [8TCHD][A2L1N]4560_0_2|3000|1530                   38.5413
#>   vehicle.position.longitude
#> 1                    -8.8672
#> 2                    -8.8701
#> 3                    -9.0679
#> 4                    -8.9759
#> 5                    -9.1510
#> 6                    -9.0214
```
