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
#> [2026-08-27 13:05:12] Starting GTFS-RT data collection from https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs.pb
#> [20260827_130512] Iteration 1 completed
#> [1] "/tmp/Rtmphw9iD2/file20654b08f918.csv"

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
#>               vehicle.trip.trip_id vehicle.position.latitude
#> 1 [F1M13][A2L1N]4426_0_1|2700|1400                   38.5269
#> 2 [F1M13][A2L1N]4701_0_2|2700|1400                   38.6527
#> 3 [F1M13][A2L1N]4562_0_1|2700|1315                   38.5670
#> 4 [F1M13][A2L1N]4512_1_2|2700|1350                   38.5924
#> 5 [F1M13][A2L1N]4610_0_2|2700|1345                   38.6186
#> 6               [2XUL7][7NTB1]3147                   38.7376
#>   vehicle.position.longitude
#> 1                    -8.9002
#> 2                    -9.0351
#> 3                    -8.9272
#> 4                    -8.8938
#> 5                    -9.0269
#> 6                    -9.1696
```
