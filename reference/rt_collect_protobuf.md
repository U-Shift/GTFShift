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
#> [2026-08-27 16:21:08] Starting GTFS-RT data collection from https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs.pb
#> [20260827_162108] Iteration 1 completed
#> [1] "/tmp/RtmpItbvF6/file1f2f7b443a2a.csv"

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
#>                   vehicle.trip.trip_id vehicle.position.latitude
#> 1 [XS3H8][LA77N]1206_0_1_1700_1729_0_7                   38.8056
#> 2 [XS3H8][LA77N]1254_0_2_1700_1729_0_7                   38.8382
#> 3 [XS3H8][LA77N]1248_0_2_1630_1659_0_7                   38.8114
#> 4         [76XA2][N18KL]18861_20260516                   38.7446
#> 5 [XS3H8][LA77N]1211_0_1_1700_1729_0_7                   38.7782
#> 6 [XS3H8][LA77N]1630_0_1_1630_1659_0_7                   38.7876
#>   vehicle.position.longitude
#> 1                    -9.3402
#> 2                    -9.4626
#> 3                    -9.3721
#> 4                    -9.2031
#> 5                    -9.2612
#> 6                    -9.3731
```
