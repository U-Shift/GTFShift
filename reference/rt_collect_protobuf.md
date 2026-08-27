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
#> [2026-08-27 14:40:37] Starting GTFS-RT data collection from https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs.pb
#> [20260827_144037] Iteration 1 completed
#> [1] "/tmp/RtmpIOJuU6/file1f4a7668266.csv"

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
#> 1  [0277F][BNA17]2714_0_2|1|3|1530                   38.8013
#> 2      [76XA2][N18KL]4424_20251214                   39.0402
#> 3 [F1M13][A2L1N]4701_0_2|2700|1500                   38.6859
#> 4     [76XA2][N18KL]16438_20251214                   38.7454
#> 5  [0277F][BNA17]2610_0_2|1|3|1530                   38.7864
#> 6  [0277F][BNA17]2781_0_2|1|3|1515                   38.7971
#>   vehicle.position.longitude
#> 1                    -9.1223
#> 2                    -8.9215
#> 3                    -8.9397
#> 4                    -9.1406
#> 5                    -9.1828
#> 6                    -9.1768
```
