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
#> [2026-09-28 13:13:19] Starting GTFS-RT data collection from https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs.pb
#> Warning: cannot open URL 'https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs.pb': HTTP status was '504 Gateway Timeout'
#> Error in file(gtfs_rt_url, "rb"): cannot open the connection to 'https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs.pb'

# Read data
collection <- read.csv(destination_file)
#> Warning: cannot open file '/tmp/RtmpehivHH/file1ebb1ad75075.csv': No such file or directory
#> Error in file(file, "rt"): cannot open the connection

names(collection)
#> Error: object 'collection' not found

head(
  collection |>
    dplyr::select("vehicle.trip.trip_id", "vehicle.position.latitude", "vehicle.position.longitude")
)
#> Error: object 'collection' not found
```
