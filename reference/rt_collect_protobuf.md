# Collect GTFS-RT data (with Protocol Buffers support)

Collect GTFS-RT data (with Protocol Buffers support)

## Usage

``` r
rt_collect_protobuf(
  gtfs_rt_url,
  destination_file,
  fields_collect = c("id", "vehicle.trip.trip_id", "vehicle.position.latitude",
    "vehicle.position.longitude", "vehicle.position.speed", "vehicle.timestamp",
    "vehicle.current_status", "vehicle.current_stop_sequence", "vehicle.stop_id"),
  scrape_interval = 60,
  log_file = NA
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

## Details

Downloads GTFS-RT data from the specified URL at regular intervals and
saves them to the destination file.

This function will run indefinitely until manually stopped. Each
downloaded file is named with a timestamp to ensure uniqueness.

## Examples

``` r
if (FALSE) { # \dontrun{
GTFShift::rt_collect_protobuf("https://api.example.com/gtfs-rt-protobuf", "gtfs_rt_data.csv")
} # }
```
