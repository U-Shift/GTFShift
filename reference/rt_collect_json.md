# Collect GTFS-RT data from a JSON feed at regular intervals

Collect GTFS-RT data from a JSON feed at regular intervals

## Usage

``` r
rt_collect_json(
  gtfs_rt_url,
  destination_file,
  header_key = "header",
  entity_key = "entity",
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

  String. URL of the GTFS-RT feed in JSON format.

- destination_file:

  String. File to save the downloaded GTFS-RT data. Content is appended
  in each iteration.

- header_key:

  String (Default "header"). Key in the JSON corresponding to the feed
  header. Set to NA if not present.

- entity_key:

  String (Default "entity"). Key in the JSON corresponding to the feed
  entities. Set to NA if response is a flat list. Use "." for nested
  keys.

- fields_collect:

  Character vector. Fields to extract from each entity in the feed. Use
  "." for nested keys.

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
GTFShift::rt_collect_json(
  gtfs_rt_url = "https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs",
  entity_key = "data.entity",
  destination_file = destination_file,
  scrape_interval = -1 # Negative to run only once
)
#> [2026-07-30 14:52:34] Starting GTFS-RT data collection from https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs
#> [20260730_145234] Iteration 1 completed
#> [1] "/tmp/RtmpX8K2XZ/file1ec24dfc71ff.csv"

# Read data
collection <- read.csv(destination_file)

names(collection)
#> [1] "id"                            "vehicle.trip.trip_id"         
#> [3] "vehicle.position.latitude"     "vehicle.position.longitude"   
#> [5] "vehicle.position.speed"        "vehicle.timestamp"            
#> [7] "vehicle.current_status"        "vehicle.current_stop_sequence"
#> [9] "vehicle.stop_id"              

head(
  collection |>
    dplyr::select("vehicle.trip.trip_id", "vehicle.position.latitude", "vehicle.position.longitude")
)
#>                vehicle.trip.trip_id vehicle.position.latitude
#> 1 [Y8LCX][BNA17]2106_0_2|140|3|1455                  38.90797
#> 2   [Y8LCX][BNA17]2727_0_1|1|3|1530                  38.81549
#> 3   [Y8LCX][BNA17]2781_0_2|1|3|1545                  38.78255
#> 4 [Y8LCX][BNA17]2740_0_2|140|3|1510                  38.92623
#> 5 [Y8LCX][BNA17]2756_0_1|140|3|1500                  38.90348
#> 6   [Y8LCX][BNA17]2303_0_1|1|3|1540                  38.90309
#>   vehicle.position.longitude
#> 1                  -9.246960
#> 2                  -9.121830
#> 3                  -9.188004
#> 4                  -9.296973
#> 5                  -9.190214
#> 6                  -9.048524
```
