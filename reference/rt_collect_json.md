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
#> [2026-08-27 14:40:35] Starting GTFS-RT data collection from https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs
#> [20260827_144035] Iteration 1 completed
#> [1] "/tmp/RtmpIOJuU6/file1f4a45210c30.csv"

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
#>               vehicle.trip.trip_id vehicle.position.latitude
#> 1  [0277F][BNA17]2714_0_2|1|3|1530                  38.80131
#> 2      [76XA2][N18KL]4424_20251214                  39.04020
#> 3 [F1M13][A2L1N]4701_0_2|2700|1500                  38.68588
#> 4     [76XA2][N18KL]16438_20251214                  38.74543
#> 5  [0277F][BNA17]2610_0_2|1|3|1530                  38.78644
#> 6  [0277F][BNA17]2781_0_2|1|3|1515                  38.79710
#>   vehicle.position.longitude
#> 1                  -9.122298
#> 2                  -8.921500
#> 3                  -8.939689
#> 4                  -9.140616
#> 5                  -9.182842
#> 6                  -9.176847
```
