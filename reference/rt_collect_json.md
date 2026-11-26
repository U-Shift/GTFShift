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
  gtfs_rt_url = "https://go.tmlmobilidade.pt/hub/api/v1/0XJC9/vehicles/positions/gtfs",
  entity_key = "data.entity",
  destination_file = destination_file,
  scrape_interval = -1 # Negative to run only once
)
#> [2026-11-26 11:57:58] Starting GTFS-RT data collection from https://go.tmlmobilidade.pt/hub/api/v1/0XJC9/vehicles/positions/gtfs
#> [20261126_115758] Iteration 1 completed
#> [1] "/tmp/RtmpQ2mk5a/file1e6d76e1a726.csv"

# Read data
if (file.exists(destination_file)) {
  collection <- read.csv(destination_file)
  names(collection)
  head(
    collection |>
      dplyr::select(
        "vehicle.trip.trip_id", "vehicle.position.latitude",
        "vehicle.position.longitude"
      )
  )
}
#>                                vehicle.trip.trip_id vehicle.position.latitude
#> 1 [1NN3V][HF16N]M42-1-005-A-U-12h00-ConceicaodaAbob                  38.73024
#> 2 [1NN3V][HF16N]M26-1-011-A-U-11h55-ParedeTerminalC                  38.69274
#> 3 [1NN3V][HF16N]M22-2-015-A-U-11h05-CascaisTerminal                  38.73511
#> 4 [1NN3V][HF16N]M31-2-015-A-U-11h00-HospitaldeCasca                  38.69030
#> 5 [1NN3V][HF16N]M27-1-019-A-U-11h45-CascaisEstacaoA                  38.70026
#> 6  [1NN3V][HF16N]M38-1-011-A-U-12h00-EstorilEstacao                  38.70349
#>   vehicle.position.longitude
#> 1                  -9.324445
#> 2                  -9.354412
#> 3                  -9.366183
#> 4                  -9.335148
#> 5                  -9.437755
#> 6                  -9.398900
```
