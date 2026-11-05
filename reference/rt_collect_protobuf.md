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
  gtfs_rt_url = "https://go.tmlmobilidade.pt/hub/api/v1/0XJC9/vehicles/positions/gtfs.pb",
  destination_file = destination_file,
  scrape_interval = -1 # Negative to run only once
)
#> [2026-11-05 13:48:53] Starting GTFS-RT data collection from https://go.tmlmobilidade.pt/hub/api/v1/0XJC9/vehicles/positions/gtfs.pb
#> [20261105_134853] Iteration 1 completed
#> [1] "/tmp/RtmpYA0vRV/file1e4b677b01d4.csv"

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
#> 1   [1NN3V][HF16N]M37-1-012-A-U-13h40-ELeclercNorte                   38.7213
#> 2   [1NN3V][HF16N]M37-1-012-A-U-13h40-ELeclercNorte                   38.7251
#> 3 [1NN3V][HF16N]M27-1-026-A-U-13h30-CascaisEstacaoA                   38.6959
#> 4 [1NN3V][HF16N]M42-2-006-A-U-13h40-EscolaSecCarcav                   38.7104
#> 5 [1NN3V][HF16N]M30-1-023-A-U-13h40-ParedeTerminalC                   38.6928
#> 6 [1NN3V][HF16N]M33-2-016-A-U-13h45-ELeclercSDoming                   38.7133
#>   vehicle.position.longitude
#> 1                    -9.3443
#> 2                    -9.3426
#> 3                    -9.4390
#> 4                    -9.3295
#> 5                    -9.3545
#> 6                    -9.3370
```
