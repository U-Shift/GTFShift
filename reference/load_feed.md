# Read GTFS feed, fixing integrity errors

Read GTFS feed, fixing integrity errors

## Usage

``` r
load_feed(
  path,
  store_path = NA,
  create_transfers = FALSE,
  transfer_distance = 300,
  transfer_time = 120,
  transfer_street_routing = FALSE,
  headers = NULL
)
```

## Arguments

- path:

  String. The location of the GTFS zip file. Either local or URL.

- store_path:

  String (Optional). If provided, GTFS feed zip is stored at location.
  The file is overwritten if it already exists.

- create_transfers:

  Boolean (Default FALSE). When true, generates `transfers.txt`,
  aggregating close stops.

- transfer_distance:

  Integer (Default 300). Upper straight-line distance limit in meters
  for transfers.

- transfer_time:

  Integer (Default 120). Minimum time in seconds for transfers; all
  values below this will be replaced with this value, particularly all
  those defining in-place transfers where stop longitudes and latitudes
  remain identical.

- transfer_street_routing:

  Boolean (Default FALSE). If TRUE, transfer times are calculated by
  routing throughout the underlying street network (downloaded
  automatically).

- headers:

  Named list or character vector (Optional). Custom HTTP headers for
  credentials when accessing the GTFS zip file URL.

## Value

tidygtfs. The loaded GTFS feed.

## Details

In addition to loading the GTFS feed, this method validates its
integrity and applies the proper corrections if it does not comply with
the following validations:

- `stop_times.txt` with empty `arrival_time` or `departure_time`,
  filtering rows that do not comply.

- Feeds with missing `shapes.txt` file, generating it using
  [`GTFShift::create_shapes_from_stops()`](https://u-shift.github.io/GTFShift/reference/create_shapes_from_stops.md).

When generating transfers, those already existing in each GTFS file are
kept, extended with new ones computed based on the stops network of the
final aggregated version. This computation is executed with
[`gtfsrouter::gtfs_transfer_table()`](https://rdrr.io/pkg/gtfsrouter/man/gtfs_transfer_table.html),
with the parameters `d_limit=transfer_distance`,
`min_transfer_time=transfer_time` and
`network_times=transfer_street_routing`. The other parameters are
applied the library default values.

## See also

[`GTFShift::create_shapes_from_stops()`](https://u-shift.github.io/GTFShift/reference/create_shapes_from_stops.md)

[`tidytransit::read_gtfs()`](https://r-transit.github.io/tidytransit/reference/read_gtfs.html)

[`gtfsrouter::gtfs_transfer_table()`](https://rdrr.io/pkg/gtfsrouter/man/gtfs_transfer_table.html)

## Examples

``` r
# Simple call
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip", package = "GTFShift")
)

summary(gtfs)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, shapes, calendar, calendar_dates, stops
#> agency       Transportes Colectivos do Barreiro
#> service      from 2026-06-08 to 2026-12-31
#> uses         stop_times (no frequencies)
#> # routes      27
#> # trips       40
#> # stop_ids   228
#> # stop_names 153
#> # shapes      27


# Simple call with missing shapes (triggering shapes creation because missing on GTFS file)
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_ttsl_sample_no_shapes.zip", package = "GTFShift")
)
#> Warning: > CREATED shapes.txt, the file was missing!

summary(gtfs)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, calendar, calendar_dates, stops
#> agency       TTSL - Transtejo Soflusa
#> service      from 2020-12-19 to 2028-12-31
#> uses         stop_times (no frequencies)
#> # routes      1
#> # trips      100
#> # stop_ids    3
#> # stop_names  3
#> # shapes      5


# With some parameters to build transfers and store to given location
store_path <- withr::local_tempfile(fileext = ".zip")

gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip", package = "GTFShift"), create_transfers = TRUE, store_path
)
#> Registered S3 method overwritten by 'gtfsrouter':
#>   method       from  
#>   summary.gtfs gtfsio

head(gtfs$transfers)
#> # A tibble: 6 × 4
#>   from_stop_id to_stop_id transfer_type min_transfer_time
#>   <chr>        <chr>              <dbl>             <int>
#> 1 000012       000143                 2               170
#> 2 000012       000159                 2               243
#> 3 000012       000041                 2               164
#> 4 000013       000014                 2               319
#> 5 000013       000039                 2               394
#> 6 000013       000040                 2               120

file.exists(store_path)
#> [1] TRUE
```
