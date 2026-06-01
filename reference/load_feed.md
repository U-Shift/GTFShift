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

A tidygtfs object.

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
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("https://operator.com/gtfs.zip")
} # }
```
