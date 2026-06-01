# Merge multiple GTFS into a single aggregated file

Merge multiple GTFS into a single aggregated file

## Usage

``` r
unify(
  ...,
  prefix = FALSE,
  store_path = NA,
  create_transfers = FALSE,
  transfer_distance = 300,
  transfer_time = 120,
  transfer_street_routing = FALSE
)
```

## Arguments

- ...:

  tidygtfs\[\]. List of GTFS feeds.

- prefix:

  Boolean (Default FALSE). If TRUE, prefixes all tables with the
  agency_id, to avoid conflicts in case of same IDs in different feeds.

- store_path:

  String (Optional). If provided, aggregated feed zip is stored at
  location. The file is overwritten if it already exists.

- create_transfers:

  Boolean (Default FALSE). When true, generates transfers table,
  aggregating close stops, even if from different GTFS.

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

## Value

A tidygtfs object.

## Details

Aggregates multiple feeds using
[`gtfstools::merge_gtfs()`](https://rdrr.io/pkg/gtfstools/man/merge_gtfs.html).
When generating transfers, those already existing in each GTFS file are
kept, extended with new ones computed based on the stops network of the
final aggregated version.

This computation is executed with
[`gtfsrouter::gtfs_transfer_table()`](https://rdrr.io/pkg/gtfsrouter/man/gtfs_transfer_table.html),
with the parameters `d_limit=transfer_distance`,
`min_transfer_time=transfer_time` and
`network_times=transfer_street_routing`. The other parameters are
applied the library default values.

For a detailed example, see the
[`vignette("unify")`](https://u-shift.github.io/GTFShift/articles/unify.md).

## See also

[`gtfstools::merge_gtfs()`](https://rdrr.io/pkg/gtfstools/man/merge_gtfs.html)

[`gtfsrouter::gtfs_transfer_table()`](https://rdrr.io/pkg/gtfsrouter/man/gtfs_transfer_table.html)

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs1 <- GTFShift::load_feed("gtfs1.zip")
gtfs2 <- GTFShift::load_feed("gtfs2.zip")
unified <- GTFShift::unify(gtfs1, gtfs2, create_transfers = TRUE)
} # }
```
