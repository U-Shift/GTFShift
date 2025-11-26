# Filter GTFS feed by mode

Filter GTFS feed by mode

## Usage

``` r
filter_by_modes(gtfs, modes = list())
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- modes:

  Integer\[\]. A list with the ids of modes to consider.

## Value

A tidygtfs object with the filtered feed.

## Details

Allows to filter a GTFS feed for the type of transportation used,
allowing for a more narrow analysis of multimodal files. Refer to
`routes.txt` `route_type` parameter on [GTFS
documentation](https://gtfs.org/documentation/schedule/reference/#routestxt)
for more details.

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")
gtfs_filtered <- GTFShift::filter_by_modes(gtfs, list(0,1))
} # }
```
