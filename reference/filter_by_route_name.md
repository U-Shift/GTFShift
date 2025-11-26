# Filter GTFS feed by route name

Filter GTFS feed by route name

## Usage

``` r
filter_by_route_name(gtfs, values, short_name = TRUE, exact_match = TRUE)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- values:

  String\[\]. List of the route names to filter the feed.

- short_name:

  Boolean. If TRUE, query for route_short_name, otherwise,
  route_long_name is considered.

- exact_match:

  Boolean. If TRUE, route name is queried for an exact match, otherwise,
  partial match is considered.

## Value

A tidygtfs object with the filtered feed.

## Details

On a GTFS feed, the `route_id` rarely matches the real name of the
route, that can range from numbers, letters, words or combinations of
both. This method allows to filter the feed for the route short or long
name, with a partial or exact match.

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")
gtfs_filtered <- GTFShift::filter_by_route_name(gtfs, list("Blue line", "Red line"))
} # }
```
