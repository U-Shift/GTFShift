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

tidygtfs. The filtered GTFS feed.

## Details

On a GTFS feed, the `route_id` rarely matches the real name of the
route, that can range from numbers, letters, words or combinations of
both. This method allows to filter the feed for the route short or long
name, with a partial or exact match.

## Examples

``` r
# Load GTFS
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


# Filter by route
gtfs_route <- GTFShift::filter_by_route_name(gtfs, c("4"))

summary(gtfs_route)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, shapes, calendar, calendar_dates, stops
#> agency       Transportes Colectivos do Barreiro
#> service      from 2026-06-08 to 2026-12-11
#> uses         stop_times (no frequencies)
#> # routes     1
#> # trips      1
#> # stop_ids   10
#> # stop_names 10
#> # shapes     1
```
