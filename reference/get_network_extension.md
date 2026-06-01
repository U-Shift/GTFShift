# Get network routes extension

Get total extension of GTFS feed routes

## Usage

``` r
get_network_extension(
  gtfs,
  route_identifier = "route_id",
  direction_wise = TRUE,
  unified = FALSE,
  date = GTFShift::calendar_nextBusinessWednesday(),
  use_osm_routes = NA
)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- direction_wise:

  Boolean (Default `TRUE`). If TRUE, extension considers sum of both
  directions. Otherwise, only one direction is considered.

- unified:

  Boolean (Default `FALSE`). If TRUE, overlapping route segments are
  only counted once in the total extension.

- date:

  Date (Default
  [`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)).
  Reference date to consider when analyzing the GTFS file.

- use_osm_routes:

  osmdata::opq (Default NA). If overpass query for transit network is
  defined, analysis is performed considering OSM route geometry, using
  [`GTFShift::osm_shapes_to_routes`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md).

- route_identifier.:

  String. (Default `"route_id"`). routes.txt attribute that identifies
  routes. Accepted values: route_id, route_short_name, route_long_name.

## Value

The routes extension, in meters.

## Details

This method calculates the sum of the GTFS feed routes length,
considering, for each, the shape of the variant with the highest
frequency for the given date (using
[`GTFShift::get_route_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_route_frequency_hourly.md)).
For a detailed example, see the
[`vignette("analyse")`](https://u-shift.github.io/GTFShift/articles/analyse.md).

## See also

\[GTFShift::get_route_frequency_hourly()\]

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")
route_extension <- GTFShift::get_network_extension(gtfs)
} # }
```
