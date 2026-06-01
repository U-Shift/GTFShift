# Get aggregated frequency per hour for each bus route

For each route, returns the number of departures aggregated per hour and
direction.

## Usage

``` r
get_route_frequency_hourly(
  gtfs,
  date = GTFShift::calendar_nextBusinessWednesday(),
  use_osm_routes = NA,
  overline = FALSE
)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- date:

  Date (Default
  [`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)).
  Reference date to consider when analyzing the GTFS file.

- use_osm_routes:

  osmdata::opq (Default NA). If overpass query for transit network is
  defined, analysis is performed considering OSM route geometry, using
  [`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md).

- overline:

  Boolean (Default FALSE). If TRUE, routes are aggregated using
  [`stplanr::overline2()`](https://docs.ropensci.org/stplanr/reference/overline.html),
  overlapping lines and converting them into a single route network.

## Value

An `sf` `data.frame` object with the following columns (the first three
are only present if `overline=FALSE`):

- route_id:

  The `route_id` attribute from `routes.txt` file.

- route_short_name:

  The `route_short_name` attribute from `routes.txt` file.

- shape_id:

  The `shape_id` attribute from `shapes.txt` file.

- direction_id:

  The `direction_id` attribute from `trips.txt` file (if attribute
  present in GTFS feed).

- hour:

  The hour for which the frequency applies (24 hour format).

- frequency:

  The number of services for the route that depart from the first stop
  for the corresponding 60 minutes period.

- geometry:

  The route shape.

## Details

This method analyses the GTFS feed for a representative day, generating
for each route the number of services aggregated per hour and direction.
It assumes the time of departure at the first stop as a reference for
each trip geometry.

By default, it estimates the next business Wednesday, relevant for the
peak hour.

The `overline` parameter enables the aggregation of bus routes that
share common line segments, returning a sum of frequencies per road
segment, using
[`stplanr::overline2()`](https://docs.ropensci.org/stplanr/reference/overline.html).

Optionally, using `use_osm_routes` parameter, it retrieves the
geometries from OpenStreetMap by matching the tag `gtfs:shape_id`,
overwriting the original GTFS `shapes.txt`. This is particularly useful
if the GTFS shapes do not share the same geometry. For instance, if the
edges of the lines do not overlap or do not follow the same
route-over-the-road – which is very common, even besides [GTFS
recommendation](https://gtfs.org/documentation/schedule/schedule-best-practices/#shapestxt)
– geometries might not be aggregated correctly, causing inconsistent
results. By relying on a common road network, such as OSM, it is
possible to overcome this issue and aggregate the bus routes correctly.

For a detailed example, see the
[`vignette("analyse")`](https://u-shift.github.io/GTFShift/articles/analyse.md).

Adapted from <https://github.com/Bondify/GTFS_in_R/>.

## See also

[`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)

[`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)

[`stplanr::overline2()`](https://docs.ropensci.org/stplanr/reference/overline.html)

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")
frequency_analysis <- GTFShift::get_route_frequency_hourly(gtfs)
} # }
```
