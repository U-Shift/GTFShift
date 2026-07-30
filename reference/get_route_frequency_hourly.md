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

sf data.frame. Hourly route frequencies, with the following columns (the
first three are only present if `overline=FALSE`):

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

For a detailed example, see the `vignette("analyse")`.

Adapted from
[github.com/Bondify/GTFS_in_R](https://web.archive.org/web/20201223060409/https://github.com/Bondify/GTFS_in_R/).

## See also

[`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)

[`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)

[`stplanr::overline2()`](https://docs.ropensci.org/stplanr/reference/overline.html)

## Examples

``` r
# Subset GTFS for one route only, for demo purposes
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip", package = "GTFShift")
)
gtfs <- GTFShift::filter_by_route_name(gtfs, c("1", "2", "3", "4"))

# Get frequency
frequency_analysis <- GTFShift::get_route_frequency_hourly(
  gtfs,
  date = gtfs$calendar$start_date[1]
)
#> Analysing GTFS for 2026-06-10...
#> > Filtering by reference date 2026-06-10...

head(frequency_analysis |> sf::st_drop_geometry())
#> # A tibble: 6 × 6
#>   route_id       shape_id     route_short_name direction_id  hour frequency
#>   <chr>          <chr>        <chr>                   <int> <int>     <int>
#> 1 3_3-SA-TERM_R2 3-SA-TERM_R2 3                           0    20         1
#> 2 3_3-TER-CS_CAS 3-TER-CS_CAS 3                           1    23         1
#> 3 3_3-TER-SA_CAS 3-TER-SA_CAS 3                           1     0         1
#> 4 3_3-TER-SA_CAS 3-TER-SA_CAS 3                           1     1         1
#> 5 3_3-TERM-SA_LC 3-TERM-SA_LC 3                           1    10         1
#> 6 3_3-TERM-SA_R2 3-TERM-SA_R2 3                           1     6         1
```
