# Get aggregated frequency per hour for each OSM way

For each OSM way with GTFS service, returns the number of departures
aggregated per hour and direction.

## Usage

``` r
get_way_frequency_hourly(
  gtfs,
  q,
  date = GTFShift::calendar_nextBusinessWednesday(),
  keep_osm_attributes = FALSE
)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- q:

  osmdata::opq. Overpass query for transit network, to obtain OSM route
  ways, using
  [`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md).

- date:

  Date (Default
  [`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)).
  Reference date to consider when analyzing the GTFS file.

- keep_osm_attributes:

  Boolean (Default FALSE). Whether to keep all OSM way attributes in the
  output `sf` object.

## Value

An `sf` `data.frame` object with the following columns:

- `way_osm_id`, the `osm_id` attribute from OSM way.

- `hour`, the hour for which the frequency applies (24 hour format).

- `frequency`, the number of services for the route that depart from the
  first stop for the corresponding 60 minutes period.

- `routes`, the list of route_ids that use the way.

- `shapes`, the list of shape_ids that use the way.

- `geometry`, the route shape.

- (if `keep_osm_attributes = TRUE`) all OSM way attributes.

## Details

This method analyses the GTFS feed for a representative day, finding for
each route the corresponding OSM ways using
[`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)
(routes not on OSM are ignored), aggregating the number of services per
hour and direction for each.

For a detailed example, see the
[`vignette("analyse")`](https://u-shift.github.io/GTFShift/articles/analyse.md).

## See also

[`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)

[`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs = GTFShift::load_feed("gtfs.zip")
q = opq(bbox=sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |> add_osm_feature(key = "route", value = "bus")
frequency_analysis = GTFShift::get_way_frequency_hourly(gtfs, q)
} # }
```
