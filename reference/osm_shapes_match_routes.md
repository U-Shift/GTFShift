# Get OSM routes that match shapes, based on geometrical match

Get OSM routes that match shapes, based on geometrical match

## Usage

``` r
osm_shapes_match_routes(
  gtfs,
  q,
  geometry = TRUE,
  gtfs_match = "route_short_name",
  osm_match = "ref",
  gtfs_osm_match_exact = TRUE,
  log_file = NA
)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- q:

  osmdata::opq. Overpass query for transit network

- geometry:

  Boolean (Default TRUE). If TRUE, returns sf object with geometry,
  otherwise, a simple data.frame.

- gtfs_match:

  String (Default route_short_name). routes.txt attribute that
  identifies routes. Accepted values: route_id, route_short_name,
  route_long_name.

- osm_match:

  String (Default ref). OSM attribute that identifies routes by matching
  with gtfs_match. Accepted values: ref, name, gtfs:route_id.

- gtfs_osm_match_exact:

  Boolean (Default TRUE). If TRUE, gtfs and route names are matched
  strictly. Otherwise, partial string match is considered (all words in
  gtfs_match must be in osm_match, ignoring case).

- log_file:

  String (Optional). If provided, will log warnings to this file, in
  addition to the console.

## Value

A `data.frame` (`sf` if `geometry=TRUE`) with the following columns:

- `route_id`, the `route_id` attribute from `routes.txt` file.

- `shape_id`, the `shape_id` attribute from `shapes.txt` file.

- `osm_id`, the `osm_id` attribute from OSM route relation.

- `distance_diff`, the difference, in meters, between GTFS shape and OSM
  route lengths.

- `points_diff`, the sum of the difference, in meters, between GTFS
  shape and OSM route start and end points.

- `stops_diff`, the difference between GTFS and OSM routes number of
  stops.

- `route_short_name`, the `route_short_name` attribute from `routes.txt`
  file.

- `route_long_name`, the `route_long_name` attribute from `routes.txt`
  file.

- `osm_ref`, the `ref` attribute from OSM route relation.

- `osm_name`, the `name` attribute from OSM route relation.

- `geometry`, the geometrical data for the OSM route relation.

## Details

For each route, matches its trips' shapes with OSM route relations.

The calculation is performed considering, for each GTFS route, the
subset of OSM routes that match the route identifier (based on
`gtfs_match` and `osm_match`). Then, for each shape, the geometrical
match is performed considering the OSM route that minimizes the distance
between start and end points, total length and number of stops.

Be aware that the result might ignore some GTFS routes, in the following
cases:

- If there is no OSM route relation that matches the GTFS route
  identifier;

- If, for a GTFS route, there is any OSM route relation that has
  entry/exit stops not respecting the right order;

- If, for the same route, distinct shapes are associated to the same OSM
  route.

If any of these errors occurs, warnings will be thrown at end of the
method execution, and those GTFS route will be ignored in the results.

Nevertheless, provided there are enough OSM routes, all the GTFS shapes
for each route will necessarily be associated with an OSM one. This
might generate wrong results if the topology of routes on OSM does not
match the GTFS shapes for that route. Refer to `distance_diff`,
`points_diff` and `stops_diff` on the results table to validate the
results and identify misassociations.

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")

q <- opq("Lisbon")  |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

shapes_match_routes <- GTFShift::osm_shapes_match_routes(gtfs, q)
} # }
```
