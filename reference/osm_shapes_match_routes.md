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
  log_file = NA,
  osm_file = NULL,
  num_cores = 1,
  osm_stop_order_relaxed = FALSE,
  osm_route_type = "bus"
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

- osm_file:

  character (Optional). Location of OSM extract file with `osm.pbf`
  format. Refer to
  [`osmextract::oe_download()`](https://docs.ropensci.org/osmextract/reference/oe_download.html)
  for more details. If not provided OSM Overpass API is called through
  [`osmdata::osmdata_sf()`](https://docs.ropensci.org/osmdata/reference/osmdata_sf.html).

- num_cores:

  Integer (Default 1). Number of cores to use for parallel computation.
  Only supported on Unix-like systems (Linux, macOS).

- osm_stop_order_relaxed:

  Boolean (Default FALSE). If TRUE, OSM routes with entry/exit stops not
  respecting the right order will still be matched (this may indicate
  OSM data integrity problems). If FALSE, these routes will be ignored.

- osm_route_type:

  character (Default "bus"). OSM route type. Used to query OSM network
  (e.g., 'bus', 'train').

## Value

A `data.frame` (`sf` if `geometry=TRUE`) with the following columns:

- route_id:

  The `route_id` attribute from `routes.txt` file.

- shape_id:

  The `shape_id` attribute from `shapes.txt` file.

- osm_id:

  The `osm_id` attribute from OSM route relation.

- distance_diff:

  The difference, in meters, between GTFS shape and OSM route lengths.

- points_diff:

  The sum of the difference, in meters, between GTFS shape and OSM route
  start and end points.

- stops_diff:

  The difference between GTFS and OSM routes number of stops.

- route_short_name:

  The `route_short_name` attribute from `routes.txt` file.

- route_long_name:

  The `route_long_name` attribute from `routes.txt` file.

- osm_ref:

  The `ref` attribute from OSM route relation.

- osm_name:

  The `name` attribute from OSM route relation.

- geometry:

  The geometrical data for the OSM route relation.

## Details

For each route, matches its trips' shapes with OSM route relations.

The calculation is performed considering, for each GTFS route, the
subset of OSM routes that match the route identifier (based on
`gtfs_match` and `osm_match`). Then, for each shape, the geometrical
match is performed considering the OSM route \\j\\ that minimizes the
closeness metric \\C(i, j)\\ for GTFS shape \\i\\:

\$\$C(i, j) = d(\text{init}\_{GTFS, i}, \text{init}\_{OSM, j}) +
d(\text{fin}\_{GTFS, i}, \text{fin}\_{OSM, j}) + \|L\_{GTFS, i} -
L\_{OSM, j}\| + \frac{L\_{GTFS, i}}{N\_{stops, i}} \cdot \|N\_{stops,
i} - N\_{stops, j}\|\$\$

where:

- \\d(\text{init}\_{GTFS, i}, \text{init}\_{OSM, j})\\ is the distance
  between the starting points/stops of the GTFS shape \\i\\ and the OSM
  route \\j\\.

- \\d(\text{fin}\_{GTFS, i}, \text{fin}\_{OSM, j})\\ is the distance
  between the ending points/stops of the GTFS shape \\i\\ and the OSM
  route \\j\\.

- \\L\_{GTFS, i}\\ and \\L\_{OSM, j}\\ are the total lengths of the GTFS
  shape \\i\\ and the OSM route \\j\\, respectively.

- \\N\_{stops, i}\\ and \\N\_{stops, j}\\ are the number of stops on the
  GTFS shape \\i\\ and the OSM route \\j\\, respectively. The term
  \\\frac{L\_{GTFS, i}}{N\_{stops, i}}\\ represents the average distance
  between stops on the GTFS shape, serving as a scale factor for the
  difference in the number of stops.

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

q <- opq("Lisbon") |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

# To use OSM API:
shapes_match_routes <- GTFShift::osm_shapes_match_routes(gtfs, q)

# To use a local OSM file:
osm_file <- oe_download("https://download.geofabrik.de/europe/portugal-latest.osm.pbf")
shapes_match_routes <- GTFShift::osm_shapes_match_routes(gtfs, q, osm_file = osm_file)
} # }
```
