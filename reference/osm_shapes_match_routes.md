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
  osm_route_type = "bus",
  metric_crs = 3857
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

- metric_crs:

  Integer or character (Default 3857). Projected CRS used to compute
  shapes and routes lengths and stop-to-stop distances.

## Value

data.frame. Matched routes (`sf` if `geometry=TRUE`) with the following
columns:

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

The matching algorithm is formulated as follows: Let \\R\\ be a GTFS
route identifier.

**1. Filtering and Base Data Selection:** Let \\\mathcal{O}\_R = \\O_1,
\dots, O_m\\\\ be the set of candidate OSM route relations matching the
identifier \\R\\ (based on `gtfs_match` and `osm_match`). If
\\\mathcal{O}\_R\\ is empty, route \\R\\ is skipped. Unless
`osm_stop_order_relaxed = TRUE`, any relation in \\\mathcal{O}\_R\\ with
entry/exit stops not in the correct order is discarded. We also retrieve
the set of GTFS shapes associated with route \\R\\, denoted as
\\\mathcal{S}\_R = \\S_1, \dots, S_n\\\\.

**2. Feature Extraction:** For each GTFS shape \\S_i \in
\mathcal{S}\_R\\:

- Extract the start and end coordinates of its trips' first and last
  stops: \\\text{init}\_{GTFS, i}\\ and \\\text{fin}\_{GTFS, i}\\.

- Compute the shape's total length \\L\_{GTFS, i}\\ and the number of
  stop times \\N\_{stops, i}\\.

For each candidate OSM route relation \\O_j \in \mathcal{O}\_R\\:

- Extract the coordinates of the first and last stops/platforms:
  \\\text{init}\_{OSM, j}\\ and \\\text{fin}\_{OSM, j}\\.

- Compute the relation's geometry length \\L\_{OSM, j}\\ and the number
  of stop/platform nodes \\N\_{stops, j}\\.

**3. Closeness Metric Evaluation:** For each GTFS shape \\S_i\\, we
calculate the closeness metric \\C(i, j)\\ for all candidate OSM routes
\\O_j \in \mathcal{O}\_R\\: \$\$C(i, j) = d(\text{init}\_{GTFS, i},
\text{init}\_{OSM, j}) + d(\text{fin}\_{GTFS, i}, \text{fin}\_{OSM,
j}) + \|L\_{GTFS, i} - L\_{OSM, j}\| + \frac{L\_{GTFS, i}}{N\_{stops,
i}} \cdot \|N\_{stops, i} - N\_{stops, j}\|\$\$

where:

- \\d(\cdot)\\ is the Euclidean distance.

- The term \\\frac{L\_{GTFS, i}}{N\_{stops, i}}\\ represents the average
  distance between stops on the GTFS shape, serving as a scale factor
  for the difference in the number of stops.

Shape \\S_i\\ is associated with the OSM route \\O\_{j^\*}\\ that
minimizes the closeness metric: \$\$j^\* = \operatorname{argmin}\_{j}
C(i, j)\$\$

**4. Conflict Resolution:** If multiple GTFS shapes are associated with
the same OSM route \\O_j\\, only the shape \\S_i\\ that minimizes the
closeness metric is retained. The other conflicting shapes are ignored
and a warning is triggered.

Be aware that the result might ignore some GTFS routes, in the following
cases:

- If there is no OSM route relation that matches the GTFS route
  identifier;

- If, for a GTFS route, there is any OSM route relation that has
  entry/exit stops not respecting the right order (unless
  `osm_stop_order_relaxed` is set to TRUE);

- If, for the same route, distinct shapes are associated to the same OSM
  route. In that case, only the shape that minimizes the closeness
  metric is retained.

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
# Subset GTFS for one route only, for demo purposes
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip",
  package = "GTFShift"
))
gtfs <- GTFShift::filter_by_route_name(gtfs, c("1", "2", "3", "4"))

# Build query and prepare osm extract (possible to use API as alternative)
q <- osmdata::opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |>
  osmdata::add_osm_feature(key = "route", value = "bus") |>
  osmdata::add_osm_feature(key = "operator", value = "Transportes Colectivos do Barreiro")
osm_file <- system.file("extdata/samples", "osmextract_tcb_network.pbf", package = "GTFShift")

# Get OSM route geometries based on geometrical match
shapes_osm_routes <- GTFShift::osm_shapes_match_routes(
  gtfs, q,
  osm_file = osm_file,
  metric_crs = 3763, # Make sure to addapt to the projection that better suits your location
)
#> > Found 12 GTFS shapes and 101 stops
#> > Found 75 OSM route relations and 233 stops/platforms
#> > Unpacking results
#> > Combining results
#> > Re-attaching OSM metadata and geometries
#> > Getting route metadata
#> > Associated 12 shapes (100.00% of 12 total) of 12 routes (100.00% of 12 total) with OSM routes, corresponding to 16 trips (100.00% of 16 total), with a mean distance of 41.02 meters for points, 83.69 meters for route length and a mean difference of 0.17 stops
#> > Of those, 12 shapes (100.00% of 12 matched) have a distance difference below 1000 meters AND a points difference below 500 meters

head(shapes_osm_routes |> dplyr::select(shape_id, osm_id, distance_diff, points_diff, stops_diff))
#> Simple feature collection with 6 features and 5 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.081368 ymin: 38.62453 xmax: -9.025695 ymax: 38.66264
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 6
#>   shape_id osm_id distance_diff points_diff stops_diff                  geometry
#>   <chr>    <chr>          <dbl>       <dbl>      <dbl>     <MULTILINESTRING [°]>
#> 1 1-QVBB-… 18957…         133.       409.            1 ((-9.050197 38.66117, -9…
#> 2 1-TERM   94486…          60.1       11.9           1 ((-9.078279 38.65236, -9…
#> 3 2-TERM   94486…          38.9       16.2           0 ((-9.078345 38.65221, -9…
#> 4 3-SA-TE… 18958…          74.5        6.83          0 ((-9.031332 38.62453, -9…
#> 5 3-SA-TE… 18958…          99.4        6.83          0 ((-9.031332 38.62453, -9…
#> 6 3-TERM-… 11728…         133.         5.06          0 ((-9.078088 38.65212, -9…
```
