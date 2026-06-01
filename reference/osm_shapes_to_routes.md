# Get OSM routes geometry considering gtfs:shape_id match

Get OSM routes geometry considering gtfs:shape_id match

## Usage

``` r
osm_shapes_to_routes(
  gtfs,
  q,
  ways = FALSE,
  ways_tags = c("lanes", "psv", "bus", "way", "parking", "name"),
  osm_file = NULL,
  osm_route_type = "bus"
)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- q:

  osmdata::opq. Overpass query for transit network.

- ways:

  boolean (Default False). If true, relation is disaggregated in ways.

- ways_tags:

  character vector (Default
  `c("lanes", "psv", "bus", "way", "parking", "name")`). List of OSM way
  tags to extract when `ways` parameter is set to true. Match is done
  using
  [`tidyselect::contains()`](https://tidyselect.r-lib.org/reference/starts_with.html).

- osm_file:

  character (Optional). Location of OSM extract file with `osm.pbf`
  format. Refer to
  [`osmextract::oe_download()`](https://docs.ropensci.org/osmextract/reference/oe_download.html)
  for more details. If not provided OSM Overpass API is called through
  [`osmdata::osmdata_sf()`](https://docs.ropensci.org/osmdata/reference/osmdata_sf.html).

- osm_route_type:

  character (Default "bus"). OSM route type. Used to query OSM network
  (e.g., 'bus', 'train').

## Value

A `sf` `data.frame` with the following columns:

- shape_id:

  The `shape_id` attribute from `shapes.txt` file.

- osm_id:

  The `osm_id` attribute from OSM route relation.

- way_osm_id:

  The `osm_id` attribute from OSM way (if `ways` parameter is set to
  true).

- \*:

  Any column that matches `ways_tags` parameter.

- geometry:

  The geometrical data for the OSM route relation.

Shapes that do not have a match on OSM are ignored. If that occurs, a
warning is displayed during the method execution, informing about the
missing geometries.

## Details

For each route, matches its trips' shapes with OSM route relations,
considering the OSM `gtfs:shape_id` attribute.

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")

q <- opq("Lisbon") |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

# To use OSM API:
shapes_geometry_osm <- GTFShift::osm_shapes_to_routes(gtfs, q)

# To use a local OSM file:
osm_file <- oe_download("https://download.geofabrik.de/europe/portugal-latest.osm.pbf")
shapes_geometry_osm <- GTFShift::osm_shapes_to_routes(gtfs, q, osm_file = osm_file)
} # }
```
