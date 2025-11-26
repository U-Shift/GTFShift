# Get OSM routes geometry considering gtfs:shape_id match

Get OSM routes geometry considering gtfs:shape_id match

## Usage

``` r
osm_shapes_to_routes(gtfs, q)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- q:

  osmdata::opq. Overpass query for transit network.

## Value

A `sf` `data.frame` with the following columns:

- `shape_id`, the `shape_id` attribute from `shapes.txt` file.

- `osm_id`, the `osm_id` attribute from OSM route relation.

- `geometry`, the geometrical data for the OSM route relation.

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

q = opq("Lisbon")  |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

shapes_geometry_osm = GTFShift::osm_shapes_to_routes(gtfs, q)
} # }
```
