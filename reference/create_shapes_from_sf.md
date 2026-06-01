# Build shapes from simple feature object

Build shapes from simple feature object

## Usage

``` r
create_shapes_from_sf(sf_shapes, gtfs)
```

## Arguments

- sf_shapes:

  sf object associating `shape_id` with an sf object (either LINESTRING
  or MULTILINESTRING)

- gtfs:

  tidygtfs. GTFS feed.

## Value

A `data.table` representing a GTFS shapes table.

## Details

This function builds the shapes.txt file from a simple feature object.
It first converts any MULTILINESTRING geometries to LINESTRING
geometries using the `multiline_to_sorted_linestring`, with the first
stop of each trip as the starting point. Then, it converts the
LINESTRING geometries to a data.table representing a GTFS shapes table
using
[`gtfstools::convert_sf_to_shapes`](https://rdrr.io/pkg/gtfstools/man/convert_sf_to_shapes.html).

## See also

[`gtfstools::convert_sf_to_shapes`](https://rdrr.io/pkg/gtfstools/man/convert_sf_to_shapes.html)

[`GTFShift::multiline_to_sorted_linestring`](https://u-shift.github.io/GTFShift/reference/multiline_to_sorted_linestring.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")
q <- opq("Lisbon") |>
    add_osm_feature(key = "route", value = c("bus", "tram")) |>
    add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

shapes_sf <- GTFShift::osm_shapes_to_routes(gtfs, q)

gtfs$shapes <- GTFShift::create_shapes_from_sf(shapes_sf, gtfs)
} # }
```
