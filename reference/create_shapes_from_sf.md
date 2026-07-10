# Build shapes from simple feature object

Build shapes from simple feature object

## Usage

``` r
create_shapes_from_sf(
  sf_shapes,
  gtfs,
  metric_crs = 3857,
  shape_dist_traveled = FALSE
)
```

## Arguments

- sf_shapes:

  sf object associating `shape_id` with an sf object (either LINESTRING
  or MULTILINESTRING).

- gtfs:

  tidygtfs. GTFS feed.

- metric_crs:

  numeric (Default 3857). EPSG code for a metric CRS used when computing
  distances (passed to `multiline_to_sorted_linestring`).

- shape_dist_traveled:

  Boolean (Default FALSE). If TRUE, computes `shape_dist_traveled` for
  each generated shape.

## Value

A `data.table` representing a GTFS shapes table. Includes
`shape_dist_traveled` if `shape_dist_traveled = TRUE`.

## Details

This function builds the shapes.txt file from a simple feature object.

It first converts any MULTILINESTRING geometries to LINESTRING
geometries using the `multiline_to_sorted_linestring`, using a point
guide per shape: all ordered stops when the selected trip is circular
(first and last `stop_id` are equal), or only the first stop otherwise.
Then, it converts the LINESTRING geometries to a data.table representing
a GTFS shapes table using
[`gtfstools::convert_sf_to_shapes`](https://rdrr.io/pkg/gtfstools/man/convert_sf_to_shapes.html).

Coordinates are 4326 (WGS 84) by default, following GTFS specifications.

Optionally, when `shape_dist_traveled = TRUE`, it estimates cumulative
distance along each shape for all generated points and appends this as
`shape_dist_traveled`. This metric is computed in the units of
`metric_crs`, using
[`GTFShift::project_points_along_geometry()`](https://u-shift.github.io/GTFShift/reference/project_points_along_geometry.md).

## See also

[`gtfstools::convert_sf_to_shapes()`](https://rdrr.io/pkg/gtfstools/man/convert_sf_to_shapes.html)

[`GTFShift::multiline_to_sorted_linestring()`](https://u-shift.github.io/GTFShift/reference/multiline_to_sorted_linestring.md)

[`GTFShift::project_points_along_geometry()`](https://u-shift.github.io/GTFShift/reference/project_points_along_geometry.md)

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
