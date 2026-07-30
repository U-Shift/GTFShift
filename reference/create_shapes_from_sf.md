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

data.frame. A GTFS shapes table. Includes `shape_dist_traveled` if
`shape_dist_traveled = TRUE`.

## Details

This function builds the shapes.txt file from a simple feature object.

It first converts any MULTILINESTRING geometries to LINESTRING
geometries using the `multiline_to_sorted_linestring`, using a point
guide per shape: all ordered stops when the selected trip is circular
(first and last `stop_id` are equal), or the first two stops otherwise.
Then, it converts the LINESTRING geometries to a data.frame representing
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
# Load sample GTFS
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip", package = "GTFShift")
)

# Load TCB OSM routes sample linestring
osm_routes = sf::st_read(
  system.file("extdata/samples", "osm_routes_tcb.gpkg", package = "GTFShift"),
  quiet = TRUE
) |> dplyr::filter(shape_id %in% gtfs$shapes$shape_id) |> dplyr::sample_n(1)

head(osm_routes)
#> Simple feature collection with 1 feature and 3 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.081368 ymin: 38.62453 xmax: -9.031332 ymax: 38.66264
#> Geodetic CRS:  WGS 84
#>     osm_id     shape_id       route_id                           geom
#> 1 18958058 3-SA-TERM_R2 3_3-SA-TERM_R2 MULTILINESTRING ((-9.031332...

# Create shapes.txt for geometries
shapes_txt <- GTFShift::create_shapes_from_sf(
  osm_routes, gtfs, 
  metric_crs = 3763, # Make sure to addapt to the projection that better suits your location
  shape_dist_traveled = TRUE
)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

head(shapes_txt)
#>        shape_id shape_pt_lon shape_pt_lat shape_pt_sequence shape_dist_traveled
#>          <char>        <num>        <num>             <int>               <num>
#> 1: 3-SA-TERM_R2    -9.031332     38.62453                 1             0.00000
#> 2: 3-SA-TERM_R2    -9.031376     38.62458                 2             0.00000
#> 3: 3-SA-TERM_R2    -9.031686     38.62495                 3            50.05256
#> 4: 3-SA-TERM_R2    -9.031686     38.62495                 4            50.05256
#> 5: 3-SA-TERM_R2    -9.031714     38.62498                 5            60.06307
#> 6: 3-SA-TERM_R2    -9.031745     38.62501                 6            60.06307
```
