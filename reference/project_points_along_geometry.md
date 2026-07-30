# Project points onto a linear geometry

Projects point geometries to the closest location along a single
LINESTRING or MULTILINESTRING and estimates each projected point
position as cumulative distance from the start of the line.

## Usage

``` r
project_points_along_geometry(
  geometry,
  points,
  geometry_sample_meters = 10,
  metric_crs = 3857
)
```

## Arguments

- geometry:

  sf or sfc object with exactly one linear geometry (LINESTRING or
  MULTILINESTRING).

- points:

  sf or sfc object with point geometries to be projected.

- geometry_sample_meters:

  Numeric (Default 10). Sampling step used to discretize the line when
  estimating cumulative distance along geometry.

- metric_crs:

  Integer or character (Default 3857). Projected CRS used to compute
  nearest points, line sampling, and cumulative distances.

## Value

data.frame. Input points projected along geometry with four columns:

- closest_on_geometry:

  An `sfc_POINT` column with the projected location on the line.

- distance_to_closest_on_geometry:

  Numeric distance from each input point to its projected location on
  the line.

- distance_along_geometry:

  Numeric cumulative distance from the line start to the projected
  location.

- distance_along_geometry_reversed:

  Numeric cumulative distance from the line end to the projected
  location.

If `points` is empty, returns an empty data.frame with the same columns.

## Details

The function first computes nearest points from each input point to
`geometry` with
[`sf::st_nearest_points()`](https://r-spatial.github.io/sf/reference/st_nearest_points.html),
keeping the point on the line. Then, it samples the line at regular
intervals and assigns cumulative distance by nearest sampled location.

Distances are always computed in `metric_crs` units. The returned
projected points are transformed back to the original `geometry` CRS.

## Examples

``` r
# Get sample points from GTFS-RT collection
rt_collect_file <- system.file(
  "extdata/samples", "gtfs_rt_sample_tcb_4_4-CS-TERM.csv", package = "GTFShift"
)
points <- read.csv(rt_collect_file) |> 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> dplyr::sample_n(5)

head(points |> dplyr::select(geometry))
#> Simple feature collection with 5 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -9.07844 ymin: 38.63273 xmax: -9.0315 ymax: 38.65209
#> Geodetic CRS:  WGS 84
#>                    geometry
#> 1 POINT (-9.07844 38.65209)
#> 2  POINT (-9.0315 38.63614)
#> 3 POINT (-9.04844 38.64169)
#> 4 POINT (-9.03277 38.63273)
#> 5 POINT (-9.05829 38.64186)

# Get route geometry for points
osm_routes <- sf::st_read(
  system.file("extdata/samples", "osm_routes_tcb.gpkg", package = "GTFShift"),
  quiet = TRUE
) |> dplyr::filter(route_id %in% points$route_id)

head(osm_routes)
#> Simple feature collection with 1 feature and 3 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.078595 ymin: 38.6307 xmax: -9.03216 ymax: 38.65478
#> Geodetic CRS:  WGS 84
#>     osm_id  shape_id    route_id                           geom
#> 1 18957507 4-CS-TERM 4_4-CS-TERM MULTILINESTRING ((-9.032408...

# Project points to geometry
points_projected <- GTFShift::project_points_along_geometry(
  geometry = osm_routes,
  points = points,
  metric_crs = 3763 # Make sure to addapt to the projection that better suits your location
)

head(points_projected)
#> Simple feature collection with 5 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -9.078512 ymin: 38.63273 xmax: -9.032493 ymax: 38.6521
#> Geodetic CRS:  WGS 84
#>   distance_to_closest_on_geometry distance_along_geometry
#> 1                        6.450152               6464.7182
#> 2                      145.430240                  0.0000
#> 3                        8.700216               2385.4309
#> 4                        1.160281                320.7302
#> 5                        1.230545               3457.8725
#>   distance_along_geometry_reversed        closest_on_geometry
#> 1                            0.000  POINT (-9.078512 38.6521)
#> 2                         6464.718 POINT (-9.032493 38.63509)
#> 3                         4079.287 POINT (-9.048347 38.64166)
#> 4                         6143.988 POINT (-9.032757 38.63273)
#> 5                         3006.846 POINT (-9.058299 38.64185)
```
