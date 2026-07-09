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

A data.frame with one row per input point and four columns:

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
if (FALSE) { # \dontrun{
line <- sf::st_sfc(
  sf::st_linestring(matrix(c(0, 0, 100, 0, 200, 100), ncol = 2, byrow = TRUE)),
  crs = 3857
)
pts <- sf::st_sfc(sf::st_point(c(20, 10)), sf::st_point(c(150, 40)), crs = 3857)

projected <- project_points_along_geometry(line, pts, geometry_sample_meters = 5)
} # }
```
