# Estimate commercial speed from GTFS-RT trip updates

Projects each real-time vehicle position to its corresponding trip
geometry, computes cumulative distance along the shape, and derives
segment speed between consecutive updates.

## Usage

``` r
rt_commercial_speed(
  rt_collection,
  trips_geometries,
  rt_collection_trips_geometries_match_col = "trip_id",
  geometry_sample_meters = 10,
  metric_crs = 3857
)
```

## Arguments

- rt_collection:

  sf data.frame with GTFS-RT updates for multiple trips. Must include at
  least `trip_id` and `timestamp` columns.

- trips_geometries:

  sf data.frame with trip geometries. Geometry must be LINESTRING.

- rt_collection_trips_geometries_match_col:

  Character (Default `"trip_id"`). Column name present in both
  `rt_collection` and `trips_geometries` used to match updates to trip
  geometry.

- geometry_sample_meters:

  Numeric (Default 10). Sampling step used when projecting points along
  trip geometry and estimating cumulative distance.

- metric_crs:

  Integer or character (Default 3857). Projected CRS used to compute
  distances and speeds.

## Value

An `sf` object based on `rt_collection`, with added columns:

- closest_on_shape:

  Projected point on trip geometry.

- distance_to_closest_on_geometry:

  Distance from each update point to its projected location on the shape
  (meters).

- distance_along_geometry:

  Cumulative distance along trip geometry (meters).

- distance_along_geometry_reversed:

  Cumulative distance from shape end to projected location (meters).

- time_since_prev_sec:

  Elapsed time since previous update (seconds).

- distance_since_prev_meters:

  Distance increment since previous update (meters).

- speed_kmh:

  Estimated speed between consecutive updates (km/h).

## Details

For each trip (grouped by `trip_id`), let \\\\(x_i, t_i)\\\_{i=1}^n\\
denote the ordered sequence of real-time observations, where \\x_i\\ is
the vehicle position and \\t_i\\ the corresponding timestamp, with \\t_1
\le t_2 \le \dots \le t_n\\. Each observation is projected onto the trip
geometry using
[`GTFShift::project_points_along_geometry()`](https://u-shift.github.io/GTFShift/reference/project_points_along_geometry.md),
yielding a projected point \\\hat{x}\_i\\ and two cumulative distances:
\$\$d_i = \text{distance_along_geometry}(\hat{x}\_i)\$\$
\$\$d_i^{\mathrm{rev}} =
\text{distance_along_geometry_reversed}(\hat{x}\_i)\$\$

Distance between consecutive updates is computed as the minimum between
two alternatives, both using absolute differences:

1.  Normal direction: difference in `distance_along_geometry`.

2.  Reversed direction: difference using
    `distance_along_geometry_reversed` to better handle circular shapes.

The selected distance increment is used to compute speed as:
\$\$speed\_{km/h} = \frac{\Delta distance\\ (m)}{1000} \div \frac{\Delta
time\\ (s)}{3600}\$\$

Trips with fewer than 2 updates are ignored with a warning. The distance
increment is defined as the minimum of two alternative
cumulative-distance differences: \$\$\Delta d_i^{\mathrm{fwd}} = \left\|
d_i - d\_{i-1} \right\|\$\$ \$\$\Delta d_i^{\mathrm{circ}} = \left\|
d_i - d\_{i-1}^{\mathrm{rev}} \right\|\$\$ \$\$\Delta d_i =
\min\left(\Delta d_i^{\mathrm{fwd}}, \Delta
d_i^{\mathrm{circ}}\right).\$\$

The second term is a redundancy designed to avoid overstating movement
on circular geometries. In particular, after a vehicle completes a loop,
a forward comparison may treat two nearby physical positions as far
apart in cumulative distance if the geometry origin has been crossed.
Comparing \\d_i\\ against \\d\_{i-1}^{\mathrm{rev}}\\ provides an
auxiliary distance candidate that helps avoid overstating movement in
that situation.

Commercial speed is then estimated by \$\$v_i = \frac{\Delta d_i}{\Delta
t_i}\$\$ and reported in kilometers per hour as \$\$v_i^{\mathrm{km/h}}
= \frac{\Delta d_i}{1000} \cdot \frac{3600}{\Delta t_i}.\$\$

Trips with fewer than two observations are ignored with a warning
because \\\Delta t_i\\ and \\\Delta d_i\\ are undefined in that case.

Method
[`GTFShift::multiline_to_sorted_linestring()`](https://u-shift.github.io/GTFShift/reference/multiline_to_sorted_linestring.md)
can be used to convert MULTILINESTRING geometries to LINESTRING if
needed.

## See also

[`GTFShift::project_points_along_geometry()`](https://u-shift.github.io/GTFShift/reference/project_points_along_geometry.md)

[`GTFShift::multiline_to_sorted_linestring()`](https://u-shift.github.io/GTFShift/reference/multiline_to_sorted_linestring.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rt_collection <- read.csv("rt_collection.csv") # sf object with GTFS-RT updates (trip_id, timestamp, geometry)
trips_geometries <- sf::st_read("osm_geometries.gpkg") # sf object with LINESTRING geometry per trip
speeds <- GTFShift::rt_commercial_speed(rt_collection, trips_geometries)
} # }
```
