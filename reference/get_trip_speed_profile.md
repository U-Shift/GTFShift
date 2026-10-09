# Get trip speed profile from GTFS-RT speed estimates

Computes aggregated speed metrics for trips or groups from the output of
[`GTFShift::rt_average_speed()`](https://u-shift.github.io/GTFShift/reference/rt_average_speed.md).
Aggregates by trip, route, and day (customizable via the `by`
parameter), calculating commercial speed (distance between first and
last updates along geometry divided by elapsed time), alternative
commercial speed (considering the 2nd and penultimate updates to avoid
terminal wait biases), and measures of centrality and spread for speed
observations.

## Usage

``` r
get_trip_speed_profile(
  rt_speed,
  by = c("trip_id", "route_id", "day"),
  speed_col = "speed_kmh",
  time_col = "timestamp"
)
```

## Arguments

- rt_speed:

  data.frame or sf data.frame. The result of
  [`GTFShift::rt_average_speed()`](https://u-shift.github.io/GTFShift/reference/rt_average_speed.md).
  Must contain at least the columns `distance_along_geometry`,
  `distance_along_geometry_reversed`, and `timestamp`.

- by:

  Character vector (Default `c("trip_id", "route_id", "day")`). Columns
  to aggregate by. If `"day"` is included in `by` but not present in
  `rt_speed`, it is automatically derived from the `timestamp` column.
  Set to `NULL` or `character(0)` to compute metrics across the entire
  dataset.

- speed_col:

  Character (Default `"speed_kmh"`). Column name present in `rt_speed`
  representing estimated speed between consecutive updates (in km/h).

- time_col:

  Character (Default `"timestamp"`). Column name present in `rt_speed`
  representing update timestamps (numeric epoch, POSIXct, or Date).

## Value

data.frame. Aggregated speed profile metrics with one row per group.

## Details

For each group defined by `by` (by default, each unique combination of
`trip_id`, `route_id`, and `day`), observations are ordered
chronologically by `timestamp`.

Let \\\\(d_i, d_i^{\mathrm{rev}}, t_i)\\\_{i=1}^n\\ denote the ordered
sequence of updates, where \\d_i\\ is the distance along geometry
(meters), \\d_i^{\mathrm{rev}}\\ is the reversed distance along geometry
(meters), and \\t_i\\ is the timestamp (seconds).

To accommodate circular geometries (where starting and terminal
positions may map to the same location on the shape), the distance
traveled between two observations \\i\\ and \\j\\ (\\j \> i\\) is
calculated by considering both the normal and reversed distances and
taking the maximum: \$\$\Delta d\_{i, j}^{\mathrm{fwd}} = \left\| d_j -
d_i \right\|\$\$ \$\$\Delta d\_{i, j}^{\mathrm{circ}} = \left\| d_j -
d_i^{\mathrm{rev}} \right\|\$\$ \$\$\Delta d\_{i, j} = \max\left(\Delta
d\_{i, j}^{\mathrm{fwd}}, \Delta d\_{i, j}^{\mathrm{circ}}\right)\$\$

**Commercial speed** is calculated as the total distance traveled
between the first and last updates divided by the elapsed time:
\$\$v\_{\mathrm{commercial}} = \frac{\Delta d\_{1, n}}{1000} \div
\frac{t_n - t_1}{3600}\$\$ If \\n \< 2\\ or \\t_n \le t_1\\,
`commercial_speed` is `NA`.

**Alternative commercial speed** (`commercial_speed_alt`) uses the 2nd
and penultimate (\\n-1\\) observations to eliminate potential dwell
times or layovers at the terminal stops:
\$\$v\_{\mathrm{commercial\\alt}} = \frac{\Delta d\_{2, n-1}}{1000} \div
\frac{t\_{n-1} - t_2}{3600}\$\$ If \\n \< 4\\ or \\t\_{n-1} \le t_2\\,
`commercial_speed_alt` is `NA`.

If `by` does not include `trip_id` (e.g., aggregating at route or day
level), `commercial_speed` and `commercial_speed_alt` are computed per
trip and averaged across trips within each group.

**Measures of centrality and spread** are calculated from all valid
(non-NA, finite) speed observations in `speed_kmh` for each group:

- timestamp_min:

  Earliest timestamp in the trip/group.

- timestamp_max:

  Latest timestamp in the trip/group.

- commercial_speed:

  Commercial speed between first and last update (km/h).

- commercial_speed_alt:

  Alternative commercial speed between 2nd and penultimate update
  (km/h).

- speed_avg:

  Arithmetic mean speed (km/h).

- speed_median:

  Median speed (km/h).

- speed_sd:

  Standard deviation of speeds (km/h).

- speed_var:

  Variance of speeds (km/h)^2.

- speed_min:

  Minimum observed speed (km/h).

- speed_max:

  Maximum observed speed (km/h).

- speed_p15:

  15th percentile speed (km/h).

- speed_p25:

  25th percentile (1st quartile) speed (km/h).

- speed_p75:

  75th percentile (3rd quartile) speed (km/h).

- speed_p85:

  85th percentile speed (km/h).

- speed_iqr:

  Interquartile range (speed_p75 - speed_p25) (km/h).

- speed_count:

  Count of valid speed observations.

- n_updates:

  Total number of position updates in the group.

In addition, an attribute `"global_summary"` is attached to the returned
data frame, providing the same metrics evaluated over the entire input
dataset.

## See also

[`GTFShift::rt_average_speed()`](https://u-shift.github.io/GTFShift/reference/rt_average_speed.md)

## Examples

``` r
# \donttest{
# Get GTFS-RT data collection and route geometries
rt_collect_file <- system.file(
  "extdata/samples", "gtfs_rt_sample_tcb_4_4-CS-TERM.csv", package = "GTFShift"
)
rt_collection <- read.csv(rt_collect_file) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::select(-speed)

osm_routes <- sf::st_read(
  system.file("extdata/samples", "osm_routes_tcb.gpkg", package = "GTFShift"),
  quiet = TRUE
) |>
  dplyr::filter(route_id %in% rt_collection$route_id) |>
  dplyr::mutate(geom = GTFShift::multiline_to_sorted_linestring(geom, metric_crs = 3763))

# Compute speeds with rt_average_speed()
speed <- GTFShift::rt_average_speed(
  rt_collection = rt_collection,
  trips_geometries = osm_routes,
  rt_collection_trips_geometries_match_col = "route_id",
  metric_crs = 3763
)
#> Warning: Trip 4_4-CS-TERM has less than 2 updates. Ignoring it.

# Compute trip speed profile
profile <- GTFShift::get_trip_speed_profile(speed)
head(profile)
#> # A tibble: 2 × 20
#>   trip_id       route_id day        timestamp_min timestamp_max commercial_speed
#>   <chr>         <chr>    <date>             <int>         <int>            <dbl>
#> 1 20260514_DUP… 4_4-CS-… 2026-05-14    1778737742    1778738882             9.91
#> 2 20260515_DUP… 4_4-CS-… 2026-05-15    1778823962    1778827442             6.69
#> # ℹ 14 more variables: commercial_speed_alt <dbl>, speed_avg <dbl>,
#> #   speed_median <dbl>, speed_sd <dbl>, speed_var <dbl>, speed_min <dbl>,
#> #   speed_max <dbl>, speed_p15 <dbl>, speed_p25 <dbl>, speed_p75 <dbl>,
#> #   speed_p85 <dbl>, speed_iqr <dbl>, speed_count <int>, n_updates <int>

# Customize aggregation (e.g. by route and day)
route_profile <- GTFShift::get_trip_speed_profile(speed, by = c("route_id", "day"))
head(route_profile)
#> # A tibble: 2 × 19
#>   route_id    day        timestamp_min timestamp_max commercial_speed
#>   <chr>       <date>             <int>         <int>            <dbl>
#> 1 4_4-CS-TERM 2026-05-14    1778737742    1778738882             9.91
#> 2 4_4-CS-TERM 2026-05-15    1778823962    1778827442             6.69
#> # ℹ 14 more variables: commercial_speed_alt <dbl>, speed_avg <dbl>,
#> #   speed_median <dbl>, speed_sd <dbl>, speed_var <dbl>, speed_min <dbl>,
#> #   speed_max <dbl>, speed_p15 <dbl>, speed_p25 <dbl>, speed_p75 <dbl>,
#> #   speed_p85 <dbl>, speed_iqr <dbl>, speed_count <int>, n_updates <int>
# }
```
