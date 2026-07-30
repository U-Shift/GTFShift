# Aggregate lines based on overlap with target network

Aggregate lines based on overlap with target network

## Usage

``` r
network_overline(
  target_network,
  lines,
  attr,
  target_network_split = 100,
  fun = sum,
  join_dist = 10,
  metric_crs = 3857
)
```

## Arguments

- target_network:

  sf. A spatial object representing the target network.

- lines:

  sf. A spatial object representing the lines to aggregate.

- attr:

  String. The attribute to aggregate the lines by.

- target_network_split:

  Integer (Default 100). If not NA, network is split in segments of
  defined meters.

- fun:

  Method (Default [`base::sum`](https://rdrr.io/r/base/sum.html)).
  Function to summarise the attributes by.

- join_dist:

  Integer (Default 10). Meters to consider when joining routes and
  network segments.

- metric_crs:

  Integer or character (Default 3857). Projected CRS used to compute
  segment lengths and join distances in meters.

## Value

sf. Spatial network object extended with aggregated values.

## Details

This method allows for the lines aggregation. Given a target network, it
identifies (using
[`stplanr::rnet_join()`](https://docs.ropensci.org/stplanr/reference/rnet_join.html))
the segments corresponding to each line and uses them to aggregate the
attribute defined in the parameters.

It provides an alternative to
[`GTFShift::get_route_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_route_frequency_hourly.md)
with the attribute `overline=TRUE`, which creates an aggregated network
based on the lines overlap. Instead, `GTFShift::network_overline()`
finds, for each network segment, the overlapping lines and aggregates
their `attr` values, using `fun`.

## See also

[`stplanr::rnet_join()`](https://docs.ropensci.org/stplanr/reference/rnet_join.html)

## Examples

``` r
# Subset GTFS for one route only, for demo purposes
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip", package = "GTFShift")
)
gtfs <- GTFShift::filter_by_route_name(gtfs, c("4", "1"))

# Load OSM network to serve as target network
target_network = sf::st_read(
  system.file("extdata/samples", "osm_ways_tcb.gpkg", package = "GTFShift"),
  quiet = TRUE
)

head(target_network)
#> Simple feature collection with 6 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.047385 ymin: 38.64837 xmax: -9.046362 ymax: 38.65368
#> Geodetic CRS:  WGS 84
#>    osm_id                           geom
#> 1 8493048 LINESTRING (-9.046362 38.64...
#> 2 8493052 LINESTRING (-9.046538 38.64...
#> 3 8493056 LINESTRING (-9.046362 38.64...
#> 4 8493060 LINESTRING (-9.04706 38.648...
#> 5 8493094 LINESTRING (-9.046603 38.64...
#> 6 8494673 LINESTRING (-9.046743 38.65...

# Get route frequency (and geometry)
frequency_analysis <- GTFShift::get_route_frequency_hourly(
  gtfs, 
  date = gtfs$calendar$start_date[1]
) |> 
dplyr::group_by(shape_id) |>
dplyr::summarize(frequency = max(frequency))
#> Analysing GTFS for 2026-06-08...
#> > Filtering by reference date 2026-06-08...

head(frequency_analysis)
#> Simple feature collection with 3 features and 2 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -9.08136 ymin: 38.6307 xmax: -9.0277 ymax: 38.66246
#> Geodetic CRS:  WGS 84
#> # A tibble: 3 × 3
#>   shape_id    frequency                                                 geometry
#>   <chr>           <int>                                           <GEOMETRY [°]>
#> 1 1-QVBB-TERM         1 LINESTRING (-9.050235 38.66111, -9.050147 38.66114, -9.…
#> 2 1-TERM              1 MULTILINESTRING ((-9.078167 38.65241, -9.07825 38.65243…
#> 3 4-CS-TERM           1 LINESTRING (-9.032607 38.63507, -9.032527 38.63508, -9.…

# Aggregate frequencies based on geometry overlap using GTFShift::network_overline
suppressWarnings({ 
  overline <- GTFShift::network_overline(
    target_network = target_network, 
    lines = frequency_analysis, 
    attr = "frequency",
    metric_crs = 3763 # Make sure to addapt to the projection that better suits your location
  )
})

head(overline |> st_drop_geometry())
#>      osm_id frequency
#>      <char>     <int>
#> 1:  8493048         2
#> 2:  8493094         2
#> 3: 23806682         2
#> 4: 40685608         1
#> 5: 40685608         1
#> 6: 40685608         1
```
