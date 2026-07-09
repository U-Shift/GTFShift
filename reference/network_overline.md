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

A spatial object of the target network, extended with the aggregated
values.

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
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("https://operator.com/gtfs.zip")
target_network = st_read("network_centerlines.gpkg")
frequency_analysis <- GTFShift::get_route_frequency_hourly(gtfs, overline=FALSE)
GTFShift::network_overline(
  target_network,
  frequency_analysis |> filter(arrival_hour==8),
  attr = "frequency"
)
} # }
```
