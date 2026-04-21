# Get prioritization stats

Get statistics about lane prioritization

## Usage

``` r
get_prioritization_stats(
  lane_prioritization,
  weight = c("length", "frequency")
)
```

## Arguments

- lane_prioritization:

  sf data.frame. Lane prioritization.

- weight:

  Character. Weight to use for weighted mean. Accepted values: "length",
  "frequency".

## Value

List with statistics about lane prioritization, with the following
attributes:

- extension:

  Total length of the prioritized network, in meters.

- extension_bus_lane:

  Total length of the bus lane segments, in meters.

- speed_avg:

  Average speed of the prioritized network, in km/h.

- speed_min:

  Minimum speed of the prioritized network, in km/h.

- speed_max:

  Maximum speed of the prioritized network, in km/h.

- n_lanes_avg:

  Average number of lanes in the prioritized network.

- n_lanes_min:

  Minimum number of lanes in the prioritized network.

- n_lanes_max:

  Maximum number of lanes in the prioritized network.

## Examples

``` r
if (FALSE) { # \dontrun{
lane_prioritization <- GTFShift::prioritize_lanes(gtfs, q)
stats <- GTFShift::get_prioritization_stats(lane_prioritization)
} # }
```
