# Extend prioritization with GTFS-RT based speed metrics

This function extends lane segment indicators for prioritization with
speed metrics produced with GTFS-RT data.

## Usage

``` r
rt_extend_prioritization(
  lane_prioritization,
  rt_collection,
  rt_current_status = c("IN_TRANSIT_TO"),
  lane_buffer = 15,
  metric_crs = 3857
)
```

## Arguments

- lane_prioritization:

  sf data.frame. Result of
  [`GTFShift::prioritize_lanes()`](https://u-shift.github.io/GTFShift/reference/prioritize_lanes.md)

- rt_collection:

  sf data.frame. GTFS-RT data collection. Must include `speed` column.

- rt_current_status:

  Character vector (Default `c("IN_TRANSIT_TO")`). If the
  `current_status` column is present in the `rt_collection` data, only
  points with `current_status` in this vector are considered.

- lane_buffer:

  numeric (Default 15). Buffer distance (in meters) to create around
  lane segments to capture nearby GTFS-RT points.

- metric_crs:

  Integer or character (Default 3857). Projected CRS used to apply lane
  buffer distances in meters.

## Value

The `lane_prioritization` `sf` `data.frame`, extended with the following
columns:

- speed_avg:

  The average speed of the vehicles on the way.

- speed_median:

  The median speed of the vehicles on the way.

- speed_p25:

  The 25th percentile speed of the vehicles on the way.

- speed_p75:

  The 75th percentile speed of the vehicles on the way.

- speed_count:

  The number of speed observations on the way.

## Details

Extends the `lane_prioritization` data with speed metrics calculated
from the GTFS-RT data points that fall within a buffer around each lane
segment.

If GTFS-RT data does not provide speed information, it can be inferred
from the progression of position updates through time using
[`GTFShift::rt_average_speed()`](https://u-shift.github.io/GTFShift/reference/rt_average_speed.md).

Refer to
[`GTFShift::rt_collect_json()`](https://u-shift.github.io/GTFShift/reference/rt_collect_json.md)
or
[`GTFShift::rt_collect_protobuf()`](https://u-shift.github.io/GTFShift/reference/rt_collect_protobuf.md)
for details on GTFS-RT data collection.

## Examples

``` r
if (FALSE) { # \dontrun{
rt_collect_file <- "gtfs_rt_data.csv"
GTFShift::rt_collect_json("https://api.example.com/gtfs-rt", rt_collect_file)
lane_prioritization <- GTFShift::prioritize_lanes(gtfs, osm_query)

rt_collection <- read.csv(rt_collect_file) |> sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
lane_prioritization_extended <- GTFShift::rt_extend_prioritization(
  lane_prioritization = lane_prioritization,
  rt_collection = rt_collection
)
} # }
```
