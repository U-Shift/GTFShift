# 7. GTFS Real Time

``` r
library(GTFShift)
```

## Introduction

GTFS-RT extends the GTFS static data model by providing real time
operational information. From service alerts, to trip updates, but also
vehicle positions. The collection of this data can provide valuable
insights about how planned operation performs in practice. `GTFShift`
provides several methods to enable this data collection and analysis.

## Collect GTFS-RT data

To collect GTFS-RT data, use
[`GTFShift::rt_collect()`](https://u-shift.github.io/GTFShift/reference/rt_collect.md),
which fetches data from a GTFS-RT endpoint and saves it to a CSV file
for further analysis. The method runs in a loop, fetching data at
regular intervals (default is every 30 seconds) until manually stopped
(CTRL+C).

``` r
rt_collect_file <- "gtfs_rt_data.csv"
GTFShift::rt_collect("https://api.example.com/gtfs-rt", rt_collect_file)
```

`GTFS::rt_collect_protobuf()` is an alternative method for GTFS-RT feeds
using Protocol Buffers encoding.

## Extend prioritization with GTFS-RT data

Once GTFS-RT data is collected, it can be used to extend lane
prioritization analysis.
[`GTFShift::rt_extend_prioritization()`](https://u-shift.github.io/GTFShift/reference/rt_extend_prioritization.md)
takes a lane prioritization data frame and a GTFS-RT collection (as an
`sf` object) and enriches the prioritization with real-time metrics.
Refer to the method documentation for the full details.

``` r
lane_prioritization <- GTFShift::prioritize_lanes(gtfs, osm_query)

rt_collection <- read.csv(rt_collect_file) |> sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

lane_prioritization_extended <- GTFShift::rt_extend_prioritization(
  lane_prioritization = lane_prioritization,
  rt_collection = rt_collection
)
```

The resulting `lane_prioritization_extended` data frame includes
additional columns with speed metrics, such as average speed, median
speed, and speed percentiles, providing a more comprehensive view of
lane performance based on real-time data.

``` r
mapview::mapview(
  lane_prioritization_extended,
  zcol = "speed_avg",
  layer.name = "Average speed per lane"
)

mapview::mapview(
  lane_prioritization_extended |> dplyr::filter(is_bus_lane),
  layer.name="Bus lane",
  color="#3BC1A8"
) + mapview::mapview(
  lane_prioritization_extended |> dplyr::filter(!is_bus_lane & frequency>=5 & !is.na(n_lanes) & n_lanes_direction>1 & speed_avg<15),
  layer.name="NO bus lane with 5 or + bus/h + 1 lane/dir and avg_speed < 15km/h",
  color="#F63049"
)
```
