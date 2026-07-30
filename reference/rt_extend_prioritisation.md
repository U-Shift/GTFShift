# Extend prioritisation with GTFS-RT based speed metrics

This function extends lane segment indicators for prioritisation with
speed metrics produced with GTFS-RT data.

## Usage

``` r
rt_extend_prioritisation(
  lane_prioritisation,
  rt_collection,
  rt_current_status = c("IN_TRANSIT_TO"),
  lane_buffer = 15,
  metric_crs = 3857
)
```

## Arguments

- lane_prioritisation:

  sf data.frame. Result of
  [`GTFShift::prioritise_lanes()`](https://u-shift.github.io/GTFShift/reference/prioritise_lanes.md)

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

sf data.frame. Extended lane prioritisation with the following columns:

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

Extends the `lane_prioritisation` data with speed metrics calculated
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
# Subset GTFS for one route only, for demo purposes
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip",
  package = "GTFShift"
))
gtfs <- GTFShift::filter_by_route_name(gtfs, c("4"))

# Build query and prepare osm extract (possible to use API as alternative)
q <- osmdata::opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |>
  osmdata::add_osm_feature(key = "route", value = "bus") |>
  osmdata::add_osm_feature(key = "operator", value = "Transportes Colectivos do Barreiro")
osm_file <- system.file("extdata/samples", "osmextract_tcb_network.pbf", package = "GTFShift")

# Prioritise lanes
lane_prioritisation <- GTFShift::prioritise_lanes(
  gtfs, q,
  osm_file = osm_file,
  date = gtfs$calendar$start_date[1]
)
#> Analysing GTFS for 2026-06-08...
#> > Filtering by reference date 2026-06-08...
#> Matched 1 shapes (100.00% of 1 in GTFS) of 1 routes (100.00% of 1 in GTFS) with OSM routes!

# Extend with GTFS-RT data collection
rt_collect_file <- system.file(
  "extdata/samples", "gtfs_rt_sample_tcb_4_4-CS-TERM.csv",
  package = "GTFShift"
)
rt_collection <- read.csv(rt_collect_file) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

lane_prioritisation_extended <- GTFShift::rt_extend_prioritisation(
  lane_prioritisation = lane_prioritisation,
  rt_collection = rt_collection,
  metric_crs = 3763 # Make sure to addapt to the projection that better suits your location
)

head(
  lane_prioritisation_extended |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(speed_count)) |>
    dplyr::select(way_osm_id, speed_avg, speed_count)
)
#> # A tibble: 6 × 3
#>   way_osm_id speed_avg speed_count
#>   <chr>          <dbl>       <int>
#> 1 1020522356      2.42           1
#> 2 1020522582     11.6            1
#> 3 1024730739      9.60           2
#> 4 1330475061      4.55           1
#> 5 1330479487      0.08           1
#> 6 1375708221      4.72           1
```
