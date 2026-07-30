# Get prioritisation stats

Get statistics about lane prioritisation

## Usage

``` r
get_prioritisation_stats(
  lane_prioritisation,
  weight = c("length", "frequency"),
  metric_crs = 3857
)
```

## Arguments

- lane_prioritisation:

  sf data.frame. Lane prioritisation.

- weight:

  Character. Weight to use for weighted mean. Accepted values: "length",
  "frequency".

- metric_crs:

  Integer or character (Default 3857). Projected CRS used to compute
  lengths in meters.

## Value

List. Statistics about lane prioritisation, with the following
attributes:

- extension:

  Total length of the prioritised network, in meters.

- extension_bus_lane:

  Total length of the bus lane segments, in meters.

- speed_avg:

  Average speed of the prioritised network, in km/h.

- speed_min:

  Minimum speed of the prioritised network, in km/h.

- speed_max:

  Maximum speed of the prioritised network, in km/h.

- n_lanes_circulation_avg:

  Average number of lanes in the prioritised network.

- n_lanes_circulation_min:

  Minimum number of lanes in the prioritised network.

- n_lanes_circulation_max:

  Maximum number of lanes in the prioritised network.

## Examples

``` r
# Subset GTFS for one route only, for demo purposes
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip", package = "GTFShift")
)
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

# Get statistics for prioritisation
stats <- GTFShift::get_prioritisation_stats(lane_prioritisation, metric_crs = 3763)

data.frame(metric = names(stats), value = unlist(stats, use.names = FALSE))
#>                    metric       value
#> 1               extension 6464.718236
#> 2      extension_bus_lane    0.000000
#> 3 n_lanes_circulation_avg    1.746812
#> 4 n_lanes_circulation_min    1.000000
#> 5 n_lanes_circulation_max    4.000000
#> 6     n_lanes_parking_avg    0.000000
#> 7     n_lanes_parking_min    0.000000
#> 8     n_lanes_parking_max    0.000000
```
