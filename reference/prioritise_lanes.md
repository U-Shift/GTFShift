# Prioritise road network lanes for bus lane implementation

For each OSM way with GTFS service, aggregates its characteristics to
assist in the bus lane implementation prioritisation

## Usage

``` r
prioritise_lanes(
  gtfs,
  q,
  date = GTFShift::calendar_nextBusinessWednesday(),
  keep_osm_attributes = FALSE,
  osm_file = NULL
)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- q:

  osmdata::opq. Overpass query for transit network, to obtain OSM route
  ways, using
  [`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md).

- date:

  Date (Default
  [`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)).
  Reference date to consider when analyzing the GTFS file.

- keep_osm_attributes:

  Boolean (Default FALSE). Whether to keep all OSM way attributes in the
  output `sf` object.

- osm_file:

  character (Optional). Location of OSM extract file with `osm.pbf`
  format. Refer to
  [`osmextract::oe_download()`](https://docs.ropensci.org/osmextract/reference/oe_download.html)
  for more details. If not provided OSM Overpass API is called through
  [`osmdata::osmdata_sf()`](https://docs.ropensci.org/osmdata/reference/osmdata_sf.html).

## Value

sf data.frame. Prioritised lanes with the following columns:

- way_osm_id:

  The `osm_id` attribute from OSM way.

- hour:

  The hour for which the frequency applies (24 hour format).

- frequency:

  The number of services for the route that depart from the first stop
  for the corresponding 60 minutes period.

- is_bus_lane:

  Whether the way has a bus lane.

- n_lanes_parking:

  The number of parking lanes.

- n_lanes_circulation:

  The number of circulation lanes.

- n_directions:

  The number of travel directions.

- n_lanes_circulation_direction:

  The number of circulation lanes per direction.

- routes:

  The list of route_id that use the way.

- shapes:

  The list of shape_id that use the way.

- geometry:

  The route shape.

- (if `keep_osm_attributes = TRUE`):

  All OSM way attributes.

## Details

This method analyses the GTFS feed for a representative day, returning a
data.frame with the road segments where transit routes run and for each,
a set of parameters that can be used to prioritise bus lane
implementations.

Its functionality is a bundle that encapsulates the logic of several
methods from the package, including
[`GTFShift::get_way_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_way_frequency_hourly.md)
and
[`GTFShift::osm_bus_lanes()`](https://u-shift.github.io/GTFShift/reference/osm_bus_lanes.md),
that can be used separately if needed.

Mind that this method uses
[`GTFShift::get_way_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_way_frequency_hourly.md)
to match routes with OSM ways, which requires that the OSM relation
mapping is well defined for the transit routes. Routes that do not have
an OSM match are ignored.

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

lane_prioritisation <- GTFShift::prioritise_lanes(
  gtfs, q, 
  osm_file = osm_file, 
  date = gtfs$calendar$start_date[1]
)
#> Analysing GTFS for 2026-06-08...
#> > Filtering by reference date 2026-06-08...
#> Matched 1 shapes (100.00% of 1 in GTFS) of 1 routes (100.00% of 1 in GTFS) with OSM routes!

head(
  lane_prioritisation |> 
  dplyr::select(way_osm_id, hour, frequency, is_bus_lane, n_lanes_circulation, routes)
)
#> Simple feature collection with 6 features and 6 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.070888 ymin: 38.64434 xmax: -9.060215 ymax: 38.65214
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 7
#>   way_osm_id  hour frequency is_bus_lane n_lanes_circulation routes   
#>   <chr>      <int>     <int> <lgl>                     <dbl> <list>   
#> 1 1020152013     5         1 FALSE                         2 <chr [1]>
#> 2 1020165026     5         1 FALSE                         2 <chr [1]>
#> 3 1020521795     5         1 FALSE                         2 <chr [1]>
#> 4 1020521796     5         1 FALSE                         1 <chr [1]>
#> 5 1020521810     5         1 FALSE                         2 <chr [1]>
#> 6 1020521811     5         1 FALSE                         2 <chr [1]>
#> # ℹ 1 more variable: geometry <LINESTRING [°]>
```
