# Get aggregated frequency per hour for each OSM way

For each OSM way with GTFS service, returns the number of departures
aggregated per hour and direction.

## Usage

``` r
get_way_frequency_hourly(
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

sf data.frame. Hourly way frequencies, with the following columns:

- way_osm_id:

  The `osm_id` attribute from OSM way.

- hour:

  The hour for which the frequency applies (24 hour format).

- frequency:

  The number of services for the route that depart from the first stop
  for the corresponding 60 minutes period.

- routes:

  The list of route_ids that use the way.

- shapes:

  The list of shape_ids that use the way.

- geometry:

  The route shape.

- (if `keep_osm_attributes = TRUE`):

  All OSM way attributes.

## Details

This method analyses the GTFS feed for a representative day, finding for
each route the corresponding OSM ways using
[`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)
(routes not on OSM are ignored), aggregating the number of services per
hour and direction for each.

For a detailed example, see the `vignette("analyse")`.

## See also

[`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)

[`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)

## Examples

``` r
# Subset GTFS for one route only, for demo purposes
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip", package = "GTFShift")
)
gtfs <- GTFShift::filter_by_route_name(gtfs, c("1", "2", "3", "4"))

# Build query and prepare osm extract (possible to use API as alternative)
q <- osmdata::opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |> 
  osmdata::add_osm_feature(key = "route", value = "bus") |> 
  osmdata::add_osm_feature(key = "operator", value = "Transportes Colectivos do Barreiro")
osm_file <- system.file("extdata/samples", "osmextract_tcb_network.pbf", package = "GTFShift")

# Get frequency
frequency_analysis <- GTFShift::get_way_frequency_hourly(
  gtfs, q, 
  date = gtfs$calendar$start_date[1],
  osm_file = osm_file
)
#> Analysing GTFS for 2026-06-10...
#> > Filtering by reference date 2026-06-10...
#> Matched 12 shapes (100.00% of 12 in GTFS) of 12 routes (100.00% of 12 in GTFS) with OSM routes!

head(frequency_analysis |> sf::st_drop_geometry())
#> # A tibble: 6 × 5
#>   way_osm_id  hour frequency routes    shapes   
#>   <chr>      <int>     <int> <list>    <list>   
#> 1 1020123867     0         1 <chr [1]> <chr [1]>
#> 2 1020123867     1         1 <chr [1]> <chr [1]>
#> 3 1020123867     6         1 <chr [1]> <chr [1]>
#> 4 1020123867    10         1 <chr [1]> <chr [1]>
#> 5 1020123867    23         1 <chr [1]> <chr [1]>
#> 6 1020152005     0         1 <chr [1]> <chr [1]>
```
