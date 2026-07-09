# Prioritize road network lanes for bus lane implementation

For each OSM way with GTFS service, aggregates its characteristics to
assist in the bus lane implementation prioritization

## Usage

``` r
prioritize_lanes(
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

An `sf` `data.frame` object with the following columns:

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
a set of parameters that can be used to prioritize bus lane
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
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")
q <- opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |> add_osm_feature(key = "route", value = "bus")

# To use OSM API:
lanes_analysis <- GTFShift::prioritize_lanes(gtfs, q)

# To use a local OSM file:
osm_file <- oe_download("https://download.geofabrik.de/europe/portugal-latest.osm.pbf")
lanes_analysis <- GTFShift::prioritize_lanes(gtfs, q, osm_file = osm_file)
} # }
```
