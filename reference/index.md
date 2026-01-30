# Package index

## Get GTFS feeds

- [`load_feed()`](https://u-shift.github.io/GTFShift/reference/load_feed.md)
  : Read GTFS feed, fixing integrity errors
- [`query_mobilitydatabase()`](https://u-shift.github.io/GTFShift/reference/query_mobilitydatabase.md)
  : Query Mobility Database API for GTFS feeds

## Prioritize

- [`prioritize_lanes()`](https://u-shift.github.io/GTFShift/reference/prioritize_lanes.md)
  : Prioritize road network lanes for bus lane implementation
- [`rt_extend_prioritization()`](https://u-shift.github.io/GTFShift/reference/rt_extend_prioritization.md)
  : Extend prioritization with GTFS-RT metrics

## Filter

- [`filter_by_agency()`](https://u-shift.github.io/GTFShift/reference/filter_by_agency.md)
  : Filter GTFS feed by agency
- [`filter_by_modes()`](https://u-shift.github.io/GTFShift/reference/filter_by_modes.md)
  : Filter GTFS feed by mode
- [`filter_by_route_name()`](https://u-shift.github.io/GTFShift/reference/filter_by_route_name.md)
  : Filter GTFS feed by route name

## Analyse

- [`get_network_extension()`](https://u-shift.github.io/GTFShift/reference/get_network_extension.md)
  : Get network routes extension
- [`get_route_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_route_frequency_hourly.md)
  : Get aggregated frequency per hour for each bus route
- [`get_stop_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_stop_frequency_hourly.md)
  : Get aggregated frequency per hour for each bus stop
- [`get_way_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_way_frequency_hourly.md)
  : Get aggregated frequency per hour for each OSM way
- [`network_overline()`](https://u-shift.github.io/GTFShift/reference/network_overline.md)
  : Aggregate lines based on overlap with target network

## Classify

- [`classify_frequency_los()`](https://u-shift.github.io/GTFShift/reference/classify_frequency_los.md)
  : Classify bus frequency level of service based on HCM

## Manipulate

- [`unify()`](https://u-shift.github.io/GTFShift/reference/unify.md) :
  Merge multiple GTFS into a single aggregated file
- [`create_calendar()`](https://u-shift.github.io/GTFShift/reference/create_calendar.md)
  : Create calendar.txt from calendar_dates.txt

## OpenStreetMaps

- [`osm_bus_lanes()`](https://u-shift.github.io/GTFShift/reference/osm_bus_lanes.md)
  : Export designated bus lanes from OpenStreetMaps
- [`osm_centerlines()`](https://u-shift.github.io/GTFShift/reference/osm_centerlines.md)
  : Get centerlines for OSM road network
- [`osm_shapes_match_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_match_routes.md)
  : Get OSM routes that match shapes, based on geometrical match
- [`osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)
  : Get OSM routes geometry considering gtfs:shape_id match
- [`osm_trips_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_trips_to_routes.md)
  : Get OSM routes geometry considering gtfs:trip_id match

## Realtime

- [`rt_collect()`](https://u-shift.github.io/GTFShift/reference/rt_collect.md)
  : Collect GTFS-RT data
- [`rt_collect_protobuf()`](https://u-shift.github.io/GTFShift/reference/rt_collect_protobuf.md)
  : Collect GTFS-RT data (with Protocol Buffers support)
- [`rt_extend_prioritization()`](https://u-shift.github.io/GTFShift/reference/rt_extend_prioritization.md)
  : Extend prioritization with GTFS-RT metrics

## Utils

- [`calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)
  : Get next business Wednesday
