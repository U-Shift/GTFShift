# Package index

## Get GTFS feeds

- [`load_feed()`](https://u-shift.github.io/GTFShift/reference/load_feed.md)
  : Read GTFS feed, fixing integrity errors
- [`query_mobilitydatabase()`](https://u-shift.github.io/GTFShift/reference/query_mobilitydatabase.md)
  : Query Mobility Database API for GTFS feeds

## Prioritize for bus lane implementation

- [`prioritize_lanes()`](https://u-shift.github.io/GTFShift/reference/prioritize_lanes.md)
  : Prioritize road network lanes for bus lane implementation
- [`rt_extend_prioritization()`](https://u-shift.github.io/GTFShift/reference/rt_extend_prioritization.md)
  : Extend prioritization with GTFS-RT based speed metrics
- [`get_prioritization_stats()`](https://u-shift.github.io/GTFShift/reference/get_prioritization_stats.md)
  : Get prioritization stats

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
- [`get_prioritization_stats()`](https://u-shift.github.io/GTFShift/reference/get_prioritization_stats.md)
  : Get prioritization stats
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
- [`create_shapes_from_sf()`](https://u-shift.github.io/GTFShift/reference/create_shapes_from_sf.md)
  : Build shapes from simple feature object
- [`create_shapes_from_stops()`](https://u-shift.github.io/GTFShift/reference/create_shapes_from_stops.md)
  : Build shapes from GTFS stops data

## OpenStreetMaps

- [`osm_bus_lanes()`](https://u-shift.github.io/GTFShift/reference/osm_bus_lanes.md)
  : Export designated bus lanes from OpenStreetMaps
- [`osm_centerlines()`](https://u-shift.github.io/GTFShift/reference/osm_centerlines.md)
  : Get centerlines for OSM road network
- [`osm_shapes_match_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_match_routes.md)
  : Get OSM routes that match shapes, based on geometrical match
- [`osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)
  : Get OSM routes geometry considering gtfs:shape_id match

## Realtime

- [`rt_average_speed()`](https://u-shift.github.io/GTFShift/reference/rt_average_speed.md)
  : Estimate average speed for GTFS-RT trip updates
- [`rt_collect_json()`](https://u-shift.github.io/GTFShift/reference/rt_collect_json.md)
  : Collect GTFS-RT data from a JSON feed at regular intervals
- [`rt_collect_protobuf()`](https://u-shift.github.io/GTFShift/reference/rt_collect_protobuf.md)
  : Collect GTFS-RT data from a Protocol Buffers feed at regular
  intervals
- [`rt_extend_prioritization()`](https://u-shift.github.io/GTFShift/reference/rt_extend_prioritization.md)
  : Extend prioritization with GTFS-RT based speed metrics

## Utils

- [`calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)
  : Get next business Wednesday
- [`multiline_to_sorted_linestring()`](https://u-shift.github.io/GTFShift/reference/multiline_to_sorted_linestring.md)
  : Convert a MULTILINESTRING to a sorted LINESTRING
- [`project_points_along_geometry()`](https://u-shift.github.io/GTFShift/reference/project_points_along_geometry.md)
  : Project points onto a linear geometry
