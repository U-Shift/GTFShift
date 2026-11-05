# Changelog

## GTFShift 1.2.0

- Use spherical geometry (s2 via sf default) to compute distances
  instead of Mercator, with the option to override with a local
  projected CRS still available
  ([\#105](https://github.com/U-Shift/GTFShift/issues/105))
- Improved
  [`load_feed()`](https://u-shift.github.io/GTFShift/reference/load_feed.md)
  integrity validation
  ([\#104](https://github.com/U-Shift/GTFShift/issues/104)):
  - Linearly interpolate missing `stop_times` arrival and departure
    times with
    [`tidytransit::interpolate_stop_times()`](https://r-transit.github.io/tidytransit/reference/interpolate_stop_times.html)
    instead of dropping stops
    ([\#102](https://github.com/U-Shift/GTFShift/issues/102))
  - Remove duplicate entries across tables with
    [`gtfstools::remove_duplicates()`](https://ipea.github.io/gtfstools/reference/remove_duplicates.html)
    ([\#104](https://github.com/U-Shift/GTFShift/issues/104))
  - Remove unused IDs across tables with
    [`gtfstools::remove_unused_ids()`](https://ipea.github.io/gtfstools/reference/remove_unused_ids.html)
    ([\#104](https://github.com/U-Shift/GTFShift/issues/104))
- Bumped `gtfstools` minimum version dependency to `>= 2.0.0`
  ([\#103](https://github.com/U-Shift/GTFShift/issues/103))
- Fail gracefully with informative messages when internet resources are
  unavailable to comply with CRAN policy
  ([\#106](https://github.com/U-Shift/GTFShift/issues/106))

## GTFShift 1.1.0

CRAN release: 2026-10-09

- New function `get_trip_speed_profile` to compute commercial speed and
  other speed related metrics at trip level
- Extended `rt_extend_prioritisation` output with `speed_p15` and
  `speed_p85`
- Improved file management when using OSM local extract files to avoid
  writting on folders without the proper permissions

## GTFShift 1.0.0

CRAN release: 2026-09-27

- Stable version
- CRAN release
