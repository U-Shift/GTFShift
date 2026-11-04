# GTFShift (development version)

- Use spherical geometry (s2 via sf default) to compute distances instead of Mercator, with the option to override with a local projected CRS still available (#105)
- Improved `load_feed()` integrity validation (#104):
  - Linearly interpolate missing `stop_times` arrival and departure times with `tidytransit::interpolate_stop_times()` instead of dropping stops (#102)
  - Remove duplicate entries across tables with `gtfstools::remove_duplicates()` (#104)
  - Remove unused IDs across tables with `gtfstools::remove_unused_ids()` (#104)
- Bumped `gtfstools` minimum version dependency to `>= 2.0.0` (#103)
- Fail gracefully with informative messages when internet resources are unavailable to comply with CRAN policy (#106)



# GTFShift 1.1.0

- New function `get_trip_speed_profile` to compute commercial speed and other speed related metrics at trip level
- Extended `rt_extend_prioritisation` output with `speed_p15` and `speed_p85`
- Improved file management when using OSM local extract files to avoid writting on folders without the proper permissions

# GTFShift 1.0.0

- Stable version
- CRAN release
