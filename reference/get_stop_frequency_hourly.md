# Get aggregated frequency per hour for each bus stop

For each stop, returns the number of departures aggregated per hour.

## Usage

``` r
get_stop_frequency_hourly(
  gtfs,
  date = GTFShift::calendar_nextBusinessWednesday()
)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- date:

  Date (Default
  [`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)).
  Reference date to consider when analyzing the GTFS file.

## Value

An `sf` `data.frame` object with the following columns:

- `stop_id`, the `stop_id` attribute from `stops.txt` file.

- `hour`, the hour for which the frequency applies (24 hour format).

- `frequency`, the number of services provided at the stop for the
  corresponding 60 minutes period.

- `geometry`, the stop coordinates.

## Details

This method analyses the GTFS feed for a representative day, generating
for each stop the number of services aggregated per hour. For a detailed
example, see the
[`vignette("analyse")`](https://u-shift.github.io/GTFShift/articles/analyse.md).

## See also

[`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")
frequency_analysis <- GTFShift::get_stop_frequency_hourly(gtfs)
} # }
```
