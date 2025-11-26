# Create calendar.txt from calendar_dates.txt

Create calendar.txt from calendar_dates.txt

## Usage

``` r
create_calendar(gtfs)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

## Value

A data.frame for calendar.txt.

## Details

When `calendar_dates.txt` declares all service dates, `calendar.txt`
becomes optional in the [GTFS feed
specification](https://gtfs.org/documentation/schedule/reference/#dataset-files).
However, to perform some operations, this table might be necessary.

This method allows to create a `calendar.txt` table, based on the
`calendar_dates.txt`. It performs an approximation, considering, for
each `service_id`, the minimum and maximum dates and setting each week
day to true if it has any date that matches that date. The results might
not be 100

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")
gtfs$calendar <- GTFShift::create_calendar(gtfs)
} # }
```
