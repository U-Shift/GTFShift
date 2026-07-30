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

data.frame. Table for calendar.txt.

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
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_ttsl_sample_no_shapes.zip", package = "GTFShift")
)
#> Warning: > CREATED shapes.txt, the file was missing!

head(gtfs$calendar_dates |> dplyr::filter(exception_type == 1))
#> # A tibble: 6 × 3
#>   service_id date       exception_type
#>   <chr>      <date>              <int>
#> 1 03D1       2026-08-15              1
#> 2 03D1       2026-10-05              1
#> 3 03D1       2026-12-01              1
#> 4 03D1       2026-12-08              1
#> 5 03D1       2026-12-25              1
#> 6 03D1       2027-01-01              1

gtfs_calendar <- GTFShift::create_calendar(gtfs)

gtfs_calendar
#> # A tibble: 1 × 10
#>   service_id monday tuesday wednesday thursday friday saturday sunday start_date
#>   <chr>       <int>   <int>     <int>    <int>  <int>    <int>  <int> <date>    
#> 1 03D1            1       1         1        1      1        1      0 2026-08-15
#> # ℹ 1 more variable: end_date <date>
```
