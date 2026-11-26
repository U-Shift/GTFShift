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

sf data.frame. Hourly stop frequencies, with the following columns:

- stop_id:

  The `stop_id` attribute from `stops.txt` file.

- hour:

  The hour for which the frequency applies (24 hour format).

- frequency:

  The number of services provided at the stop for the corresponding 60
  minutes period.

- geometry:

  The stop coordinates.

## Details

This method analyses the GTFS feed for a representative day, generating
for each stop the number of services aggregated per hour. For a detailed
example, see the `vignette("analyse")`.

## See also

[`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md)

## Examples

``` r
# Subset GTFS for one route only, for demo purposes
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip", package = "GTFShift")
)
gtfs <- GTFShift::filter_by_route_name(gtfs, c("1", "2", "3", "4"))

# Get frequency
frequency_analysis <- GTFShift::get_stop_frequency_hourly(
  gtfs,
  date = gtfs$calendar$start_date[1]
)
#> Analysing GTFS for 2026-06-10...
#> > Filtering by reference date 2026-06-10...
#> > Found 6 routes operating 5 trips on 56 stops...
#> > Identified 1 service patterns matching date: DF / Projeto A _26-1783608301691
#> > Calculating stop frequencies for hours 0 to 23...
#> Finished GTFS analysis!

head(frequency_analysis)
#> Simple feature collection with 6 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -9.081225 ymin: 38.65708 xmax: -9.078934 ymax: 38.65942
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 4
#>   stop_id  hour frequency             geometry
#>   <chr>   <int>     <int>          <POINT [°]>
#> 1 000002      6         1 (-9.078934 38.65708)
#> 2 000002     10         1 (-9.078934 38.65708)
#> 3 000002     23         1 (-9.078934 38.65708)
#> 4 000003      6         1 (-9.081225 38.65942)
#> 5 000003     11         1 (-9.081225 38.65942)
#> 6 000003     23         1 (-9.081225 38.65942)
```
