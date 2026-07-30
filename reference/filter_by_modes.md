# Filter GTFS feed by mode

Filter GTFS feed by mode

## Usage

``` r
filter_by_modes(gtfs, modes = list())
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- modes:

  Integer\[\]. A list with the ids of modes to consider.

## Value

tidygtfs. The filtered GTFS feed.

## Details

Allows to filter a GTFS feed for the type of transportation used,
allowing for a more narrow analysis of multimodal files. Refer to
`routes.txt` `route_type` parameter on [GTFS
documentation](https://gtfs.org/documentation/schedule/reference/#routestxt)
for more details.

## Examples

``` r
# Load sample feed with multiple modes
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_merged_sample.zip", package = "GTFShift")
)

gtfs$routes |> dplyr::select(route_id, route_type)
#> # A tibble: 2 × 2
#>   route_id route_type
#>   <chr>         <int>
#> 1 2_2-TERM          3
#> 2 3_0               4

summary(gtfs)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, shapes, calendar, calendar_dates, stops
#> agencies     Transportes Colectivos do Barreiro, TTSL - Transtejo Soflusa
#> service      from 2020-12-19 to 2028-12-31
#> uses         stop_times (no frequencies)
#> # routes       2
#> # trips      101
#> # stop_ids    34
#> # stop_names  34
#> # shapes       6


# Filter by bus mode (ferry agency should be excluded)
gtfs_bus <- gtfs |> GTFShift::filter_by_modes(modes = c(3))

gtfs_bus$routes |> dplyr::select(route_id, route_type)
#> # A tibble: 1 × 2
#>   route_id route_type
#>   <chr>         <int>
#> 1 2_2-TERM          3

summary(gtfs_bus)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, shapes, calendar, calendar_dates, stops
#> agencies     Transportes Colectivos do Barreiro, TTSL - Transtejo Soflusa
#> service      from 2026-08-03 to 2026-08-28
#> uses         stop_times (no frequencies)
#> # routes      1
#> # trips       1
#> # stop_ids   31
#> # stop_names 31
#> # shapes      1
```
