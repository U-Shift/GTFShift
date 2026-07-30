# Filter GTFS feed by agency

Filter GTFS feed by agency

## Usage

``` r
filter_by_agency(gtfs, id = NA, name = NA)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- id:

  Integer (Optional when name). Ids of the agency.

- name:

  String (Optional when id). Name of the agency.

## Value

tidygtfs. The filtered GTFS feed.

## Details

Allows to filter a GTFS feed for the agency, using the id, name or both.
Returns empty feed it none provided.

## Examples

``` r
# Load sample feed with multiple agencies
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_merged_sample.zip", package = "GTFShift")
)

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


# Filter by id
gtfs_id_8 = gtfs |> GTFShift::filter_by_agency(id = "8")

summary(gtfs_id_8)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, shapes, calendar, calendar_dates, stops
#> agency       Transportes Colectivos do Barreiro
#> service      from 2026-08-03 to 2026-08-28
#> uses         stop_times (no frequencies)
#> # routes      1
#> # trips       1
#> # stop_ids   31
#> # stop_names 31
#> # shapes      1


# Filter by name 
gtfs_ttsl <- gtfs |> GTFShift::filter_by_agency(name = "TTSL - Transtejo Soflusa") 

summary(gtfs_ttsl)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, shapes, calendar, calendar_dates, stops
#> agency       TTSL - Transtejo Soflusa
#> service      from 2020-12-19 to 2028-12-31
#> uses         stop_times (no frequencies)
#> # routes      1
#> # trips      100
#> # stop_ids    3
#> # stop_names  3
#> # shapes      5
```
