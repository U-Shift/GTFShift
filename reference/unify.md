# Merge multiple GTFS into a single aggregated file

Merge multiple GTFS into a single aggregated file

## Usage

``` r
unify(
  ...,
  prefix = FALSE,
  store_path = NA,
  create_transfers = FALSE,
  transfer_distance = 300,
  transfer_time = 120,
  transfer_street_routing = FALSE
)
```

## Arguments

- ...:

  tidygtfs\[\]. List of GTFS feeds.

- prefix:

  Boolean (Default FALSE). If TRUE, prefixes all tables with the
  agency_id, to avoid conflicts in case of same IDs in different feeds.

- store_path:

  String (Optional). If provided, aggregated feed zip is stored at
  location. The file is overwritten if it already exists.

- create_transfers:

  Boolean (Default FALSE). When true, generates transfers table,
  aggregating close stops, even if from different GTFS.

- transfer_distance:

  Integer (Default 300). Upper straight-line distance limit in meters
  for transfers.

- transfer_time:

  Integer (Default 120). Minimum time in seconds for transfers; all
  values below this will be replaced with this value, particularly all
  those defining in-place transfers where stop longitudes and latitudes
  remain identical.

- transfer_street_routing:

  Boolean (Default FALSE). If TRUE, transfer times are calculated by
  routing throughout the underlying street network (downloaded
  automatically).

## Value

tidygtfs. The unified GTFS feed.

## Details

Aggregates multiple feeds using
[`gtfstools::merge_gtfs()`](https://rdrr.io/pkg/gtfstools/man/merge_gtfs.html).
When generating transfers, those already existing in each GTFS file are
kept, extended with new ones computed based on the stops network of the
final aggregated version.

This computation is executed with
[`gtfsrouter::gtfs_transfer_table()`](https://rdrr.io/pkg/gtfsrouter/man/gtfs_transfer_table.html),
with the parameters `d_limit=transfer_distance`,
`min_transfer_time=transfer_time` and
`network_times=transfer_street_routing`. The other parameters are
applied the library default values.

For a detailed example, see the `vignette("unify")`.

## See also

[`gtfstools::merge_gtfs()`](https://rdrr.io/pkg/gtfstools/man/merge_gtfs.html)

[`gtfsrouter::gtfs_transfer_table()`](https://rdrr.io/pkg/gtfsrouter/man/gtfs_transfer_table.html)

## Examples

``` r
# Load multiple GTFS files
gtfs_1 <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip", package = "GTFShift")
)

summary(gtfs_1)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, shapes, calendar, calendar_dates, stops
#> agency       Transportes Colectivos do Barreiro
#> service      from 2026-06-08 to 2026-12-31
#> uses         stop_times (no frequencies)
#> # routes      27
#> # trips       40
#> # stop_ids   228
#> # stop_names 153
#> # shapes      27

gtfs_1$agency
#> # A tibble: 1 × 8
#>   agency_id agency_name      agency_url agency_timezone agency_lang agency_phone
#>   <chr>     <chr>            <chr>      <chr>           <chr>       <chr>       
#> 1 8         Transportes Col… https://w… Europe/Lisbon   pt          212068787   
#> # ℹ 2 more variables: agency_fare_url <chr>, agency_email <chr>

head(gtfs_1$trips)
#> # A tibble: 6 × 9
#>   route_id       service_id          trip_id trip_headsign direction_id shape_id
#>   <chr>          <chr>               <chr>   <chr>                <int> <chr>   
#> 1 3_3-SA-TERM_R2 DF / Projeto A _26… DF_3-S… 3 TERMINAL (…            0 3-SA-TE…
#> 2 3_3-TER-CS_CAS DF / Projeto A _26… DF_3-T… 3 CIDADE SOL…            1 3-TER-C…
#> 3 3_3-TER-SA_CAS DF / Projeto A _26… DF_3-T… 3 STº ANTÓNI…            1 3-TER-S…
#> 4 3_3-TERM-SA_R2 DF / Projeto A _26… DF_3-T… 3 STº ANTÓNI…            1 3-TERM-…
#> 5 3_3-TERM-SA_LC DF / Projeto A _26… DF_3-T… 3 STº ANTÓNI…            1 3-TERM-…
#> 6 3_3-TER-SA_CAS DF / Projeto A _26… DF_3-T… 3 STº ANTÓNI…            1 3-TER-S…
#> # ℹ 3 more variables: wheelchair_accessible <int>, bikes_allowed <int>,
#> #   pattern_id <chr>

gtfs_2 <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_ttsl_sample_no_shapes.zip", package = "GTFShift")
)
#> Warning: > CREATED shapes.txt, the file was missing!

summary(gtfs_2)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, calendar, calendar_dates, stops
#> agency       TTSL - Transtejo Soflusa
#> service      from 2020-12-19 to 2028-12-31
#> uses         stop_times (no frequencies)
#> # routes      1
#> # trips      100
#> # stop_ids    3
#> # stop_names  3
#> # shapes      5

gtfs_2$agency
#> # A tibble: 1 × 8
#>   agency_id agency_name      agency_url agency_timezone agency_lang agency_phone
#>   <chr>     <chr>            <chr>      <chr>           <chr>       <chr>       
#> 1 4         TTSL - Transtej… https://t… Europe/Lisbon   pt          +3512104224…
#> # ℹ 2 more variables: agency_fare_url <chr>, agency_email <chr>

head(gtfs_2$trips)
#> # A tibble: 6 × 9
#>   route_id service_id trip_id   trip_headsign direction_id wheelchair_accessible
#>   <chr>    <chr>      <chr>     <chr>                <int>                 <int>
#> 1 3_0      03D1       0303A080… Cais do Sodré            0                     1
#> 2 3_0      03D1       0303A090… Cais do Sodré            0                     1
#> 3 3_0      03D1       0303A110… Cais do Sodré            0                     1
#> 4 3_0      03D1       0303A130… Cais do Sodré            0                     1
#> 5 3_0      03D1       0303A150… Cais do Sodré            0                     1
#> 6 3_0      03D1       0303A163… Cais do Sodré            0                     1
#> # ℹ 3 more variables: bikes_allowed <int>, pattern_id <chr>, shape_id <chr>

# Unify them
unified <- GTFShift::unify(gtfs_1, gtfs_2, prefix = TRUE)
#> 1. Starting merge process...
#> FINISHED :)

summary(unified)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, shapes, calendar, calendar_dates, stops
#> agencies     Transportes Colectivos do Barreiro, TTSL - Transtejo Soflusa
#> service      from 2020-12-19 to 2028-12-31
#> uses         stop_times (no frequencies)
#> # routes      28
#> # trips      140
#> # stop_ids   231
#> # stop_names 156
#> # shapes      32

unified$agency
#> # A tibble: 2 × 8
#>   agency_id agency_name      agency_url agency_timezone agency_lang agency_phone
#>   <chr>     <chr>            <chr>      <chr>           <chr>       <chr>       
#> 1 8_8       Transportes Col… https://w… Europe/Lisbon   pt          212068787   
#> 2 4_4       TTSL - Transtej… https://t… Europe/Lisbon   pt          +3512104224…
#> # ℹ 2 more variables: agency_fare_url <chr>, agency_email <chr>

head(unified$trips)
#> # A tibble: 6 × 9
#>   route_id         service_id        trip_id trip_headsign direction_id shape_id
#>   <chr>            <chr>             <chr>   <chr>                <int> <chr>   
#> 1 8_3_3-SA-TERM_R2 8_DF / Projeto A… 8_DF_3… 3 TERMINAL (…            0 8_3-SA-…
#> 2 8_3_3-TER-CS_CAS 8_DF / Projeto A… 8_DF_3… 3 CIDADE SOL…            1 8_3-TER…
#> 3 8_3_3-TER-SA_CAS 8_DF / Projeto A… 8_DF_3… 3 STº ANTÓNI…            1 8_3-TER…
#> 4 8_3_3-TERM-SA_R2 8_DF / Projeto A… 8_DF_3… 3 STº ANTÓNI…            1 8_3-TER…
#> 5 8_3_3-TERM-SA_LC 8_DF / Projeto A… 8_DF_3… 3 STº ANTÓNI…            1 8_3-TER…
#> 6 8_3_3-TER-SA_CAS 8_DF / Projeto A… 8_DF_3… 3 STº ANTÓNI…            1 8_3-TER…
#> # ℹ 3 more variables: wheelchair_accessible <int>, bikes_allowed <int>,
#> #   pattern_id <chr>
```
