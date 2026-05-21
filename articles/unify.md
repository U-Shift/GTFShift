# 3. Aggregate GTFS feeds

``` r

library(GTFShift)
library(tidytransit)
library(dplyr)
library(mapview)
```

## Introduction

Public transit analysis takes advantage of the standardized GTFS format.
However, its provision by operator makes it difficult for network
aggregated analysis, considering connectivity and multimodality.
[`GTFShift::unify()`](https://u-shift.github.io/GTFShift/reference/unify.md)
proposes a simple solution to this problem, generating an aggregated
GTFS file given several instances of these.

> This article uses GTFS feeds from the library GTFS database for
> Portugal as an example. Refer to the
> [vignette(“download”)](https://u-shift.github.io/GTFShift/articles/download)
> for more details.

``` r

# Get GTFS from library GTFS database for Portugal
data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))

gtfs_list = lapply(c("barreiro", "fertagus"), function(ID) {
  feed = GTFShift::load_feed(data$URL[data$ID == ID], create_transfers=FALSE)
  summary(feed)
  return(feed)
})
```

    #> tidygtfs object
    #> files        agency, routes, stop_times, trips, shapes, dead_runs, layover, blocks, calendar, calendar_dates, feed_info, stops
    #> agency       Transportes Colectivos do Barreiro
    #> service      from 2026-05-06 to 2026-12-31
    #> uses         stop_times (no frequencies)
    #> # routes       76
    #> # trips      3239
    #> # stop_ids    279
    #> # stop_names  162
    #> # shapes       76
    #> tidygtfs object
    #> files        agency, routes, stop_times, trips, fare_attributes, fare_rules, shapes, vehicles, calendar, calendar_dates, feed_info, stops
    #> agency       Fertagus
    #> service      from 2026-04-13 to 2026-10-13
    #> uses         stop_times (no frequencies)
    #> # routes       3
    #> # trips      260
    #> # stop_ids    14
    #> # stop_names  14
    #> # shapes       6

## Unify GTFS

The unification is performed through
[`GTFShift::unify()`](https://u-shift.github.io/GTFShift/reference/unify.md),
producing a single GTFS instance, saved as a ZIP file. Option
`create_transfers` enables the generation of `transfers.txt`,
aggregating close stops, even if from different GTFS.

``` r

# Perform unification
gtfs_united = GTFShift::unify(gtfs_list[[1]], gtfs_list[[2]], create_transfers = TRUE)

summary(gtfs_united)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, fare_attributes, fare_rules, shapes, transfers, dead_runs, layover, blocks, vehicles, ., calendar, calendar_dates, feed_info, stops
#> agencies     Transportes Colectivos do Barreiro, Fertagus
#> service      from 2026-04-13 to 2026-12-31
#> uses         stop_times (no frequencies)
#> # routes       79
#> # trips      3499
#> # stop_ids    293
#> # stop_names  176
#> # shapes       82

summary(gtfs_united$transfers)
#>     from_stop_id      to_stop_id   transfer_type min_transfer_time
#>  Length   :1150   Length   :1150   Min.   :2     Min.   :120.0    
#>  N.unique : 277   N.unique : 277   1st Qu.:2     1st Qu.:120.0    
#>  N.blank  :   0   N.blank  :   0   Median :2     Median :247.0    
#>  Min.nchar:   6   Min.nchar:   6   Mean   :2     Mean   :245.0    
#>  Max.nchar:   7   Max.nchar:   7   3rd Qu.:2     3rd Qu.:349.8    
#>                                    Max.   :2     Max.   :416.0
```

It can be displayed using `mapview`.

#### Aggregated routes

``` r

shape_agency = gtfs_united$trips |>
  left_join(gtfs_united$routes, by = "route_id") |>
  left_join(gtfs_united$agency, by = "agency_id") |>
  select(shape_id, agency_id, agency_name) |>
  distinct()

shapes_sf = tidytransit::shapes_as_sf(gtfs_united$shapes) |>
  left_join(shape_agency, by = "shape_id") |>
  filter(!is.na(agency_id))
```

``` r

mapview::mapview(shapes_sf, zcol = "agency_name", legend = TRUE, layer.name="Agency")
```

#### Aggregated stops

``` r

stop_agency = gtfs_united$stop_times  |>
  left_join(gtfs_united$trips, by = "trip_id") |>
  left_join(gtfs_united$routes, by = "route_id") |>
  left_join(gtfs_united$agency, by = "agency_id") |>
  select(stop_id, agency_id, agency_name) |>
  distinct()

stops_sf = tidytransit::stops_as_sf(gtfs_united$stops) |>
  left_join(stop_agency, by = "stop_id") |>
  filter(!is.na(agency_id))
```

``` r

mapview::mapview(stops_sf, zcol = "agency_name", legend = TRUE, layer.name="Agency")
```
