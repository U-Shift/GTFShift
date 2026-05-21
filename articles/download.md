# 1. Getting transit data

``` r

library(GTFShift)
library(tidytransit)
```

## Introduction

Performing analysis over transit requires data on the operation and
infrastructure. [GTFS](https://gtfs.org/) is an open standard that
offers this information in a simple and complete way.

GTFShift includes some built in methods that can assist gathering it,
explored in this article.

## Read a GTFS file

GTFShift makes use of several R packages to implement its features, but
most of it is built upon
[tidytransit](https://r-transit.github.io/tidytransit/), so all the
methods that accept a GTFS object should be a `tidygtfs` object.

To create it, you can either use
[`GTFShift::load_feed()`](https://u-shift.github.io/GTFShift/reference/load_feed.md)
or the tidytransit method,
[`tidytransit::read_gtfs()`](https://r-transit.github.io/tidytransit/reference/read_gtfs.html).
Actually,
[`GTFShift::load_feed()`](https://u-shift.github.io/GTFShift/reference/load_feed.md)
is an extension of the last, with the additional features of scanning
the feed for any integrity errors and fixing them automatically, as well
as the option to store it locally.

``` r

# Using tidytransit
gtfs = tidytransit::load_feed("https://operator.com/gtfs.zip")

# Using GTFShift
gtfs = GTFShift::load_feed("https://operator.com/gtfs.zip")
```

## Find GTFS feeds

### Using open catalogues

The gathering of the GTFS files can be simplified by using public
archives like [mobilitydatabase.org](https://mobilitydatabase.org/) or
[transit.land](https://www.transit.land/).

GTFShift provides a method to query Mobility Database:
[`GTFShift::query_mobilitydatabase()`](https://u-shift.github.io/GTFShift/reference/query_mobilitydatabase.md).
It queries the `/v1/gtfs_feeds` API endpoint, returning a list of GTFS
feeds with information about the providers, the area they cover and an
URL to download them.

To use it, an access token must be provided. It can be obtained for free
at Mobility Database [website](https://mobilitydatabase.org/account).

``` r

aml = sf::st_read("https://github.com/U-Shift/MQAT/raw/refs/heads/main/geo/MUNICIPIOSgeo.gpkg", quiet = TRUE) |> sf::st_bbox()

# usethis::edit_r_environ() # to set MOBILITY_DATABASE variable for this code chunk to work
feeds = GTFShift::query_mobilitydatabase(
  refresh_token = Sys.getenv("MOBILITY_DATABASE"),
  bounding_filter_method = "completely_enclosed",
  bbox = aml
)

feeds |>
  dplyr::filter(status == "active") |>
  dplyr::select(provider, status, producer_url) |>
  head()
#>                                      provider status
#> 1                                      Carris active
#> 2                 Cascais Próxima, E.M., S.A. active
#> 3                                    Fertagus active
#> 4                     Metro de Lisboa (Metro) active
#> 5 Metro Transportes do Sul, Metro Sul do Tejo active
#> 6           Transportes Coletivos do Barreiro active
#>                                                                       producer_url
#> 1                            https://gateway.carris.pt/gateway/gtfs/api/v2.11/GTFS
#> 2 https://drive.google.com/uc?export=download&id=13ucYiAJRtu-gXsLa02qKJrGOgDjbnUWX
#> 3                             https://www.fertagus.pt/GTFSTMLzip/Fertagus_GTFS.zip
#> 4                      https://www.metrolisboa.pt/google_transit/googleTransit.zip
#> 5                                              https://mts.pt/imt/MTS-20240129.zip
#> 6                https://www.tcbarreiro.pt/front/files/sample_gtfs/GTFS-TCB_24.zip

gtfs = GTFShift::load_feed(feeds$producer_url[2], create_transfers=FALSE)
summary(gtfs)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, shapes, calendar, calendar_dates, stops
#> agency       Carris
#> service      from 2026-01-01 to 2026-06-05
#> uses         stop_times (no frequencies)
#> # routes       174
#> # trips      77928
#> # stop_ids    2331
#> # stop_names  1138
#> # shapes       306
```

### Using GTFShift incorporated database for Portugal

This library offers a small database with a compilation of GTFS files
for Portuguese operators. It is a CSV file, available at
`extdata/gtfs_sources_pt.csv`, and has the following attributes:

- `ID`, a string unique identifier of the region/city the GTFS file
  applies to;
- `LastUpdate`, the date at which this database entry was last updated;
- `ReferenceDate`, a representative Wednesday that falls within the GTFS
  calendar;
- `URL`, the URL at which the GTFS file is available;
- `GTFSDocs`, the URL to the page that documents the operator GTFS.

``` r

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
head(data)
#>         ID LastUpdate ReferenceDate
#> 1       cp 2025-02-01    2025-02-05
#> 2    autna 2025-02-01    2025-02-05
#> 3      AML 2025-02-01    2025-02-05
#> 4 barreiro 2025-02-01    2025-02-05
#> 5  cascais 2025-02-01    2025-02-05
#> 6   lisboa 2025-02-01    2025-02-05
#>                                                                                    URL
#> 1                                                  https://publico.cp.pt/gtfs/gtfs.zip
#> 2     https://drive.google.com/uc?export=download&id=1gah1x10RyFu7gJPweBcCXPd9vcFJFQ7c
#> 3                                              https://api.carrismetropolitana.pt/gtfs
#> 4                                          https://backend.tcbarreiro.pt/download-gtfs
#> 5 https://drive.google.com/u/0/uc?id=13ucYiAJRtu-gXsLa02qKJrGOgDjbnUWX&export=download
#> 6                                 https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS
#>                                                                           URL.RT
#> 1                                                                               
#> 2                                                                               
#> 3                                 https://api.carrismetropolitana.pt/v1/vehicles
#> 4                                                                               
#> 5                                                                               
#> 6 https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS/realtime/vehiclepositions
#>            Type
#> 1 Long distance
#> 2 Long distance
#> 3   Inter-urban
#> 4         Urban
#> 5         Urban
#> 6         Urban
#>                                                                                                                  GTFSDocs
#> 1                                                                             https://www.transit.land/operators/o-eyc-cp
#> 2                                                                https://www.transit.land/operators/o-ez-autnatransportes
#> 3                                                                              https://github.com/carrismetropolitana/api
#> 4                                                                                              https://www.tcbarreiro.pt/
#> 5                                                          https://dadosabertos.cascais.pt/pt_PT/dataset/gtfs-mobicascais
#> 6 https://gateway.carris.pt/apiui/#!/apis/2c05b837-1c8e-4b34-85b8-371c8edb344b/pages/d7f1c190-908d-4615-b1c1-90908d4615f5

gtfs = GTFShift::load_feed(data$URL[1], create_transfers = FALSE) # example with CP - trains
summary(gtfs)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, calendar, calendar_dates, stops
#> agency       CP - Comboios de Portugal
#> service      from 2025-12-14 to 2026-12-12
#> uses         stop_times (no frequencies)
#> # routes      181
#> # trips      1736
#> # stop_ids    454
#> # stop_names  454
#> # shapes      253
```
