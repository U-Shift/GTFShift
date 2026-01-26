# 5. Classify transit data

``` r
library(GTFShift)
library(tidytransit)
library(mapview)
library(sf)
library(dplyr)
```

## Introduction

Data classification helps to categorize transit data into meaningful
groups based on specific criteria or thresholds. This process is
essential for interpreting complex datasets, enabling transit planners
and analysts to make informed decisions regarding service improvements,
resource allocation, and policy development. `GTFShift` provides methods
that encapsulate methodologies for this purpose. This document explores
their applicability with simple examples.

> This article uses a GTFS feed from the library GTFS database for
> Portugal as an example. Refer to the
> [vignette(“download”)](https://u-shift.github.io/GTFShift/articles/download)
> for more details.

``` r
# Get GTFS from library GTFS database for Portugal
data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_id = "barreiro"
gtfs = GTFShift::load_feed(data$URL[data$ID == gtfs_id], create_transfers=FALSE)
```

## Bus frequency LOS

Bus frequency level of service (LOS) classification is a methodology
used to evaluate the quality of bus services based on their frequency.
The classification is based on the Highway Capacity Manual (HCM) 2000
guidelines on “Service Frequency LOS for Urban Scheduled Transit
Service” (Exhibit 27-1), implemented in
[`GTFShift::classify_frequency_los()`](https://u-shift.github.io/GTFShift/reference/classify_frequency_los.md).

The LOS categories range from A to F, with A representing the highest
level of service (most frequent buses) and F representing the lowest
level of service (least frequent buses).

![HCM Exhibit 27-1: Service Frequency LOS for Urban Scheduled Transit
Service](figures/hcm_ex_27_1.png)

HCM Exhibit 27-1: Service Frequency LOS for Urban Scheduled Transit
Service

Note that this method is an adaptation of the original method, as it
originally classifies LOS based on headway (time) and not frequency
(number of buses per hour), being this last one a proxy.

Also, mind that HCM states that “LOS is determined by destination from a
given transit stop, since several routes may serve a given stop but not
all may serve a particular destination”. Applications of this method to
other scenarios should consider this aspect.

``` r
# Get route frequency analysis
frequency_analysis = GTFShift::get_route_frequency_hourly(gtfs)

# Classify frequency level of service
frequency_los = GTFShift::classify_frequency_los(frequency_analysis)

frequency_los
#> Simple feature collection with 435 features and 7 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.084467 ymin: 38.57161 xmax: -9.01185 ymax: 38.67369
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>          route_id route_short_name direction_id hour frequency    shape_id
#> 1  10_10-COINA-FT               10            0    6         1 10-COINA-FT
#> 2  10_10-COINA-FT               10            0    7         2 10-COINA-FT
#> 3  10_10-COINA-FT               10            0    8         1 10-COINA-FT
#> 4  10_10-COINA-FT               10            0    9         2 10-COINA-FT
#> 5  10_10-COINA-FT               10            0   10         1 10-COINA-FT
#> 6  10_10-COINA-FT               10            0   11         2 10-COINA-FT
#> 7  10_10-COINA-FT               10            0   12         1 10-COINA-FT
#> 8  10_10-COINA-FT               10            0   13         2 10-COINA-FT
#> 9  10_10-COINA-FT               10            0   14         1 10-COINA-FT
#> 10 10_10-COINA-FT               10            0   15         2 10-COINA-FT
#>                          geometry frequency_los
#> 1  LINESTRING (-9.051876 38.58...             E
#> 2  LINESTRING (-9.051876 38.58...             D
#> 3  LINESTRING (-9.051876 38.58...             E
#> 4  LINESTRING (-9.051876 38.58...             D
#> 5  LINESTRING (-9.051876 38.58...             E
#> 6  LINESTRING (-9.051876 38.58...             D
#> 7  LINESTRING (-9.051876 38.58...             E
#> 8  LINESTRING (-9.051876 38.58...             D
#> 9  LINESTRING (-9.051876 38.58...             E
#> 10 LINESTRING (-9.051876 38.58...             D

table(frequency_los$frequency_los)
#> 
#>   A   B   C   D   E 
#>   4  42  91 101 197
```

``` r
mapview::mapview(
  frequency_los |> filter(hour == 8),
  zcol = "frequency_los",
  layer.name = "Bus Frequency LOS at 8 AM"
)
```
