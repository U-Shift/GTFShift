# Prioritize bus lane implementation

``` r
library(GTFShift)
library(tidytransit)
library(mapview)
library(dplyr)
library(osmdata)
```

## Introduction

Bus lanes have the potential to improve the reliability of bus
operations by limiting the negative impacts of traffic congestion,
reducing the variability of travel times, and increasing the average
commercial speed - which can ultimately be used to increase service
frequency.

However, introducing them on road infrastructure affects other modes,
such as private vehicles, which may experience a decrease in the level
of service due to reduced allocated space, potentially jeopardizing
public acceptance.

Common criteria for implementing bus lanes include:

- **Frequency of buses (and trams) per hour and direction, at a peak
  hour;**
- **Number of lanes in the same direction;**
- Existing traffic conditions;
- Existing bus lanes in the area (from a network continuity
  perspective).

GTFShift provides methods to analyse the dimensions in bold, namely:

- [`GTFShift::get_way_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_way_frequency_hourly.md),
  to obtain the frequency of services per hour for each road segment
  with transit service and the characteristics of such each segments
  (such as number os lanes).

> [`GTFShift:: get_route_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_route_frequency_hourly.md)
> is an alternative

- [`GTFShift::osm_bus_lanes()`](https://u-shift.github.io/GTFShift/reference/osm_bus_lanes.md),
  to identify existing bus lanes in the road network.

This document explores how to use these methods in a combined way to
assist public transport planners in prioritizing bus lane
implementations. For details on the several encapsulated features, refer
to the numerated articles in the menu, that explore in detail each of
the specific approaches followed.

## Prioritize lanes

[`GTFShift::prioritize_lanes()`](https://u-shift.github.io/GTFShift/reference/prioritize_lanes.md)
is a simple method that bundles the logic of the ones mentioned above,
returning a data.frame with the relevant characteristics for each road
segment with transit service. With a single call, it returns all the
aggregated information needed to prioritize bus lane implementations.

``` r
# Get GTFS from library GTFS database for Portugal
data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_id = "lisboa"
gtfs = GTFShift::load_feed(data$URL[data$ID == gtfs_id], create_transfers=FALSE)

osm_q = opq(bbox=sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes)))  |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

lanes = prioritize_lanes(gtfs, osm_q)
summary(lanes)
#>   way_osm_id             hour         frequency      is_bus_lane    
#>  Length:133226      Min.   : 0.00   Min.   : 1.000   Mode :logical  
#>  Class :character   1st Qu.: 8.00   1st Qu.: 3.000   FALSE:121210   
#>  Mode  :character   Median :13.00   Median : 6.000   TRUE :12016    
#>                     Mean   :12.56   Mean   : 9.637                  
#>                     3rd Qu.:18.00   3rd Qu.:13.000                  
#>                     Max.   :23.00   Max.   :99.000                  
#>     n_lanes       n_directions   n_lanes_direction          geometry     
#>  Min.   :1.000   Min.   :1.000   Min.   :0.500     LINESTRING   :133226  
#>  1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000     epsg:4326    :     0  
#>  Median :2.000   Median :1.000   Median :1.000     +proj=long...:     0  
#>  Mean   :2.141   Mean   :1.369   Mean   :1.693                           
#>  3rd Qu.:3.000   3rd Qu.:2.000   3rd Qu.:2.000                           
#>  Max.   :7.000   Max.   :2.000   Max.   :6.000
```

The aggregated data can then be manipulated according to the
prioritization criteria defined by the user. For instance, the following
code classifies (in red) the road segments considering a minimum
frequency of 5 buses/hour and more than 1 lane per direction as high
priority for bus lane implementation.

Additionally, it displays the current bus lane network, highlighting in
green the segments that meet the frequency and lane criteria, and in
yellow those that do not meet the criteria but still have bus lanes.

> The classification of frequent service as 5 buses/hour is based on the
> HCM guidelines for level of service, where a frequency of 5 buses/hour
> corresponds to a level of service C or better. Refer to the [Classify
> transit
> data](https://u-shift.github.io/GTFShift/articles/classify.html#bus-frequency-los)
> article for more details.

``` r
mapview::mapview(
  lanes |> filter((frequency<=4 | (is.na(n_lanes) | n_lanes_direction<=1)) & is_bus_lane),
  layer.name="Bus lane with - 5 bus/h OR - 1 lane/dir",
  color="#DAD887"
) + mapview::mapview(
  lanes |> filter(frequency>=5 & !is.na(n_lanes) & n_lanes_direction>1 & is_bus_lane),
  layer.name="Bus lane with 5 or + bus/h + 1 lane/dir",
  color="#3BC1A8"
) + mapview::mapview(
  lanes |> filter(frequency>=5 & !is.na(n_lanes) & n_lanes_direction>1 & !is_bus_lane),
  layer.name="NO bus lane with 5 or + bus/h + 1 lane/dir",
  color="#F63049"
)
```
