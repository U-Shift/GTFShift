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

- Frequency of buses (and trams) per hour and direction, at a peak hour;
- Number of lanes in the same direction;
- Existing traffic conditions;
- Existing bus lanes in the area (from a network continuity
  perspective).

GTFShift provides methods to analyse these dimensions, namely:

- [`GTFShift::get_way_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_way_frequency_hourly.md),
  to obtain the frequency of services per hour for each road segment
  with transit service and its associated characteristics (such as
  number of lanes).

- [`GTFShift::osm_bus_lanes()`](https://u-shift.github.io/GTFShift/reference/osm_bus_lanes.md),
  to identify existing bus lanes in the road network.

- [`GTFShift::rt_collect_json()`](https://u-shift.github.io/GTFShift/reference/rt_collect_json.md)
  or
  [`GTFShift::rt_collect_protobuf()`](https://u-shift.github.io/GTFShift/reference/rt_collect_protobuf.md),
  to collect GTFS-RT data, which can be later used with
  [`GTFShift::rt_extend_prioritization()`](https://u-shift.github.io/GTFShift/reference/rt_extend_prioritization.md)
  to include real-time operational metrics in the prioritization
  analysis.

This document explores how to use these methods in a combined way to
assist public transport planners in prioritizing bus lane
implementations. For details on the several encapsulated features and
method variations, refer to the numbered articles in the menu, that
explore in detail each of the specific approaches followed.

## Prioritize lanes

### Generate base indicators

[`GTFShift::prioritize_lanes()`](https://u-shift.github.io/GTFShift/reference/prioritize_lanes.md)
is a simple method that generates indicators for most of the criteria
mentioned above using GTFS and OpenStreetMaps data (service frequency
and lane characteristics). With a single call, it returns a data.frame
with the relevant metrics for each road segment with transit service.

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
#>  Length:132794      Min.   : 0.00   Min.   : 1.000   Mode :logical  
#>  Class :character   1st Qu.: 8.00   1st Qu.: 3.000   FALSE:120778   
#>  Mode  :character   Median :13.00   Median : 6.000   TRUE :12016    
#>                     Mean   :12.56   Mean   : 9.654                  
#>                     3rd Qu.:18.00   3rd Qu.:13.000                  
#>                     Max.   :23.00   Max.   :99.000                  
#>     n_lanes       n_directions   n_lanes_direction    routes         
#>  Min.   :1.000   Min.   :1.000   Min.   :1.000     Length:132794     
#>  1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000     Class :character  
#>  Median :2.000   Median :1.000   Median :1.000     Mode  :character  
#>  Mean   :2.143   Mean   :1.362   Mean   :1.699                       
#>  3rd Qu.:3.000   3rd Qu.:2.000   3rd Qu.:2.000                       
#>  Max.   :7.000   Max.   :2.000   Max.   :6.000                       
#>           geometry     
#>  LINESTRING   :132794  
#>  epsg:4326    :     0  
#>  +proj=long...:     0  
#>                        
#>                        
#> 
```

### Extend with GTFS-RT data

If GTFS-RT data is available, it can be used to extend the
prioritization analysis with real-time operational metrics, such as
average speed. This can help identify road segments where buses are
experiencing significant delays due to traffic congestion, which may
benefit from bus lane implementation.

Refer to the [GTFS Real
Time](https://u-shift.github.io/GTFShift/articles/rt.md) article for
details on how to collect GTFS-RT data and extend the prioritization
analysis.

### Visualize results

The aggregated data can then be manipulated according to the
prioritization criteria defined by the user. For instance, the following
code highlights (in red) the road segments as high priority for bus lane
implementation if they have more than 1 lane per direction and a
frequency above the median number of buses per hour registered at 8:00.

``` r
lanes_0800 = lanes |> filter(hour==8)
summary(lanes_0800)
#>   way_osm_id             hour     frequency     is_bus_lane        n_lanes     
#>  Length:6704        Min.   :8   Min.   : 1.00   Mode :logical   Min.   :1.000  
#>  Class :character   1st Qu.:8   1st Qu.: 5.00   FALSE:6153      1st Qu.:1.000  
#>  Mode  :character   Median :8   Median :10.00   TRUE :551       Median :2.000  
#>                     Mean   :8   Mean   :13.28                   Mean   :2.115  
#>                     3rd Qu.:8   3rd Qu.:18.00                   3rd Qu.:3.000  
#>                     Max.   :8   Max.   :99.00                   Max.   :7.000  
#>   n_directions   n_lanes_direction    routes                   geometry   
#>  Min.   :1.000   Min.   :1.000     Length:6704        LINESTRING   :6704  
#>  1st Qu.:1.000   1st Qu.:1.000     Class :character   epsg:4326    :   0  
#>  Median :1.000   Median :1.000     Mode  :character   +proj=long...:   0  
#>  Mean   :1.362   Mean   :1.675                                            
#>  3rd Qu.:2.000   3rd Qu.:2.000                                            
#>  Max.   :2.000   Max.   :6.000

p50_frequency = quantile(lanes_0800$frequency, 0.5, na.rm=TRUE)
```

``` r
mapview::mapview(
  lanes_0800 |> filter(is_bus_lane),
  layer.name="Bus lane",
  color="#3BC1A8",
  homebutton=FALSE
) + mapview::mapview(
  lanes_0800 |> filter(!is_bus_lane & frequency>=p50_frequency & !is.na(n_lanes) & n_lanes_direction>1),
  layer.name=sprintf("NO bus lane with +%d bus/h +1 lane/dir", p50_frequency-1),
  color="#F63049",
  homebutton=FALSE
)
```

This visual representation allows to easily identify not only the
high-priority segments for bus lane implementation, but also their
spatial distribution across the existent network. A process that extends
the results by incorporating the network continuity perspective,
enabling the identification and eventual prioritization of critical
segments that connect bus lanes but have a bad performance.
