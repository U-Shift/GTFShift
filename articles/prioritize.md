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
```

``` r
summary(lanes)
#>   way_osm_id             hour         frequency      is_bus_lane    
#>  Length:132056      Min.   : 0.00   Min.   : 1.000   Mode :logical  
#>  Class :character   1st Qu.: 8.00   1st Qu.: 3.000   FALSE:120040   
#>  Mode  :character   Median :13.00   Median : 6.000   TRUE :12016    
#>                     Mean   :12.56   Mean   : 9.673                  
#>                     3rd Qu.:18.00   3rd Qu.:13.000                  
#>                     Max.   :23.00   Max.   :99.000                  
#>  n_lanes_parking   n_lanes_circulation    n_lanes       n_directions  
#>  Min.   :0.00000   Min.   :1.00        Min.   :1.000   Min.   :1.000  
#>  1st Qu.:0.00000   1st Qu.:1.00        1st Qu.:1.000   1st Qu.:1.000  
#>  Median :0.00000   Median :2.00        Median :2.000   Median :1.000  
#>  Mean   :0.01438   Mean   :2.15        Mean   :2.165   Mean   :1.364  
#>  3rd Qu.:0.00000   3rd Qu.:3.00        3rd Qu.:3.000   3rd Qu.:2.000  
#>  Max.   :2.00000   Max.   :7.00        Max.   :7.000   Max.   :2.000  
#>  n_lanes_circulation_direction n_lanes_direction    routes         
#>  Min.   :1.000                 Min.   :1.000     Length:132056     
#>  1st Qu.:1.000                 1st Qu.:1.000     Class :character  
#>  Median :1.000                 Median :1.000     Mode  :character  
#>  Mean   :1.703                 Mean   :1.713                       
#>  3rd Qu.:2.000                 3rd Qu.:2.000                       
#>  Max.   :6.000                 Max.   :6.000                       
#>             geom       
#>  LINESTRING   :132056  
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

``` r
rt_collection = read.csv("rt_collect_file.csv") |> 
  sf::st_as_sf(coords = c("vehicle.position.longitude", "vehicle.position.latitude"), crs = 4326)

lanes = GTFShift::rt_extend_prioritization(
  lane_prioritization = lanes,
  rt_collection = rt_collection
)
```

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
#>   way_osm_id             hour     frequency     is_bus_lane    
#>  Length:6665        Min.   :8   Min.   : 1.00   Mode :logical  
#>  Class :character   1st Qu.:8   1st Qu.: 5.00   FALSE:6114     
#>  Mode  :character   Median :8   Median :10.00   TRUE :551      
#>                     Mean   :8   Mean   :13.32                  
#>                     3rd Qu.:8   3rd Qu.:18.00                  
#>                     Max.   :8   Max.   :99.00                  
#>                                                                
#>  n_lanes_parking   n_lanes_circulation    n_lanes       n_directions  
#>  Min.   :0.00000   Min.   :1.000       Min.   :1.000   Min.   :1.000  
#>  1st Qu.:0.00000   1st Qu.:1.000       1st Qu.:1.000   1st Qu.:1.000  
#>  Median :0.00000   Median :2.000       Median :2.000   Median :1.000  
#>  Mean   :0.01425   Mean   :2.122       Mean   :2.136   Mean   :1.365  
#>  3rd Qu.:0.00000   3rd Qu.:3.000       3rd Qu.:3.000   3rd Qu.:2.000  
#>  Max.   :2.00000   Max.   :7.000       Max.   :7.000   Max.   :2.000  
#>                                                                       
#>  n_lanes_circulation_direction n_lanes_direction    routes         
#>  Min.   :1.000                 Min.   :1.000     Length:6665       
#>  1st Qu.:1.000                 1st Qu.:1.000     Class :character  
#>  Median :1.000                 Median :1.000     Mode  :character  
#>  Mean   :1.679                 Mean   :1.689                       
#>  3rd Qu.:2.000                 3rd Qu.:2.000                       
#>  Max.   :6.000                 Max.   :6.000                       
#>                                                                    
#>    speed_avg         speed_median      speed_p25        speed_p75       
#>  Min.   : 0.01694   Min.   : 0.000   Min.   : 0.000   Min.   : 0.01694  
#>  1st Qu.: 8.53002   1st Qu.: 7.994   1st Qu.: 6.138   1st Qu.:10.19889  
#>  Median : 9.83646   Median : 9.259   Median : 7.251   Median :11.79780  
#>  Mean   :10.24983   Mean   : 9.589   Mean   : 7.377   Mean   :12.39146  
#>  3rd Qu.:11.48475   3rd Qu.:10.886   3rd Qu.: 8.469   3rd Qu.:13.90043  
#>  Max.   :64.79895   Max.   :44.118   Max.   :41.508   Max.   :77.74918  
#>  NA's   :128        NA's   :128      NA's   :128      NA's   :128       
#>   speed_count     route_names                   geom     
#>  Min.   :   1.0   Length:6665        LINESTRING   :6665  
#>  1st Qu.:  42.0   Class :character   epsg:4326    :   0  
#>  Median : 101.0   Mode  :character   +proj=long...:   0  
#>  Mean   : 176.5                                          
#>  3rd Qu.: 217.0                                          
#>  Max.   :5541.0                                          
#>  NA's   :128

p50_frequency = quantile(lanes_0800$frequency, 0.5, na.rm=TRUE)
p50_speed = quantile(lanes_0800$speed_avg, 0.5, na.rm=TRUE)
```

``` r
mapview::mapview(
  lanes_0800 |> filter(is_bus_lane & (frequency<p50_frequency | (is.na(n_lanes) | n_lanes_direction<=1) | speed_avg<=p50_speed)),
  layer.name=sprintf("Bus lane with -%d bus/h OR -2 lane/dir OR %.2f km/h or - avg. speed", p50_frequency, p50_speed),
  color="#DAD887",
  homebutton=FALSE,
  lwd=3

) + mapview::mapview(
  lanes_0800 |> filter(is_bus_lane & frequency>=p50_frequency & !is.na(n_lanes) & n_lanes_direction>1 & speed_avg>p50_speed),
  layer.name=sprintf("Bus lane with +%d bus/h AND +1 lane/dir AND +%.2f km/h avg.speed", p50_frequency-1, p50_speed),
  color="#3BC1A8",
  homebutton=FALSE,
  lwd=3
) + mapview::mapview(
  lanes_0800 |> filter(!is_bus_lane & frequency>=p50_frequency & !is.na(n_lanes) & n_lanes_direction>1 & speed_avg<=p50_speed),
  layer.name=sprintf("NO bus lane with +%d bus/h AND +1 lane/dir AND %.2f km/h or - avg.speed", p50_frequency-1, p50_speed),
  color="#F63049",
  homebutton=FALSE,
  lwd=3
)
```

This visual representation allows to easily identify not only the
high-priority segments for bus lane implementation, but also their
spatial distribution across the existent network. A process that extends
the results by incorporating the network continuity perspective,
enabling the identification and eventual prioritization of critical
segments that connect bus lanes but have a bad performance.
