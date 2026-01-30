# 4. Analyse GTFS feeds

``` r
library(GTFShift)
library(tidytransit)
library(mapview)
library(sf)
library(dplyr)
```

## Introduction

Analyzing public transit feeds is important to understand its
territorial coverage and dynamics, both on its spatial and temporal
dimensions. `GTFShift` provides several methods that encapsulate
pre-defined methodologies for them. This document explores their
applicability with simple examples.

> This article uses a GTFS feed from the library GTFS database for
> Portugal as an example. Refer to the
> [vignette(“download”)](https://u-shift.github.io/GTFShift/articles/download)
> for more details.

``` r
# Get GTFS from library GTFS database for Portugal
data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_id = "lisboa"
gtfs = GTFShift::load_feed(data$URL[data$ID == gtfs_id], create_transfers=FALSE)
```

## Analyse network extension

To analyse the extension of a transit network, use
[`GTFShift::get_network_extension()`](https://u-shift.github.io/GTFShift/reference/get_network_extension.md),
which computes the total length of all routes in the GTFS feed,
considering, for each, the shape of the variant with the highest
frequency for a given date.

> In this example, we intend to get a value close to the official 757 km
> [reported by
> Carris](https://www.carris.pt/media/ikvdezmc/relat%C3%B3rio-e-contas-carris-2024.pdf).
> For this purpose, we use parameters `direction_wise=TRUE` to consider
> both directions for each route, but removing route overlaps (with
> `unified=TRUE`). Additionally, we use OSM open data to get more
> accurate route geometries (through parameter `use_osm_routes`).

``` r
library(osmdata)
library(units)

osm_q = opq(bbox=sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes)))  |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

route_extent_carris = get_network_extension(gtfs, route_identifier="route_short_name", direction_wise = TRUE, use_osm_routes = osm_q, unified = TRUE)
drop_units(route_extent_carris/1000)
#> [1] 810.5296
```

## Analyse hourly frequency per stop

To analyse frequencies at stops, use
[`GTFShift::get_stop_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_stop_frequency_hourly.md),
producing, for each, an aggregated counting of bus servicing it per
hour.

> By default, the analysis is performed for next business Wednesday, in
> Portugal. Refer to
> [`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md),
> for more details. You can override this, using `date` parameter.

``` r
# Perform frequency analysis
frequencies_stop = GTFShift::get_stop_frequency_hourly(gtfs)
summary(frequencies_stop)
#>    stop_id               hour         frequency              geometry    
#>  Length:44733       Min.   : 0.00   Min.   : 1.00   POINT        :44733  
#>  Class :character   1st Qu.: 8.00   1st Qu.: 3.00   epsg:4326    :    0  
#>  Mode  :character   Median :13.00   Median : 5.00   +proj=long...:    0  
#>                     Mean   :12.75   Mean   : 7.05                        
#>                     3rd Qu.:18.00   3rd Qu.:10.00                        
#>                     Max.   :23.00   Max.   :45.00
```

Its returns an `sf` `data.frame` that can be displayed using mapview, or
stored in GeoPackage format.

``` r
# Display map
mapview::mapview(
  frequencies_stop |>
    filter(hour == 8 &
           frequency > 2),
  zcol = "frequency",
  legend = TRUE,
  cex = 4,
  layer.name = "Frequency (hour)"
)
```

``` r

# Store in GeoPackage format
# st_write(frequencies_stop, "database/transit/bus_stop_frequency.gpkg", append=FALSE, quiet = TRUE)
```

## Analyse hourly frequency per road segment

To analyse frequencies at road segments, use
[`GTFShift::get_way_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_way_frequency_hourly.md),
returning aggregated results per hour and road segment, using OSM ways.

> Mind that routes without an OSM match are ignored. Refer to
> [`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)
> for more details.

``` r
frequencies_way = GTFShift::get_way_frequency_hourly(gtfs, osm_q)
summary(frequencies_way)
#>   way_osm_id             hour         frequency               geometry     
#>  Length:132782      Min.   : 0.00   Min.   : 1.000   LINESTRING   :132782  
#>  Class :character   1st Qu.: 8.00   1st Qu.: 3.000   epsg:4326    :     0  
#>  Mode  :character   Median :13.00   Median : 6.000   +proj=long...:     0  
#>                     Mean   :12.56   Mean   : 9.654                         
#>                     3rd Qu.:18.00   3rd Qu.:13.000                         
#>                     Max.   :23.00   Max.   :99.000
quantile(frequencies_way$frequency)
#>   0%  25%  50%  75% 100% 
#>    1    3    6   13   99
```

It returns an `sf` `data.frame` that can be displayed using mapview.

``` r
mapview::mapview(
  frequencies_way |> filter(hour == 8 & frequency > 2),
  zcol = "frequency",
  layer.name = "Frequency (hour)"
)
```

## Analyse hourly frequency per route

The frequency analysis can also be performed route wise. For this
purpose, use
[`GTFShift::get_route_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_route_frequency_hourly.md),
returning aggregated results per hour and route.

The analysis can be performed for each route individually.

> By default, the analysis is performed for next business Wednesday, in
> Portugal. Refer to
> [`GTFShift::calendar_nextBusinessWednesday()`](https://u-shift.github.io/GTFShift/reference/calendar_nextBusinessWednesday.md),
> for more details. You can override this, using `date` parameter.

``` r
frequencies_route = GTFShift::get_route_frequency_hourly(gtfs)
summary(frequencies_route)
#>    route_id         route_short_name    direction_id         hour      
#>  Length:3397        Length:3397        Min.   :0.0000   Min.   : 0.00  
#>  Class :character   Class :character   1st Qu.:0.0000   1st Qu.: 9.00  
#>  Mode  :character   Mode  :character   Median :0.0000   Median :13.00  
#>                                        Mean   :0.4477   Mean   :13.15  
#>                                        3rd Qu.:1.0000   3rd Qu.:18.00  
#>                                        Max.   :1.0000   Max.   :23.00  
#>    frequency        shape_id                  geometry   
#>  Min.   : 1.000   Length:3397        LINESTRING   :3397  
#>  1st Qu.: 2.000   Class :character   epsg:4326    :   0  
#>  Median : 3.000   Mode  :character   +proj=long...:   0  
#>  Mean   : 3.415                                          
#>  3rd Qu.: 4.000                                          
#>  Max.   :12.000
quantile(frequencies_route$frequency)
#>   0%  25%  50%  75% 100% 
#>    1    2    3    4   12
```

The `overline` parameter allows for an even more aggregated screening of
the operation, clustering routes that overlap and converting them into a
single route network. This allows for a better visualization of the
volumes of frequencies per each segment of the network and can help
prioritizing interventions in the network.

``` r
frequencies_route_overline = GTFShift::get_route_frequency_hourly(gtfs, overline = TRUE)
summary(frequencies_route_overline)
#>    frequency           hour                geometry     
#>  Min.   :  1.00   Min.   : 0.00   LINESTRING   :138368  
#>  1st Qu.:  4.00   1st Qu.: 9.00   epsg:4326    :     0  
#>  Median :  7.00   Median :13.00   +proj=long...:     0  
#>  Mean   : 10.12   Mean   :13.03                         
#>  3rd Qu.: 13.00   3rd Qu.:18.00                         
#>  Max.   :121.00   Max.   :23.00
quantile(frequencies_route_overline$frequency)
#>   0%  25%  50%  75% 100% 
#>    1    4    7   13  121
```

#### Aggregated frequencies for 8 a.m.

``` r
mapview::mapview(
  frequencies_route |> filter(hour == 8 & frequency > 2),
  zcol = "frequency",
  layer.name = "Frequency (hour)"
)
```

#### Aggregated frequencies for routes overline for 8 a.m.

``` r
# above 2 per hour
mapview::mapview(
  frequencies_route_overline |> filter(
    hour == 8 &
      frequency > 2 # optional
  ),
  zcol = "frequency",
  # lwd = "frequency",
  layer.name = "Frequency (hour)"
)
```

### Improve visualization

Using the `overline` attribute in
[`GTFShift::get_route_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_route_frequency_hourly.md)
might not be the best option if the GTFS shapes do not share exactly the
same geometry. In those cases, the overlapping lines might not be merged
correctly, causing inconsistent results, such as a street with different
frequencies along it, despite not having any bus stop in between those
differences.

![Overline error example](figures/analyse_overline_error.png)

Overline error example

This is a [known issue](https://github.com/ropensci/stplanr/issues/575)
of
[`stplanr::overline2()`](https://docs.ropensci.org/stplanr/reference/overline.html),
the method used for the network aggregation that has not been solved
yet.

As an alternative, `GTFShift` provides some methods that allow to
overcome this issue, by correcting its geometry or aggregating the
network with open data.

#### Correcting geometry with OSM open data

GTFShift offers several methods that allow to get routes geometry from
OpenStreetMaps. Refer to
[vignette(“osm”)](https://u-shift.github.io/GTFShift/articles/osm.html#get-osm-bus-routes)
for more details.

#### Aggregating the network with OSM open data

There are several methods to aggregate a transit network. One approach
is through the determination of the centerlines of the roads where the
vehicles operate. GTFShift provides a method that encapsulates Python
[neatnet](https://uscuni.org/neatnet/index.html) package for this
purpose. Refer to
[vignette(“osm”)](https://u-shift.github.io/GTFShift/articles/osm.html#get-centerlines-for-osm-road-network)
for more details.

> During the development of this project, no R packages were found
> suiting this purpose.
> [Centerline](https://github.com/atsyplenkov/centerline) package has
> this feature in its roadmap. Currently, there are available solutions
> for [Python](https://uscuni.org/neatnet/index.html) or
> [ArcGis](https://pro.arcgis.com/en/pro-app/latest/tool-reference/cartography/merge-divided-roads.htm).

### Aggregating frequencies over a target network

As an alternative to the
[`GTFShift::get_route_frequency_hourly()`](https://u-shift.github.io/GTFShift/reference/get_route_frequency_hourly.md)
method using the `overline=TRUE` parameter,
[`GTFShift::network_overline()`](https://u-shift.github.io/GTFShift/reference/network_overline.md)
provides a different frequency aggregation functionality.

Given a target network, it identifies the segments corresponding to each
route and uses them to aggregate the attribute defined in the
parameters.

Below is provided an example, that uses the centerlines for the Carris
network as a target network, generated using
[ArcGis](https://pro.arcgis.com/en/pro-app/latest/tool-reference/cartography/merge-divided-roads.htm).
GTFShift provides
[`GTFShift::osm_centerlines()`](https://u-shift.github.io/GTFShift/reference/osm_centerlines.md)
method to generate this kind of network from OSM data. Refer to
[vignette(“osm”)](https://u-shift.github.io/GTFShift/articles/osm.html#get-centerlines-for-osm-road-network)
for more details.

``` r
network = sf::st_read(
  system.file("extdata", "centerline_carris.gpkg", package = "GTFShift"), 
  quiet = TRUE
)

frequencies_route_overline_improved = GTFShift::network_overline(
  network, 
  frequencies_route |> filter(hour == 8),
  attr = "frequency"
)

quantile(frequencies_route_overline_improved$frequency)
#>   0%  25%  50%  75% 100% 
#>    1    6   11   20  121
```

``` r
mapview::mapview(
  frequencies_route_overline_improved |> filter(frequency > 20),
  zcol = "frequency",
  layer.name = "Frequency (hour)"
)
```
