# 5. Get OSM data

``` r
library(GTFShift)
library(tidytransit)
library(dplyr)
```

## Introduction

OpenStreetMaps (OSM) is an important data source for transit analysis,
due to its rich, open, and detailed geographic data. GTFShift includes
some methods that allow to access its information directly.

## Download bus lanes

Dedicated bus lanes can improve bus transit operation. Understanding
their spatial distribution is important to study operation dynamics.
`osm_bus_lanes` allows to obtain the bus lanes network on OpenStreetMaps
for a given area.

``` r
aml = sf::st_read("https://github.com/U-Shift/MQAT/raw/refs/heads/main/geo/MUNICIPIOSgeo.gpkg", quiet = TRUE)
lisboa = aml |> dplyr::filter(Concelho == "Lisboa") |> sf::st_bbox()

bus_lanes = GTFShift::osm_bus_lanes(lisboa) |> select(osm_id)
```

``` r
mapview::mapview(bus_lanes, layer.name = "Bus lanes")
```

## Get centerlines for OSM road network

Performing an aggregated analysis for the spatial distribution of the
route frequencies might be easier if the routes are projected over a
simplified road network (refer to
[vignette(“analyse”)](https://u-shift.github.io/GTFShift/articles/analyse.html#improve-visualization-with-network_overline)
for more details).

[`GTFShift::osm_centerlines()`](https://u-shift.github.io/GTFShift/reference/osm_centerlines.md)
allows to generate this simplification by creating the centerlines for
the road network exported from OpenStreetMaps, using Python
[neatnet](https://uscuni.org/neatnet/) package.

#### Original network

``` r
library(osmdata)

road_osm = opq("Arroios, Lisboa, Portugal") |>
    add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", "tertiary", "residential", "unclassified", "living_street")) |>
    add_osm_feature(key = "area", value = "!yes") |>
    osmdata_sf() |>
    osm_poly2line()

road_osm = road_osm$osm_lines
```

``` r
mapview::mapview(road_osm)
```

#### Simplified network

``` r
centerlines = GTFShift::osm_centerlines(place="Arroios, Lisboa, Portugal")
#> Using Python: /usr/bin/python3.12
#> Creating virtual environment '~/.virtualenvs/r-reticulate' ...
#> Done!
#> Installing packages: pip, wheel, setuptools
#> Installing packages: numpy
#> Virtual environment '~/.virtualenvs/r-reticulate' successfully created.
#> Using virtual environment '/home/runner/.virtualenvs/r-reticulate' ...
```

``` r
mapview::mapview(centerlines)
```

## Get OSM bus routes

OpenStreetMaps defines bus routes as a
[relation](https://wiki.openstreetmap.org/wiki/Tag:route%3Dbus) of ways
(usually roads) and nodes (stops and platforms). GTFShift provides
methods to use them in the GTFS analysis.

> For demonstration purposes, the next snippets will use the Lisbon
> urban bus network.

``` r
# Get GTFS from library GTFS database for Portugal
data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_id = "lisboa"
gtfs = GTFShift::load_feed(data$URL[data$ID == gtfs_id], create_transfers=FALSE)

# Build OSM query
library(osmdata)
q = opq("Lisbon")  |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)
```

### Matching GTFS ids

GTFS routes shapes and OSM bus routes are linked through OSM `gtfs:*`
keys.
[`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)
and
[`GTFShift::osm_trips_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_trips_to_routes.md)
allow to query OSM for the routes matching the feed trips, given,
respectively, their shape or trip id.

``` r
# Subset feed for some routes only, for demonstration purposes
gtfs_794 = GTFShift::filter_by_route_name(gtfs, list("794"))

# Match shapes geometry
shapes_geometry_osm = GTFShift::osm_shapes_to_routes(gtfs_794, q)

shapes_geometry_osm
#> Simple feature collection with 2 features and 2 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.133896 ymin: 38.70714 xmax: -9.099847 ymax: 38.76858
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 3
#>   shape_id       osm_id                                                 geometry
#>   <chr>          <chr>                                     <MULTILINESTRING [°]>
#> 1 115_0_ASC_shp  15470713 ((-9.13309 38.70745, -9.133067 38.70747), (-9.133067 …
#> 2 115_0_DESC_shp 15470712 ((-9.09986 38.76819, -9.099875 38.76803, -9.099904 38…
```

#### GTFS shapes

``` r
shapes_sf = tidytransit::shapes_as_sf(gtfs_794$shapes)
mapview::mapview(shapes_sf, zcol = "shape_id", legend = TRUE, layer.name="GTFS shapes")
```

#### OSM routes

``` r
mapview::mapview(shapes_geometry_osm, zcol = "shape_id", legend = TRUE, layer.name="OSM routes")
```

### Matching shapes geometry

Despite existing, OSM `gtfs:*` keys are not widely used. In July 2025,
only 3.1% of relations tagged as `route=bus` had the `gtfs:shape_id` key
set (9 784 of 312 049).

To overcome this issue,
[`GTFShift::osm_shapes_match_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_match_routes.md)
performs the association between the OSM bus routes and the GTFS shapes
considering, for each route, the start and end points and total length.

> GTFShift provides a test script that runs this method for several
> cities around the world at
> [dev/test_osm_shapes_match_routes_local.R](https://github.com/U-Shift/GTFShift/blob/main/dev/test_osm_shapes_match_routes_local.R).

``` r
# Subset feed for some routes only, for demonstration purposes
gtfs_subset = GTFShift::filter_by_route_name(gtfs, list("736", "750", "15E", "65B"))

# Match shapes geometry
shapes_match_routes = GTFShift::osm_shapes_match_routes(gtfs_subset, q)
#> > Found 14 GTFS shapes and 229 stops
#> > Found 300 OSM route relations and 4910 bus stops/platforms
#> > Associated 14 shapes (100.00% of 14 total) of 8 routes (100.00% of 8 total) with OSM routes, with a mean distance of 25.42 meters for points, 32.28 meters for route length and a mean difference of 0.29 stops
#> > Of those, 14 shapes (100.00% of 14 matched) have a distance difference below 1000 meters, a points difference below 500 meters

summary(shapes_match_routes)
#>    route_id           shape_id            osm_id          distance_diff  
#>  Length:14          Length:14          Length:14          Min.   : 5.22  
#>  Class :character   Class :character   Class :character   1st Qu.:19.92  
#>  Mode  :character   Mode  :character   Mode  :character   Median :31.30  
#>                                                           Mean   :32.28  
#>                                                           3rd Qu.:39.84  
#>                                                           Max.   :73.03  
#>   points_diff       stops_diff     route_short_name   route_long_name   
#>  Min.   : 7.851   Min.   :0.0000   Length:14          Length:14         
#>  1st Qu.:12.197   1st Qu.:0.0000   Class :character   Class :character  
#>  Median :22.718   Median :0.0000   Mode  :character   Mode  :character  
#>  Mean   :25.424   Mean   :0.2857                                        
#>  3rd Qu.:37.073   3rd Qu.:0.7500                                        
#>  Max.   :49.869   Max.   :1.0000                                        
#>             geometry 
#>  MULTILINESTRING:14  
#>  epsg:4326      : 0  
#>  +proj=long...  : 0  
#>                      
#>                      
#> 

# Visualize results
shapes_match_routes$map_name = paste(
  shapes_match_routes$route_short_name,
  " | ",
  shapes_match_routes$shape_id, 
  " | ", 
  shapes_match_routes$osm_id
)
```

``` r
mapview::mapview(shapes_match_routes, zcol = "map_name", legend = TRUE, layer.name="route_short_name | shape_id | osm_id")
```

#### Updating OSM routes with shape_id

The association between OSM route relations id and the GTFS shapes_id
returned by
[`GTFShift::osm_shapes_match_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_match_routes.md)
can be used to update OpenStreetMaps data. Refer to [Extra. Update OSM
data](https://u-shift.github.io/GTFShift/articles/osm_update.md) for
more details.

#### Validating OSM routes

The warning logs displayed by
[`GTFShift::osm_shapes_match_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_match_routes.md)
can be used to validate the OSM routes, identifying shapes without a
match, or even OSM existing routes that violate OSM integrity rules,
such as having stops in the wrong order.
