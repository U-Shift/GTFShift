# 6. Get OSM data

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

## Get OSM data for bus routes

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

### Routes (matching GTFS id)

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
```

``` r
shapes_geometry_osm
#> Simple feature collection with 2 features and 2 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.133896 ymin: 38.70714 xmax: -9.099847 ymax: 38.76858
#> Geodetic CRS:  WGS 84
#>         shape_id   osm_id                           geom
#> 1 115_0_DESC_shp 15470712 MULTILINESTRING ((-9.09986 ...
#> 2  115_0_ASC_shp 15470713 MULTILINESTRING ((-9.13309 ...
```

``` r
# Get original shapes, for comparison
shapes_sf = tidytransit::shapes_as_sf(gtfs_794$shapes)
```

#### GTFS shapes

``` r
mapview::mapview(shapes_sf, zcol = "shape_id", legend = TRUE, layer.name="GTFS shapes")
```

#### OSM routes

``` r
mapview::mapview(shapes_geometry_osm, zcol = "shape_id", legend = TRUE, layer.name="OSM routes")
```

### Ways (matching GTFS id)

By setting the `ways` parameter to `TRUE`,
[`GTFShift::osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)
enables the retrieval of the OSM routes disaggregated by the individual
ways that compose them.

``` r
# Match shapes geometry disaggregated by ways
shapes_ways_osm = GTFShift::osm_shapes_to_routes(gtfs_794, q, ways=TRUE)
```

``` r
shapes_ways_osm |> select(shape_id, osm_id, way_osm_id, lanes)
#> Simple feature collection with 324 features and 4 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.133896 ymin: 38.70714 xmax: -9.099847 ymax: 38.76858
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>          shape_id   osm_id way_osm_id lanes                           geom
#> 1  115_0_DESC_shp 15470712  992933214  <NA> LINESTRING (-9.09986 38.768...
#> 2  115_0_DESC_shp 15470712 1011258338  <NA> LINESTRING (-9.099918 38.76...
#> 3  115_0_DESC_shp 15470712  999581059  <NA> LINESTRING (-9.099932 38.76...
#> 4  115_0_DESC_shp 15470712  992933213  <NA> LINESTRING (-9.100159 38.76...
#> 5  115_0_DESC_shp 15470712  999581060  <NA> LINESTRING (-9.100426 38.76...
#> 6  115_0_DESC_shp 15470712  999581062  <NA> LINESTRING (-9.100692 38.76...
#> 7  115_0_DESC_shp 15470712  999581061  <NA> LINESTRING (-9.100959 38.76...
#> 8  115_0_DESC_shp 15470712  990897632  <NA> LINESTRING (-9.101227 38.76...
#> 9  115_0_DESC_shp 15470712  232018440     2 LINESTRING (-9.101357 38.76...
#> 10 115_0_DESC_shp 15470712 1415272770     2 LINESTRING (-9.102562 38.76...
```

``` r
mapview::mapview(shapes_ways_osm, zcol = "way_osm_id", legend = FALSE, layer.name="OSM ways")
```

### Routes (matching shapes geometry)

Despite existing, OSM `gtfs:*` keys are not widely used. In July 2025,
only 3.1% of relations tagged as `route=bus` had the `gtfs:shape_id` key
set (9 784 of 312 049).

To overcome this issue,
[`GTFShift::osm_shapes_match_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_match_routes.md)
performs the association between the OSM bus routes and the GTFS shapes
considering a geometrical match. For each GTFS route identifier
(provided as a parameter), the function first selects the subset of OSM
route relations that match it. Then, for each GTFS shape associated with
the route, it identifies the most similar OSM route relation by
minimizing a cost function that combines the distance between start and
end points, total length, and number of stops.

> The match between the GTFS and OSM identifiers is strict by default,
> but parameter `gtfs_osm_match_exact` can be set to `FALSE` to allow a
> partial match (e.g., matching GTFS route `15E` with OSM routes
> `Carris/15E`, `15E-1`, etc.).

> GTFShift provides a test script that runs this method for several
> cities around the world at
> [dev/test_osm_shapes_match_routes_local.R](https://github.com/U-Shift/GTFShift/blob/main/dev/test_osm_shapes_match_routes_local.R).

``` r
# Subset feed for some routes only, for demonstration purposes
gtfs_subset = GTFShift::filter_by_route_name(gtfs, list("736", "750", "15E", "65B"))

# Match shapes geometry
shapes_match_routes = GTFShift::osm_shapes_match_routes(gtfs_subset, q)
#> > Found 14 GTFS shapes and 231 stops
#> > Found 300 OSM route relations and 4911 bus stops/platforms
#> > Associated 14 shapes (100.00% of 14 total) of 8 routes (100.00% of 8 total) with OSM routes, with a mean distance of 25.42 meters for points, 49.86 meters for route length and a mean difference of 0.43 stops
#> > Of those, 14 shapes (100.00% of 14 matched) have a distance difference below 1000 meters, a points difference below 500 meters
```

``` r
summary(shapes_match_routes)
#>    route_id           shape_id            osm_id          distance_diff   
#>  Length:14          Length:14          Length:14          Min.   :  5.22  
#>  Class :character   Class :character   Class :character   1st Qu.: 20.05  
#>  Mode  :character   Mode  :character   Mode  :character   Median : 31.36  
#>                                                           Mean   : 49.86  
#>                                                           3rd Qu.: 43.68  
#>                                                           Max.   :278.50  
#>   points_diff       stops_diff     route_short_name   route_long_name   
#>  Min.   : 7.851   Min.   :0.0000   Length:14          Length:14         
#>  1st Qu.:12.197   1st Qu.:0.0000   Class :character   Class :character  
#>  Median :22.718   Median :0.0000   Mode  :character   Mode  :character  
#>  Mean   :25.424   Mean   :0.4286                                        
#>  3rd Qu.:37.073   3rd Qu.:0.7500                                        
#>  Max.   :49.869   Max.   :3.0000                                        
#>    osm_name           osm_ref                       geom   
#>  Length:14          Length:14          MULTILINESTRING:14  
#>  Class :character   Class :character   epsg:4326      : 0  
#>  Mode  :character   Mode  :character   +proj=long...  : 0  
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
