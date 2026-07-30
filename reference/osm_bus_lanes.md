# Export designated bus lanes from OpenStreetMaps

Export designated bus lanes from OpenStreetMaps

## Usage

``` r
osm_bus_lanes(bbox, osm_file = NULL)
```

## Arguments

- bbox:

  bbox. Area from which to export bus lanes.

- osm_file:

  character (Optional). Location of OSM extract file with `osm.pbf`
  format. Refer to
  [`osmextract::oe_download()`](https://docs.ropensci.org/osmextract/reference/oe_download.html)
  for more details. If not provided OSM Overpass API is called through
  [`osmdata::osmdata_sf()`](https://docs.ropensci.org/osmdata/reference/osmdata_sf.html).

## Value

sf data.frame. OSM bus lanes.

## Details

Exports roads tagged as designated bus lanes on OpenStreetMaps for given
area.

## Examples

``` r
# Create bbox for Lisbon
bbox <- sf::st_as_sfc(sf::st_bbox(c(
  xmin = -9.229836, ymin = 38.691399, 
  xmax = -9.087387, ymax = 38.796760
), crs = 4326))

# Use sample osmextract for Lisbon highways
osm_file <- system.file(
  "extdata/samples", "osmextract_lisbon_highways_sample.pbf", package = "GTFShift"
)

# Export bus lanes
bus_lanes <- GTFShift::osm_bus_lanes(bbox, osm_file = osm_file)

names(bus_lanes)
#>  [1] "osm:id"             "name"               "highway"           
#>  [4] "waterway"           "aerialway"          "barrier"           
#>  [7] "man:made"           "railway"            "psv:lanes"         
#> [10] "psv:lanes:forward"  "psv:lanes:backward" "lanes:psv"         
#> [13] "lanes:psv:forward"  "psv"                "z:order"           
#> [16] "other:tags"         "geometry"          

head(bus_lanes |> dplyr::select(`osm:id`, name))
#> Simple feature collection with 6 features and 2 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.155793 ymin: 38.73346 xmax: -9.144997 ymax: 38.75853
#> Geodetic CRS:  WGS 84
#>     osm:id                    name                       geometry
#> 1 15144371            Campo Grande LINESTRING (-9.155793 38.75...
#> 2 19725271 Praça Duque de Saldanha LINESTRING (-9.144997 38.73...
#> 3 19725285    Avenida da República LINESTRING (-9.145092 38.73...
#> 4 19725393    Avenida da República LINESTRING (-9.146321 38.73...
#> 5 19725406    Avenida da República LINESTRING (-9.148015 38.74...
#> 6 19851261            Campo Grande LINESTRING (-9.152813 38.75...
```
