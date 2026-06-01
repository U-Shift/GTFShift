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

osm_lines in sf format

## Details

Exports roads tagged as designated bus lanes on OpenStreetMaps for given
area.

## Examples

``` r
if (FALSE) { # \dontrun{
BBOX <- sf::st_bbox(city_limit)

# To use OSM API:
bus_lanes <- GTFShift::osm_bus_lanes(BBOX)

# To use a local OSM file:
osm_file <- oe_download("https://download.geofabrik.de/europe/portugal-latest.osm.pbf")
bus_lanes <- GTFShift::osm_bus_lanes(BBOX, osm_file = osm_file)
} # }
```
