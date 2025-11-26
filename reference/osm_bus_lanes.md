# Export designated bus lanes from OpenStreetMaps

Export designated bus lanes from OpenStreetMaps

## Usage

``` r
osm_bus_lanes(bbox)
```

## Arguments

- bbox:

  bbox. Area from which to export bus lanes.

## Value

osm_lines in sf format

## Details

Exports roads tagged as designated bus lanes on OpenStreetMaps for given
area.

## Examples

``` r
if (FALSE) { # \dontrun{
BBOX = sf::st_bbox(city_limit)
bus_lanes <- GTFShift::osm_bus_lanes(BBOX)
} # }
```
