# Get centerlines for OSM road network

Get centerlines for OSM road network

## Usage

``` r
osm_centerlines(bbox = NULL, place = NULL, use_buildings = TRUE, venv = NA)
```

## Arguments

- bbox:

  bbox (Optional, if place provided). Area from which to export bus
  lanes.

- place:

  String (Optional, if bbox provided). Place from which to export bus
  lanes.

- use_buildings:

  Boolean (Default TRUE). Uses buildings from OSM as exclusion_mask for
  neatnet.

- venv:

  String (Default creates a new one). Python environment where neatnet
  will run.

## Value

osm_lines in sf format

## Details

Exports road network from OpenStreetMaps for given area and uses Python
[neatnet](https://uscuni.org/neatnet/) package to compute its
centerlines.

One of `bbox` or `place` must be provided. If both, `bbox` is
considered.

Parameter `use_buildings` exports building footprints from OSM for
better results on the network simplification process.

## Examples

``` r
if (FALSE) { # \dontrun{
BBOX = sf::st_bbox(city_limit)
network <- GTFShift::osm_centerlines(BBOX)
} # }
```
