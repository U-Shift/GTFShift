# Get centerlines for OSM road network

Get centerlines for OSM road network

## Usage

``` r
osm_centerlines(
  bbox = NULL,
  place = NULL,
  osm_file = NULL,
  use_buildings = TRUE,
  venv = NA
)
```

## Arguments

- bbox:

  bbox (Optional, if place provided). Area from which to export bus
  lanes.

- place:

  String (Optional, if bbox provided). Place from which to export bus
  lanes.

- osm_file:

  String (Optional). Path to a local OpenStreetMap PBF file (\`.pbf\`).

- use_buildings:

  Boolean (Default TRUE). Uses buildings from OSM as exclusion_mask for
  neatnet.

- venv:

  String (Default creates a new one). Python environment where neatnet
  will run.

## Value

sf data.frame. OSM centerlines.

## Details

Exports road network from OpenStreetMaps for given area and uses Python
[neatnet](https://uscuni.org/neatnet/) package to compute its
centerlines.

One of `bbox`, `place`, or `osm_file` must be provided.

Parameter `use_buildings` exports building footprints from OSM for
better results on the network simplification process.

This method was adapted from
[uscuni.org/neatnet](https://uscuni.org/neatnet/intro.html) by [Miguel
Relvas Pires](https://github.com/miguelrelvaspires) in the scope of his
[master's
thesis](https://scholar.tecnico.ulisboa.pt/records/DhKWeFU5YLpMDcOhQbKR4f7ul05HCQnZr7ND).
The full code (Python) of his work is openly available at
[GitHub](https://github.com/U-Shift/lp_streets).

## Author

[Miguel Relvas Pires](https://github.com/miguelrelvaspires)

## Examples

``` r
# Get sample OSM extract
osm_file <- system.file("extdata/samples", "relation_6384187.pbf", package = "GTFShift")

network <- GTFShift::osm_centerlines(
  place = "Arroios, Lisboa, Portugal",
  osm_file = osm_file
)

head(network)
#> Simple feature collection with 6 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.148239 ymin: 38.72801 xmax: -9.143884 ymax: 38.73345
#> Geodetic CRS:  WGS 84
#>   X_status                           geom
#> 1  changed LINESTRING (-9.148239 38.72...
#> 2  changed LINESTRING (-9.145421 38.73...
#> 3  changed LINESTRING (-9.144909 38.73...
#> 4  changed LINESTRING (-9.144937 38.73...
#> 5  changed LINESTRING (-9.145914 38.72...
#> 6  changed LINESTRING (-9.144386 38.73...

table(network$X_status)
#> 
#>  changed      new original 
#>       65       36      374 
```
