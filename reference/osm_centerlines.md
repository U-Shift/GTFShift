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
#> Using Python: /usr/bin/python3.12
#> Creating virtual environment '~/.virtualenvs/r-reticulate' ... 
#> + /usr/bin/python3.12 -m venv /home/runner/.virtualenvs/r-reticulate
#> Done!
#> Installing packages: pip, wheel, setuptools
#> + /home/runner/.virtualenvs/r-reticulate/bin/python -m pip install --upgrade pip wheel setuptools
#> Installing packages: numpy
#> + /home/runner/.virtualenvs/r-reticulate/bin/python -m pip install --upgrade --no-user numpy
#> Virtual environment '~/.virtualenvs/r-reticulate' successfully created.
#> Error in use_python(python, required = required): failed to initialize requested version of Python

head(network)
#> Error: object 'network' not found

table(network$X_status)
#> Error: object 'network' not found
```
