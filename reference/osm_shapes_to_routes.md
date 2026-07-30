# Get OSM routes geometry considering gtfs:shape_id match

Get OSM routes geometry considering gtfs:shape_id match

## Usage

``` r
osm_shapes_to_routes(
  gtfs,
  q,
  ways = FALSE,
  ways_tags = c("lanes", "psv", "bus", "way", "parking", "name"),
  osm_file = NULL,
  osm_route_type = "bus"
)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- q:

  osmdata::opq. Overpass query for transit network.

- ways:

  boolean (Default False). If true, relation is disaggregated in ways.

- ways_tags:

  character vector (Default
  `c("lanes", "psv", "bus", "way", "parking", "name")`). List of OSM way
  tags to extract when `ways` parameter is set to true. Match is done
  using
  [`tidyselect::contains()`](https://tidyselect.r-lib.org/reference/starts_with.html).

- osm_file:

  character (Optional). Location of OSM extract file with `osm.pbf`
  format. Refer to
  [`osmextract::oe_download()`](https://docs.ropensci.org/osmextract/reference/oe_download.html)
  for more details. If not provided OSM Overpass API is called through
  [`osmdata::osmdata_sf()`](https://docs.ropensci.org/osmdata/reference/osmdata_sf.html).

- osm_route_type:

  character (Default "bus"). OSM route type. Used to query OSM network
  (e.g., 'bus', 'train').

## Value

sf data.frame. Matched shape to route geometries with the following
columns:

- shape_id:

  The `shape_id` attribute from `shapes.txt` file.

- osm_id:

  The `osm_id` attribute from OSM route relation.

- way_osm_id:

  The `osm_id` attribute from OSM way (if `ways` parameter is set to
  true).

- \*:

  Any column that matches `ways_tags` parameter.

- geometry:

  The geometrical data for the OSM route relation.

Shapes that do not have a match on OSM are ignored. If that occurs, a
warning is displayed during the method execution, informing about the
missing geometries.

## Details

For each route, matches its trips' shapes with OSM route relations,
considering the OSM `gtfs:shape_id` attribute.

## Examples

``` r
# Subset GTFS for one route only, for demo purposes
gtfs <- GTFShift::load_feed(system.file("extdata/samples",
  "gtfs_tcb_sample.zip", package = "GTFShift")
)
gtfs <- GTFShift::filter_by_route_name(gtfs, c("1", "2", "3", "4"))

# Build query and prepare osm extract (possible to use API as alternative)
q <- osmdata::opq(bbox = sf::st_bbox(tidytransit::shapes_as_sf(gtfs$shapes))) |>
  osmdata::add_osm_feature(key = "route", value = "bus") |>
  osmdata::add_osm_feature(key = "operator", value = "Transportes Colectivos do Barreiro")
osm_file <- system.file("extdata/samples", "osmextract_tcb_network.pbf", package = "GTFShift")

# Get OSM route geometries based on gtfs:shape_id match
shapes_osm_routes <- GTFShift::osm_shapes_to_routes(
  gtfs, q,
  osm_file = osm_file
)
#> Matched 12 shapes (100.00% of 12 in GTFS) of 12 routes (100.00% of 12 in GTFS) with OSM routes!

head(shapes_osm_routes |> dplyr::select(shape_id, osm_id))
#> Simple feature collection with 6 features and 2 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.081368 ymin: 38.62355 xmax: -9.027701 ymax: 38.66264
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 3
#>   shape_id     osm_id                                                   geometry
#>   <chr>        <chr>                                       <MULTILINESTRING [°]>
#> 1 3-SA-TERM_R2 18958058 ((-9.031332 38.62453, -9.031376 38.62458, -9.031686 38.…
#> 2 3-TER-CS_CAS 18970565 ((-9.078088 38.65212, -9.078097 38.65209, -9.078101 38.…
#> 3 3-TER-SA_CAS 18970569 ((-9.078088 38.65212, -9.078097 38.65209, -9.078101 38.…
#> 4 3-TERM-SA_R2 18958059 ((-9.078088 38.65212, -9.078097 38.65209, -9.078101 38.…
#> 5 3-TERM-SA_LC 18958057 ((-9.078088 38.65212, -9.078097 38.65209, -9.078101 38.…
#> 6 1-QVBB-TERM  18957439 ((-9.050197 38.66117, -9.049968 38.66106, -9.049801 38.…

nrow(shapes_osm_routes)
#> [1] 12

# Get OSM ways instead
shapes_osm_ways <- GTFShift::osm_shapes_to_routes(
  gtfs, q,
  osm_file = osm_file,
  ways = TRUE
)
#> Matched 12 shapes (100.00% of 12 in GTFS) of 12 routes (100.00% of 12 in GTFS) with OSM routes!

head(shapes_osm_ways |> dplyr::select(way_osm_id, shape_id, osm_id))
#> Simple feature collection with 6 features and 3 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -9.036197 ymin: 38.62453 xmax: -9.031332 ymax: 38.63184
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 4
#>   way_osm_id shape_id     osm_id                                        geometry
#>   <chr>      <chr>        <chr>                                 <LINESTRING [°]>
#> 1 1375708225 3-SA-TERM_R2 18958058 (-9.031332 38.62453, -9.031376 38.62458, -9.…
#> 2 1309504742 3-SA-TERM_R2 18958058 (-9.031686 38.62495, -9.031714 38.62498, -9.…
#> 3 1309504715 3-SA-TERM_R2 18958058 (-9.031871 38.62508, -9.031965 38.62512, -9.…
#> 4 680803363  3-SA-TERM_R2 18958058 (-9.03533 38.62817, -9.035276 38.62812, -9.0…
#> 5 980761035  3-SA-TERM_R2 18958058 (-9.03533 38.62817, -9.0353 38.62827, -9.035…
#> 6 75489574   3-SA-TERM_R2 18958058 (-9.035202 38.62859, -9.035139 38.62874, -9.…

nrow(shapes_osm_ways)
#> [1] 1247
```
