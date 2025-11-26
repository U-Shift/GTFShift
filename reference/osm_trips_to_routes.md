# Get OSM routes geometry considering gtfs:trip_id match

Get OSM routes geometry considering gtfs:trip_id match

## Usage

``` r
osm_trips_to_routes(gtfs, q)
```

## Arguments

- gtfs:

  tidygtfs. GTFS feed.

- q:

  osmdata::opq. Overpass query for transit network.

## Value

A `sf` `data.frame` with the following columns:

- `trip_id`, the `trip_id` attribute from `trips.txt` file.

- `osm_id`, the `osm_id` attribute from OSM route relation.

- `geometry`, the geometrical data for the OSM route relation.

## Details

For each route, matches its trips with OSM route relations, considering
the OSM `gtfs:trip_id` attribute.

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- GTFShift::load_feed("gtfs.zip")

q = opq("Lisbon")  |>
  add_osm_feature(key = "route", value = c("bus", "tram")) |>
  add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

trips_geometry_osm = GTFShift::osm_trips_to_routes(gtfs, q)
} # }
```
