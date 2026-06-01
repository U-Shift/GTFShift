# Generate GTFS with OSM geometries

## Introduction

The match between GTFS shapes and OSM routes geometries using either
[`GTFShift::osm_shapes_match_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_match_routes.md)
or
[`GTFShift:: osm_shapes_to_routes()`](https://u-shift.github.io/GTFShift/reference/osm_shapes_to_routes.md)
enables the retrieval of harmonized geometries for the GTFS network.
However, OSM geometries are often MULTILINESTRING objects. In contrast,
GTFS shapes.txt file requires a sequence of points to define each shape.

[`GTFShift::create_shapes_from_sf()`](https://u-shift.github.io/GTFShift/reference/create_shapes_from_sf.md)
can be used to convert the MULTILINESTRING objects to sorted LINESTRING
objects.

## Example

``` r

# Get GTFS from library GTFS database for Portugal
data <- read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_id <- "lisboa"
gtfs <- GTFShift::load_feed(data$URL[data$ID == gtfs_id], create_transfers = FALSE)

# Build OSM query
library(osmdata)
q <- opq("Lisbon") |>
    add_osm_feature(key = "route", value = c("bus", "tram")) |>
    add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)

# Get OSM geometries
shapes_geometry_osm <- GTFShift::osm_shapes_to_routes(gtfs, q)

# Filter GTFS for shapes (mind that OSM might not have a match for all shapes)
osm_shape_ids <- unique(shapes_geometry_osm$shape_id)
gtfs_trips_with_osm_geometry <- gtfs$trips |>
    filter(shape_id %in% osm_shape_ids) |>
    pull(trip_id) |>
    unique()
gtfs <- tidytransit::filter_feed_by_trips(gtfs, trip_ids = gtfs_trips_with_osm_geometry)

# Convert OSM geometries to LINESTRING objects
shapes_osm <- GTFShift::create_shapes_from_sf(shapes_geometry_osm, gtfs)

# Update GTFS shapes with OSM geometries
gtfs$shapes <- shapes_osm

# Write GTFS to file
tidytransit::write_gtfs(gtfs, "gtfs_osm.zip")
```
