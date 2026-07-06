library(sf)
library(gtfstools)
library(dplyr)
library(GTFShift)
library(mapview)

shape_id_debug = "sjr4"

gtfs <- load_feed("../GTFShift-web/scripts/osm_gtfs/aml_barreiro_cascais_lisboa/run_20260526_102424/gtfs_cascais.zip")
sf_shapes_original <- tidytransit::shapes_as_sf(gtfs$shapes)
summary(gtfs)
# mapview(sf_shapes_original |> filter(shape_id==shape_id_debug))

sf_shapes <- sf::st_read("../GTFShift-web/scripts/osm_match/cascais/gtfs_20260507/run_20260507_113820/shapes_match_cascais_gtfs20260507_run20260507.gpkg")
summary(sf_shapes)
mapview(sf_shapes |> filter(shape_id==shape_id_debug))

sf_shapes |> distinct(shape_id, .keep_all=TRUE) |> filter(shape_id==shape_id_debug)



gtfs_osm <- load_feed("../GTFShift-web/scripts/osm_gtfs/aml_barreiro_cascais_lisboa/run_20260526_102424/gtfs_cascais_osm.zip")
summary(gtfs_osm)
sf_shapes_osm <- tidytransit::shapes_as_sf(gtfs_osm$shapes)

mapview::mapview(sf_shapes_osm, zcol = "shape_id")

shapes_gtfstools
shapes_gtfstools_sf <- tidytransit::shapes_as_sf(shapes_gtfstools)
shapes_gtfstools_sf
mapview(shapes_gtfstools_sf, zcol="shape_id")

bbox <- sf::st_bbox(sf_shapes_original <- tidytransit::shapes_as_sf(gtfs$shapes))
mapview::mapview(bbox)
library(osmdata)
q <- opq(bbox = bbox) |>
  add_osm_feature(key = "route", value = "bus", key_exact = TRUE) |>
  add_osm_feature(key = "operator", value = c("TCB", "Transportes Colectivos do Barreiro"), key_exact = TRUE)
  