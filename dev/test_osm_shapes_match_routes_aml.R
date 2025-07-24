library(GTFShift)
library(dplyr)

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
# For historical versions, refer to https://mobilitydatabase.org/feeds/gtfs/mdb-2027
gtfs_aml = GTFShift::load_feed(data$URL[data$ID == "AML"], create_transfers=FALSE)

gtfs_aml_shapes = tidytransit::shapes_as_sf(gtfs_aml$shapes)
bbox_aml = sf::st_bbox(gtfs_aml_shapes)

 # Build OSM query
library(osmdata)
q_aml = opq(bbox_aml)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "network", value = "Carris Metropolitana", key_exact = TRUE)

# Match shapes geometry
shapes_aml_match_routes = GTFShift::osm_shapes_match_routes(gtfs_aml, q_aml, log_file = "releases/shapes_match_aml_v0_7_1_run20250723_gtfs20250619.r.log")

nrow(shapes_aml_match_routes)
summary(shapes_aml_match_routes)

total = nrow(shapes_aml_match_routes) # 1484
total
total = length(unique(gtfs_aml$shapes$shape_id)) # 1575
total

# View(shapes_aml_match_routes |> sf::st_drop_geometry())

valid = nrow(shapes_aml_match_routes |> filter(distance_diff<1000 & points_diff<500)) # 1198
valid
valid/total*100 # 72.69 %

write.csv(shapes_aml_match_routes |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff),
  shape_id_original = shape_id,
  shape_id = substr(shape_id, 1, nchar(shape_id) - 6) # Remove the last 6 characters (different in each GTFS feed version :/)
), "releases/shapes_match_aml_v0_7_1_run20250723_gtfs20250619.csv", row.names = FALSE)
sf::st_write(shapes_aml_match_routes, "dev/shapes_match_aml_v0_7_1_run20250723_gtfs20250619.gpkg")

# To compare with previous
shapes_aml_match_routes_previous = read.csv("dev/shapes_match_aml_v0_7_0_20250716.gpkg")
diff = shapes_aml_match_routes |>
  sf::st_drop_geometry() |>
  select(shape_id, osm_id) |>
  left_join(shapes_aml_match_routes_previous |> sf::st_drop_geometry() |> select(shape_id, osm_id), by="shape_id") |>
  mutate(diff = ifelse(is.na(osm_id.y), "new", ifelse(is.na(osm_id.x), "removed", ifelse(osm_id.x == osm_id.y, "same", "different")))) |>
  filter(diff != "same")

nrow(diff)

diff

quartiles_stops_before = quantile(shapes_aml_match_routes_previous$stops_diff, probs = c(0.25, 0.5, 0.75, 0.9, 0.95))
quartiles_stops_before
quartiles_stops_after = quantile(shapes_aml_match_routes$stops_diff, probs = c(0.25, 0.5, 0.75, 0.9, 0.95))
quartiles_stops_after

routes_before = unique(shapes_aml_match_routes_previous$route_short_name)
length(routes_before)
routes_after = unique(shapes_aml_match_routes$route_short_name)
length(routes_after)

valid_before = nrow(shapes_aml_match_routes_previous |> filter(distance_diff<1000 & points_diff<500))
valid_before
