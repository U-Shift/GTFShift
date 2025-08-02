library(GTFShift)
library(dplyr)

output = "releases/v0_7_0/shapes_match_aml_gtfs20250619_run20250801"

data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
# For historical versions, refer to https://mobilitydatabase.org/feeds/gtfs/mdb-2027

# gtfs_aml = GTFShift::load_feed(data$URL[data$ID == "AML"], create_transfers=FALSE)
gtfs_aml = GTFShift::load_feed("https://files.mobilitydatabase.org/mdb-2027/mdb-2027-202506190444/mdb-2027-202506190444.zip")
tidytransit::write_gtfs(gtfs_aml, "releases/v0_7_0/gtfs_aml_20250619.zip")

total_network_aml = length(unique(gtfs_aml$routes$route_short_name))
total_network_aml # 685

gtfs_aml_shapes = tidytransit::shapes_as_sf(gtfs_aml$shapes)
bbox_aml = sf::st_bbox(gtfs_aml_shapes)

 # Build OSM query
library(osmdata)
q_aml = opq(bbox_aml)  |>
  add_osm_feature(key = "route", value = c("bus")) |>
  add_osm_feature(key = "network", value = "Carris Metropolitana", key_exact = TRUE)

# Match shapes geometry
shapes_aml_match_routes = GTFShift::osm_shapes_match_routes(gtfs_aml, q_aml, log_file = sprintf("%s.r.log", output))
summary(shapes_aml_match_routes)
# View(shapes_aml_match_routes |> sf::st_drop_geometry())

total_shapes = length(unique(gtfs_aml$shapes$shape_id))
total_shapes # 1575

matches_shapes = nrow(shapes_aml_match_routes)
matches_shapes # 1409
matches_shapes/total_shapes*100 # 89.46

valid_shapes = nrow(shapes_aml_match_routes |> filter(distance_diff<1000 & points_diff<500))
valid_shapes # 1090
valid_shapes/total_shapes*100 # 69.20

total_routes = length(unique(gtfs_aml$routes$route_id))
total_routes # 913

matches_routes = length(unique(shapes_aml_match_routes$route_id))
matches_routes # 818
matches_routes/total_routes*100 # 89.6

write.csv(shapes_aml_match_routes |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff),
  shape_id_original = shape_id,
  shape_id = substr(shape_id, 1, nchar(shape_id) - 6) # Remove the last 6 characters (different in each GTFS feed version :/)
), sprintf("%s.csv", output), row.names = FALSE)
sf::st_write(shapes_aml_match_routes, sprintf("%s.gpkg", output))


# Extra: To compare with previous

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


# Extra: Merge two results
nrow(shapes_aml_match_routes)

gtfs_1 = shapes_aml_match_routes |> mutate(shape_id_original = shape_id, shape_id = substr(shape_id, 1, nchar(shape_id) - 6))
nrow(gtfs_1)
length(unique(gtfs_1$route_short_name))

# shapes_aml_match_routes = read.csv("...")
gtfs_2 = shapes_aml_match_routes |> mutate(shape_id_original = shape_id, shape_id = substr(shape_id, 1, nchar(shape_id) - 6))
nrow(gtfs_2)
length(unique(gtfs_2$route_short_name))

shapes_aml_match_routes_aggregated = bind_rows(
  gtfs_2, # GTFS 20250728
  gtfs_1 |> filter(!(shape_id %in% gtfs_2$shape_id)) # GTFS 20250619
)
nrow(shapes_aml_match_routes_aggregated)
class(shapes_aml_match_routes_aggregated)
View(shapes_aml_match_routes_aggregated|>sf::st_drop_geometry())
length(unique(shapes_aml_match_routes_aggregated$route_short_name))

output_aggregated = "releases/v0_7_0/shapes_match_aml_gtfs20250619&20250801_run20250801"
write.csv(shapes_aml_match_routes_aggregated |> sf::st_drop_geometry() |> mutate(
  distance_diff=round(distance_diff),
  points_diff=round(points_diff)
), sprintf("%s.csv", output_aggregated), row.names = FALSE)
sf::st_write(shapes_aml_match_routes, sprintf("%s.gpkg", output_aggregated))
