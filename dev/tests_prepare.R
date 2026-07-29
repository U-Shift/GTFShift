library(dplyr)

# Sample GTFS -----------------------------------------------

TABLES <- c("agency", "routes", "trips", "shapes", "calendar", "calendar_dates", "stops", "stop_times")
simplify_gtfs <- function(gtfs) {
  # Remove tables not in TABLES
  gtfs[!names(gtfs) %in% TABLES] <- NULL
  tidytransit::as_tidygtfs(gtfs)
}

# Prepare sample GTFS with enough twists to make tests intersting :)
gtfs_tcb <- tidytransit::read_gtfs("https://backend.tcbarreiro.pt/download-gtfs")
summary(gtfs_tcb)
names(gtfs_tcb)

# View(gtfs_tcb$stops)
# View(gtfs_tcb$stop_times)

sample_safe <- function(x, n) {
  if (length(x) == 0) {
    return(c())
  }
  sample(x, min(n, length(x)))
}

# Get stops with parent_id
stops_interesting <- gtfs_tcb$stops[!is.na(gtfs_tcb$stops$parent_station), ]$stop_id
trips_with_stops_interesting <- gtfs_tcb$stop_times |>
  filter(stop_id %in% stops_interesting) |>
  pull(trip_id) |>
  unique() |>
  sample_safe(5)

# Get stop_times with departure_time > 24:00:00
trips_with_stop_times_interesting <- gtfs_tcb$stop_times |>
  mutate(departure_time_hh = as.numeric(substr(departure_time, 1, 2))) |>
  filter(departure_time_hh >= 24) |>
  pull(trip_id) |>
  unique() |>
  sample_safe(5)

# Get one trip per each 3 hours block
trips_per_3_hours <- c()
for (hour in seq(0, 21, by = 3)) {
  trips_per_3_hours <- c(
    trips_per_3_hours,
    gtfs_tcb$stop_times |>
      mutate(departure_time_hh = as.numeric(substr(departure_time, 1, 2))) |>
      filter(departure_time_hh >= hour & departure_time_hh < hour + 3) |>
      pull(trip_id) |>
      unique() |>
      sample_safe(1)
  )
}

# Get week days and weekends
service_ids <- unique(gtfs_tcb$calendar$service_id)

trips_interesting <- unique(c(
  trips_with_stops_interesting,
  trips_with_stop_times_interesting,
  trips_per_3_hours,
  # Select 5 random trips for each service_id
  service_ids |>
    lapply(function(sid) {
      gtfs_tcb$trips |>
        filter(service_id == sid) |>
        pull(trip_id) |>
        unique() |>
        sample_safe(5)
    }) |>
    unlist()
))
length(trips_interesting)

gtfs_tcb_filtered <- tidytransit::filter_feed_by_trips(gtfs_tcb, trips_interesting)
names(gtfs_tcb_filtered)
gtfs_tcb_filtered <- simplify_gtfs(gtfs_tcb_filtered)
summary(gtfs_tcb_filtered)
names(gtfs_tcb_filtered)

# Merge with TTSL to cover multiple modes
gtfs_ttsl <- tidytransit::read_gtfs("https://api.transtejo.pt/files/GTFS.zip")
summary(gtfs_ttsl)

# Get only Seixal trips
trips_seixal <- gtfs_ttsl$trips |>
  filter(route_id == "3_0") |>
  pull(trip_id)

gtfs_ttsl_filtered <- tidytransit::filter_feed_by_trips(gtfs_ttsl, trips_seixal)
names(gtfs_ttsl_filtered)
gtfs_ttsl_filtered <- simplify_gtfs(gtfs_ttsl_filtered)
names(gtfs_ttsl_filtered)
summary(gtfs_ttsl_filtered)

# Remove shapes to create sample for load_feed
gtfs_ttsl_filtered_no_shapes <- gtfs_ttsl_filtered[!names(gtfs_ttsl_filtered) %in% "shapes"]
gtfs_ttsl_filtered_no_shapes <- tidytransit::as_tidygtfs(gtfs_ttsl_filtered_no_shapes)
names(gtfs_ttsl_filtered_no_shapes)
summary(gtfs_ttsl_filtered_no_shapes)
tidytransit::write_gtfs(gtfs_ttsl_filtered_no_shapes, "inst/extdata/gtfs_ttsl_sample_no_shapes.zip")

# For each table at ttsl, make sure tcb only has the same columns (remove from either when not in both, keeping parent_station if present in either)
names(gtfs_ttsl_filtered$stops)
names(gtfs_tcb_filtered$stops)
for (table in names(gtfs_ttsl_filtered)) {
  if (table %in% names(gtfs_tcb_filtered)) {
    common_cols <- intersect(names(gtfs_ttsl_filtered[[table]]), names(gtfs_tcb_filtered[[table]]))
    if ("parent_station" %in% names(gtfs_ttsl_filtered[[table]]) || "parent_station" %in% names(gtfs_tcb_filtered[[table]])) {
      common_cols <- unique(c(common_cols, "parent_station"))
    }
    gtfs_ttsl_filtered[[table]] <- gtfs_ttsl_filtered[[table]][, intersect(common_cols, names(gtfs_ttsl_filtered[[table]]))]
    gtfs_tcb_filtered[[table]] <- gtfs_tcb_filtered[[table]][, intersect(common_cols, names(gtfs_tcb_filtered[[table]]))]
  }
}
names(gtfs_ttsl_filtered$stops)
names(gtfs_tcb_filtered$stops)

# Merge GTFS
gtfs_tcb_filtered_simpler <- tidytransit::filter_feed_by_trips(gtfs_tcb_filtered, trips_interesting[1])
summary(gtfs_tcb_filtered_simpler)
gtfs_merged <- unify(gtfs_tcb_filtered_simpler, gtfs_ttsl_filtered) # , prefix = TRUE)

# Store samples to extdara
tidytransit::write_gtfs(gtfs_tcb_filtered, "inst/extdata/gtfs_tcb_sample.zip")
tidytransit::write_gtfs(gtfs_merged, "inst/extdata/gtfs_merged_sample.zip")

# Sample OSM -----------------------------------------------

## Filter TCB relations directly from the PBF --------------------------------
bash = """
# 1. Filter ALL bus routes
osmium tags-filter portugal-latest.osm.pbf r/route=bus -o all_buses.pbf --overwrite

# 2. Filter ONLY TCB networks from those bus routes (using exact string matching)
osmium tags-filter all_buses.pbf \
  r/network=TCB \
  r/network="Transportes Coletivos do Barreiro" \
  r/network="Transportes Colectivos do Barreiro" \
  r/operator=TCB \
  r/operator="Transportes Coletivos do Barreiro" \
  -o tcb_relations.pbf --overwrite

# Generate the recursive members file directly from the full Portugal PBF
osmium getid -r -t -I tcb_relations.pbf portugal-latest.osm.pbf -o osmextract_tcb_network.pbf --overwrite

# Generate gpkg to validate 
ogr2ogr -f GPKG osmextract_tcb_network.gpkg osmextract_tcb_network.pbf
"""

## Filter TCB relations layers to gpkg --------------------------------
OSM_EXPORT_GPKG = "~/.local/share/R/osmextract/osmextract_tcb_network.gpkg"
sf::st_layers(OSM_EXPORT_GPKG)
ways = sf::st_read(OSM_EXPORT_GPKG, layer="lines")
mapview::mapview(ways)
View(ways)
sf::st_write(ways |> dplyr::select(osm_id), "inst/extdata/osm_ways_tcb.gpkg")

routes = sf::st_read(OSM_EXPORT_GPKG, layer="multilinestrings")
routes$shape_id <- ifelse(grepl('"gtfs:shape_id"=>"', routes$other_tags), sub('.*"gtfs:shape_id"=>"([^"]+)".*', '\\1', routes$other_tags), NA_character_)
routes$route_id <- ifelse(grepl('"gtfs:route_id"=>"', routes$other_tags), sub('.*"gtfs:route_id"=>"([^"]+)".*', '\\1', routes$other_tags), NA_character_)
# route_id starts with 1_, 2_, 3_ or 4_ 
routes = routes |> filter(grepl("^[1-4]_", route_id))
mapview::mapview(routes)
View(routes |> sf::st_drop_geometry())
sf::st_write(routes |> dplyr::select(osm_id, shape_id, route_id), "inst/extdata/samples/osm_routes_tcb.gpkg", delete_dns = TRUE)

## Filter Lisbon highways --------------------------------
census_aml = sf::st_read("https://github.com/U-Shift/MQAT/raw/refs/heads/main/data/census.gpkg", quiet = TRUE)
names(census_aml)
# Filter by those that have UID starting by 
# 110657 (Avenidas Novas), 110654 (Alvalade) and 110655 (Areeiro)
census_aml = census_aml |> 
  mutate(UID = as.character(UID)) |> 
  filter(grepl("^(110657|110654|110655)", UID)) |>
  sf::st_union() |>
  sf::st_transform(4326)
mapview::mapview(census_aml)

sf::st_write(census_aml, "~/.local/share/R/osmextract/lisbon_bbox.geojson", delete_dns = TRUE)

"""
# Option B: Two-step process
# 1. Clip the full PBF to your GeoJSON boundary
osmium extract -p lisbon_bbox.geojson portugal-latest.osm.pbf -o lisbon_bbox_clip.pbf --overwrite

# 2. Extract all ways tagged with 'highway' (along with their nodes)
osmium tags-filter lisbon_bbox_clip.pbf w/highway=primary,secondary,tertiary -o lisbon_highways.pbf --overwrite
"""

## OSM for all elements inside relation 6384187 --------------------------------
"""
# 1. Extract relation 6384187 and its member ways to build the boundary polygon
osmium getid -r -t portugal-latest.osm.pbf r6384187 -o relation_boundary.pbf --overwrite

# 2. Convert relation boundary to GeoJSON polygon
ogr2ogr -f GeoJSON relation_boundary.geojson relation_boundary.pbf multipolygons

# 3. Clip full PBF to the relation geometry boundary (broad extract)
osmium extract -p relation_boundary.geojson portugal-latest.osm.pbf -o relation_area_all.pbf --overwrite

# 4. Filter to ONLY the OSM elements needed by osm_centerline_neatnet.py:
#    - Ways: ALL highway=* types (pyrosm needs the full network; Python filters types later)
#    - Ways/areas: building=* for exclusion mask (Step 2 in Python)
#    - Ways/areas: landuse=construction|cemetery for exclusion mask
#    - Ways/areas: amenity=school for exclusion mask
#    - Ways/areas: leisure=pitch for exclusion mask
#    Referenced nodes are included automatically by osmium tags-filter
osmium tags-filter relation_area_all.pbf \
  "w/highway" \
  "wa/building" \
  "wa/landuse=construction,cemetery" \
  "wa/amenity=school" \
  "wa/leisure=pitch" \
  -o relation_6384187_filtered.pbf --overwrite

# 5. Merge filtered elements with the relation boundary to include r6384187
#    relation_boundary.pbf (from step 1) already contains the relation + its members
osmium merge relation_6384187_filtered.pbf relation_boundary.pbf -o relation_6384187.pbf --overwrite

# 6. (Optional) Convert to GPKG for validation in R/GIS
ogr2ogr -f GPKG relation_6384187.gpkg relation_6384187.pbf
"""


