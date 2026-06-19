library(dplyr)
library(mapview)

gtfs = tidytransit::read_gtfs("https://publico.cp.pt/gtfs/gtfs.zip")
summary(gtfs)
gtfs$shapes


# https://github.com/lxparapessoas/rede-madrugada/blob/main/analysis.md
stations_lisbon = c(
  "94_61101", # Sintra
  "94_62042", # Meleças
  "94_59006", # Rossio
  "94_31039", # Lisboa Oriente
  "94_33001", # Azambuja
  "94_30007", # Lisboa SA
  "94_31310", # Castanheira Ribatejo
  "94_67025", # Alcântara-terra
  "94_69260", # Cascais
  "94_69179", # Oeiras
  "94_69005", # Cais do Sodré
  "94_95000", # Barreiro
  "94_91058" # Praias do Sado A
)
routes_lisbon = gtfs$routes %>%
  filter(grepl(paste(stations_lisbon, collapse = "|"), route_id) & (route_short_name == "U" | grepl("^Linha", route_short_name)))
trips_lisbon = gtfs$trips %>%
  filter(route_id %in% routes_lisbon$route_id)
gtfs = tidytransit::filter_feed_by_trips(gtfs, trip_ids = trips_lisbon$trip_id) 

summary(gtfs)

gtfs = GTFShift::create_shapes_from_stops(gtfs)
# View(gtfs$shapes)
gtfs_shapes_sf <- tidytransit::shapes_as_sf(gtfs$shapes)
mapview(gtfs_shapes_sf)

manipulate_gtfs_cp <- function(gtfs) {
  # Method to manipulate GTFS routes names, to enable match with OSM names
  # See https://github.com/U-Shift/GTFShift/issues/35 for more details

  # String replace service acronym in gtfs$routes$route_short_name by extended name
  # Example: "AP" by "Alfa Pendular",  "IC" by "Intercidades"
  gtfs$routes$route_short_name <- gsub("AP", "Alfa Pendular", gtfs$routes$route_short_name)
  gtfs$routes$route_short_name <- gsub("IC", "Intercidades", gtfs$routes$route_short_name)
  gtfs$routes$route_short_name <- gsub("IR", "InterR", gtfs$routes$route_short_name)
  gtfs$routes$route_short_name <- gsub("R", "Regional", gtfs$routes$route_short_name)
  gtfs$routes$route_short_name <- gsub("U", "Urbano", gtfs$routes$route_short_name)

  # Extend gtfs$routes$route_short_name with origin/destination station names
  gtfs$routes <- gtfs$routes |>
    mutate(
      from = str_split_fixed(route_id, "-", 3)[, 2],
      to = str_split_fixed(route_id, "-", 3)[, 3]
    ) |>
    left_join(gtfs$stops |> select(stop_id, stop_name) |> rename(from_name = stop_name), by = c("from" = "stop_id")) |>
    left_join(gtfs$stops |> select(stop_id, stop_name) |> rename(to_name = stop_name), by = c("to" = "stop_id")) |>
    mutate(route_short_name = sprintf("%s %s %s", route_short_name, from_name, to_name))

  return(gtfs)
}
gtfs <- manipulate_gtfs_cp(gtfs)

# Match shapes
bbox <- sf::st_bbox(gtfs_shapes_sf)
q <- opq(bbox = bbox) |>
  add_osm_feature(key = "route", value = "train", key_exact = TRUE) |>
  add_osm_feature(key = "operator", value = "Comboios de Portugal", key_exact = TRUE)
  
osm_match <- GTFShift::osm_shapes_match_routes(
  gtfs, 
  q,
  gtfs_match = "route_short_name",
  osm_match = "name",
  gtfs_osm_match_exact = FALSE,
  osm_route_type = "train"
)
