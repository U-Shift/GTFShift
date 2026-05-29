library(GTFShift)
library(dplyr)
library(stringr)

library(osmdata)
get_overpass_url()
set_overpass_url("https://maps.mail.ru/osm/tools/overpass/api/interpreter")
set_overpass_url("https://overpass.private.coffee/api/interpreter") # 4 servers with 20 cores, 256GB RAM, SSD each
get_overpass_url()

# Parameters
output_root <- "releases/v0_8_2"

regions <- data.frame(
  name = character(),
  gtfs = character(),
  query = I(list())
)
data <- read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))

regions <- rbind( # AML
  regions,
  data.frame(
    name = "AML",
    # For historical versions, refer to https://mobilitydatabase.org/feeds/gtfs/mdb-2027
    gtfs_url = "https://files.mobilitydatabase.org/mdb-2027/mdb-2027-202506190444/mdb-2027-202506190444.zip",
    gtfs_day = "20250619",
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "Carris Metropolitana", key_exact = TRUE)
    )))
  )
)
regions <- rbind( # Barreiro
  regions,
  data.frame(
    name = "barreiro",
    gtfs_url = data$URL[data$ID == "barreiro"],
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "Transportes Coletivos do Barreiro", key_exact = TRUE)
    ))),
    geofabrik_region = "europe/portugal"
  )
)
regions <- rbind( # Braga
  regions,
  data.frame(
    name = "braga",
    gtfs_url = data$URL[data$ID == "braga"],
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "operator", value = "Transportes Urbanos de Braga", key_exact = TRUE)
    )))
  )
)
regions <- rbind( # Cascais
  regions,
  data.frame(
    name = "cascais",
    gtfs_url = data$URL[data$ID == "cascais"],
    gtfs_day = gsub("-", "", Sys.Date()),
    gtfs_manipulate = "manipulate_gtfs_cascais",
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "MobiCascais", key_exact = TRUE)
    )))
  )
)
regions <- rbind( # Funchal
  regions,
  data.frame(
    name = "funchal",
    gtfs_url = data$URL[data$ID == "funchal"],
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "operator", value = "HF", key_exact = TRUE)
    )))
  )
)
regions <- rbind( # Lagos
  regions,
  data.frame(
    name = "lagos",
    gtfs_url = data$URL[data$ID == "lagos"],
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "operator", value = "ONDA", key_exact = TRUE)
    )))
  )
)
regions <- rbind( # Lisboa
  regions,
  data.frame(
    name = "lisboa",
    gtfs_url = data$URL[data$ID == "lisboa"],
    gtfs_day = Sys.Date(),
    query = I(list(list(
      list(key = "route", value = c("bus", "tram"), key_exact = TRUE),
      list(key = "network", value = "Carris", key_exact = TRUE)
    ))),
    geofabrik_region = "europe/portugal"
  )
)
regions <- rbind( # Madrid
  regions,
  data.frame(
    name = "madrid",
    gtfs_url = data$URL[data$ID == "madrid"],
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "operator", value = "Empresa Municipal de Transportes de Madrid", key_exact = TRUE)
    )))
  )
)
regions <- rbind( # STCP
  regions,
  data.frame(
    name = "stcp",
    gtfs_url = data$URL[data$ID == "stcp"],
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "operator", value = "STCP", key_exact = TRUE)
    )))
  )
)
regions <- rbind( # Toulouse
  regions,
  data.frame(
    name = "toulouse",
    gtfs_url = "https://data.toulouse-metropole.fr/explore/dataset/tisseo-gtfs/files/fc1dda89077cf37e4f7521760e0ef4e9/download/",
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "Tisséo", key_exact = TRUE)
    ))),
    geofabrik_region = "europe/france/midi-pyrenees"
  )
)

regions <- rbind( # CP Portugal
  regions,
  data.frame(
    name = "cp_pt",
    gtfs_url = "https://publico.cp.pt/gtfs/gtfs.zip",
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("train"), key_exact = TRUE),
      list(key = "operator", value = "Comboios de Portugal", key_exact = TRUE)
    ))),
    gtfs_match = "route_short_name",
    osm_match = "name",
    gtfs_manipulate = "manipulate_gtfs_cp",
    gtfs_osm_match_exact = FALSE
  )
)

regions <- rbind( # NYC, Bronx
  regions,
  data.frame(
    name = "nyc_bronx",
    gtfs_url = "https://rrgtfsfeeds.s3.amazonaws.com/gtfs_bx.zip",
    gtfs_day = Sys.Date(),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "operator", value = "Metropolitan Transportation Authority", key_exact = TRUE)
    )))
  )
)

# Fuenlabrada, ES
regions <- rbind(
  regions,
  data.frame(
    name = "fuenlabrada",
    gtfs_url = "https://api.control.optibus.co/opendata/v1/gtfs?uid=c-5cfcd2d1",
    gtfs_day = Sys.Date(),
    gtfs_manipulate = "manipulate_gtfs_fuenlabrada",
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "operator", value = "EMT Fuenlabrada", key_exact = TRUE)
    )))
  )
)

regions <- rbind(
  regions,
  data.frame(
    name = "guelph",
    gtfs_url = "http://guelph.ca/uploads/google/google_transit.zip",
    gtfs_day = Sys.Date(),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "Guelph Transit", key_exact = TRUE)
    ))),
    geofabrik_region = "north-america/canada/ontario"
  )
)

regions <- rbind(
  regions,
  data.frame(
    name = "rome",
    # For historical versions, refer to https://mobilitydatabase.org/feeds/gtfs_rt/mdb-1776
    gtfs_url = "https://romamobilita.it/sites/default/files/rome_static_gtfs.zip",
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "ATAC", key_exact = TRUE)
    ))),
    geofabrik_region = "europe/italy/centro"
  )
)

regions <- bind_rows( # Fertagus
  regions,
  data.frame(
    name = "fertagus",
    gtfs_url = data$URL[data$ID == "fertagus"],
    gtfs_day = gsub("-", "", Sys.Date()),
    gtfs_match = "route_long_name",
    gtfs_manipulate = "manipulate_gtfs_fertagus",
    query = I(list(list(
      list(key = "route", value = c("train"), key_exact = TRUE),
      list(key = "network", value = "Fertagus", key_exact = TRUE)
    ))),
    osm_route_type = "train",
    geofabrik_region = "europe/portugal"
  )
)

regions <- bind_rows( # Metro Lisboa
  regions,
  data.frame(
    name = "metroLisboa",
    gtfs_url = "../GTFShift-web/scripts/osm_match/metrolisboa/gtfs_20260526/googleTransit.zip",
    gtfs_day = gsub("-", "", Sys.Date()),
    # gtfs_day_filter = TRUE,
    gtfs_match = "route_long_name",
    query = I(list(list(
      list(key = "route", value = c("subway"), key_exact = TRUE),
      list(key = "network", value = "Metropolitano de Lisboa", key_exact = TRUE)
    ))),
    osm_route_type = "subway",
    geofabrik_region = "europe/portugal"
  )
)


# Helpers

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

manipulate_gtfs_fuenlabrada <- function(gtfs) {
  # Append "L" suffix to route_short_name
  gtfs$routes$route_short_name <- paste0("L", gtfs$routes$route_short_name)
  return(gtfs)
}

manipulate_gtfs_cascais <- function(gtfs) {
  # Change route_short_name from numeric to M%02d format (1 should become M01) to assure OSM ref match
  gtfs$routes$route_short_name <- sprintf("M%02d", as.numeric(gtfs$routes$route_short_name))
  return(gtfs)
}

manipulate_gtfs_fertagus <- function(gtfs) {
  # Replace Lisboa by Roma-Areeiro on routes
  gtfs$routes$route_long_name <- gsub("Lisboa -", "Roma-Areeiro", gtfs$routes$route_long_name)
  return(gtfs)
}

# main()
for (i in 1:nrow(regions)) {
  region <- regions[i, ]
  message(sprintf("\n\nRunning for %s (%s)...", region$name, region$gtfs_day))

  output <- sprintf("%s/%s/%s", output_root, region$name, region$gtfs_day)
  if (!dir.exists(output)) {
    dir.create(output, recursive = TRUE)
  }

  gtfs <- load_feed(region$gtfs_url)
  assign(sprintf("gtfs_%s_%s", region$name, region$gtfs_day), gtfs)
  tidytransit::write_gtfs(gtfs, sprintf("%s/gtfs_%s_%s.zip", output, region$name, region$gtfs_day))

  gtfs_shapes <- tidytransit::shapes_as_sf(gtfs$shapes)
  bbox <- sf::st_bbox(gtfs_shapes)

  if (!is.null(region$gtfs_manipulate)) {
    gtfs <- get(region$gtfs_manipulate)(gtfs)
  }

  # Build OSM query
  q <- opq(bbox = bbox)
  for (feat in region$query[[1]]) {
    q <- add_osm_feature(
      q,
      key = feat$key,
      value = feat$value,
      key_exact = if (!is.null(feat$key_exact)) feat$key_exact else FALSE
    )
  }
  assign(sprintf("q_%s_gtfs%s", region$name, region$gtfs_day), q)

  osm_file <- NULL
  if (!is.null(region$geofabrik_region)) {
    osm_file <- osmextract::oe_download(
      sprintf("https://download.geofabrik.de/%s-latest.osm.pbf", region$geofabrik_region),
      file_basename = sprintf("%s_%s.osm.pbf", str_replace_all(region$geofabrik_region, "/", "_"), format(Sys.Date(), "%Y%m%d"))
    )
  }

  # Match shapes geometry
  shapes_match_routes <- osm_shapes_match_routes(
    gtfs, q,
    gtfs_match = if (!is.null(region$gtfs_match)) region$gtfs_match else "route_short_name",
    osm_match = if (!is.null(region$osm_match)) region$osm_match else "ref",
    gtfs_osm_match_exact = if (!is.null(region$gtfs_osm_match_exact)) region$gtfs_osm_match_exact else TRUE,
    log_file = sprintf("%s/shapes_match_%s_gtfs%s_run%s.r.log", output, region$name, region$gtfs_day, gsub("-", "", Sys.Date())),
    osm_file = osm_file
  )
  assign(sprintf("shapes_match_routes_%s_gtfs%s", region$name, region$gtfs_day), shapes_match_routes)

  write.csv(shapes_match_routes |> sf::st_drop_geometry() |> mutate(
    distance_diff = round(distance_diff),
    points_diff = round(points_diff)
  ), sprintf("%s/shapes_match_%s_gtfs%s_run%s.csv", output, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), row.names = FALSE)
  sf::st_write(shapes_match_routes, sprintf("%s/shapes_match_%s_gtfs%s_run%s.gpkg", output, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), append = FALSE)
}

shapes_match_routes
nrow(shapes_match_routes)

# DEBUG

# > See OSM routes geometry
routes <- gtfs$routes |>
  # filter(grepl("Sintra", route_short_name)) |>
  mutate(
    from = str_split_fixed(route_id, "-", 3)[, 2],
    to = str_split_fixed(route_id, "-", 3)[, 3]
  ) |>
  left_join(gtfs$stops |> select(stop_id, stop_name) |> rename(from_name = stop_name), by = c("from" = "stop_id")) |>
  left_join(gtfs$stops |> select(stop_id, stop_name) |> rename(to_name = stop_name), by = c("to" = "stop_id")) |>
  right_join(shapes_match_routes |> select(osm_id, route_id, shape_id), by = "route_id") |>
  sf::st_as_sf()

routes
mapview::mapview(routes, zcol = "osm_id")

# > Draw original shapes for those routes
routes_original <- routes |>
  sf::st_drop_geometry() |>
  left_join(gtfs_shapes, by = "shape_id") |>
  sf::st_as_sf()

mapview::mapview(routes_original, zcol = "osm_id")
