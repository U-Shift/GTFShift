library(GTFShift)
library(dplyr)
library(osmdata)

# Parameters
output = "releases/v0_7_1"

regions = data.frame(
  name = character(),
  gtfs = character(),
  query = I(list())
)
data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))

regions = rbind( # AML
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
regions = rbind( # Barreiro
  regions,
  data.frame(
    name = "barreiro",
    gtfs_url = data$URL[data$ID == "barreiro"],
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "Transportes Coletivos do Barreiro", key_exact = TRUE)
    )))
  )
)
regions = rbind( # Braga
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
regions = rbind( # Cascais
  regions,
  data.frame(
    name = "cascais",
    gtfs_url = data$URL[data$ID == "cascais"],
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "MobiCascais", key_exact = TRUE)
    )))
  )
)
regions = rbind( # Funchal
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
regions = rbind( # Lagos
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
regions = rbind( # Lisboa
  regions,
  data.frame(
    name = "lisboa",
    gtfs_url = data$URL[data$ID == "lisboa"],
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus", "tram"), key_exact = TRUE),
      list(key = "network", value = "Carris", key_exact = TRUE)
    )))
  )
)
regions = rbind( # Madrid
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
regions = rbind( # STCP
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
regions = rbind( # Toulouse
  regions,
  data.frame(
    name = "toulouse",
    gtfs_url = "https://data.toulouse-metropole.fr/explore/dataset/tisseo-gtfs/files/fc1dda89077cf37e4f7521760e0ef4e9/download/",
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "operator", value = "Tisséo", key_exact = TRUE)
    )))
  )
)


# main()
for(i in 1:nrow(regions)) {
  region <- regions[i, ]
  message(sprintf("\n\nRunning for %s (%s)...", region$name, region$gtfs_day))

  gtfs = GTFShift::load_feed(region$gtfs_url)
  assign(sprintf("gtfs_%s_%s", region$name, region$gtfs_day), gtfs)
  tidytransit::write_gtfs(gtfs, sprintf("%s/gtfs_%s_%s.zip", output, region$name, region$gtfs_day))

  gtfs_shapes = tidytransit::shapes_as_sf(gtfs$shapes)
  bbox = sf::st_bbox(gtfs_shapes)

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

  # Match shapes geometry
  shapes_match_routes = GTFShift::osm_shapes_match_routes(gtfs, q, log_file = sprintf("%s/shapes_match_%s_gtfs%s_run%s.r.log", output, region$name, region$gtfs_day, gsub("-", "", Sys.Date())))
  assign(sprintf("shapes_match_routes_%s_gtfs%s", region$name, region$gtfs_day), shapes_match_routes)

  write.csv(shapes_match_routes |> sf::st_drop_geometry() |> mutate(
    distance_diff=round(distance_diff),
    points_diff=round(points_diff)
  ), sprintf("%s/shapes_match_%s_gtfs%s_run%s.csv", output, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), row.names = FALSE)
  sf::st_write(shapes_match_routes, sprintf("%s/shapes_match_%s_gtfs%s_run%s.gpkg", output, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), append=FALSE)
}
