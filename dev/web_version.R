# Script to generate pre-processed data for GTFShift web dashboard

library(GTFShift)
library(dplyr)
library(osmdata)
library(stringr)

# Parameters
output = "releases/web"

regions = data.frame(
  name = character(),
  gtfs = character(),
  query = I(list())
)
data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))

regions = rbind( # Lisboa
  regions,
  data.frame(
    name = "lisboa",
    gtfs_url = data$URL[data$ID == "lisboa"],
    gtfs_day = calendar_nextBusinessWednesday(),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "Carris", key_exact = TRUE)
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

# main()
if (!dir.exists(output)) {
  dir.create(output, recursive = TRUE)
}

for(i in 1:nrow(regions)) {
  region <- regions[i, ]
  message(sprintf("\n\nRunning for %s (%s)...", region$name, region$gtfs_day))

  output_region = sprintf("%s/%s/%s", output, regions$name, region$gtfs_day)
  if (!dir.exists(output_region)) {
    dir.create(output_region, recursive = TRUE)
  }

  gtfs = GTFShift::load_feed(region$gtfs_url)
  assign(sprintf("gtfs_%s_%s", region$name, region$gtfs_day), gtfs)
  tidytransit::write_gtfs(gtfs, sprintf("%s/gtfs_%s_%s.zip", output_region, region$name, region$gtfs_day))

  gtfs_shapes = tidytransit::shapes_as_sf(gtfs$shapes)
  bbox = sf::st_bbox(gtfs_shapes)

  if (!is.null(region$gtfs_manipulate)) {
    gtfs = get(region$gtfs_manipulate)(gtfs)
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

  # Match shapes geometry
  prioritization = prioritize_lanes(gtfs, q, date=region$gtfs_day)
  assign(sprintf("prioritization_%s_gtfs%s", region$name, region$gtfs_day), shapes_match_routes)

  write.csv(prioritization |> sf::st_drop_geometry(), sprintf("%s/prioritization_%s_gtfs%s_run%s.csv", output_region, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), row.names = FALSE)
  sf::st_write(prioritization, sprintf("%s/prioritization_%s_gtfs%s_run%s.gpkg", output_region, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), append=FALSE)
}


