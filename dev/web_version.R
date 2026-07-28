# Script to generate pre-processed data for GTFShift web dashboard

library(GTFShift)
library(dplyr)
library(osmdata)
library(stringr)
library(sf)

# Parameters
output = "releases/web"

regions = data.frame(
  name = character(),
  name_long = character(),
  gtfs = character(),
  query = I(list()),
  rt_interval = character(),
  rt_collection = I(list()) # sf object

)
data = read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))

regions = rbind( # Lisboa
  regions,
  data.frame(
    name = "lisboa_rt",
    name_long = "Lisboa, Portugal",
    gtfs_url = data$URL[data$ID == "lisboa"],
    gtfs_day = calendar_nextBusinessWednesday(),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "Carris", key_exact = TRUE)
    ))),
    rt_interval = "12-16/05/2025",
    rt_collection = I(list(sf::st_read("releases/gtfs_rt_data/carris_updates.csv") |> # 12 to 16/05/2025
      mutate(
        lon = str_replace(lon, "c\\(", ""),
        lat = str_replace(lat, "\\)", ""),
        speed = as.numeric(speed)
      ) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)))
  )
)
#
# regions = rbind( # STCP
#   regions,
#   data.frame(
#     name = "stcp",
#     gtfs_url = data$URL[data$ID == "stcp"],
#     gtfs_day = gsub("-", "", Sys.Date()),
#     query = I(list(list(
#       list(key = "route", value = c("bus"), key_exact = TRUE),
#       list(key = "operator", value = "STCP", key_exact = TRUE)
#     )))
#   )
# )

# main()
stop_buffer_size = 15 # meters

if (!dir.exists(output)) {
  dir.create(output, recursive = TRUE)
}

for(i in 1:nrow(regions)) {
  region <- regions[i, ]
  message(sprintf("\n\nRunning for %s (%s)...", region$name, region$gtfs_day))

  output_region = sprintf("%s/%s/%s", output, region$name, region$gtfs_day)
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

  # Prioritise based on planned operation and infrastructure characteristics
  prioritisation = prioritise_lanes(gtfs, q, date=region$gtfs_day)
  assign(sprintf("prioritisation_%s_gtfs%s", region$name, region$gtfs_day), prioritisation)

  write.csv(prioritisation |> sf::st_drop_geometry(), sprintf("%s/prioritisation_%s_gtfs%s_run%s.csv", output_region, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), row.names = FALSE)
  sf::st_write(prioritisation, sprintf("%s/prioritisation_%s_gtfs%s_run%s.gpkg", output_region, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), append=FALSE)

  # prioritisation = st_read(sprintf("%s/prioritisation_%s_gtfs%s_run%s.gpkg", output_region, region$name, region$gtfs_day, gsub("-", "", Sys.Date())))

  # Extend with real-time data if available
  if (!is.na(region$rt_collection)) {
    rt_collection = region$rt_collection[[1]]
    # Filter updates, to remove those close to bus stops
    gtfs_stops = tidytransit::stops_as_sf(gtfs$stops, crs=4326)

    within_distance <- st_is_within_distance(rt_collection |> st_transform(crs=3857), gtfs_stops |> st_transform(crs=3857), dist = stop_buffer_size)

    rt_collection_filtered = rt_collection[lengths(within_distance) == 0, ]

    # Extend prioritisation with real-time data
    prioritisation = rt_extend_prioritisation(
      lane_prioritisation = prioritisation,
      rt_collection = rt_collection_filtered
    )
  }

  # Replace route_id with route names, considering that prioritisation$routes has multiple route_ids separated by ";"
  route_names = gtfs$routes[, c("route_id", "route_short_name", "route_long_name")]
  prioritisation = prioritisation |>
    mutate(row_n = row_number())
  prioritisation_routes = prioritisation |>
    st_drop_geometry() |>
    tidyr::separate_rows(routes, sep = ";")
  routes_covered = prioritisation_routes |>
    select(routes) |>
    distinct()
  routes_covered = routes_covered |>
    left_join(route_names, by = c("routes" = "route_id")) |>
    mutate(
      route_name = ifelse(!is.na(route_short_name) & route_short_name != "", route_short_name, route_long_name)
    ) |>
    select(routes, route_name)
  prioritisation_routes = prioritisation_routes |>
    left_join(routes_covered, by = c("routes" = "routes"))
  prioritisation_routes_grouped = prioritisation_routes |>
    group_by(row_n) |>
    summarise(
      route_names = paste(unique(route_name), collapse = ";"),
      .groups = "drop"
    )
  prioritisation = prioritisation |>
    left_join(prioritisation_routes_grouped, by = c("row_n" = "row_n")) |>
    select(-row_n)

  # Save outputs
  write.csv(prioritisation |> sf::st_drop_geometry(), sprintf("%s/prioritisation_%s_gtfs%s_run%s_extended.csv", output_region, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), row.names = FALSE)
  sf::st_write(prioritisation, sprintf("%s/prioritisation_%s_gtfs%s_run%s_extended.gpkg", output_region, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), append=FALSE)
  geojson_file = sprintf("%s/prioritisation_%s_gtfs%s_run%s_extended.geojson", output_region, region$name, region$gtfs_day, gsub("-", "", Sys.Date()))
  sf::st_write(prioritisation, geojson_file, append=FALSE, delete_dsn = TRUE)

  # Open geojson with jsonlite, to extend with technical metadata
  geojson_data = jsonlite::read_json(geojson_file, digits=NA) # To avoid precision loss in coordinates

  dataCensus = function (numberArray) {
    return(list(
      min = min(numberArray, na.rm=TRUE),
      max = max(numberArray, na.rm=TRUE),
      p25 = as.numeric(quantile(numberArray, 0.25, na.rm=TRUE)),
      p75 = as.numeric(quantile(numberArray, 0.75, na.rm=TRUE)),
      mean = mean(numberArray, na.rm=TRUE),
      median = as.numeric(median(numberArray, na.rm=TRUE)),
      variance = var(numberArray, na.rm=TRUE),
      sd = sd(numberArray, na.rm=TRUE)
    ))
  }

  rt_list = NA
  if(!is.na(region$rt_collection)) {
    rt_list = list(
      url = "", # To be edited manually
      period = region$rt_interval,
      stop_buffer_size = stop_buffer_size
    )
  }
  census_frequency_hour = list()
  for(h in 0:23) {
    prioritisation_hour = prioritisation |> filter(hour == h)
    census_frequency_hour[[as.character(h)]] = dataCensus(prioritisation_hour$frequency)
  }

  metadata = list(
    region = region$name_long,
    gtfs = list(
      date = region$gtfs_day,
      url = region$gtfs_url
    ),
    osm_query = lapply(region$query[[1]], function(feat) {
      list(
        key = feat$key,
        value = feat$value,
        key_exact = if (!is.null(feat$key_exact)) feat$key_exact else FALSE
      )
    }),
    prioritisation = list(
      routes_missing = paste(gtfs$routes |> filter(!route_id %in% routes_covered$routes) |> pull(route_short_name), collapse = ";"),
      routes_covered = nrow(routes_covered),
      routes_total = nrow(gtfs$routes)
    ),
    data_census = list(
      frequency = dataCensus(prioritisation$frequency),
      frequency_hour = census_frequency_hour,
      speed_avg = dataCensus(prioritisation$speed_avg),
      lanes = dataCensus(prioritisation$n_lanes_direction)
    ),
    rt = rt_list,
    execution = list(
      moment = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      script = "dev/web_version.R",
      git_commit = system("git rev-parse HEAD", intern=TRUE)
    ),
    environment = list(
      r = R.version.string,
      GTFShift = as.character(packageVersion("GTFShift")),
      os = Sys.info()[["sysname"]],
      os_release = Sys.info()[["release"]]
    )
  )

  geojson_data$metadata <- metadata
  # Save at geojson_file.replace(".geojson", "_with_metadata.geojson")
  geojson_file_metadata = gsub(".geojson$", "_with_metadata.geojson", geojson_file)
  jsonlite::write_json(
    geojson_data,
    geojson_file_metadata,
    auto_unbox = TRUE,
    digits=NA # To avoid precision loss in coordinates
  )
}

# Debug
library(sf)
prioritisation = st_read("releases/web/lisboa/2026-02-04/prioritisation_lisboa_rt_gtfs2026-02-04_run20260203_extended.geojson")
prioritisation_0800 = prioritisation |> filter(hour==8)
p50_frequency = quantile(prioritisation_0800$frequency, 0.5, na.rm=TRUE)
p50_speed = quantile(prioritisation_0800$speed_avg, 0.5, na.rm=TRUE)
mapview::mapview(
  prioritisation_0800 |> filter(is_bus_lane & (frequency<p50_frequency | (is.na(n_lanes) | n_lanes_direction<=1) | speed_avg<=p50_speed)),
  layer.name=sprintf("Bus lane with -%d bus/h OR -2 lane/dir OR %.2f km/h or - avg. speed", p50_frequency, p50_speed),
  color="#DAD887",
  homebutton=FALSE,
  lwd=3

) + mapview::mapview(
  prioritisation_0800 |> filter(is_bus_lane & frequency>=p50_frequency & !is.na(n_lanes) & n_lanes_direction>1 & speed_avg>p50_speed),
  layer.name=sprintf("Bus lane with +%d bus/h AND +1 lane/dir AND +%.2f km/h avg.speed", p50_frequency-1, p50_speed),
  color="#3BC1A8",
  homebutton=FALSE,
  lwd=3
) + mapview::mapview(
  prioritisation_0800 |> filter(!is_bus_lane & frequency>=p50_frequency & !is.na(n_lanes) & n_lanes_direction>1 & speed_avg<=p50_speed),
  layer.name=sprintf("NO bus lane with +%d bus/h AND +1 lane/dir AND %.2f km/h or - avg.speed", p50_frequency-1, p50_speed),
  color="#F63049",
  homebutton=FALSE,
  lwd=3
)
