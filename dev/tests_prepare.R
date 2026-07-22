library(dplyr)

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
