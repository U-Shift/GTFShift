#' Collect GTFS-RT data
#'
#'
#' @param gtfs_rt_url String. URL of the GTFS-RT feed in JSON format.
#' @param destination_file String. File to save the downloaded GTFS-RT data. Content is appended in each iteration.
#' @param header_key String (Default "header"). Key in the JSON corresponding to the feed header. Set to NA if not present.
#' @param entity_key String (Default "entity"). Key in the JSON corresponding to the feed entities. Set to NA if response is a flat list.
#' @param fields_collect Character vector. Fields to extract from each entity in the feed.
#' @param scrape_interval Integer (Default 60). Interval in seconds between each download. Negative to run only once.
#' @param log_file String (Optional). Path to a log file to save download logs.
#'
#' @details
#' Downloads GTFS-RT data from the specified URL at regular intervals and saves them to the destination file.
#'
#' This function will run indefinitely until manually stopped.
#'
#'
#' @examples
#' \dontrun{
#' GTFShift::rt_collect("https://api.example.com/gtfs-rt", "gtfs_rt_data.csv")
#' }
#'
#' @import jsonlite
#' @import progress
#'
#' @export
rt_collect <- function(
    gtfs_rt_url, destination_file,
    header_key="header", # Optional
    entity_key="entity",
    fields_collect = c("id", "vehicle.trip.trip_id", "vehicle.position.latitude", "vehicle.position.longitude", "vehicle.position.speed", "vehicle.timestamp", "vehicle.current_status", "vehicle.current_stop_sequence", "vehicle.stop_id"),
    scrape_interval = 60, log_file = NA
) {
  # Log script start
  m = sprintf("[%s] Starting GTFS-RT data collection from %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), gtfs_rt_url)
  message(m)
  if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)

  # Each scrape_interval seconds, download the GTFS-RT feed and save it to the destination folder
  count = 0
  repeat {
    count = count + 1
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    feed <- jsonlite::fromJSON(gtfs_rt_url)

    if (!is.na(entity_key)) {
      entities <- as.data.frame(feed[[entity_key]])
    } else {
      entities <- feed
    }

    # For each field in fields_collect, extract the data and add it to the data frame
    feed_df <- data.frame()
    for (field in fields_collect) {
      field_parts <- unlist(strsplit(field, "\\."))
      field_data <- entities
      for (part in field_parts) {
        if (part %in% names(field_data)) {
          field_data <- field_data[[part]]
        } else {
          field_data <- NA
          break
        }
      }
      if (nrow(feed_df) == 0) {
        feed_df <- data.frame(field_data)
        names(feed_df) <- field
      } else {
        feed_df[[field]] <- field_data
      }
    }

    if (!is.na(header_key)) {
      header = feed[[header_key]]
      if ("timestamp" %in% names(header)) {
        feed_df$feed_timestamp <- header$timestamp
      }
      if ("incrementality" %in% names(header)) {
        feed_df$feed_incrementality <- header$incrementality
      }
    }

    write.table(
      feed_df,
      file = destination_file,
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists(destination_file), # only write header if file is new
      append = TRUE
    )

    m = sprintf("[%s] Iteration %d completed", timestamp, count)
    message(m)
    if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)

    # Wait for scrape_interval seconds before the next download
    if (scrape_interval<0) {
      break
    }
    interval_start <- Sys.time()
    pb <- progress::progress_bar$new( # Track progress
      format = "Sleeping [:bar] :percent :spin elapsed=:elapsed",
      clear = FALSE, show_after=0
    )
    pb$update(0)
    repeat {
      elapsed_time <- as.numeric(difftime(Sys.time(), interval_start, units="secs"))
      if (elapsed_time >= scrape_interval) break;
      pb$update( min(elapsed_time / scrape_interval, 1) );
      Sys.sleep(0.1);
    }
    pb$update(1)
  }
}
