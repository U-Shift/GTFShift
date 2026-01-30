#' Collect GTFS-RT data (with Protocol Buffers support)
#'
#'
#' @param gtfs_rt_url String. URL of the Protocol Buffers GTFS-RT feed.
#' @param destination_file String. File to save the downloaded GTFS-RT data. Content is appended in each iteration.
#' @param fields_collect Character vector. Fields to extract from each entity in the feed.
#' @param scrap_interval Integer (Default 60). Interval in seconds between each download. Negative to run only once.
#' @param log_file String (Optional). Path to a log file to save download logs.
#'
#' @details
#' Downloads GTFS-RT data from the specified URL at regular intervals and saves them to the destination file.
#'
#' This function will run indefinitely until manually stopped. Each downloaded file is named with a timestamp to ensure uniqueness.
#'
#'
#' @examples
#' \dontrun{
#' GTFShift::rt_collect_protobuf("https://api.example.com/gtfs-rt-protobuf", "gtfs_rt_data.csv")
#' }
#'
#' @import RProtoBuf
#' @import jsonlite
#'
#' @export
rt_collect_protobuf <- function(
    gtfs_rt_url, destination_file,
    fields_collect = c("id", "vehicle.trip.trip_id", "vehicle.position.latitude", "vehicle.position.longitude", "vehicle.position.speed", "vehicle.timestamp", "vehicle.current_status", "vehicle.current_stop_sequence", "vehicle.stop_id"),
    scrap_interval = 60, log_file = NA
) {
  # Log script start
  m = sprintf("[%s] Starting GTFS-RT data collection from %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), gtfs_rt_url)
  message(m)
  if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)

  # Each scrap_interval seconds, download the GTFS-RT feed and save it to the destination folder
  count = 0
  repeat {
    count = count + 1
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    # Load protobuf
    RProtoBuf::readProtoFiles((system.file("extdata", "gtfs-realtime.proto", package = "GTFShift")))
    f <- file(gtfs_rt_url, "rb")
    feed <- RProtoBuf::read(`transit_realtime.FeedMessage`, f)
    close(f)

    # Convert to R list
    fields <- names(feed)

    protobuf_to_list <- function(msg) {
      if (!inherits(msg, "Message")) return(msg)

      # get all fields
      fields <- names(msg)

      lapply(fields, function(f) {
        value <- msg[[f]]

        # recursively convert nested Message objects
        if (inherits(value, "Message")) {
          protobuf_to_list(value)
        } else if (is.list(value)) {
          lapply(value, protobuf_to_list)
        } else {
          value
        }
      }) |> setNames(fields)
    }

    feed_list <- protobuf_to_list(feed)
    temp_json = tempfile(fileext = ".json")
    write_json(
      feed_list,
      temp_json,
      pretty = TRUE,
      auto_unbox = TRUE
    )

    suppressMessages({
      rt_collect(
        gtfs_rt_url = temp_json,
        destination_file = destination_file,
        fields_collect = fields_collect,
        scrap_interval = -1,
        log_file = NA
      )
    })

    m = sprintf("[%s] Iteration %d completed", timestamp, count)
    message(m)
    if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)

    # Wait for scrap_interval seconds before the next download
    if (scrap_interval<0) {
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
      if (elapsed_time >= scrap_interval) break;
      pb$update( min(elapsed_time / scrap_interval, 1) );
      Sys.sleep(0.1);
    }
    pb$update(1)
  }
}
