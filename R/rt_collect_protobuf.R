#' Collect GTFS-RT data from a Protocol Buffers feed at regular intervals
#'
#'
#' @param gtfs_rt_url String. URL of the Protocol Buffers GTFS-RT feed.
#' @param destination_file String. File to save the downloaded GTFS-RT data. Content is appended in each iteration.
#' @param fields_collect Character vector. Fields to extract from each entity in the feed.
#' @param scrape_interval Integer (Default 60). Interval in seconds between each download. Negative to run only once.
#' @param log_file String (Optional). Path to a log file to save download logs.
#' @param headers Named list or character vector (Optional). Custom HTTP headers for credentials when accessing the GTFS-RT feed URL.
#'
#' @details
#' Downloads GTFS-RT data from the specified URL at regular intervals and saves them to the destination file.
#'
#' This function will run indefinitely until manually stopped (CTRL + C).
#'
#'
#' @examples
#' # Create file
#' destination_file <- tempfile(fileext = ".csv")
#'
#' # Collect data
#' GTFShift::rt_collect_protobuf(
#'   gtfs_rt_url = "https://go.tmlmobilidade.pt/hub/api/v1/realtime/vehicles/positions/gtfs.pb",
#'   destination_file = destination_file,
#'   scrape_interval = -1 # Negative to run only once
#' )
#'
#' # Read data
#' collection <- read.csv(destination_file)
#'
#' names(collection)
#'
#' head(
#'   collection |>
#'     dplyr::select("vehicle.trip.trip_id", "vehicle.position.latitude", "vehicle.position.longitude")
#' )
#'
#' @importFrom jsonlite write_json
#' @importFrom stats setNames
#' @export
rt_collect_protobuf <- function(
  gtfs_rt_url, destination_file,
  fields_collect = c("id", "vehicle.trip.trip_id", "vehicle.position.latitude", "vehicle.position.longitude", "vehicle.position.speed", "vehicle.timestamp", "vehicle.current_status", "vehicle.current_stop_sequence", "vehicle.stop_id"),
  scrape_interval = 60, log_file = NA, headers = NULL
) {
  # Log script start
  m <- sprintf("[%s] Starting GTFS-RT data collection from %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), gtfs_rt_url)
  message(m)
  if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)

  # Each scrape_interval seconds, download the GTFS-RT feed and save it to the destination folder
  count <- 0
  repeat {
    count <- count + 1
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    # Load protobuf
    if (!requireNamespace("RProtoBuf", quietly = TRUE)) {
      stop("Package 'RProtoBuf' is required for this function. Install it with: install.packages('RProtoBuf')")
    }
    RProtoBuf::readProtoFiles((system.file("extdata", "gtfs-realtime.proto", package = "GTFShift")))
    if (grepl("^http", gtfs_rt_url) && !is.null(headers)) {
      temp_pb <- tempfile(fileext = ".pb")
      res <- httr::GET(gtfs_rt_url, httr::add_headers(.headers = headers), httr::write_disk(temp_pb, overwrite = TRUE))
      httr::stop_for_status(res)
      f <- file(temp_pb, "rb")
    } else {
      f <- file(gtfs_rt_url, "rb")
    }
    on.exit(close(f), add = TRUE)
    feed_desc <- RProtoBuf::P("transit_realtime.FeedMessage")
    feed <- RProtoBuf::read(feed_desc, f)
    close(f)
    on.exit(NULL, add = FALSE)

    # Convert to R list
    fields <- names(feed)

    protobuf_to_list <- function(msg) {
      if (!inherits(msg, "Message")) {
        return(msg)
      }

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
    temp_json <- tempfile(fileext = ".json")
    jsonlite::write_json(
      feed_list,
      temp_json,
      pretty = TRUE,
      auto_unbox = TRUE
    )

    suppressMessages({
      rt_collect_json(
        gtfs_rt_url = temp_json,
        destination_file = destination_file,
        fields_collect = fields_collect,
        scrape_interval = -1,
        log_file = NA
      )
    })

    m <- sprintf("[%s] Iteration %d completed", timestamp, count)
    message(m)
    if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)

    # Wait for scrape_interval seconds before the next download
    if (scrape_interval < 0) {
      break
    }
    interval_start <- Sys.time()
    if (!requireNamespace("progress", quietly = TRUE)) {
      stop("Package 'progress' is required for this function. Install it with: install.packages('progress')")
    }
    pb <- progress::progress_bar$new( # Track progress
      format = "Sleeping [:bar] :percent :spin elapsed=:elapsed",
      clear = FALSE, show_after = 0
    )
    pb$update(0)
    repeat {
      elapsed_time <- as.numeric(difftime(Sys.time(), interval_start, units = "secs"))
      if (elapsed_time >= scrape_interval) break
      pb$update(min(elapsed_time / scrape_interval, 1))
      Sys.sleep(0.1)
    }
    pb$update(1)
  }
}
