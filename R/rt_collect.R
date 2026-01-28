#' Collect GTFS-RT data
#'
#'
#' @param gtfs_rt_url String. URL of the GTFS-RT feed in JSON format.
#' @param destination_folder String. Folder to save the downloaded GTFS-RT files.
#' @param scrap_interval Integer (Default 60). Interval in seconds between each download.
#' @param log_file String (Optional). Path to a log file to save download logs.
#'
#' @details
#' Downloads GTFS-RT data from the specified URL at regular intervals and saves them to the destination folder.
#'
#' This function will run indefinitely until manually stopped. Each downloaded file is named with a timestamp to ensure uniqueness.
#'
#'
#' @examples
#' \dontrun{
#' GTFShift::rt_collect("https://api.example.com/gtfs-rt", "gtfs_rt_data")
#' }
#'
#'
#' @export
rt_collect <- function(gtfs_rt_url, destination_folder, scrap_interval = 60, log_file = NA, proto_buffer=FALSE) {
  # Create destination folder if it doesn't exist
  if (!dir.exists(destination_folder)) {
    dir.create(destination_folder, recursive = TRUE)
  }

  # Log script start
  m = sprintf("[%s] Starting GTFS-RT data collection from %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), gtfs_rt_url)
  message(m)
  if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)

  # Each scrap_interval seconds, download the GTFS-RT feed and save it to the destination folder
  count = 0
  repeat {
    count = count + 1
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    destination_file <- file.path(destination_folder, paste0(timestamp, ".json"))
    download.file(gtfs_rt_url, destfile = destination_file, mode = "wb")

    m = sprintf("[%s] %d files downloaded", timestamp, count)
    message(m)
    if (!is.na(log_file)) cat(paste(m, "\n"), file = log_file, append = TRUE)

    Sys.sleep(scrap_interval)  # Wait for scrap_interval seconds before the next download
  }
}
