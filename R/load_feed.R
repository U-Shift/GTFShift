#' Read GTFS feed, fixing integrity errors
#'
#' @param path String. The location of the GTFS zip file. Either local or URL.
#' @param store_path. String. (Optional) If provided, GTFS feed zip is stored at location. The file is overwritten if it already exists.
#' @param create_transfers Boolean. (Default FALSE) When true, generates transfers.txt, aggregating close stops.
#' @param transfer_distance Integer. (Default 300) Upper straight-line distance limit in metres for transfers.
#' @param transfer_time Integer. (Default 120) Minimum time in seconds for transfers; all values below this will be replaced with this value, particularly all those defining in-place transfers where stop longitudes and latitudes remain identical.
#' @param transfer_street_routing (Default FALSE) If TRUE, transfer times are calculated by routing throughout the underlying street network (downloaded automatically).
#'
#' @details
#' In addition to loading the GTFS feed, this method validates its integrity and applies the proper corrections if it does not comply with the following validations:
#' \itemize{
#'  \item `stop_times.txt` with empty `arrival_time` or `departure_time`, filtering rows that do not comply.
#'  \item Feeds with missing `shapes.txt` file, generating it using `GTFSwizard::get_shapes()`.
#' }
#' When generating transfers, those already existing in each GTFS file are kept, extended with new ones computed based on the stops network of the final aggregated version. This computation is executed with `gtfsrouter::gtfs_transfer_table`, with the parameters `d_limit=transfer_distance`, `min_transfer_time=transfer_time` and `network_times=transfer_street_routing`. The other parameters are applied the library default values.
#'
#' @returns A tidygtfs object.
#'
#' @seealso [tidytransit::read_gtfs()]
#' @seealso [GTFSwizard::get_shapes()]
#' @seealso [gtfsrouter::gtfs_transfer_table()]
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::load_feed("https://operator.com/gtfs.zip")
#' }
#'
#' @import tidytransit
#'
#' @export
load_feed <- function(path, store_path=NA, create_transfers=FALSE, transfer_distance=300, transfer_time=120, transfer_street_routing=FALSE) {

  # LOAD GTFS
  gtfs <- tidytransit::read_gtfs(path)

  # VALIDATE integrity

  ## Clean empty stop_times arrival/departure (happened with Cascais GTFS) which raises an error at filter_feed_by_date method
  stopsNPrev <- length(gtfs$stop_times$trip_id)
  gtfs$stop_times <- gtfs$stop_times[!is.na(gtfs$stop_times$arrival_time), ]
  stopsNAfter <- length(gtfs$stop_times$trip_id)
  if (stopsNPrev != stopsNAfter) {
    warning(sprintf("> FIXED GTFS, there were %d stop times without arrival time!", stopsNPrev-stopsNAfter))
  }

  ## If trips does not have shape_id column, create empty one
  if (!("shape_id" %in% names(gtfs$trips))) {
    gtfs$trips$shape_id = NA
  }

  ## If no shapes.txt, create them automatically with GTFSwizard
  if (!("shapes" %in% names(gtfs))) {
    # GTFSwizard::get_shapes is automatically applied when it detected shapes are missing
    suppressMessages(suppressWarnings({gtfs_fixed <- GTFSwizard::as_wizardgtfs(gtfs)}))
    gtfs$shapes <- gtfs_fixed$shapes
    gtfs$trips <- gtfs_fixed$trips
    warning(sprintf("> CREATED shapes.txt, the file was missing!"))
  }

  # Generate transfers.txt
  if (create_transfers) {
    # Store in  temporary file because gtfsrouter can not convert from tidytransit format
    temp_dir <- tempfile()
    dir.create(temp_dir)
    gtfs_temp <- file.path(temp_dir, "gtfs.zip")
    tidytransit::write_gtfs(gtfs, gtfs_temp)

    suppressMessages(suppressWarnings({gtfs_transfers <- gtfsrouter::extract_gtfs(gtfs_temp)}))
    gtfs_transfers <- gtfsrouter::gtfs_transfer_table(gtfs_transfers, d_limit=transfer_distance, min_transfer_time=transfer_time, network_times=transfer_street_routing)

    # gtfsrouter::extract_gtfs converts stop times to seconds, lets get it back to the format HH:mm before storing it...
    gtfs_transfers$stop_times$arrival_time <- sapply(gtfs_transfers$stop_times$arrival_time, time_convert_seconds_to_hms)
    gtfs_transfers$stop_times$departure_time <- sapply(gtfs_transfers$stop_times$departure_time, time_convert_seconds_to_hms)

    gtfs <- tidytransit::as_tidygtfs(gtfs_transfers)
  }

  # STORE GTFS
  if (!is.na(store_path)) {
    if (!dir.exists(dirname(store_path))) {
      dir.create(dirname(store_path), recursive = TRUE)
    }
    tidytransit::write_gtfs(gtfs, store_path)
  }

  return(gtfs)
}
