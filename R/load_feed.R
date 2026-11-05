#' Read GTFS feed, fixing integrity errors
#'
#' @param path String. The location of the GTFS zip file. Either local or URL.
#' @param store_path String (Optional). If provided, GTFS feed zip is stored at location. The file is overwritten if it already exists.
#' @param create_transfers Boolean (Default FALSE). When true, generates \code{transfers.txt}, aggregating close stops.
#' @param transfer_distance Integer (Default 300). Upper straight-line distance limit in meters for transfers.
#' @param transfer_time Integer (Default 120). Minimum time in seconds for transfers; all values below this will be replaced with this value, particularly all those defining in-place transfers where stop longitudes and latitudes remain identical.
#' @param transfer_street_routing Boolean (Default FALSE). If TRUE, transfer times are calculated by routing throughout the underlying street network (downloaded automatically).
#' @param headers Named list or character vector (Optional). Custom HTTP headers for credentials when accessing the GTFS zip file URL.
#'
#' @details
#' In addition to loading the GTFS feed, this method validates its integrity and applies the proper corrections if it does not comply with the following validations:
#' \itemize{
#'  \item Removes duplicated entries across tables using \code{gtfstools::remove_duplicates()}.
#'  \item \code{stop_times.txt} with empty \code{arrival_time} or \code{departure_time}, linearly interpolating missing times using \code{tidytransit::interpolate_stop_times()}.
#'  \item Removes unused IDs across tables using \code{gtfstools::remove_unused_ids()}.
#'  \item Feeds with missing \code{shapes.txt} file, generating it using \code{GTFShift::create_shapes_from_stops()}.
#' }
#' When generating transfers, those already existing in each GTFS file are kept, extended with new ones computed based on the stops
#' network of the final aggregated version. This computation is executed with \code{gtfsrouter::gtfs_transfer_table()}, with
#' the parameters \code{d_limit=transfer_distance}, \code{min_transfer_time=transfer_time} and \code{network_times=transfer_street_routing}.
#' The other parameters are applied the library default values.
#'
#' @returns tidygtfs. The loaded GTFS feed.
#'
#' @seealso \code{GTFShift::create_shapes_from_stops()}
#' @seealso \code{tidytransit::read_gtfs()}
#' @seealso \code{tidytransit::interpolate_stop_times()}
#' @seealso \code{gtfstools::remove_duplicates()}
#' @seealso \code{gtfstools::remove_unused_ids()}
#' @seealso \code{gtfsrouter::gtfs_transfer_table()}
#'
#' @examples
#' # Simple call
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_tcb_sample.zip", package = "GTFShift")
#' )
#' 
#' summary(gtfs)
#'
#' 
#' # Simple call with missing shapes (triggering shapes creation because missing on GTFS file)
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_ttsl_sample_no_shapes.zip", package = "GTFShift")
#' )
#' 
#' summary(gtfs)
#'
#' 
#' # With some parameters to build transfers and store to given location
#' store_path <- withr::local_tempfile(fileext = ".zip")
#' 
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_tcb_sample.zip", package = "GTFShift"), create_transfers = TRUE, store_path
#' )
#' 
#' head(gtfs$transfers)
#' 
#' file.exists(store_path)
#'
#' @import tidytransit
#' @importFrom gtfstools remove_duplicates remove_unused_ids
#'
#' @export
load_feed <- function(path, store_path = NA, create_transfers = FALSE, transfer_distance = 300, transfer_time = 120, transfer_street_routing = FALSE, headers = NULL) {
  # If path is a URL and headers are provided, download first
  if (grepl("^http", path) && !is.null(headers)) {
    temp_zip <- withr::local_tempfile(fileext = ".zip")
    res <- httr::GET(path, httr::add_headers(.headers = headers), httr::write_disk(temp_zip, overwrite = TRUE))
    httr::stop_for_status(res)
    path <- temp_zip
  }

  # LOAD GTFS
  gtfs <- withCallingHandlers(
    tidytransit::read_gtfs(path),
    warning = function(w) {
      if (grepl("Duplicated ids found|The returned object is not a tidygtfs object", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )

  # VALIDATE integrity

  ## Deduplicate entries across tables
  gtfs <- gtfstools::remove_duplicates(gtfs)
  gtfs <- suppressWarnings(tidytransit::as_tidygtfs(gtfs))

  ## Interpolate missing stop_times arrival/departure
  if ("stop_times" %in% names(gtfs) && !is.null(gtfs$stop_times) && nrow(gtfs$stop_times) > 0) {
    has_arr <- "arrival_time" %in% names(gtfs$stop_times)
    has_dep <- "departure_time" %in% names(gtfs$stop_times)
    missing_rows <- sum(
      (if (has_arr) is.na(gtfs$stop_times$arrival_time) else FALSE) |
      (if (has_dep) is.na(gtfs$stop_times$departure_time) else FALSE)
    )
    if (missing_rows > 0) {
      gtfs <- tidytransit::interpolate_stop_times(gtfs)
      warning(sprintf("> FIXED GTFS, interpolated %d stop times with missing times!", missing_rows))
    }
  }

  ## Remove unused IDs across tables
  if ("trips" %in% names(gtfs) && "trip_id" %in% names(gtfs$trips) &&
      "stop_times" %in% names(gtfs) && "trip_id" %in% names(gtfs$stop_times)) {
    gtfs <- gtfstools::remove_unused_ids(gtfs)
    gtfs <- suppressWarnings(tidytransit::as_tidygtfs(gtfs))
  }

  ## If trips does not have shape_id column, create empty one
  if (!("shape_id" %in% names(gtfs$trips))) {
    gtfs$trips$shape_id <- NA
  }

  ## If no shapes.txt, create them
  if (!("shapes" %in% names(gtfs))) {
    gtfs <- create_shapes_from_stops(gtfs)
    warning(sprintf("> CREATED shapes.txt, the file was missing!"))
  }

  # Generate transfers.txt
  if (create_transfers) {
    if (!requireNamespace("gtfsrouter", quietly = TRUE)) {
      stop("Package 'gtfsrouter' is required to generate transfers. Install it with: install.packages('gtfsrouter')")
    }
    # Store in  temporary file because gtfsrouter can not convert from tidytransit format
    temp_dir <- withr::local_tempdir()
    gtfs_temp <- file.path(temp_dir, "gtfs.zip")
    tidytransit::write_gtfs(gtfs, gtfs_temp)

    suppressMessages(suppressWarnings({
      gtfs_transfers <- gtfsrouter::extract_gtfs(gtfs_temp)
    })) # Suppress warning that has no transfers, as they will be generated next
    gtfs_transfers <- gtfsrouter::gtfs_transfer_table(gtfs_transfers, d_limit = transfer_distance, min_transfer_time = transfer_time, network_times = transfer_street_routing)
    gtfs$transfers <- gtfs_transfers$transfers

    gtfs <- tidytransit::as_tidygtfs(gtfs)
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
