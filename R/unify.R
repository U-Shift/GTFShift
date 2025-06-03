#' Merge multiple GTFS into a single aggregated file
#'
#' @param ... tidygtfs[]. List of GTFS feeds.
#' @param store_path. String. (Optional) If provided, aggregated feed zip is stored at location. The file is overwritten if it already exists.
#' @param create_transfers Boolean. (Default TRUE) When true, generates transfers.txt, aggregating close stops, even if from different GTFS.
#' @param transfer_distance Integer. (Default 300) Upper straight-line distance limit in meters for transfers.
#' @param transfer_time Integer. (Default 120) Minimum time in seconds for transfers; all values below this will be replaced with this value, particularly all those defining in-place transfers where stop longitudes and latitudes remain identical.
#' @param transfer_street_routing (Default FALSE) If TRUE, transfer times are calculated by routing throughout the underlying street network (downloaded automatically).
#'
#' @details
#' Aggregates multiple feeds using `GTFSwizard::merge_gtfs()`.
#' When generating transfers, those already existing in each GTFS file are kept, extended with new ones computed based on the stops network of the final aggregated version. This computation is executed with `gtfsrouter::gtfs_transfer_table`, with the parameters `d_limit=transfer_distance`, `min_transfer_time=transfer_time` and `network_times=transfer_street_routing`. The other parameters are applied the library default values.
#' For a detailed example, see the \code{vignette("unify")}.
#'
#' @returns A tidygtfs object.
#'
#' @examples
#' \dontrun{
#' gtfs1 <- GTFShift::load_feed("gtfs1.zip")
#' gtfs2 <- GTFShift::load_feed("gtfs2.zip")
#' unified <- GTFShift::unify(gtfs1, gtfs2, create_transfers=TRUE)
#' }
#'
#' @seealso [GTFSwizard::merge_gtfs()]
#' @seealso [gtfsrouter::gtfs_transfer_table()]
#'
#' @importFrom GTFSwizard as_wizardgtfs merge_gtfs
#' @importFrom gtfsrouter extract_gtfs gtfs_transfer_table
#'
#' @export
unify <- function(..., store_path=NA, create_transfers=TRUE, transfer_distance=300, transfer_time=120, transfer_street_routing=FALSE) {

  gtfss = list(...)

  # Initial validation
  has_calendar = sapply(gtfss, function(g) "calendar" %in% names(g))
  has_calendar_dates_only = sapply(gtfss, function(g) "calendar_dates" %in% names(g) && !"calendar" %in% names(g))
  if (any(has_calendar) && any(has_calendar_dates_only) && !all(has_calendar_dates_only)) {
    stop("Feeds structure is inconsistent. There is at least one feed with calendar_dates.txt, but without calendar.txt, when others have it. Check GTFShift::create_calendar to fix this.")
  }

  # Convert to GTFSWizard and merge them
  message(sprintf("1. Starting merge process..."))
  merged = NULL
  for (gtfs in gtfss) {
    gtfs_w = GTFSwizard::as_wizardgtfs(gtfs)
    message(sprintf("> %s", gtfs$agency$agency_name))
    if (is.null(merged)) {
      merged <- gtfs_w
    } else {
      merged <- GTFSwizard::merge_gtfs(merged, gtfs_w) # https://r-packages.io/packages/GTFSwizard/merge_gtfs
    }
    message(sprintf("> Merged with %d routes, %d trips and %d stops...", length(merged$routes$route_id), length(merged$trips$trip_id), length(merged$stops$stop_id)))
  }

  # Store in  temporary file because tidytransit can not convert from GTFSWizard format
  temp_dir <- tempfile()
  dir.create(temp_dir)
  gtfs_temp <- file.path(temp_dir, "gtfs.zip")
  GTFSwizard::write_gtfs(merged, gtfs_temp)

  gtfs <- tidytransit::read_gtfs(gtfs_temp)

  # Generate transfers.txt
  if (create_transfers) {
    message(sprintf("2. Generating transfers..."))

    suppressMessages(suppressWarnings({gtfs_transfers <- gtfsrouter::extract_gtfs(gtfs_temp)})) # Suppress warning that has no transfers, as they will be generated next
    gtfs_transfers <- gtfsrouter::gtfs_transfer_table(gtfs_transfers, d_limit=transfer_distance, min_transfer_time=transfer_time, network_times=transfer_street_routing)
    gtfs$transfers <- gtfs_transfers$transfers

    gtfs <- tidytransit::as_tidygtfs(gtfs)
  }

  # STORE GTFS
  if (!is.na(store_path)) {
    message(sprintf("3. Storing file..."))
    if (!dir.exists(dirname(store_path))) {
      dir.create(dirname(store_path), recursive = TRUE)
    }
    tidytransit::write_gtfs(gtfs, store_path)
  }

  message(sprintf("FINISHED :)"))
  return(gtfs)
}
