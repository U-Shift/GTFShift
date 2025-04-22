#' GTFS unify
#'
#' Merge multiple GTFS into a single aggregated file.
#'
#' @param gtfss tidygtfs[]. List of GTFS loaded using GTFSwizard::read_gtfs.
#' @param zipfile String. Path to the zip file the feed should be written to. The file is overwritten if it already exists.
#' @param generateTransfers Boolean. When true (default), generates transfers.txt, aggregating close stops, even if from different GTFS.
#' @param transfer_distance Integer. Upper straight-line distance limit in metres for transfers.
#' @param transfer_time Integer. Minimum time in seconds for transfers; all values below this will be replaced with this value, particularly all those defining in-place transfers where stop longitudes and latitudes remain identical.
#' @param transfer_street_routing If TRUE, transfer times are calculated by routing throughout the underlying street network (downloaded automatically).
#'
#' @details
#' When generating transfers, those already existing in each GTFS file are kept, extended with new ones computed based on the stops network of the final aggregated version. This computation is executed with `gtfsrouter::gtfs_transfer_table`, with the parameters `d_limit=transfer_distance`, `min_transfer_time=transfer_time` and `network_times=transfer_street_routing`. The other parameters are applied the library default values.
#' For a detailed example, see the vignette \code{vignette("unify")}.
#'
#' @returns The path of the file written.
#'
#' @examples
#' \dontrun{
#' gtfs1 <- GTFSwizard::read_gtfs("gtfs1.zip")
#' gtfs2 <- GTFSwizard::read_gtfs("gtfs2.zip")
#' gtfs_unify(list(gtfs1, gtfs2), "gtfs_unified.zip", TRUE)
#' }
#'
#' @seealso [GTFSwizard::read_gtfs()]
#' @seealso [gtfsrouter::gtfs_transfer_table()]
#'
#' @import GTFSwizard
#' @import gtfsrouter
#'
#' @export
unify <- function(gtfss, zipfile, generateTransfers=TRUE, transfer_distance=300, transfer_time=120, transfer_street_routing=FALSE) {

  # Open them with GTFSWizard and merge them
  message(sprintf("1. Starting merge process..."))
  merged = NULL
  for (gtfs in gtfss) {
    message(sprintf("> %s", gtfs$agency$agency_name))
    if (is.null(merged)) {
      merged <- gtfs
    } else {
      merged <- GTFSwizard::merge_gtfs(merged, gtfs) # https://r-packages.io/packages/GTFSwizard/merge_gtfs
    }
    message(sprintf("> Merged with %d routes, %d trips and %d stops...", length(merged$routes$route_id), length(merged$trips$trip_id), length(merged$stops$stop_id)))
  }

  # Store merged GTFS and return its location
  message(sprintf("2. Done! Storing result to %s...", zipfile))
  GTFSwizard::write_gtfs(merged, zipfile)

  # Generate transfers.txt
  if (generateTransfers) {
    message(sprintf("3. Generating transfers..."))
    merged_router <- gtfsrouter::extract_gtfs(zipfile)
    # Use default parameters:
    # 200 meters distance, 120 sec time, routing through the road network (?)
    merged_router <- gtfsrouter::gtfs_transfer_table(merged_router, d_limit=transfer_distance, min_transfer_time=transfer_time, network_times=transfer_street_routing)

    # gtfsrouter::extract_gtfs converts stop times to seconds, lets get it back to the format HH:mm before storing it...
    merged_router$stop_times$arrival_time <- sapply(merged_router$stop_times$arrival_time, time_convert_seconds_to_hms)
    merged_router$stop_times$departure_time <- sapply(merged_router$stop_times$departure_time, time_convert_seconds_to_hms)

    message(sprintf("> Done! Storing result to %s...", zipfile))
    GTFSwizard::write_gtfs(merged_router, zipfile)
  }

  message(sprintf("FINISHED :)"))
  return(zipfile)
}
