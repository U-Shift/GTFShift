#' GTFS download and validate integrity
#'
#' Download GTFS file fixing any irregularities.
#'
#' @param gtfs_location String. The location of the GTFS zip file. Either local or URL.
#' @param zipfile String. Path to the zip file the feed should be written to. The file is overwritten if it already exists.
#'
#' @details
#' In addition to downloading the GTFS feed zip file, this method validates its integrity and applies the proper corrections if it does not comply with the following validations:
#' - `stop_times.txt` with empty `arrival_time` or `departure_time`, filtering rows that do not comply;
#' - Feeds with missing `shapes.txt` file, generating it with `GTFShift::create_shapes`.
#'
#' @returns The path of the file written.
#'
#' @seealso [GTFShift::create_shapes()]
#'
#' @examples
#' \dontrun{
#' download("https://operator.com/gtfs.zip", "operator_gtfs.zip")
#' }
#'
#' @import tidytransit
#'
#' @export
download <- function(gtfs_location, zipfile) {

  message(sprintf("Downloading GTFS file for %s...", gtfs_location))

  # DOWNLOAD GTFS
  if (!dir.exists(dirname(zipfile))) {
    dir.create(dirname(zipfile), recursive = TRUE)
  }
  gtfs <- tidytransit::read_gtfs(gtfs_location)

  # VALIDATE integrity

  ## Clean empty stop_times arrival/departure (happened with Cascais GTFS) which raises an error at filter_feed_by_date method
  stopsNPrev <- length(gtfs$stop_times$trip_id)
  gtfs$stop_times <- gtfs$stop_times[!is.na(gtfs$stop_times$arrival_time), ]
  stopsNAfter <- length(gtfs$stop_times$trip_id)
  if (stopsNPrev != stopsNAfter) {
    warning(sprintf("> FIXED GTFS, there were %d stop times without arrival time!", stopsNPrev-stopsNAfter))
  }

  # STORE GTFS (zip generated here, because next validation uses GTFSWizard and not tidytransit, which are incompatible)
  tidytransit::write_gtfs(gtfs, zipfile)

  ## If no shapes.txt, create them automatically with GTFSwizard
  if (!("shapes" %in% names(gtfs))) {
    create_shapes(zipfile, zipfile)
    warning(sprintf("> CREATED shapes.txt, the file as missing!"))
  }

  return(zipfile)
}
