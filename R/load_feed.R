#' Read GTFS feed, fixing integrity errors
#'
#' @param path String. The location of the GTFS zip file. Either local or URL.
#' @param store_path. String. (Optional) If provided, GTFS feed zip is stored at location. The file is overwritten if it already exists.
#'
#' @details
#' In addition to loading the GTFS feed, this method validates its integrity and applies the proper corrections if it does not comply with the following validations:
#' \itemize{
#'  \item `stop_times.txt` with empty `arrival_time` or `departure_time`, filtering rows that do not comply.
#'  \item Feeds with missing `shapes.txt` file, generating it using `GTFSwizard::get_shapes()`.
#' }
#'
#' @returns A tidygtfs object.
#'
#' @seealso [tidytransit::read_gtfs()]
#' @seealso [GTFSwizard::get_shapes()]
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::load_feed("https://operator.com/gtfs.zip")
#' }
#'
#' @import tidytransit
#'
#' @export
load_feed <- function(path, store_path=NA) {

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
    gtfs_fixed <- GTFSwizard::as_wizardgtfs(gtfs)
    gtfs$shapes <- gtfs_fixed$shapes
    gtfs$trips <- gtfs_fixed$trips
    warning(sprintf("> CREATED shapes.txt, the file was missing!"))
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
