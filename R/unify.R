#' Merge multiple GTFS into a single aggregated file
#'
#' @param ... tidygtfs[]. List of GTFS feeds.
#' @param prefix Boolean (Default FALSE). If TRUE, prefixes all tables with the agency_id, to avoid conflicts in case of same IDs in different feeds.
#' @param store_path String (Optional). If provided, aggregated feed zip is stored at location. The file is overwritten if it already exists.
#' @param create_transfers Boolean (Default FALSE). When true, generates transfers table, aggregating close stops, even if from different GTFS.
#' @param transfer_distance Integer (Default 300). Upper straight-line distance limit in meters for transfers.
#' @param transfer_time Integer (Default 120). Minimum time in seconds for transfers; all values below this will be replaced with this value, particularly all those defining in-place transfers where stop longitudes and latitudes remain identical.
#' @param transfer_street_routing Boolean (Default FALSE). If TRUE, transfer times are calculated by routing throughout the underlying street network (downloaded automatically).
#'
#' @details
#' Aggregates multiple feeds using \code{gtfstools::merge_gtfs()}.
#' When generating transfers, those already existing in each GTFS file are kept, extended with new ones computed based
#' on the stops network of the final aggregated version.
#'
#' This computation is executed with \code{gtfsrouter::gtfs_transfer_table()},
#' with the parameters \code{d_limit=transfer_distance}, \code{min_transfer_time=transfer_time} and
#' \code{network_times=transfer_street_routing}. The other parameters are applied the library default values.
#'
#' For a detailed example, see the \code{vignette("unify")}.
#'
#' @returns tidygtfs. The unified GTFS feed.
#'
#' @examples
#' # Load multiple GTFS files
#' gtfs_1 <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_tcb_sample.zip", package = "GTFShift")
#' )
#' 
#' summary(gtfs_1)
#' 
#' gtfs_1$agency
#' 
#' head(gtfs_1$trips)
#' 
#' gtfs_2 <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_ttsl_sample_no_shapes.zip", package = "GTFShift")
#' )
#' 
#' summary(gtfs_2)
#' 
#' gtfs_2$agency
#' 
#' head(gtfs_2$trips)
#' 
#' # Unify them
#' unified <- GTFShift::unify(gtfs_1, gtfs_2, prefix = TRUE)
#' 
#' summary(unified)
#' 
#' unified$agency
#' 
#' head(unified$trips)
#'
#' @seealso \code{gtfstools::merge_gtfs()}
#' @seealso \code{gtfsrouter::gtfs_transfer_table()}
#'
#' @importFrom gtfstools merge_gtfs
#'
#' @export
unify <- function(..., prefix = FALSE, store_path = NA, create_transfers = FALSE, transfer_distance = 300, transfer_time = 120, transfer_street_routing = FALSE) {
  gtfss <- list(...)

  # Merge them
  message(sprintf("1. Starting merge process..."))
  prefix_arg <- FALSE
  if (prefix) prefix_arg <- unlist(lapply(gtfss, function(feed) paste(feed$agency$agency_id, collapse = "_")))
  gtfs <- gtfstools::merge_gtfs(
    gtfss,
    prefix = prefix_arg
  ) # https://ipeagit.github.io/gtfstools/reference/merge_gtfs.html
  gtfs <- tidytransit::as_tidygtfs(gtfs) # Get back to tidytransit format

  # Generate transfers.txt
  if (create_transfers) {
    message(sprintf("2. Generating transfers..."))

    if (!requireNamespace("gtfsrouter", quietly = TRUE)) {
      stop("Package 'gtfsrouter' is required to generate transfers. Install it with: install.packages('gtfsrouter')")
    }

    # Store in  temporary file because gtfsrouter can only read files
    temp_dir <- tempfile()
    dir.create(temp_dir)
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
    message(sprintf("3. Storing file..."))
    if (!dir.exists(dirname(store_path))) {
      dir.create(dirname(store_path), recursive = TRUE)
    }
    tidytransit::write_gtfs(gtfs, store_path)
  }

  message(sprintf("FINISHED :)"))
  return(gtfs)
}
