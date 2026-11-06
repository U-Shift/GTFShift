#' Filter GTFS feed by mode
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param modes Integer[]. A list with the ids of modes to consider.
#'
#' @details
#' Allows to filter a GTFS feed for the type of transportation used, allowing for a more narrow analysis of multimodal files.
#' Refer to \code{routes.txt} \code{route_type} parameter on
#' \href{https://gtfs.org/documentation/schedule/reference/#routestxt}{GTFS documentation} for more details.
#'
#' @returns tidygtfs. The filtered GTFS feed.
#'
#' @examples
#' # Load sample feed with multiple modes
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_merged_sample.zip", package = "GTFShift")
#' )
#' 
#' gtfs$routes |> dplyr::select(route_id, route_type)
#' 
#' summary(gtfs)
#' 
#' 
#' # Filter by bus mode (ferry agency should be excluded)
#' gtfs_bus <- gtfs |> GTFShift::filter_by_modes(modes = c(3))
#' 
#' gtfs_bus$routes |> dplyr::select(route_id, route_type)
#' 
#' summary(gtfs_bus)
#'
#' @import tidytransit
#' @import dplyr
#' @importFrom rlang .data
#' @srrstats {G2.0} Validates that `modes` is a non-empty vector or list (`length(modes) > 0`).
#' @srrstats {G2.0a} Documents that `modes` is a vector/list of integer mode IDs.
#'
#' @export
filter_by_modes <- function(gtfs, modes=list()) {
  if (length(modes) == 0) {
    stop("modes must be a non-empty vector or list of mode IDs")
  }

  # Get routes that match query
  routes = gtfs$routes |>
    filter(
      .data$route_type %in% modes
    )

  # Get trips that match those routes
  trips = gtfs$trips |>
    filter(.data$route_id %in% routes$route_id)

  # Filter feed by trip id
  gtfs_filtered = tidytransit::filter_feed_by_trips(gtfs, trip_ids = trips$trip_id)

  return(gtfs_filtered)
}
