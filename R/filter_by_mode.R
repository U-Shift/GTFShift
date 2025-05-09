#' Filter GTFS feed by mode
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param modes Integer[]. A list with the ids of modes to consider.
#'
#' @details
#' Allows to filter a GTFS feed for the type of transportation used, allowing for a more narrow analysis of multimodal files.
#' Refer to routes.txt `route_type` parameter on \href{https://gtfs.org/documentation/schedule/reference/#routestxt}{GTFS documentation} for more details.
#'
#' @returns A `tidygtfs` object with the filtered feed.
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::load_feed("gtfs.zip")
#' gtfs_filtered <- GTFShift::filter_by_modes(gtfs, list(0,1))
#' }
#'
#' @import tidytransit
#' @import dplyr
#'
#' @export
filter_by_modes <- function(gtfs, modes=list()) {

  # Get routes that match query
  routes = gtfs$routes %>%
    filter(
      route_type %in% modes
    )

  # Get trips that match those routes
  trips = gtfs$trips %>%
    filter(route_id %in% routes$route_id)

  # Filter feed by trip id
  gtfs_filtered = tidytransit::filter_feed_by_trips(gtfs, trip_ids = trips$trip_id)

  return(gtfs_filtered)
}
