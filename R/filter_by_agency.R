#' Filter GTFS feed by agency
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param id Integer. Ids of the agency (conditionally required).
#' @param name String. Name of the agency (conditionally required).
#'
#' @details
#' Allows to filter a GTFS feed for the agency, using the id, name or both. Returns empty feed it none provided.
#'
#' @returns A `tidygtfs` object with the filtered feed.
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::download_feed("gtfs.zip")
#' gtfs_filtered_by_id <- GTFShift::filter_by_agency(gtfs, agency_id=2)
#' gtfs_filtered_by_name <- GTFShift::filter_by_agency(gtfs, agency_name="City bus company")
#' }
#'
#' @import tidytransit
#' @import dplyr
#'
#' @export
filter_by_agency <- function(gtfs, id=NA, name=NA) {

  # Get agencies that match query
  agencies = gtfs$agency %>%
      filter(
        if (!is.na(id) & !is.na(name)) agency_id==id && agency_name==name
        else if (!is.na(id)) agency_id==id
        else if (!is.na(name)) agency_name==name
        else FALSE
      )

  # Get routes that match query
  routes = gtfs$routes %>%
    filter(
      agency_id %in% agencies$agency_id
    )

  # Get trips that match those routes
  trips = gtfs$trips %>%
    filter(route_id %in% routes$route_id)

  # Filter feed by trip id
  gtfs_filtered = tidytransit::filter_feed_by_trips(gtfs, trip_ids = trips$trip_id)

  return(gtfs_filtered)
}
