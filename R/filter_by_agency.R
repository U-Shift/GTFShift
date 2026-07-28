#' Filter GTFS feed by agency
#'
#' @param gtfs tidygtfs. GTFS feed.
#' @param id Integer (Optional when name). Ids of the agency.
#' @param name String (Optional when id). Name of the agency.
#'
#' @details
#' Allows to filter a GTFS feed for the agency, using the id, name or both. Returns empty feed it none provided.
#'
#' @returns A tidygtfs object with the filtered feed.
#'
#' @examples
#' # Load sample feed with multiple agencies
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples", "gtfs_merged_sample.zip", package = "GTFShift"))
#' 
#' summary(gtfs)
#' 
#' 
#' # Filter by id
#' gtfs_id_8 = gtfs |> GTFShift::filter_by_agency(id = "8")
#' 
#' summary(gtfs_id_8)
#' 
#' 
#' # Filter by name 
#' gtfs_ttsl <- gtfs |> GTFShift::filter_by_agency(name = "TTSL - Transtejo Soflusa") 
#' 
#' summary(gtfs_ttsl)
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

  # Filter agency table
  routes_agencies <- unique(gtfs_filtered$routes$agency_id)
  gtfs_filtered$agency = gtfs_filtered$agency |> filter(agency_id %in% routes_agencies)

  return(gtfs_filtered)
}
