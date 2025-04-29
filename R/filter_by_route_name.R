#' Filter GTFS feed by route name
#'
#' @param gtfs tidygtfs. GTFS loaded using tidytransit::read_gtfs.
#' @param values String[]. List of the route names to filter the feed.
#' @param short_name Boolean. If true, query for route_short_name, otherwise, route_long_name is considered.
#' @param exact_match Boolean. If true, route name is queried for an exact match, otherwise, partial match is considered.
#'
#' @details
#' On a GTFS feed, the `route_id` rarely matches the real name of the route, that can range from numbers, letters, words or combinations of both.
#' This method allows to filter the feed for the route short or long name, with a partial or exact match.
#'
#' @returns A `tidygtfs` object with the filtered feed.
#'
#' @examples
#' \dontrun{
#' gtfs <- tidytransit::read_gtfs("gtfs.zip")
#' gtfs_filtered <- filter_by_route_name(gtfs, list("Blue line", "Red line"))
#' }
#'
#' @seealso [tidytransit::read_gtfs()]
#'
#' @import tidytransit
#' @import dplyr
#' @import stringr
#'
#' @export
filter_by_route_name <- function(gtfs, values, short_name=TRUE, exact_match=TRUE) {

  # Get routes that match query
  pattern <- paste(unlist(values), collapse = "|")

  routes = gtfs$routes %>%
    filter(
      if (short_name & exact_match) route_short_name %in% values
      else if (short_name) str_detect(route_short_name, regex(pattern, ignore_case = TRUE))
      else if (!short_name & exact_match) route_long_name %in% values
      else str_detect(route_long_name, regex(pattern, ignore_case = TRUE))
    )

  # Get trips that match those routes
  trips = gtfs$trips %>%
    filter(route_id %in% routes$route_id)

  # Filter feed by trip id
  gtfs_filtered = tidytransit::filter_feed_by_trips(gtfs, trip_ids = trips$trip_id)

  return(gtfs_filtered)
}
