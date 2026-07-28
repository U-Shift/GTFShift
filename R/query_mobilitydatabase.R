#' Query Mobility Database API for GTFS feeds
#
#'
#' @param access_token String (Optional when refresh_token). Access token.
#' @param refresh_token String (Optional when access_token). Refresh token.
#' @param bounding_filter_method String (Default partially_enclosed). Filtering method to use with the dataset_latitudes and dataset_longitudes parameters.
#' @param limit Integer (Default 10). The number of items to be returned.
#' @param offset Integer (Default 0). Offset of the first item to return.
#' @param country_code String (Optional). Filter feeds by their exact country code.
#' @param subdivision_name String (Optional). List only feeds with the specified value. Can be a partial match.
#' @param municipality String (Optional). List only feeds with the specified value. Can be a partial match. Case insensitive.
#' @param bbox bbox (Optional). Area from which to get GTFS feeds. Converted to API dataset_latitudes and dataset_longitudes URL parameters.
#' @param is_official. Boolean (Optional). If TRUE, only return official feeds.
#'
#' @details
#' This method queries \href{https://mobilitydatabase.org/}{Mobility Database} API, allowing to get a list of GTFS feeds documented at this platform.
#' To use it, an access or a refresh token must be provided.
#' It can be obtained for free at Mobility Database website.\cr\cr
#' For more details on the parameters, refer to \url{https://mobilitydata.github.io/mobility-feed-api/SwaggerUI/index.html#/feeds/getGtfsFeeds}.\cr\cr
#' Some useful columns of the returned data.frame (refer to the API documentation for a full list) are:
#' \describe{
#'   \item{provider}{The name of the GTFS provider.}
#'   \item{status}{Tells if the feed is active, inactive or deprecated.}
#'   \item{producer_url}{The GTFS feed URL. Can be used to download.}
#' }
#'
#' @returns data.frame with query results
#'
#'
#' @examplesIf nzchar(Sys.getenv("MOBILITY_DATABASE"))
#' feeds <- GTFShift::query_mobilitydatabase(
#'   refresh_token = Sys.getenv("MOBILITY_DATABASE"),
#'   country_code = "PT",
#'   is_official = TRUE
#' )
#' 
#' head(feeds |> dplyr::select(id, provider, producer_url))
#'
#' @importFrom httr GET POST add_headers content http_error http_status
#' @import dplyr
#'
#' @export
query_mobilitydatabase <- function(access_token = NA,
  refresh_token = NA,
  bounding_filter_method = "partially_enclosed",
  limit = 10,
  offset = 0,
  country_code = NA,
  subdivision_name = NA,
  municipality = NA,
  bbox = NA,
  is_official = NA
) {
  # Validate parameters
  if (is.na(access_token) && is.na(refresh_token)) {
    stop("No token provided! At least one of the access or refresh tokens must be provided as an argument.")
  }

  # If refresh token, get access token from API
  if (is.na(access_token) && !is.na(refresh_token)) {
    body <- sprintf('{"refresh_token": "%s"}', refresh_token)
    response <- POST(
      "https://api.mobilitydatabase.org/v1/tokens",
      add_headers(`Content-Type` = "application/json"),
      body = body
    )
    content <- content(response, as = "parsed")
    if (http_error(response)) {
      stop(sprintf("Mobility database bad response: %s", http_status(response)))
    }
    access_token <- content$access_token
  }

  # Query mobility database
  url <- "https://api.mobilitydatabase.org/v1/gtfs_feeds"

  params <- list(
    bounding_filter_method = bounding_filter_method,
    limit = limit,
    offset = offset
  )
  if (!is.na(country_code)) params["country_code"] <- country_code
  if (!is.na(subdivision_name)) params["subdivision_name"] <- subdivision_name
  if (!is.na(municipality)) params["municipality"] <- municipality
  if (!is.na(bbox)) {
    params["dataset_latitudes"] <- sprintf("%f,%f", bbox$ymin[[1]], bbox$ymax[[1]])
    params["dataset_longitudes"] <- sprintf("%f,%f", bbox$xmin[[1]], bbox$xmax[[1]])
  }
  if (!is.na(is_official)) params["is_official"] <- is_official

  response <- GET(
    url,
    query = params,
    add_headers(
      `accept` = "application/json",
      `Authorization` = sprintf("Bearer %s", access_token)
    )
  )

  # Convert response to data.frame
  content <- content(response, as = "parsed")

  if (http_error(response)) {
    stop(sprintf("Mobility database bad response: %s", http_status(response)))
  }


  safe_extract <- function(x) if (is.null(x)) NA else x

  feeds_df <- bind_rows(lapply(content, function(feed) {
    data.frame(
      id = safe_extract(feed$id),
      data_type = safe_extract(feed$data_type),
      created_at = safe_extract(feed$created_at),
      provider = safe_extract(feed$provider),
      feed_contact_email = safe_extract(feed$feed_contact_email),
      status = safe_extract(feed$status),
      official = safe_extract(feed$official),
      official_updated_at = safe_extract(feed$official_updated_at),
      feed_name = safe_extract(feed$feed_name),
      note = safe_extract(feed$note),

      # Flatten nested 'source_info'
      producer_url = safe_extract(feed$source_info$producer_url),
      license_url = safe_extract(feed$source_info$license_url),

      # First location (if exists)
      country_code = if (length(feed$locations) >= 1) safe_extract(feed$locations[[1]]$country_code) else NA,
      country = if (length(feed$locations) >= 1) safe_extract(feed$locations[[1]]$country) else NA,
      subdivision_name = if (length(feed$locations) >= 1) safe_extract(feed$locations[[1]]$subdivision_name) else NA,
      municipality = if (length(feed$locations) >= 1) safe_extract(feed$locations[[1]]$municipality) else NA,

      # Flatten latest_dataset (may be NULL)
      latest_dataset_id = safe_extract(feed$latest_dataset$id),
      latest_dataset_url = safe_extract(feed$latest_dataset$hosted_url),
      min_lat = safe_extract(feed$latest_dataset$bounding_box$minimum_latitude),
      max_lat = safe_extract(feed$latest_dataset$bounding_box$maximum_latitude),
      min_lon = safe_extract(feed$latest_dataset$bounding_box$minimum_longitude),
      max_lon = safe_extract(feed$latest_dataset$bounding_box$maximum_longitude),
      downloaded_at = safe_extract(feed$latest_dataset$downloaded_at),
      service_start = safe_extract(feed$latest_dataset$service_date_range_start),
      service_end = safe_extract(feed$latest_dataset$service_date_range_end),
      agency_timezone = safe_extract(feed$latest_dataset$agency_timezone),

      # Validation summary
      validation_errors = safe_extract(feed$latest_dataset$validation_report$total_error),
      validation_warnings = safe_extract(feed$latest_dataset$validation_report$total_warning),
      stringsAsFactors = FALSE
    )
  }))

  return(feeds_df)
}
