#' Aggregate lines based on overlap with simplified network
#'
#' @param network sf. A spatial object representing the simplified network.
#' @param df sf. A spatial object representing the lines to aggregate.
#' @param attr String. The attribute to aggregate the lines by.
#' @param network_segment_length Integer. (Default 100) If not NA, network is split in segments of defined meters.
#' @param fun. Method. (Default sim) Function to summarise the attributes by.
#'
#' @details
#' This method allows for the lines aggregation. Given a simplified network, it identifies (using `stplanr::rnet_join()`)
#' the segments corresponding to each line and uses them to aggregate the attribute defined in the parameters.
#'
#'
#' @returns A spatial object of the network provided, extended with the aggregated values. A `segment` column will be also added to the network, as a unique identifier used for the aggregation process.
#'
#' @examples
#' \dontrun{
#' network = st_read("network_centerlines.gpkg")
#' frequency_analysis <- GTFShift::get_route_frequency_hourly(gtfs)
#' GTFShift::network_overline(network, frequency_analysis %>% filter(arrival_hour==8), attr = "frequency")
#' }
#'
#' @seealso [stplanr::rnet_join]
#'
#' @import stplanr
#' @import sf
#' @import dplyr
#'
#' @export
network_overline <- function(network, df, attr, network_segment_length=100, fun=sum) {
  # 1. Prepare network
  network_line = stplanr::line_cast(st_transform(network, crs=3857))
  if (!is.na(network_segment_length)) {
    network_segmented = stplanr::line_segment(network_line, segment_length=network_segment_length) %>% mutate(segment=row_number())
  } else {
    network_segmented = network_line
  }

  df = df %>% st_transform(crs=3857) %>% mutate(df_id=row_number())

  # 2. Overlap df and network segments
  df_network_match = rnet_join(rnet_x = df,
     rnet_y = network_segmented |> select(segment),
     length_y = FALSE,
     key_column = "df_id",
     dist = 10
  ) |> st_drop_geometry()

  df_network_attr = df_network_match %>%
    left_join(df %>% st_drop_geometry() %>% select(attr, df_id), by="df_id")

  # 2. Group attr by segment
  df_network_segment = df_network_attr %>% select(segment, attr) %>% group_by(segment) %>% summarise(!!attr:=fun(frequency))

  # 3. Get geometry back
  result = network_segmented %>%
    filter(segment %in% df_network_segment$segment) %>%
    left_join(df_network_segment, by="segment")

  return(result)
}
