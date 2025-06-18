#' Aggregate lines based on overlap with target network
#'
#' @param target_network sf. A spatial object representing the target network.
#' @param lines sf. A spatial object representing the lines to aggregate.
#' @param attr String. The attribute to aggregate the lines by.
#' @param target_network_split Integer (Default 100). If not NA, network is split in segments of defined meters.
#' @param fun Method (Default \code{base::sum}). Function to summarise the attributes by.
#' @param join_dist Integer (Default 10). Meters to consider when joining routes and network segments.
#'
#' @details
#' This method allows for the lines aggregation. Given a target network, it identifies (using \code{stplanr::rnet_join()})
#' the segments corresponding to each line and uses them to aggregate the attribute defined in the parameters.
#'
#' It provides an alternative to \code{GTFShift::get_route_frequency_hourly()} with the attribute \code{overline=TRUE}, which
#' creates an aggregated network based on the lines overlap. Instead, \code{GTFShift::network_overline()} finds, for each network
#' segment, the overlapping lines and aggregates their \code{attr} values, using \code{fun}.
#'
#'
#' @returns A spatial object of the target network, extended with the aggregated values.
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::load_feed("https://operator.com/gtfs.zip")
#' target_network = st_read("network_centerlines.gpkg")
#' frequency_analysis <- GTFShift::get_route_frequency_hourly(gtfs, overline=FALSE)
#' GTFShift::network_overline(
#'   target_network,
#'   frequency_analysis |> filter(arrival_hour==8),
#'   attr = "frequency"
#' )
#' }
#'
#' @seealso [stplanr::rnet_join]
#'
#' @import stplanr
#' @import sf
#' @import dplyr
#'
#' @export
network_overline <- function(
    target_network,
    lines,
    attr,
    target_network_split=100,
    fun=sum,
    join_dist=10
) {
  # 1. Prepare network
  network_line = stplanr::line_cast(st_transform(target_network, crs=3857))
  if (!is.na(target_network_split)) {
    network_segmented = stplanr::line_segment(
      network_line,
      segment_length=target_network_split
    ) |> mutate(segment=row_number())
  } else {
    network_segmented = network_line |>
      mutate(segment = row_number())
  }

  df = lines |>
    st_transform(crs=3857) |>
    mutate(df_id=row_number())

  # 2. Overlap df and network segments
  df_network_match = rnet_join(
    rnet_x = df,
    rnet_y = network_segmented |>
      select(segment),
    length_y = FALSE,
    key_column = "df_id",
    dist = join_dist
  ) |> st_drop_geometry()

  df_network_attr = df_network_match |>
    left_join(df |>
                st_drop_geometry() |>
                select(attr, df_id),
              by = "df_id")

  # 3. Group attr by segment
  df_network_segment = df_network_attr |>
    select(segment, attr) |>
    group_by(segment) |>
    summarise(!!attr := fun(frequency))

  # 4. Get geometry back
  result = network_segmented |>
    filter(segment %in% df_network_segment$segment) |>
    left_join(df_network_segment, by="segment") |>
    select(-segment)

  return(result)
}
