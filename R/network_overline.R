#' Aggregate lines based on overlap with target network
#'
#' @param target_network sf. A spatial object representing the target network.
#' @param lines sf. A spatial object representing the lines to aggregate.
#' @param attr String. The attribute to aggregate the lines by.
#' @param target_network_split Integer (Default 100). If not NA, network is split in segments of defined meters.
#' @param fun Method (Default \code{base::sum}). Function to summarise the attributes by.
#' @param join_dist Integer (Default 10). Meters to consider when joining routes and network segments.
#' @param metric_crs Integer or character (Default NULL). Optional projected CRS used to compute segment lengths and join distances in meters. If NULL, spherical geometry (s2) is used by default.
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
#' @returns sf. Spatial network object extended with aggregated values.
#'
#' @examples
#' \donttest{
#' # Subset GTFS for one route only, for demo purposes
#' gtfs <- GTFShift::load_feed(system.file("extdata/samples",
#'   "gtfs_tcb_sample.zip", package = "GTFShift")
#' )
#' gtfs <- GTFShift::filter_by_route_name(gtfs, c("4", "1"))
#' 
#' # Load OSM network to serve as target network
#' target_network = sf::st_read(
#'   system.file("extdata/samples", "osm_ways_tcb.gpkg", package = "GTFShift"),
#'   quiet = TRUE
#' )
#' 
#' head(target_network)
#' 
#' # Get route frequency (and geometry)
#' frequency_analysis <- GTFShift::get_route_frequency_hourly(
#'   gtfs, 
#'   date = gtfs$calendar$start_date[1]
#' ) |> 
#' dplyr::group_by(shape_id) |>
#' dplyr::summarize(frequency = max(frequency))
#' 
#' head(frequency_analysis)
#' 
#' # Aggregate frequencies based on geometry overlap using GTFShift::network_overline
#' suppressWarnings({ 
#'   overline <- GTFShift::network_overline(
#'     target_network = target_network, 
#'     lines = frequency_analysis, 
#'     attr = "frequency"
#'   )
#' })
#' 
#' head(overline |> sf::st_drop_geometry())
#' }
#'
#' @seealso \code{stplanr::rnet_join()}
#'
#' @import sf
#' @import dplyr
#' @importFrom rlang .data
#' @srrstats {G1.1} Documents how this function improves upon `stplanr::overline2()` by aggregating arbitrary route attributes onto a segmented target network using `stplanr::rnet_join()` and a customizable summary function `fun`.
#' @srrstats {EA2.3} Accepts user-specified attribute column `attr` and aggregation function `fun` rather than assuming fixed column names.
#' @srrstats {EA4.0} Returns an `sf` network object matching the class of `target_network`.
#' @srrstats {SP1.0} Applies to both curvilinear/geographic (`metric_crs = NULL`) and Cartesian/projected (`metric_crs`) spatial domains.
#' @srrstats {SP1.1} Operates on two-dimensional (`XY`) linear network geometries.
#' @srrstats {SP2.2} Integrates with `stplanr::line_cast()`, `stplanr::line_segment()`, and `stplanr::rnet_join()`.
#' @srrstats {SP2.2a} Demonstrates embedding `GTFShift` route frequency outputs into `stplanr` road network workflows.
#' @srrstats {SP2.3} Examples load the target road network from a `.gpkg` file via `sf::st_read()`.
#' @srrstats {SP2.4} Uses `sf::st_crs()` and `sf::st_transform()` compliant with `PROJ >= 6`.
#' @srrstats {SP2.4a} Validates `metric_crs` via `WKT2` (`sf::st_crs()`).
#' @srrstats {SP2.8} Harmonizes CRS between `target_network` and `lines` prior to spatial joining.
#' @srrstats {SP2.9} Transforms the aggregated `result` back to `original_crs` before returning.
#' @srrstats {SP4.0a} Returns an `sf` object matching `target_network`.
#' @srrstats {SP4.2} Explicitly documents the returned `sf` object.
#'
#' @export
network_overline <- function(
    target_network,
    lines,
    attr,
    target_network_split=100,
    fun=sum,
    join_dist=10,
    metric_crs = NULL
) {
  if (!requireNamespace("stplanr", quietly = TRUE)) {
    stop("Package 'stplanr' is required for this function. Install it with: install.packages('stplanr')")
  }
  original_crs <- st_crs(target_network)
  if (!is.null(metric_crs)) {
    metric_crs <- suppressWarnings(sf::st_crs(metric_crs))
    if (is.na(metric_crs)) {
      stop("metric_crs should be a valid CRS value (e.g., 3857 or 'EPSG:3857')")
    }
    target_network <- st_transform(target_network, crs = metric_crs)
    lines <- st_transform(lines, crs = metric_crs)
  } else if (!is.na(original_crs) && !is.na(st_crs(lines)) && st_crs(lines) != original_crs) {
    lines <- st_transform(lines, crs = original_crs)
  }

  # 1. Prepare network
  network_line = stplanr::line_cast(target_network)
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
    mutate(df_id=row_number())

  # 2. Overlap df and network segments
  df_network_match = stplanr::rnet_join(
    rnet_x = df,
    rnet_y = network_segmented |>
      select("segment"),
    length_y = FALSE,
    key_column = "df_id",
    dist = join_dist,
    crs = st_crs(df)
  ) |> st_drop_geometry()

  df_network_attr = df_network_match |>
    left_join(df |>
                st_drop_geometry() |>
                select(all_of(attr), "df_id"),
              by = "df_id")

  # 3. Group attr by segment
  df_network_segment = df_network_attr |>
    select("segment", all_of(attr)) |>
    group_by(.data$segment) |>
    summarise(!!attr := fun(.data[[attr]]))

  # 4. Get geometry back
  result = network_segmented |>
    filter(.data$segment %in% df_network_segment$segment) |>
    left_join(df_network_segment, by="segment") |>
    select(-"segment")
  if (!is.na(original_crs) && st_crs(result) != original_crs) {
    result <- result |> st_transform(crs = original_crs)
  }

  return(result)
}
