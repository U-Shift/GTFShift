#' Build shapes from GTFS stops data
#'
#' @param gtfs tidygtfs. GTFS feed.
#'
#' @details
#' The function builds the shapes.txt file from the stop_times.txt and stops.txt files, by grouping trips with the same stop sequence and assigning them the same shape_id.
#' The resulting shapes are a simplified version of the original ones, as they do not take into account the actual path followed by the vehicles, but only the stop sequence.
#' This can be useful for some applications that do not require high precision in the shapes, and can be used as a fallback when the original feed does not include shapes.txt file.
#'
#' @returns The gtfs feed with the shapes table defined and the trips table updated with the matching shape_id.
#'
#' @examples
#' # Load GTFS without shapes
#' gtfs <- tidytransit::read_gtfs(system.file("extdata/samples", "gtfs_ttsl_sample_no_shapes.zip", package = "GTFShift"))
#' 
#' summary(gtfs)
#' 
#' # Create shapes from GTFS stops data
#' gtfs_with_shapes <- GTFShift::create_shapes_from_stops(gtfs)
#' 
#' head(gtfs_with_shapes$shapes)
#' 
#' head(gtfs_with_shapes$trips |> dplyr::select(trip_id, shape_id) |> dplyr::distinct(shape_id, .keep_all = TRUE))
#' 
#' summary(gtfs_with_shapes)
#'
#' @import dplyr
#' @importFrom tidyr unnest
#'
#' @export
create_shapes_from_stops <- function(gtfs) {
  if ("shapes" %in% names(gtfs)) {
    warning("The GTFS feed already has shapes defined! Overriding it...")
  }

  # Get stop_sequence_str for each trip (each will be a different shape)
  shapes_trips <- gtfs$stop_times |>
    select(trip_id, stop_id, stop_sequence) |>
    arrange(trip_id, stop_sequence) |>
    left_join(gtfs$stops |> select(stop_id, stop_lon, stop_lat), by = "stop_id") |>
    group_by(trip_id) |>
    arrange(stop_sequence) |>
    # Create string with stop_id sequence for each trip, to be used as a key to group trips with the same stop sequence
    mutate(stop_sequence_str = paste(stop_id, collapse = "-")) |>
    ungroup()

  # Get unique stop_sequence_str
  shapes_trips_geom <- shapes_trips |>
    select(stop_sequence_str, stop_id, stop_sequence, stop_lon, stop_lat) |>
    distinct()

  # Gnerate shape_id
  shapes <- shapes_trips |>
    group_by(stop_sequence_str) |>
    reframe(
      trip_id = list(trip_id)
    ) |>
    mutate(
      shape_id = paste0("shape-", 1:n())
    )

  # Asssociate trips to shape_id
  gtfs$trips <-
    gtfs$trips |>
    select(-shape_id) |>
    left_join(shapes |> tidyr::unnest(cols = "trip_id") |> select(trip_id, shape_id) |> distinct(), join_by(trip_id))

  # Gather shape_id and shape geometry (from shapes_trips_geom)
  gtfs$shapes <- shapes |>
    select(-trip_id) |>
    left_join(shapes_trips_geom, by = "stop_sequence_str") |>
    select(-stop_sequence_str, -stop_id) |>
    rename(
      shape_pt_lat = stop_lat,
      shape_pt_lon = stop_lon,
      shape_pt_sequence = stop_sequence
    )
  
  return(gtfs)
}
