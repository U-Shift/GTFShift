#' Build shapes from simple feature object
#'
#' @param sf_shapes sf object associating \code{shape_id} with an sf object (either LINESTRING or MULTILINESTRING).
#' @param gtfs tidygtfs. GTFS feed.
#' @param metric_crs numeric (Default 3857). EPSG code for a metric CRS used when computing distances (passed to \code{multiline_to_sorted_linestring}).
#'
#' @details
#' This function builds the shapes.txt file from a simple feature object.
#' It first converts any MULTILINESTRING geometries to LINESTRING geometries using the
#' \code{multiline_to_sorted_linestring}, with the first stop of each trip as the starting point.
#' Then, it converts the LINESTRING geometries to a data.table representing a GTFS shapes table using
#' \code{gtfstools::convert_sf_to_shapes}.
#'
#' @returns A \code{data.table} representing a GTFS shapes table.
#'
#' @examples
#' \dontrun{
#' gtfs <- GTFShift::load_feed("gtfs.zip")
#' q <- opq("Lisbon") |>
#'     add_osm_feature(key = "route", value = c("bus", "tram")) |>
#'     add_osm_feature(key = "network", value = "Carris", key_exact = TRUE)
#'
#' shapes_sf <- GTFShift::osm_shapes_to_routes(gtfs, q)
#'
#' gtfs$shapes <- GTFShift::create_shapes_from_sf(shapes_sf, gtfs)
#' }
#'
#' @import sf
#' @import gtfstools
#' @import dplyr
#'
#' @seealso \code{gtfstools::convert_sf_to_shapes}
#' @seealso \code{GTFShift::multiline_to_sorted_linestring}
#'
#' @export
create_shapes_from_sf <- function(sf_shapes, gtfs, metric_crs = 3857) {
    if (missing(metric_crs)) {
        warning(
            "Using default metric_crs (EPSG:3857). Consider setting metric_crs to a projected CRS better suited to your local context for more accurate distance calculations.",
            call. = FALSE
        )
    }

    # Initial validations
    if (!"shape_id" %in% names(sf_shapes)) {
        stop("The sf_shapes object must contain a \"shape_id\" column.")
    }

    # Get first stop geometry for each shape
    shapes_stops <- gtfs$stop_times |>
        arrange(stop_sequence) |>
        group_by(trip_id) |>
        slice(1) |> # Only departures from origin (first stop)
        select(trip_id, stop_id) |>
        left_join(gtfs$trips |> select(trip_id, shape_id), by = "trip_id") |>
        left_join(gtfs$stops |> select(stop_id, stop_name, stop_lat, stop_lon), by = "stop_id") |>
        st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
        rename(stop_point = geometry)
    
    # shape_id_debug from dev/test_create_shapes_from_sf.R
    multilinestring <- (sf_shapes |> filter(shape_id==shape_id_debug))[1, ]$geom
    start_point <- (sf_shapes_linestrings |> filter(shape_id==shape_id_debug))[1, ]$stop_point
    

    # Convert MULTILINESTRING to LINESTRING
    current_geom_col <- attr(sf_shapes, "sf_column")
    sf_shapes_linestrings <- sf_shapes |>
        left_join(
            as.data.frame(shapes_stops) |>
                select(shape_id, stop_point) |>
                distinct(shape_id, .keep_all = TRUE),
            by = "shape_id"
        ) |>
        filter(!st_is_empty(stop_point)) |> # Only consider sf_shapes that have a GTFS match
        # sample_n(10) |> # For debug only
        rowwise() |>
        mutate(!!current_geom_col := multiline_to_sorted_linestring(
            multilinestring = .data[[current_geom_col]],
            start_point = stop_point,
            metric_crs = metric_crs
        )) |>
        ungroup() |>
        select(shape_id)

    #mapview::mapview(sf_shapes_linestrings |> select(-stop_point), zcol="shape_id")
    #sf_shapes_linestrings_debug = sf_shapes_linestrings|>filter(shape_id=="1-VA-TERM")
    #mapview::mapview((sf_shapes_linestrings_debug )$geom)+
    #    mapview::mapview((sf_shapes_linestrings_debug)$stop_point) 

    # Convert LINESTRING to GTFS shapes.txt data.frame
    shapes_gtfstools <- gtfstools::convert_sf_to_shapes(
        sf_shapes_linestrings |> st_transform(4326) # Use WGS 84 as default CRS for GTFS shapes
    )

    return(shapes_gtfstools)
}
