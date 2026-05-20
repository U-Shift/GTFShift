#' Build shapes from simple feature object
#'
#' @param sf_shapes sf object associating \code{shape_id} with an sf object (either LINESTRING or MULTILINESTRING)
#'
#' @details
#' This function builds the shapes.txt file from a simple feature object.
#' It first converts any MULTILINESTRING geometries to LINESTRING geometries using the
#' \code{multiline_to_sorted_linestring} Then, it converts the LINESTRING geometries to
#' a data.table representing a GTFS shapes table using \code{gtfstools::convert_sf_to_shapes}.
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
#' gtfs$shapes <- GTFShift::create_shapes_from_sf(shapes_sf)
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
create_shapes_from_sf <- function(sf_shapes) {
    # Initial validations
    if (!"shape_id" %in% names(sf_shapes)) {
        stop("The sf_shapes object must contain a \"shape_id\" column.")
    }

    # Convert MULTILINESTRING to LINESTRING
    current_geom_col <- attr(sf_shapes, "sf_column")
    sf_shapes_linestrings <- sf_shapes |>
        # sample_n(10) |> # For debug only
        rowwise() |>
        mutate(!!current_geom_col := multiline_to_sorted_linestring(.data[[current_geom_col]]))

    # Convert LINESTRING to GTFS shapes.txt data.frame
    shapes_gtfstools <- gtfstools::convert_sf_to_shapes(
        sf_shapes_linestrings |> st_transform(4326),
        calculate_distance = FALSE
    )

    return(shapes_gtfstools)
}
