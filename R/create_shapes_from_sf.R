#' Build shapes from simple feature object
#'
#' @param sf_shapes sf object associating \code{shape_id} with an sf object (either LINESTRING or MULTILINESTRING).
#' @param gtfs tidygtfs. GTFS feed.
#' @param metric_crs numeric (Default 3857). EPSG code for a metric CRS used when computing distances (passed to \code{multiline_to_sorted_linestring}).
#' @param shape_dist_traveled Boolean (Default FALSE). If TRUE, computes
#'   \code{shape_dist_traveled} for each generated shape.
#'
#' @details
#' This function builds the shapes.txt file from a simple feature object.
#' 
#' It first converts any MULTILINESTRING geometries to LINESTRING geometries using the
#' \code{multiline_to_sorted_linestring}, using a point guide per shape:
#' all ordered stops when the selected trip is circular (first and last
#' \code{stop_id} are equal), or the first two stops otherwise.
#' Then, it converts the LINESTRING geometries to a data.table representing a GTFS shapes table using
#' \code{gtfstools::convert_sf_to_shapes}.
#' 
#' Coordinates are 4326 (WGS 84) by default, following GTFS specifications.
#' 
#' Optionally, when \code{shape_dist_traveled = TRUE}, it estimates cumulative
#' distance along each shape for all generated points and appends this as
#' \code{shape_dist_traveled}. This metric is computed in the units of 
#' \code{metric_crs}, using \code{GTFShift::project_points_along_geometry()}.
#' 
#'
#' @returns A \code{data.table} representing a GTFS shapes table. Includes
#' \code{shape_dist_traveled} if \code{shape_dist_traveled = TRUE}.
#'
#' @examples
#' # Load sample GTFS
#' gtfs <- GTFShift::load_feed(system.file("extdata", "gtfs_tcb_sample.zip", package = "GTFShift"))
#' 
#' # Load TCB OSM routes sample linestring
#' osm_routes = sf::st_read(system.file("extdata", "osm_routes_tcb.gpkg", package = "GTFShift")) |> 
#'   dplyr::filter(shape_id %in% gtfs$shapes$shape_id) |> dplyr::sample_n(1)
#' 
#' head(osm_routes)
#' 
#' # Create shapes.txt for geometries
#' shapes_txt <- GTFShift::create_shapes_from_sf(osm_routes, gtfs, metric_crs = 3763, shape_dist_traveled = TRUE)
#' 
#' head(shapes_txt)
#'
#' @import sf
#' @import gtfstools
#' @import dplyr
#'
#' @seealso \code{gtfstools::convert_sf_to_shapes()}
#' @seealso \code{GTFShift::multiline_to_sorted_linestring()}
#' @seealso \code{GTFShift::project_points_along_geometry()}
#'
#' @export
create_shapes_from_sf <- function(
    sf_shapes, 
    gtfs, 
    metric_crs = 3857,
    shape_dist_traveled = FALSE
) {
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

    # Build point guides per shape
    # > circular trip: all stops in sequence
    # > non-circular trip: first two stops
    trips_stops_sf <- gtfs$stop_times |>
        arrange(trip_id, stop_sequence) |>
        left_join(gtfs$trips |> select(trip_id, shape_id), by = "trip_id") |>
        left_join(gtfs$stops |> select(stop_id, stop_name, stop_lat, stop_lon), by = "stop_id") |>
        st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

    trips_points <- split(trips_stops_sf, trips_stops_sf$trip_id) |>
        lapply(function(trip_sf) {
            is_circular <- nrow(trip_sf) > 1 && trip_sf$stop_id[1] == trip_sf$stop_id[nrow(trip_sf)]
            trip_points <- if (is_circular) sf::st_geometry(trip_sf) else sf::st_geometry(trip_sf)[seq_len(min(2, nrow(trip_sf)))]

            data.frame(
                trip_id = trip_sf$trip_id[1],
                shape_id = trip_sf$shape_id[1],
                is_circular = is_circular,
                stringsAsFactors = FALSE,
                points = I(list(trip_points))
            )
        }) |>
        bind_rows()

    shapes_points <- trips_points |>
        arrange(shape_id, desc(is_circular), trip_id) |>
        group_by(shape_id) |>
        slice(1) |>
        ungroup() |>
        select(shape_id, points)

    # Convert MULTILINESTRING to LINESTRING
    current_geom_col <- attr(sf_shapes, "sf_column")
    sf_shapes_linestrings <- sf_shapes |>
        left_join(
            shapes_points,
            by = "shape_id"
        ) |>
        filter(lengths(points) > 0) |> # Only consider sf_shapes that have a GTFS match
        # sample_n(10) |> # For debug only
        rowwise() |>
        mutate(!!current_geom_col := multiline_to_sorted_linestring(
            multilinestring = .data[[current_geom_col]],
            points = points,
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
        sf_shapes_linestrings |> st_transform(4326), # Use WGS 84 as default CRS for GTFS shapes
        calculate_distance = FALSE
    )

    if (isTRUE(shape_dist_traveled)) {
        shapes_points_sf <- sf::st_as_sf(
            shapes_gtfstools,
            coords = c("shape_pt_lon", "shape_pt_lat"),
            crs = 4326,
            remove = FALSE
        )

        shape_dist_values <- rep(NA_real_, nrow(shapes_gtfstools))

        for (shape_id_i in unique(shapes_gtfstools$shape_id)) {
            shape_rows <- which(shapes_gtfstools$shape_id == shape_id_i)
            shape_geometry <- sf_shapes_linestrings |> filter(shape_id == shape_id_i) |> st_transform(4326)

            if (nrow(shape_geometry) == 0 || length(shape_rows) == 0) {
                next
            }

            projected <- project_points_along_geometry(
                geometry = shape_geometry,
                points = shapes_points_sf[shape_rows, ],
                geometry_sample_meters = 10,
                metric_crs = metric_crs
            )

            shape_dist_values[shape_rows] <- as.numeric(projected$distance_along_geometry)
        }

        shapes_gtfstools$shape_dist_traveled <- shape_dist_values
    }

    return(shapes_gtfstools)
}
