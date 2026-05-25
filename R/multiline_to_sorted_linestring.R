#' Convert a MULTILINESTRING to a sorted LINESTRING
#'
#' @param multilinestring sf object with MULTILINESTRING geometry
#' @param start_point (Optional) sf point geometry. If provided, the sorting of the linestrings
#'   will start from this point.
#'
#' @details
#' The function takes a MULTILINESTRING object and converts it to a LINESTRING object
#' by sorting the linestrings and combining them in the correct order.
#'
#' The sorting criteria is based on the geometric distance between the endpoint of the current linestring
#' and the endpoints of the remaining linestrings. The linestring with the minimum distance
#' to the endpoint of the current linestring is selected as the next linestring.
#' The algorithm starts either from the \code{start_point} (if provided) or from the first linestring
#' in the MULTILINESTRING object and continues until all linestrings are sorted.
#'
#' @returns A \code{sf} object with LINESTRING geometry.
#'
#' @import dplyr
#' @import sf
#' @import lwgeom
#'
#' @export
multiline_to_sorted_linestring <- function(multilinestring, start_point = NULL) {
    # browser()

    # 1. Extract all individual LINESTRING components
    linestrings <- st_cast(multilinestring, "LINESTRING") |>
        st_as_sf() |>
        st_set_geometry("geometry")

    # 2. Find the correct order by connecting endpoints
    #    - Get all start and end points
    linestrings <- linestrings |>
        mutate(
            start = lwgeom::st_startpoint(geometry),
            end = lwgeom::st_endpoint(geometry)
        )

    # 3. Reorder the linestrings by finding the best sequence

    # Find the best path (simplified approach)
    # (This part may need adjustment based on your data)
    ordered_lines <- list()
    # If start_point is provided, find the line that is closest to the start_point
    if (!is.null(start_point)) {
        nearest_idx <- st_nearest_feature(start_point, linestrings)
        current_line <- linestrings[nearest_idx, ]
        current_start <- lwgeom::st_startpoint(current_line$geometry)
        current_end <- lwgeom::st_endpoint(current_line$geometry)
        # If start_point is closest to line end point than start point, invert geometry
        if (st_distance(start_point, current_start) > st_distance(start_point, current_end)) {
            current_line$geometry = st_reverse(current_line$geometry)
        }
    } else {
        current_line <- linestrings[1, ] # Start with the first line
    }
    ordered_lines[[1]] <- current_line$geometry
    remaining_lines <- linestrings[-1, ]

    while (nrow(remaining_lines) > 0) {
        last_point <- lwgeom::st_endpoint(current_line$geometry)

        # Find the closest line segment to continue the route
        nearest_idx <- st_nearest_feature(last_point, remaining_lines$start)
        next_line <- remaining_lines[nearest_idx, ]

        # Check if we need to reverse the next line to connect properly
        next_start <- lwgeom::st_startpoint(next_line$geometry)
        next_end <- lwgeom::st_endpoint(next_line$geometry)

        if (st_distance(last_point, next_start) > st_distance(last_point, next_end)) {
            next_line$geometry <- st_reverse(next_line$geometry)
        }

        ordered_lines[[length(ordered_lines) + 1]] <- next_line$geometry
        remaining_lines <- remaining_lines[-nearest_idx, ]
        current_line <- next_line
    }

    # 4. Extract ALL coordinates in order
    all_coords <- do.call(rbind, lapply(ordered_lines, st_coordinates))[, 1:2]

    # 5. Create new LINESTRING from the combined coordinates
    return(st_sfc(st_linestring(all_coords), crs = st_crs(multilinestring)))
}
