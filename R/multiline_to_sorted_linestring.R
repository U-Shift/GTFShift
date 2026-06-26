#' Convert a MULTILINESTRING to a sorted LINESTRING
#'
#' @param multilinestring sf object with MULTILINESTRING geometry
#' @param start_point (Optional) sf point geometry. If provided, the sorting of the linestrings
#'   will start from this point.
#' @param metric_crs Integer or character (Default 3857). Projected CRS used to compute distances and lengths during sorting.
#'
#' @details
#' The function takes a MULTILINESTRING object and converts it to a LINESTRING object
#' by sorting the linestrings and combining them in the correct order.
#'
#' The algorithm is formulated as follows:
#' Let \eqn{\mathcal{L} = \{L_1, \dots, L_n\}} be the set of individual LINESTRING components.
#' Each component \eqn{L_i} is characterized by its start point \eqn{S(L_i)} and end point \eqn{E(L_i)}.
#'
#' \bold{1. Initialization:}
#' Let \eqn{L^{(1)}} be the first segment in the sorted sequence.
#'
#' If a \code{start_point} \eqn{P_{\text{start}}} is provided:
#' \deqn{L^{(1)} = \operatorname{argmin}_{L \in \mathcal{L}} d(P_{\text{start}}, L)}{L^(1) = argmin_{L \in \mathcal{L}} d(P_start, L)}
#' where \eqn{d(\cdot)} is the Euclidean distance. If \eqn{d(P_{\text{start}}, S(L^{(1)})) > d(P_{\text{start}}, E(L^{(1)}))},
#' the component's geometry is reversed so that it starts near \eqn{P_{\text{start}}}.
#'
#' If no \code{start_point} is provided:
#' \deqn{L^{(1)} = L_1}{L^(1) = L_1}
#'
#' The set of remaining segments is initialized as \eqn{\mathcal{R}^{(1)} = \mathcal{L} \setminus \{L^{(1)}\}}.
#'
#' \bold{2. Iterative Step:}
#' For each step \eqn{k \ge 1}, let \eqn{L^{(k)}} be the current segment and \eqn{E^{(k)} = E(L^{(k)})} its endpoint.
#' The algorithm searches the remaining components \eqn{\mathcal{R}^{(k)}} for the closest segment:
#' \deqn{L_{\text{start}} = \operatorname{argmin}_{L \in \mathcal{R}^{(k)}} d(E^{(k)}, S(L))}{L_start = argmin_{L \in \mathcal{R}^{(k)}} d(E^(k), S(L))}
#' \deqn{L_{\text{end}} = \operatorname{argmin}_{L \in \mathcal{R}^{(k)}} d(E^{(k)}, E(L))}{L_end = argmin_{L \in \mathcal{R}^{(k)}} d(E^(k), E(L))}
#' Let \eqn{d_{\text{start}} = d(E^{(k)}, S(L_{\text{start}}))} and \eqn{d_{\text{end}} = d(E^{(k)}, E(L_{\text{end}}))}.
#' The candidate segment \eqn{L^*} is:
#' \deqn{L^* = \begin{cases} L_{\text{start}} & \text{if } d_{\text{start}} < d_{\text{end}} \\ L_{\text{end}} & \text{otherwise} \end{cases}}{L* = L_start if d_start < d_end, else L_end}
#'
#' \bold{3. Verification & Assembly:}
#' If the distance between the current segment and the candidate exceeds their combined length:
#' \deqn{d(L^{(k)}, L^*) > \text{len}(L^{(k)}) + \text{len}(L^*)}{d(L^(k), L*) > len(L^(k)) + len(L*)}
#' then \eqn{L^*} is discarded, \eqn{\mathcal{R}^{(k+1)} = \mathcal{R}^{(k)} \setminus \{L^*\}} and we find the next candidate.
#' Otherwise, \eqn{L^{(k+1)} = L^*} (reversed if \eqn{L^* = L_{\text{end}}}) and \eqn{\mathcal{R}^{(k+1)} = \mathcal{R}^{(k)} \setminus \{L^*\}}.
#'
#' The process repeats until no segments remain, and the components are merged into a single \code{LINESTRING}.
#'
#' @returns A \code{sf} object with LINESTRING geometry.
#'
#' @import dplyr
#' @import sf
#' @import lwgeom
#'
#' @export
multiline_to_sorted_linestring <- function(multilinestring, start_point = NULL, metric_crs = 3857) {
    metric_crs <- suppressWarnings(sf::st_crs(metric_crs))
    if (is.na(metric_crs)) {
        stop("metric_crs should be a valid CRS value (e.g., 3857 or 'EPSG:3857')")
    }
    original_crs <- st_crs(multilinestring)

    # browser()

    # 1. Extract all individual LINESTRING components
    linestrings <- st_cast(multilinestring, "LINESTRING") |>
        st_as_sf() |>
        st_set_geometry("geometry") |>
        st_transform(metric_crs)

    if (!is.null(start_point)) {
        start_point <- st_transform(start_point, metric_crs)
    }

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
            current_line$geometry <- st_reverse(current_line$geometry)
        }
    } else {
        current_line <- linestrings[1, ] # Start with the first line
    }
    ordered_lines[[1]] <- current_line$geometry
    remaining_lines <- linestrings[-1, ]

    while (nrow(remaining_lines) > 0) {
        last_point <- lwgeom::st_endpoint(current_line$geometry)

        # Find the closest line segment to continue the route
        nearest_idx_start <- st_nearest_feature(last_point, remaining_lines$start)
        nearest_idx_end <- st_nearest_feature(last_point, remaining_lines$end)
        if (st_distance(last_point, remaining_lines[nearest_idx_start, ]$start) < st_distance(last_point, remaining_lines[nearest_idx_end, ]$end)) {
            nearest_idx <- nearest_idx_start
        } else {
            nearest_idx <- nearest_idx_end
        }
        next_line <- remaining_lines[nearest_idx, ]
        next_start <- lwgeom::st_startpoint(next_line$geometry)
        next_end <- lwgeom::st_endpoint(next_line$geometry)


        # If they have same geometry, consider other nearest
        if (next_line$geometry == current_line$geometry) {
            if (nearest_idx == nearest_idx_start) {
                nearest_idx <- nearest_idx_end
            } else {
                nearest_idx <- nearest_idx_start
            }
            next_line <- remaining_lines[nearest_idx, ]
            next_start <- lwgeom::st_startpoint(next_line$geometry)
            next_end <- lwgeom::st_endpoint(next_line$geometry)
        }

        # Check if distance between current and next exceeds their aggregated length and if so, discard...
        if (st_distance(current_line, next_line) > (st_length(current_line) + st_length(next_line))) {
            remaining_lines <- remaining_lines[-nearest_idx, ]
            next
        }

        # Check if we need to reverse the next line to connect properly
        if (next_start == next_end) {
            # message("> Circular shape...")
        } else if (st_distance(last_point, next_start) > st_distance(last_point, next_end)) {
            next_line$geometry <- st_reverse(next_line$geometry)
        }

        ordered_lines[[length(ordered_lines) + 1]] <- next_line$geometry
        remaining_lines <- remaining_lines[-nearest_idx, ]
        current_line <- next_line
    }

    # 4. Extract ALL coordinates in order
    all_coords <- do.call(rbind, lapply(ordered_lines, st_coordinates))[, 1:2]

    # 5. Create new LINESTRING from the combined coordinates
    result <- st_sfc(st_linestring(all_coords), crs = st_crs(linestrings))
    if (!is.na(original_crs)) {
        result <- st_transform(result, original_crs)
    }
    return(result)
}
