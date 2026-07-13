#' Convert a MULTILINESTRING to a sorted LINESTRING
#'
#' @param multilinestring sf object with MULTILINESTRING geometry
#' @param points (Optional) collection of sorted point geometries used to guide ordering.
#'   If provided, the first point defines the initial segment and the second
#'   point (when available) is used as tie-break guidance for its orientation. Remaining points are
#'   used as iterative tie-break guidance.
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
#' \bold{1. Initialization}
#'
#' If guiding points are provided, let \eqn{\mathrm{start\_point}=P_1} be the first point and
#' \eqn{P_2} the second point (if available). The initial segment is chosen as
#' \deqn{L^{(1)} = \operatorname*{argmin}_{L \in \mathcal{L}} d(\mathrm{start\_point}, L).}
#' where \eqn{d(\cdot)} is the Euclidean distance. If no points are provided, \eqn{L^{(1)} = L_1} (assuming the input MULTILINESTRING is ordered).
#' 
#' Additionaly, the orientation of \eqn{L^{(1)}} is determined by comparing the distances 
#' from its edges to the remaining segments in \eqn{\mathcal{L} \setminus \{L^{(1)}\}}. 
#' The edge that is closest to any remaining segment is designated as the end of \eqn{L^{(1)}}. 
#' 
#' If both edges are equidistant to the remaining segments, the orientation is determined 
#' by the proximity to \eqn{P_2} (if available) or by orienting away from \eqn{P_1}.
#' 
#'
#' \bold{2. Iterative Step}
#'
#' At iteration \eqn{k}, with current segment endpoint \eqn{e^{(k)} = E(L^{(k)})}, define
#' for each remaining segment \eqn{L \in \mathcal{R}^{(k)}}:
#' \deqn{d_s(L) = d\!\left(e^{(k)}, S(L)\right), \qquad d_e(L) = d\!\left(e^{(k)}, E(L)\right).}
#' Segments with geometry equal to \eqn{L^{(k)}} are excluded. Candidate segments
#' minimize endpoint proximity:
#' \deqn{\mathcal{C}^{(k)} = \left\{L \in \mathcal{R}^{(k)} : \min\big(d_s(L), d_e(L)\big) = m^{(k)}\right\},
#' \quad m^{(k)} = \min_{J \in \mathcal{R}^{(k)}} \min\big(d_s(J), d_e(J)\big).}
#' Ties are broken as follows:
#' \deqn{\text{(i) if next unvisited point } Q \text{ exists, choose closest candidate, minimizing } d(Q,L) \text{ over } L \in \mathcal{C}^{(k)};}
#' \deqn{\text{(ii) if still tied, choose candidate closest to the current endpoint } e^{(k)}, \text{ minimizing } d\!\left(e^{(k)}, S(L)\right) \text{ over } L \in \mathcal{C}^{(k)}.}
#'
#' \bold{3. Verification and Assembly}
#'
#' Let \eqn{L^*} be the selected candidate. If
#' \deqn{d\!\left(L^{(k)}, L^*\right) > \operatorname{len}\!\left(L^{(k)}\right) + \operatorname{len}(L^*),}
#' then \eqn{L^*} is removed from the remaining set and the loop restarts.
#' Otherwise, \eqn{L^*} is oriented to connect from \eqn{e^{(k)}} and
#' appended to the ordered sequence. When points are provided, consecutive
#' unvisited points \eqn{Q} are marked visited when
#' \deqn{d\!\left(L^{(k+1)}, Q\right) \leq \min_{J \in \mathcal{R}^{(k+1)}} d(J, Q).}
#'
#' The ordered segments are concatenated into a single \code{LINESTRING} and
#' transformed back to the original CRS of \code{multilinestring}.
#'
#' @returns A \code{sfc} object with LINESTRING geometry.
#'
#' @import dplyr
#' @import sf
#' @import lwgeom
#'
#' @export
multiline_to_sorted_linestring <- function(
    multilinestring, 
    points = NULL,
    metric_crs = 3857
) {
    metric_crs_is_default <- missing(metric_crs)
    metric_crs <- suppressWarnings(sf::st_crs(metric_crs))
    if (is.na(metric_crs)) {
        stop("metric_crs should be a valid CRS value (e.g., 3857 or 'EPSG:3857')")
    }
    if (metric_crs_is_default) {
        warning(
            "Using default metric_crs (EPSG:3857). Consider setting metric_crs to a projected CRS better suited to your local context for more accurate distance calculations.",
            call. = FALSE
        )
    }
    original_crs <- st_crs(multilinestring)

    # browser()

    # 1. Extract all individual LINESTRING components
    linestrings <- st_cast(multilinestring, "LINESTRING") |>
        st_as_sf() |>
        st_set_geometry("geometry") |>
        st_transform(metric_crs) |>
        mutate(
            start = lwgeom::st_startpoint(geometry),
            end = lwgeom::st_endpoint(geometry)
        )

    # 2. Reorder the linestrings by finding the best sequence

    # Prepare points data if provided
    points_df <- NULL
    start_point <- NULL
    second_point <- NULL
    if (!is.null(points) & length(points) > 0) {
        points_df <- st_as_sf(data.frame(
            geometry = points,
            visited = FALSE
        )) |>
            mutate(order = row_number()) |>
            st_transform(metric_crs)
        start_point <- points_df |> slice(1) |> pull(geometry)
        if (length(points) > 1) {
            second_point <- points_df |> slice(2) |> pull(geometry)
        }
        # Mark the first point as visited
        points_df$visited[1] <- TRUE
    }

    # If start_point is defined, find the line that is closest to the start_point
    # otherwise, assume the linestrings default order to get the first line
    if (!is.null(start_point)) {
        nearest_idx <- st_nearest_feature(start_point, linestrings)
        current_line <- linestrings[nearest_idx, ]
        current_start <- lwgeom::st_startpoint(current_line$geometry)
        current_end <- lwgeom::st_endpoint(current_line$geometry)
        remaining_lines <- linestrings[-nearest_idx, ]        
    } else {
        current_line <- linestrings[1, ] # Start with the first line
        current_start <- lwgeom::st_startpoint(current_line$geometry)
        current_end <- lwgeom::st_endpoint(current_line$geometry)
        remaining_lines <- linestrings[-1, ] 
    }
    # Validate start segment orientation
    # 1st criteria: If it connects to only one other segment, orient it to connect to that segment (end point is closest to the other segment)
    # 2nd criteria: If it connects to multiple segments, orient it towards:
    #   a) If second_point is defined, orient towards it (end is closest); otherwise,
    #   b) orient away from start_point (start is closest).
    distance_start_to_remaining <- if (nrow(remaining_lines) > 0) {
        min(as.numeric(st_distance(current_start, remaining_lines$geometry)))
    } else {
        Inf
    }
    distance_end_to_remaining <- if (nrow(remaining_lines) > 0) {
        min(as.numeric(st_distance(current_end, remaining_lines$geometry)))
    } else {
        Inf
    }
    if (distance_start_to_remaining < distance_end_to_remaining) {
        current_line$geometry <- st_reverse(current_line$geometry)
    } else if (distance_start_to_remaining == distance_end_to_remaining) {
        # If second_point is defined, orient towards it (end is closest); otherwise,
        # orient away from start_point (start is closest).
        if (!is.null(second_point)) {
            second_point <- st_transform(second_point, metric_crs)
            if (st_distance(second_point, current_start) < st_distance(second_point, current_end)) {
                current_line$geometry <- st_reverse(current_line$geometry)
            }
        } else if (st_distance(start_point, current_start) > st_distance(start_point, current_end)) {
            current_line$geometry <- st_reverse(current_line$geometry)
        }
    }

    ordered_lines <- list()
    ordered_lines[[1]] <- current_line$geometry

    next_point_index <- NULL
    next_point <- NULL
    while (nrow(remaining_lines) > 0) { 
        if (!is.null(points_df)) {
            # Get index of the next point in points_df that has not been visited yet
            next_point_index <- points_df |> filter(!visited) |> slice(1) |> pull(order)
            if (length(next_point_index) == 0) {
                next_point_index <- NULL
                next_point <- NULL
            } else {
                next_point <- points_df |> filter(order == next_point_index) |> pull(geometry)
                distance_to_next_point <- st_distance(current_line, next_point)
            }
        }
        last_point <- lwgeom::st_endpoint(current_line$geometry)

        # Find distance from last_point to all remaining lines' start and end points
        distance_to_start <- as.numeric(st_distance(last_point, remaining_lines$start))
        distance_to_end <- as.numeric(st_distance(last_point, remaining_lines$end))

        # Exclude segments with identical geometry to current_line from candidate selection.
        same_as_current <- as.logical(sf::st_equals(remaining_lines$geometry, current_line$geometry, sparse = FALSE)[, 1])
        if (any(same_as_current)) {
            # message(sprintf("> Excluding %d remaining segments with identical geometry to current line from candidate selection...", sum(same_as_current)))
            distance_to_start[same_as_current] <- Inf
            distance_to_end[same_as_current] <- Inf
        }
        if (all(is.infinite(distance_to_start)) && all(is.infinite(distance_to_end))) {
            # message("> All remaining segments match current geometry, stopping extension...")
            break
        }

        # Find the closest line segment to continue the route
        # Tie-breaks:
        # 1) If next_point is provided, pick the candidate whose geometry is closest to next_point
        # 2) If still tied, pick the one whose start point is closest to last_point
        min_distance <- min(c(distance_to_start, distance_to_end))
        candidate_start_idx <- which(distance_to_start == min_distance)
        candidate_end_idx <- which(distance_to_end == min_distance)
        candidate_df <- data.frame(
            idx = c(candidate_start_idx, candidate_end_idx),
            stringsAsFactors = FALSE
        )
        candidate_df <- unique(candidate_df)
        
        if (nrow(candidate_df) > 1 && !is.null(next_point)) { # 1)
            candidate_df$geometry_to_next <- vapply(
                candidate_df$idx,
                function(candidate_i) as.numeric(st_distance(next_point, remaining_lines[candidate_i, ]$geometry)),
                numeric(1)
            )
            candidate_df <- candidate_df[candidate_df$geometry_to_next == min(candidate_df$geometry_to_next), , drop = FALSE]
        }

        if (nrow(candidate_df) > 1 && !is.null(last_point)) { # 2)
            candidate_df$start_to_last_point <- vapply(
                candidate_df$idx,
                function(candidate_i) as.numeric(st_distance(last_point, remaining_lines[candidate_i, ]$start)),
                numeric(1)
            )
            candidate_df <- candidate_df[candidate_df$start_to_last_point == min(candidate_df$start_to_last_point), , drop = FALSE]
        }

        nearest_idx <- candidate_df$idx[1]
        next_line <- remaining_lines[nearest_idx, ]
        next_start <- lwgeom::st_startpoint(next_line$geometry)
        next_end <- lwgeom::st_endpoint(next_line$geometry)

        # Check if distance between current and next exceeds their aggregated length and if so, discard...
        if (st_distance(current_line, next_line) > (st_length(current_line) + st_length(next_line))) {
            # message("> Next is farther than current + next length, discarding it...")
            remaining_lines <- remaining_lines[-nearest_idx, ]
            next
        }

        # Check if we need to reverse the next line to connect properly
        if (next_start == next_end) {
            # message("> Circular shape...")
        } else if (st_distance(last_point, next_start) > st_distance(last_point, next_end)) {
            # message("> Inverting next line...")
            next_line$geometry <- st_reverse(next_line$geometry)
        }

        ordered_lines[[length(ordered_lines) + 1]] <- next_line$geometry
        remaining_lines <- remaining_lines[-nearest_idx, ]
        current_line <- next_line

        if (!is.null(points_df)) {
            # Mark consecutive next points as visited while current_line stays at least as close as any remaining line for the next unvisited point
            repeat {
                next_unvisited <- points_df |> filter(!visited) |> slice(1)
                if (nrow(next_unvisited) == 0) {
                    break
                }
                next_point_index <- next_unvisited |> pull(order)
                next_point <- next_unvisited |> pull(geometry)
                distance_current_to_next <- as.numeric(st_distance(current_line, next_point))
                distance_remaining_to_next <- if (nrow(remaining_lines) > 0) {
                    min(as.numeric(st_distance(remaining_lines$geometry, next_point)))
                } else {
                    Inf
                }
                if (distance_current_to_next <= distance_remaining_to_next) {
                    # message(sprintf("> (Iteration %d) Next point %d is closer to current line rather than any other remaining, marking it as visited...", length(ordered_lines), next_point_index))
                    points_df$visited[points_df$order == next_point_index] <- TRUE
                } else {
                    break
                }
            }
        }
    }

    # 3. Extract ALL coordinates in order
    all_coords <- do.call(rbind, lapply(ordered_lines, st_coordinates))[, 1:2]

    # 4. Create new LINESTRING from the combined coordinates
    result <- st_sfc(st_linestring(all_coords), crs = st_crs(linestrings))
    if (!is.na(original_crs)) {
        result <- st_transform(result, original_crs)
    }
    return(result)
}
