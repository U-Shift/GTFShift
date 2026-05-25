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
        # mapview(start_point, col.regions = "yellow") + mapview(current_line) + mapview(current_start, col.regions="pink") + mapview(current_end, col.regions="gray")
        # If start_point is closest to line end point than start point, invert geometry
        if (st_distance(start_point, current_start) > st_distance(start_point, current_end)) {
            current_line$geometry = st_reverse(current_line$geometry)
        }
    } else {
        current_line <- linestrings[1, ] # Start with the first line
    }
    ordered_lines[[1]] <- current_line$geometry
    remaining_lines <- linestrings[-1, ]

    # mapview(linestrings, layer.name="linestrings") + mapview(ordered_lines[[1]], color = "red") + mapview(start_point, col.regions = "yellow")

    while (nrow(remaining_lines) > 0) { 
        # && length(ordered_lines)<=12 # 7
        #  && length(ordered_lines)<=31 # 10
        last_point <- lwgeom::st_endpoint(current_line$geometry)
        # mapview(linestrings) + mapview(ordered_lines, color="yellow") + mapview(current_line, color="red") + mapview(last_point, color="blue")
        # mapview(remaining_lines)
        # Find the closest line segment to continue the route
        nearest_idx_start <- st_nearest_feature(last_point, remaining_lines$start)
        nearest_idx_end <- st_nearest_feature(last_point, remaining_lines$end)
        nearest_idx = min(nearest_idx_start, nearest_idx_end)
        next_line <- remaining_lines[nearest_idx, ]
        next_start <- lwgeom::st_startpoint(next_line$geometry)
        next_end <- lwgeom::st_endpoint(next_line$geometry)

        # mapview(start_point, col.regions="gray") + mapview(linestrings) + mapview(ordered_lines, color="yellow") + mapview(current_line, color="red") + mapview(last_point, col.regions="orange") + mapview(next_line, color="green")
        # mapview(remaining_lines$geom) + mapview(remaining_lines$start, col.regions="pink") + mapview(remaining_lines$end, col.regions="black")

        # If they have same geometry, consider other nearest 
        if (next_line$geometry == current_line$geometry) {
            message("> Next has same geometry as current, considering other match...")
            nearest_idx = max(nearest_idx_start, nearest_idx_end)
            next_line <- remaining_lines[nearest_idx, ]
            next_start <- lwgeom::st_startpoint(next_line$geometry)
            next_end <- lwgeom::st_endpoint(next_line$geometry)
        }

        # Check if distance between current and next exceeds their aggregated length and if so, discard...
        if (st_distance(current_line, next_line) > (st_length(current_line) + st_length(next_line))) {
            message("> Next is farther than current + next length, discarding it...")
            remaining_lines <- remaining_lines[-nearest_idx, ]
            next
        }

        # Check if we need to reverse the next line to connect properly
        # mapview(current_line, color="red") + mapview(last_point, color="blue") + mapview(next_line, color="green") + mapview(next_start, col.regions="pink") + mapview(next_end, col.regions="black")
        if(next_start == next_end) {
            message("> Circular shape...")
        } else if (st_distance(last_point, next_start) > st_distance(last_point, next_end)) {
            message("> Inverting next line...")
            next_line$geometry <- st_reverse(next_line$geometry)
        }

        ordered_lines[[length(ordered_lines) + 1]] <- next_line$geometry
        remaining_lines <- remaining_lines[-nearest_idx, ]
        current_line <- next_line
    }

    # 4. Extract ALL coordinates in order
    all_coords <- do.call(rbind, lapply(ordered_lines, st_coordinates))[, 1:2]

    # Convert list of linestrings to dataframe of linestrings, preserving list order
    combined_sfc <- do.call(c, ordered_lines)
    line_df <- st_sf(geometry = combined_sfc) |> mutate(order = row_number())
    # mapview(start_point, col.regions="gray") + mapview(line_df, zcol = "order")
    # View(line_df|>st_drop_geometry())

    # Convert matrix to data.frame with x and y columns
    all_cords_df <- as.data.frame(all_coords) |>
        st_as_sf(coords = c("X", "Y"), crs = st_crs(multilinestring)) |>
        mutate(order = row_number())

    # mapview(all_cords_df, zcol = "order")

    # 5. Create new LINESTRING from the combined coordinates
    result <- st_sfc(st_linestring(all_coords), crs = st_crs(multilinestring))
    # mapview(result)
    return(st_sfc(st_linestring(all_coords), crs = st_crs(multilinestring)))
}
