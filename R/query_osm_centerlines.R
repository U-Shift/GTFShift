#' Get centerlines for OSM road network
#'
#' @param bbox bbox (Optional, if place provided). Area from which to export bus lanes.
#' @param place String (Optional, if bbox provided). Place from which to export bus lanes.
#' @param osm_file String (Optional). Path to a local OpenStreetMap PBF file (`.pbf`).
#' @param use_buildings Boolean (Default TRUE). Uses buildings from OSM as exclusion_mask for neatnet.
#' @param venv String (Default creates a new one). Python environment where neatnet will run.
#'
#' @details
#' Exports road network from OpenStreetMaps for given area and uses
#' Python \href{https://uscuni.org/neatnet/}{neatnet} package to compute its centerlines.
#'
#' One of \code{bbox}, \code{place}, or \code{osm_file} must be provided.
#'
#' Parameter \code{use_buildings} exports building footprints from OSM for better results on
#' the network simplification process.
#'
#' This method was adapted from \href{https://uscuni.org/neatnet/intro.html}{uscuni.org/neatnet}
#' by \href{https://github.com/miguelrelvaspires}{Miguel Relvas Pires} in the scope of
#' his \href{https://scholar.tecnico.ulisboa.pt/records/DhKWeFU5YLpMDcOhQbKR4f7ul05HCQnZr7ND}{master's thesis}.
#' The full code (Python) of his work is openly available at
#' \href{https://github.com/U-Shift/lp_streets}{GitHub}.
#'
#' @returns sf data.frame. OSM centerlines.
#'
#' @examples
#' \dontrun{
#' # Get sample OSM extract
#' osm_file <- system.file("extdata/samples", "relation_6384187.pbf", package = "GTFShift")
#'
#' network <- GTFShift::osm_centerlines(
#'   place = "Arroios, Lisboa, Portugal",
#'   osm_file = osm_file
#' )
#'
#' head(network)
#'
#' table(network$X_status)
#' }
#'
#' @author \href{https://github.com/miguelrelvaspires}{Miguel Relvas Pires}
#'
#' @import sf
#' @export
osm_centerlines <- function(bbox = NULL, place = NULL, osm_file = NULL, use_buildings = TRUE, venv = NA) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for this function. Install it with: install.packages('reticulate')")
  }
  # Set up Python environment
  if (!is.null(venv) && !is.na(venv)) {
    reticulate::use_virtualenv(venv, required = TRUE)
  } else if (!reticulate::py_available()) {
    venv <- reticulate::virtualenv_create()
    reticulate::use_virtualenv(venv, required = TRUE)
  }

  # Ensure dependencies are installed
  req_modules <- c("osmnx", "pandas", "geopandas", "shapely", "neatnet", "pyrosm")
  missing_modules <- req_modules[!vapply(req_modules, reticulate::py_module_available, logical(1))]
  if (length(missing_modules) > 0) {
    reticulate::py_install(packages = c("osmnx", "pandas", "geopandas", "shapely", "neatnet", "pyrosm"), pip = TRUE, pip_ignore_installed = FALSE)
  }

  # Define path to script and temp output
  py_script <- system.file("python", "osm_centerline_neatnet.py", package = "GTFShift")
  temp_file <- withr::local_tempfile(fileext = ".gpkg")

  # Call Python script via reticulate
  py_env <- new.env()
  reticulate::source_python(py_script, envir = py_env)
  py_env$get_centerline(bbox, place, use_buildings, temp_file, osm_file)

  # Read the GPKG file as sf
  result <- sf::st_read(temp_file, quiet = TRUE)
  return(result)
}
