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
#' @returns osm_lines in sf format
#'
#' @examples
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
#'
#' @import sf
#' @importFrom reticulate virtualenv_create use_virtualenv py_install source_python
#' @export
osm_centerlines <- function(bbox = NULL, place = NULL, osm_file = NULL, use_buildings = TRUE, venv = NA) {
  # Set up Python environment
  if (is.na(venv)) {
    venv <- reticulate::virtualenv_create()
  }
  reticulate::use_virtualenv(venv, required = TRUE)

  # Ensure dependencies are installed
  reticulate::py_install(packages = c("osmnx", "pandas", "geopandas", "shapely", "neatnet", "pyrosm"), pip = TRUE, pip_ignore_installed = FALSE)

  # Define path to script and temp output
  py_script <- system.file("python", "osm_centerline_neatnet.py", package = "GTFShift")
  temp_file <- tempfile(fileext = ".gpkg")

  # Call Python script via reticulate
  py_env <- new.env()
  reticulate::source_python(py_script, envir = py_env)
  py_env$get_centerline(bbox, place, use_buildings, temp_file, osm_file)

  # Read the GPKG file as sf
  result <- sf::st_read(temp_file, quiet = TRUE)
  return(result)
}
