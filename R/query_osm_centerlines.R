#' Get centerlines for OSM road network
#'
#' @param bbox bbox. (Optional, if place provided) Area from which to export bus lanes.
#' @param place String. (Optional, if bbox provided) Place from which to export bus lanes.
#' @param use_buildings Boolean. (Default True) Uses buildings from OSM as exclusion_mask for neatnet.
#' @param venv String. (Default creates a new one) Python environment where neatnet will run.
#'
#' @details
#' Exports road network from Open Street Maps for given area and uses
#' Python \href{https://uscuni.org/neatnet/}{neatnet} package to compute its centerlines.
#' One of `bbox` or `place` must be provided. If both, `bbox` is considered.
#' Parameter `use_buildings` exports building footprints from OSM for better results on
#' the network simplification process.
#'
#' @returns osm_lines in sf format
#'
#' @examples
#' \dontrun{
#' BBOX = sf::st_bbox(city_limit)
#' network <- GTFShift::osm_centerlines(BBOX)
#' }
#'
#' @import reticulate
#' @import sf
#'
#' @export
osm_centerlines <- function(bbox=NULL, place=NULL, use_buildings = TRUE, venv=NA) {

  # Set up Python environment
  if (is.na(venv)) {
    venv <- reticulate::virtualenv_create()
  }
  reticulate::use_virtualenv(venv, required = TRUE)

  # Ensure dependencies are installed
  py_install(packages = c("osmnx", "pandas", "geopandas", "shapely", "neatnet"), pip = TRUE, pip_ignore_installed=FALSE)

  # Define path to script and temp output
  py_script <- system.file("python", "osm_centerline_neatnet.py", package = "GTFShift")
  temp_file <- tempfile(fileext = ".gpkg")

  # Call Python script via reticulate
  reticulate::source_python(py_script)
  get_centerline(bbox, place, use_buildings, temp_file)

  # Read the GPKG file as sf
  result <- sf::st_read(temp_file, quiet = TRUE)
  return(result)
}
