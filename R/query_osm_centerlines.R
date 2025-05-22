#' Export centerlines for OSM road network
#'
#' @param bbox bbox. Area from which to export bus lanes.
#' @param venv String. (Default r-reticulate) Python environment where neatnet will run.
#'
#' @details
#' ...
#'
#' @returns osm_lines in sf format
#'
#' @examples
#' \dontrun{
#' BBOX = sf::st_bbox(city_limit)
#' network <- GTFShift::osm_centerlines(BBOX)
#' }
#'
#' @import osmdata
#' @import sf
#' @import dplyr
#'
#' @export
library(reticulate)
library(sf)
venv="r-reticulate"
osm_centerlines <- function(bbox, venv="r-reticulate") {

  # TODOOO! Change Município de Lisboa, Portugal to bbox!!

  # Set up Python environment
  reticulate::use_virtualenv(venv, required = TRUE)

  # Ensure dependencies are installed
  py_install(packages = c("osmnx", "pandas", "geopandas", "shapely", "neatnet"), pip = TRUE, pip_ignore_installed=TRUE)

  # Define path to script and temp output
  py_script <- system.file("python/osm_centerline_neatnet.py", package = "GTFShift")
  #TODO! Delete line below before compiling
  py_script <- "inst/python/osm_centerline_neatnet.py"

  temp_file <- tempfile(fileext = ".gpkg")

  # Call Python script via reticulate
  py_run_file(py_script, local = TRUE)

  # Optional: pass arguments to the script
  py$get_centerline("Município de Lisboa, Portugal", temp_file)

  # Read the GPKG file as sf
  result <- sf::st_read(temp_file, quiet = TRUE)
  return(result)
}
