library(testthat)
library(sf)

test_that("osm_centerlines reads generated geopkg from python call with mocked reticulate", {
    mock_line <- st_sf(
        id = 1,
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)), crs = 4326)
    )

    get_centerline_called <- FALSE

    testthat::with_mocked_bindings(
        virtualenv_create = function(...) "mock_venv",
        use_virtualenv = function(...) TRUE,
        source_python = function(file, envir = parent.frame(), ...) {
            envir$get_centerline <- function(...) {
                get_centerline_called <<- TRUE
                TRUE
            }
            TRUE
        },
        py_install = function(...) TRUE,
        py_module_available = function(...) TRUE,
        .package = "reticulate",
        code = {
            testthat::with_mocked_bindings(
                st_read = function(dsn, ...) mock_line,
                .package = "sf",
                code = {
                    res <- GTFShift::osm_centerlines(bbox = NULL, place = "Porto", venv = "mock_env")
                    expect_true(get_centerline_called)
                    expect_s3_class(res, "sf")
                }
            )
        }
    )
})

test_that("osm_centerlines creates virtualenv when venv parameter is omitted/NA", {
    mock_line <- st_sf(
        id = 1,
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)), crs = 4326)
    )

    virtualenv_create_called <- FALSE
    get_centerline_called <- FALSE

    testthat::with_mocked_bindings(
        virtualenv_create = function(...) {
            virtualenv_create_called <<- TRUE
            "mock_created_venv"
        },
        use_virtualenv = function(...) TRUE,
        source_python = function(file, envir = parent.frame(), ...) {
            envir$get_centerline <- function(...) {
                get_centerline_called <<- TRUE
                TRUE
            }
            TRUE
        },
        py_install = function(...) TRUE,
        py_module_available = function(...) TRUE,
        .package = "reticulate",
        code = {
            testthat::with_mocked_bindings(
                st_read = function(dsn, ...) mock_line,
                .package = "sf",
                code = {
                    res <- GTFShift::osm_centerlines(bbox = NULL, place = "Porto")
                    expect_true(virtualenv_create_called)
                    expect_true(get_centerline_called)
                    expect_s3_class(res, "sf")
                }
            )
        }
    )
})

test_that("osm_centerlines uses existing venv without virtualenv_create when venv is provided", {
    mock_line <- st_sf(
        id = 1,
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)), crs = 4326)
    )

    virtualenv_create_called <- FALSE

    testthat::with_mocked_bindings(
        virtualenv_create = function(...) {
            virtualenv_create_called <<- TRUE
            "mock_created_venv"
        },
        use_virtualenv = function(...) TRUE,
        source_python = function(file, envir = parent.frame(), ...) {
            envir$get_centerline <- function(...) TRUE
            TRUE
        },
        py_install = function(...) TRUE,
        py_module_available = function(...) TRUE,
        .package = "reticulate",
        code = {
            testthat::with_mocked_bindings(
                st_read = function(dsn, ...) mock_line,
                .package = "sf",
                code = {
                    res <- GTFShift::osm_centerlines(bbox = NULL, place = "Porto", venv = "existing_env")
                    expect_false(virtualenv_create_called)
                    expect_s3_class(res, "sf")
                }
            )
        }
    )
})

test_that("osm_centerlines passes osm_file parameter to python call", {
    mock_line <- st_sf(
        id = 1,
        geometry = st_sfc(st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)), crs = 4326)
    )

    received_osm_file <- NULL

    testthat::with_mocked_bindings(
        virtualenv_create = function(...) "mock_venv",
        use_virtualenv = function(...) TRUE,
        source_python = function(file, envir = parent.frame(), ...) {
            envir$get_centerline <- function(bbox, study_area, use_buildings, output_path, osm_file = NULL) {
                received_osm_file <<- osm_file
                TRUE
            }
            TRUE
        },
        py_install = function(...) TRUE,
        py_module_available = function(...) TRUE,
        .package = "reticulate",
        code = {
            testthat::with_mocked_bindings(
                st_read = function(dsn, ...) mock_line,
                .package = "sf",
                code = {
                    res <- GTFShift::osm_centerlines(osm_file = "path/to/region.osm.pbf", venv = "mock_env")
                    expect_equal(received_osm_file, "path/to/region.osm.pbf")
                    expect_s3_class(res, "sf")
                }
            )
        }
    )
})
