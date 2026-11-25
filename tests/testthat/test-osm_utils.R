test_that("filter_osm_bus_lanes correctly filters bus lane features", {
    road_osm <- sf::st_sf(
        osm_id = c("1", "2", "3"),
        psv = c("designated", "no", "no"),
        `lanes:bus` = c(NA, "0", "1"),
        geometry = sf::st_sfc(
            sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)),
            sf::st_linestring(matrix(c(1, 1, 2, 2), ncol = 2)),
            sf::st_linestring(matrix(c(2, 2, 3, 3), ncol = 2)),
            crs = 4326
        )
    )

    filtered <- filter_osm_bus_lanes(road_osm)
    expect_identical(nrow(filtered), 2L)
    expect_identical(filtered$osm_id, c("1", "3"))
})

setup_mock_xml <- function(env = parent.frame()) {
    mock_xml <- '<?xml version="1.0" encoding="UTF-8"?>
    <osm version="0.6">
      <relation id="100">
        <tag k="route" v="bus"/>
        <tag k="ref" v="701"/>
        <tag k="name" v="Line 701"/>
        <tag k="gtfs:route_id" v="R701"/>
        <member type="way" ref="10" role="outer"/>
        <member type="node" ref="20" role="stop"/>
      </relation>
      <relation id="200">
        <tag k="route" v="bus"/>
        <member type="way" ref="30" role="outer"/>
        <member type="node" ref="40" role="stop"/>
      </relation>
      <relation id="300">
        <tag k="route" v="bus"/>
        <tag k="ref" v="900"/>
        <member type="way" ref="50" role="outer"/>
        <member type="node" ref="60" role="stop"/>
      </relation>
      <relation id="400">
        <tag k="route" v="bus"/>
        <tag k="ref" v="701"/>
      </relation>
      <relation id="500">
        <tag k="route" v="bus"/>
        <tag k="ref" v="701"/>
        <member type="way" ref="70" role="outer"/>
      </relation>
      <relation id="600">
        <tag k="route" v="bus"/>
        <tag k="ref" v="701"/>
        <member type="node" ref="80" role="stop"/>
      </relation>
      <relation id="700">
        <tag k="route" v="bus"/>
        <tag k="ref" v="701exact"/>
        <member type="way" ref="90" role="outer"/>
        <member type="node" ref="1000" role="stop"/>
      </relation>
    </osm>'

    mock_xml_file <- withr::local_tempfile(fileext = ".xml", .local_envir = env)
    writeLines(mock_xml, mock_xml_file)
    return(mock_xml_file)
}

make_mock_bg_job <- function() {
    list(
        is_alive = function() FALSE,
        get_result = function() NULL
    )
}

make_mock_pb <- function() {
    list(
        tick = function(...) invisible(NULL),
        update = function(...) invisible(NULL)
    )
}

test_that("get_osm_relations parses relations with regex feature operator ~", {
  mock_xml_file <- setup_mock_xml()
  on.exit(unlink(mock_xml_file), add = TRUE)

  testthat::with_mocked_bindings(
    r_bg = function(...) make_mock_bg_job(),
    .package = "callr",
    code = {
      testthat::with_mocked_bindings(
        show_content = function(...) mock_xml_file,
        .package = "rosmium",
        code = {
          res_regex <- get_osm_relations(
            osm_file = "dummy.pbf",
            q = list(features = '["ref"~"701"]'),
            pb = make_mock_pb(),
            osm_route_type = "bus"
          )

          expect_s3_class(res_regex, "data.frame")
          # 2 members from rel 100, 2 members from rel 700
          expect_identical(nrow(res_regex), 4L)
          expect_identical(unique(res_regex$relation_osm_id), c("100", "700"))
        }
      )
    }
  )
})

test_that(
  "get_osm_relations parses relations with exact equality feature operator =",
  {
    mock_xml_file <- setup_mock_xml()
    on.exit(unlink(mock_xml_file), add = TRUE)

    testthat::with_mocked_bindings(
      r_bg = function(...) make_mock_bg_job(),
      .package = "callr",
      code = {
        testthat::with_mocked_bindings(
          show_content = function(...) mock_xml_file,
          .package = "rosmium",
          code = {
            res_exact <- get_osm_relations(
              osm_file = "dummy.pbf",
              q = list(features = '["ref"="701exact"]'),
              pb = make_mock_pb(),
              osm_route_type = "bus"
            )

            expect_s3_class(res_exact, "data.frame")
            expect_identical(nrow(res_exact), 2L)
            expect_identical(unique(res_exact$relation_osm_id), "700")
          }
        )
      }
    )
  }
)

test_that("get_osm_relations returns empty data frame when no features match", {
  mock_xml_file <- setup_mock_xml()
  on.exit(unlink(mock_xml_file), add = TRUE)

  testthat::with_mocked_bindings(
    r_bg = function(...) make_mock_bg_job(),
    .package = "callr",
    code = {
      testthat::with_mocked_bindings(
        show_content = function(...) mock_xml_file,
        .package = "rosmium",
        code = {
          res_none <- get_osm_relations(
            osm_file = "dummy.pbf",
            q = list(features = '["ref"="nonexistent"]'),
            pb = make_mock_pb(),
            osm_route_type = "bus"
          )

          expect_identical(nrow(res_none), 0L)
        }
      )
    }
  )
})

test_that(
  "get_osm_relations correctly forwards custom osm_route_type parameter",
  {
    mock_xml_file <- setup_mock_xml()
    on.exit(unlink(mock_xml_file), add = TRUE)

    state <- new.env(parent = emptyenv())
    state$passed_filter <- NULL

    testthat::with_mocked_bindings(
      r_bg = function(func, args, ...) {
        # Capture filter string passed to rosmium::tags_filter in r_bg call
        state$passed_filter <- args[[3]]
        make_mock_bg_job()
      },
      .package = "callr",
      code = {
        testthat::with_mocked_bindings(
          show_content = function(...) mock_xml_file,
          .package = "rosmium",
          code = {
            res <- get_osm_relations(
              osm_file = "dummy.pbf",
              q = list(features = '["ref"~"701"]'),
              pb = make_mock_pb(),
              osm_route_type = "train"
            )

            expect_identical(state$passed_filter, "train")
            expect_s3_class(res, "data.frame")
            expect_identical(nrow(res), 4L)
          }
        )
      }
    )
  }
)

test_that("prepare_osm_file handles NULL and non-existent files", {
  expect_null(prepare_osm_file(NULL))
  expect_identical(prepare_osm_file("non_existent.pbf"), "non_existent.pbf")
})

test_that(
  paste0(
    "prepare_osm_file returns original path when file ",
    "is in writable directory outside library"
  ),
  {
    temp_dir <- file.path(tempdir(), "test_writable_dir")
    dir.create(temp_dir, showWarnings = FALSE)
    test_file <- file.path(temp_dir, "test.pbf")
    file.create(test_file)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    res <- prepare_osm_file(test_file)
    expect_identical(res, test_file)
  }
)

test_that(
  "prepare_osm_file copies file to tempdir when directory is read-only",
  {
    temp_dir <- file.path(tempdir(), "test_ro_dir")
    dir.create(temp_dir, showWarnings = FALSE)
    test_file <- file.path(temp_dir, "test_ro.pbf")
    cat("test content", file = test_file)
    Sys.chmod(temp_dir, "0555")
    on.exit({
      Sys.chmod(temp_dir, "0755")
      unlink(temp_dir, recursive = TRUE)
      unlink(file.path(tempdir(), "test_ro.pbf"))
    }, add = TRUE)

    res <- prepare_osm_file(test_file)
    expect_true(startsWith(res, tempdir()))
    expect_identical(basename(res), "test_ro.pbf")
    expect_true(file.exists(res))
  }
)





