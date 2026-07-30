library(testthat)
library(sf)

test_that("filter_osm_bus_lanes correctly filters bus lane features", {
    road_osm <- st_sf(
        osm_id = c("1", "2", "3"),
        psv = c("designated", "no", "no"),
        `lanes:bus` = c(NA, "0", "1"),
        geometry = st_sfc(
            st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)),
            st_linestring(matrix(c(1, 1, 2, 2), ncol = 2)),
            st_linestring(matrix(c(2, 2, 3, 3), ncol = 2)),
            crs = 4326
        )
    )

    filtered <- GTFShift:::filter_osm_bus_lanes(road_osm)
    expect_equal(nrow(filtered), 2)
    expect_equal(filtered$osm_id, c("1", "3"))
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
                    res_regex <- GTFShift:::get_osm_relations(
                        osm_file = "dummy.pbf",
                        q = list(features = '["ref"~"701"]'),
                        pb = make_mock_pb(),
                        osm_route_type = "bus"
                    )

                    expect_s3_class(res_regex, "data.frame")
                    expect_equal(nrow(res_regex), 4) # 2 members from rel 100, 2 members from rel 700
                    expect_equal(unique(res_regex$relation_osm_id), c("100", "700"))
                }
            )
        }
    )
})

test_that("get_osm_relations parses relations with exact equality feature operator =", {
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
                    res_exact <- GTFShift:::get_osm_relations(
                        osm_file = "dummy.pbf",
                        q = list(features = '["ref"="701exact"]'),
                        pb = make_mock_pb(),
                        osm_route_type = "bus"
                    )

                    expect_s3_class(res_exact, "data.frame")
                    expect_equal(nrow(res_exact), 2)
                    expect_equal(unique(res_exact$relation_osm_id), "700")
                }
            )
        }
    )
})

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
                    res_none <- GTFShift:::get_osm_relations(
                        osm_file = "dummy.pbf",
                        q = list(features = '["ref"="nonexistent"]'),
                        pb = make_mock_pb(),
                        osm_route_type = "bus"
                    )

                    expect_equal(nrow(res_none), 0)
                }
            )
        }
    )
})

test_that("get_osm_relations correctly forwards custom osm_route_type parameter", {
    mock_xml_file <- setup_mock_xml()
    on.exit(unlink(mock_xml_file), add = TRUE)

    passed_filter <- NULL

    testthat::with_mocked_bindings(
        r_bg = function(func, args, ...) {
            # Capture filter string passed to rosmium::tags_filter in r_bg call
            passed_filter <<- args[[3]]
            make_mock_bg_job()
        },
        .package = "callr",
        code = {
            testthat::with_mocked_bindings(
                show_content = function(...) mock_xml_file,
                .package = "rosmium",
                code = {
                    res <- GTFShift:::get_osm_relations(
                        osm_file = "dummy.pbf",
                        q = list(features = '["ref"~"701"]'),
                        pb = make_mock_pb(),
                        osm_route_type = "train"
                    )

                    expect_equal(passed_filter, "train")
                    expect_s3_class(res, "data.frame")
                    expect_equal(nrow(res), 4)
                }
            )
        }
    )
})





