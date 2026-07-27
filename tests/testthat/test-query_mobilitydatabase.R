library(testthat)

create_mock_response <- function(status_code = 200) {
  structure(
    list(
      status_code = status_code,
      content = raw(0),
      url = "https://api.mobilitydatabase.org/v1/gtfs_feeds"
    ),
    class = "response"
  )
}

test_that("query_mobilitydatabase throws error if no token is provided", {
  expect_error(
    GTFShift::query_mobilitydatabase(),
    "No token provided!"
  )
})

test_that("query_mobilitydatabase successfully queries with access token", {
  mock_feeds <- list(
    list(
      id = "feed_1",
      data_type = "gtfs",
      created_at = "2023-01-01T00:00:00Z",
      provider = "Test Provider",
      feed_contact_email = "test@example.com",
      status = "active",
      official = TRUE,
      official_updated_at = "2023-01-01T00:00:00Z",
      feed_name = "Test Feed",
      note = NULL,
      source_info = list(
        producer_url = "https://example.com/gtfs.zip",
        license_url = "https://example.com/license"
      ),
      locations = list(
        list(
          country_code = "PT",
          country = "Portugal",
          subdivision_name = "Lisboa",
          municipality = "Lisbon"
        )
      ),
      latest_dataset = list(
        id = "dataset_1",
        hosted_url = "https://example.com/dataset_1.zip",
        bounding_box = list(
          minimum_latitude = 38.7,
          maximum_latitude = 38.8,
          minimum_longitude = -9.2,
          maximum_longitude = -9.1
        ),
        downloaded_at = "2023-01-02T00:00:00Z",
        service_date_range_start = "2023-01-01",
        service_date_range_end = "2023-12-31",
        agency_timezone = "Europe/Lisbon",
        validation_report = list(
          total_error = 0,
          total_warning = 2
        )
      )
    )
  )

  mock_resp <- create_mock_response(status_code = 200)

  testthat::with_mocked_bindings(
    GET = function(url, query, ...) {
      expect_equal(url, "https://api.mobilitydatabase.org/v1/gtfs_feeds")
      expect_equal(query$country_code, "PT")
      expect_equal(query$bounding_filter_method, "partially_enclosed")
      expect_equal(query$limit, 10)
      expect_equal(query$offset, 0)
      return(mock_resp)
    },
    .package = "httr",
    code = {
      testthat::with_mocked_bindings(
        content = function(x, ...) mock_feeds,
        http_error = function(x) FALSE,
        .package = "GTFShift",
        code = {
          df <- GTFShift::query_mobilitydatabase(access_token = "mock_access_token", country_code = "PT")
          expect_s3_class(df, "data.frame")
          expect_equal(nrow(df), 1)
          expect_equal(df$id[1], "feed_1")
          expect_equal(df$provider[1], "Test Provider")
          expect_equal(df$country_code[1], "PT")
          expect_equal(df$latest_dataset_id[1], "dataset_1")
          expect_equal(df$validation_errors[1], 0)
          expect_equal(df$validation_warnings[1], 2)
        }
      )
    }
  )
})

test_that("query_mobilitydatabase fetches access token using refresh token when access_token is missing", {
  mock_resp_post <- create_mock_response(status_code = 200)
  mock_resp_get <- create_mock_response(status_code = 200)

  post_called <- FALSE
  get_called <- FALSE

  testthat::with_mocked_bindings(
    POST = function(url, body, ...) {
      post_called <<- TRUE
      expect_equal(url, "https://api.mobilitydatabase.org/v1/tokens")
      expect_true(grepl("mock_refresh_token", body))
      return(mock_resp_post)
    },
    GET = function(url, query, ...) {
      get_called <<- TRUE
      expect_equal(url, "https://api.mobilitydatabase.org/v1/gtfs_feeds")
      return(mock_resp_get)
    },
    .package = "GTFShift",
    code = {
      testthat::with_mocked_bindings(
        content = function(x, ...) {
          if (!get_called) {
            return(list(access_token = "new_access_token_from_refresh"))
          } else {
            return(list(
              list(
                id = "feed_2",
                provider = "Provider 2",
                status = "active"
              )
            ))
          }
        },
        http_error = function(x) FALSE,
        .package = "GTFShift",
        code = {
          df <- GTFShift::query_mobilitydatabase(refresh_token = "mock_refresh_token")
          expect_true(post_called)
          expect_true(get_called)
          expect_equal(nrow(df), 1)
          expect_equal(df$id[1], "feed_2")
        }
      )
    }
  )
})

test_that("query_mobilitydatabase handles HTTP errors when getting feeds", {
  mock_resp <- create_mock_response(status_code = 401)

  testthat::with_mocked_bindings(
    GET = function(url, ...) mock_resp,
    .package = "httr",
    code = {
      testthat::with_mocked_bindings(
        content = function(x, ...) list(detail = "Unauthorized"),
        http_error = function(x) TRUE,
        http_status = function(x) "Client error: (401) Unauthorized",
        .package = "GTFShift",
        code = {
          expect_error(
            GTFShift::query_mobilitydatabase(access_token = "invalid_token"),
            "Mobility database bad response: Client error: \\(401\\) Unauthorized"
          )
        }
      )
    }
  )
})

test_that("query_mobilitydatabase handles HTTP errors during refresh token exchange", {
  mock_resp_post <- create_mock_response(status_code = 400)

  testthat::with_mocked_bindings(
    POST = function(url, ...) mock_resp_post,
    .package = "httr",
    code = {
      testthat::with_mocked_bindings(
        content = function(x, ...) list(detail = "Invalid refresh token"),
        http_error = function(x) TRUE,
        http_status = function(x) "Client error: (400) Bad Request",
        .package = "GTFShift",
        code = {
          expect_error(
            GTFShift::query_mobilitydatabase(refresh_token = "bad_refresh_token"),
            "Mobility database bad response: Client error: \\(400\\) Bad Request"
          )
        }
      )
    }
  )
})

test_that("query_mobilitydatabase formats bbox parameter correctly", {
  mock_resp <- create_mock_response(status_code = 200)

  bbox_obj <- structure(
    list(ymin = list(38.7), ymax = list(38.8), xmin = list(-9.2), xmax = list(-9.1)),
    class = "bbox"
  )

  testthat::with_mocked_bindings(
    GET = function(url, query, ...) {
      expect_equal(query$dataset_latitudes, "38.700000,38.800000")
      expect_equal(query$dataset_longitudes, "-9.200000,-9.100000")
      return(mock_resp)
    },
    .package = "httr",
    code = {
      testthat::with_mocked_bindings(
        content = function(x, ...) list(),
        http_error = function(x) FALSE,
        .package = "GTFShift",
        code = {
          df <- GTFShift::query_mobilitydatabase(access_token = "token", bbox = bbox_obj)
          expect_equal(nrow(df), 0)
        }
      )
    }
  )
})

