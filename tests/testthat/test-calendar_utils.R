library(testthat)

test_that("calendar_nextBusinessWednesday computes next Wednesday without network call when country_code is NA", {
    start_date <- as.Date("2026-07-20") # Monday
    next_wed <- GTFShift::calendar_nextBusinessWednesday(start_date = start_date, country_code = NA)
    expect_equal(next_wed, as.Date("2026-07-22"))
})

test_that("calendar_nextBusinessWednesday fetches new year holidays when next Wednesday triggers new year", {
    # 2026-12-30 is Wednesday. If 2026-12-30 is a holiday, the next Wednesday is 2027-01-06 (new year)
    start_date <- as.Date("2026-12-28") # Monday
    called_years <- c()

    testthat::with_mocked_bindings(
        GET = function(url, ...) {
            # Extract year from URL https://date.nager.at/api/v3/PublicHolidays/{year}/{country}
            yr <- gsub(".*/PublicHolidays/([0-9]+)/.*", "\\1", url)
            called_years <<- c(called_years, yr)
            
            content_json <- if (yr == "2026") '[{"date":"2026-12-30"}]' else '[]'
            structure(
                list(
                    status_code = 200,
                    url = url,
                    headers = list("Content-Type" = "application/json"),
                    content = charToRaw(content_json)
                ),
                class = "response"
            )
        },
        .package = "httr",
        code = {
            next_wed <- GTFShift::calendar_nextBusinessWednesday(start_date = start_date, country_code = "PT")
            expect_equal(next_wed, as.Date("2027-01-06"))
        }
    )

    expect_equal(called_years, c("2026", "2027"))
})

test_that("calendar_nextBusinessWednesday handles mocked holiday response", {
    start_date <- as.Date("2026-07-20") # Monday

    testthat::with_mocked_bindings(
        GET = function(url, ...) {
            structure(
                list(
                    status_code = 200,
                    url = url,
                    headers = list("Content-Type" = "application/json"),
                    content = charToRaw('[{"date":"2026-07-22"}]')
                ),
                class = "response"
            )
        },
        .package = "httr",
        code = {
            next_wed <- GTFShift::calendar_nextBusinessWednesday(start_date = start_date, country_code = "PT")
            expect_equal(next_wed, as.Date("2026-07-29"))
        }
    )
})

test_that("calendar_nextBusinessWednesday stops when API does not respond with status 200", {
    start_date <- as.Date("2026-07-20") # Monday

    testthat::with_mocked_bindings(
        GET = function(url, ...) {
            structure(
                list(
                    status_code = 500,
                    url = url
                ),
                class = "response"
            )
        },
        .package = "httr",
        code = {
            expect_error(
                GTFShift::calendar_nextBusinessWednesday(start_date = start_date, country_code = "PT"),
                "Failed to retrieve holidays. Please check your internet connection or API availability."
            )
        }
    )
})
