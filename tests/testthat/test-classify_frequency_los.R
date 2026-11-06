library(testthat)

#' @srrstats {G2.11} Tests `classify_frequency_los()` with `units`-classed frequency columns.
#' @srrstats {G2.12} Tests `classify_frequency_los()` with `data.frame` inputs containing `list` columns.
#' @srrstats {G5.2} Tests error messages on invalid `frequencies` or missing `frequency_col`.
#' @srrstats {G5.4} Tests HCM 2000 Level of Service (A-F) classification thresholds against known values.
#' @srrstats {EA6.0} Tests return object class, dimensions, column names, and column types.
#' @srrstats {EA6.0a} Tests `data.frame` class of returned object.
#' @srrstats {EA6.0b} Tests `nrow()` and `ncol()` dimensions of returned `data.frame`.
#' @srrstats {EA6.0c} Tests column names (`frequency`, `frequency_los`).
#' @srrstats {EA6.0d} Tests character type of `frequency_los`.
#' @srrstats {SP4.1} Verifies that `units` on input `frequency` column are preserved in the returned table.
test_that("classify_frequency_los assigns correct HCM level of service categories", {
    df <- data.frame(frequency = c(0, 1, 2, 4, 6, 8))
    res <- GTFShift::classify_frequency_los(df)

    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), 6)
    expect_equal(ncol(res), 2)
    expect_contains(names(res), c("frequency", "frequency_los"))
    expect_type(res$frequency_los, "character")
    expect_equal(res$frequency_los, c("F", "E", "D", "C", "B", "A"))
})

test_that("classify_frequency_los handles units and list columns and validates inputs (G2.11, G2.12, SP4.1)", {
    df_units <- data.frame(
        frequency = units::set_units(c(0, 2, 7), "1/h"),
        routes = I(list("R1", c("R1", "R2"), "R3"))
    )
    res <- GTFShift::classify_frequency_los(df_units)
    expect_s3_class(res$frequency, "units")
    expect_type(res$routes, "list")
    expect_equal(res$frequency_los, c("F", "D", "A"))

    expect_error(
        GTFShift::classify_frequency_los("not_a_df"),
        "frequencies must be a data.frame"
    )
    expect_error(
        GTFShift::classify_frequency_los(df_units, frequency_col = "missing_col"),
        "frequency_col must be a single column name"
    )
})
