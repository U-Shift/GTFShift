library(testthat)

test_that("classify_frequency_los assigns correct HCM level of service categories", {
    df <- data.frame(frequency = c(0, 1, 2, 4, 6, 8))
    res <- GTFShift::classify_frequency_los(df)

    expect_contains(names(res), "frequency_los")
    expect_equal(res$frequency_los, c("F", "E", "D", "C", "B", "A"))
})
