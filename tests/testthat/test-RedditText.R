library(testthat)
library(jonoMean)


context("Internet connection")

test_that("Internet connection", {
  expect_equal(http_error(GET("www.google.com")), FALSE)
})

