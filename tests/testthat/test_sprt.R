library(testthat)

test_that("SPRT returns decision", {
  x <- c(0,0,1,0,1,1,1,0,0,1,0,0)
  res <- sprt(x, alpha = 0.05, beta = 0.1, p0 = 0.1, p1 = 0.3)
  expect_true("decision" %in% names(res))
})
devtools::build()       # creates tar.gz
devtools::check()       # runs local checks
