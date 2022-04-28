test_that("plotCI function works", {
  # Note: could look into using snapshot test to verify output
  set.seed(123)
  x <- 1:10
  y <- rnorm(10)
  uiw <- 1
  liw <- 2
  output <- plotCI(x = x, y = y, uiw = uiw, liw = liw)
  expect_equivalent(x, output[["x"]])
  expect_equivalent(y, output[["y"]])
})
