test_that("comp2long data format conversion works", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  inputs <- SS_read(path)
  dat <- SS_readdat(file.path(path, "data.ss"), verbose = FALSE)
  x <- dat$lencomp

  # Main argument can be a list or data frame
  y.inputs <- comp2long(inputs)
  y.dat <- comp2long(dat)
  y <- comp2long(x)
  expect_equal(y, y.inputs)
  expect_equal(y, y.dat)

  # Rename second to last column
  y.weight <- comp2long(x, measure = "weight")
  expect_equal(names(y.weight)[7], "weight")

  # Shrink output by omitting zero frequencies
  n.full <- nrow(comp2long(x))
  n.pos <- nrow(comp2long(x, zero = FALSE))
  expect_equal(n.full, 800)
  expect_equal(n.pos, 418)
})
