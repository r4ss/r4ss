test_that("comp2long data format conversion works", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  inputs <- SS_read(path)
  dat <- SS_readdat(file.path(path, "data.ss"), verbose = FALSE)
  x <- dat[["lencomp"]]
  y <- dat[["agecomp"]]

  # Main argument can be a list or data frame
  out.inputs <- comp2long(inputs)
  out.dat <- comp2long(dat)
  out.x <- comp2long(x)
  expect_equal(out.x, out.inputs)
  expect_equal(out.x, out.dat)

  # Rename second to last column
  out.weight <- comp2long(x, measure = "weight")
  expect_equal(names(out.weight)[7], "weight")

  # Shrink output by omitting zero frequencies
  nx.default <- nrow(comp2long(x))
  nx.zero <- nrow(comp2long(x, zero = FALSE))
  ny.default <- nrow(comp2long(y))
  ny.zero <- nrow(comp2long(y, zero = FALSE))
  expect_equal(nx.default, 800)
  expect_equal(nx.zero, 418)
  expect_equal(ny.default, 480)
  expect_equal(ny.zero, 256)

  # Expand output by repeating recurring entries
  ny.expand <- nrow(comp2long(y, expand = TRUE))
  expect_equal(ny.expand, 400)

  # Aggregate by sex
  x.agg <- aggregate(freq~sex, comp2long(x), sum)
  y.agg <- aggregate(freq~sex, comp2long(y), sum)
  expect_equal(x.agg$freq, c(408, 392))
  expect_equal(y.agg$freq, c(216, 184))
})
