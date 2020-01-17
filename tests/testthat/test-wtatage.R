context("Read in weight-at-age data file.")

file.temp <- tempfile(pattern = "wtatage", fileext = ".ss")

test_that("Test weight-at-age is read in.", {
  sink(file = file.temp)
  cat("# This is a temp file\n")
  cat(" # With a space before the comment\n")
  cat(" 34#Nages\n")
  cat(rep(19, times = 35+6), "\n")
  cat(-9999, rep(0, times = 34+6), "\n")
  sink()
    temp <- SS_readwtatage(file = file.temp)
    expect_equal(NCOL(temp), 41, tolerance = 0.01,
      label = "Number of columns in read weight-at-age")
    expect_equal(colnames(temp)[NCOL(temp)], "34",
      label = "Last column name")
})

test_that("Test comment column is created and blank lines are okay", {
  sink(file = file.temp)
  cat("# This is a temp file\n")
  cat(" # With a space before the comment\n")
  cat(" 34#Nages\n")
  cat(rep(19, times = 35+6), "#comment", "\n\n")
  cat(rep(19, times = 35+6), "#comment", "\n\n")
  cat(-9999, rep(0, times = 34+7), "\n")
  sink()
    temp <- SS_readwtatage(file = file.temp)
    expect_equal(NCOL(temp), 42, tolerance = 0.01,
      label = "Number of columns in read weight-at-age")
    expect_equal(colnames(temp)[NCOL(temp)], "comment",
      label = "Last column name")
    expect_equal(NROW(temp), 2,
      label = "Number of rows of data")
})

unlink(file.temp, recursive = TRUE)
rm(file.temp)
