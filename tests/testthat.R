library(testthat)
library(r4ss)

# Use "extdata" in "inst" because its loaded with R packages
example_path <- system.file("extdata", package="r4ss")

test_check("r4ss")
