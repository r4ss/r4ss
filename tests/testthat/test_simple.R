###############################################################################
### automated tests of r4ss package
###############################################################################

context("Basic r4ss functions")

example_path <- system.file("extdata", package="r4ss")

###############################################################################
# testing SS_output 
###############################################################################

test_that("SS_output runs on simple_3.24 model", {
  simple3.24 <- SS_output(file.path(example_path,"simple_3.24"))
  expect_equal(tail(names(simple3.24),1), "inputs")
})

test_that("SS_output runs on simple_3.30.01 model", {
  simple3.30.01 <- SS_output(file.path(example_path,"simple_3.30.01"))
  expect_equal(tail(names(simple3.30.01),1), "inputs")
})

test_that("SS_output runs on simple_3.30.12 model", {
  simple3.30.12 <- SS_output(file.path(example_path,"simple_3.30.12"))
  expect_equal(tail(names(simple3.30.12),1), "inputs")
})

###############################################################################
# read models again so they are available in the workspace for later use
###############################################################################

simple3.24 <- SS_output(file.path(example_path,"simple_3.24"),
                        verbose=FALSE, printstats=FALSE)
simple3.30.01 <- SS_output(file.path(example_path,"simple_3.30.01"),
                        verbose=FALSE, printstats=FALSE)
simple3.30.12 <- SS_output(file.path(example_path,"simple_3.30.12"),
                        verbose=FALSE, printstats=FALSE)

###############################################################################
# specific tests for elements in the list created by SS_output
###############################################################################
test_that("SS_output list: Kobe looks right", {
  expect_true(all(simple3.24$Kobe$Year %in% 1950:2050 ))
  expect_true(all(simple3.30.01$Kobe$Year %in% 1950:2050 ))
  expect_true(all(simple3.30.12$Kobe$Year %in% 1950:2050 ))
})

###############################################################################
# testing SS_plots with models loaded above
###############################################################################

test_that("SS_plots runs on simple_3.24 model", {
  plots3.24 <- SS_plots(simple3.24, datplot=TRUE)
  expect_equal(tail(plots3.24$file,1), "data_plot2.png")
})

test_that("SS_plots runs on simple_3.30.01 model", {
  plots3.30.01 <- SS_plots(simple3.30.01, datplot=TRUE)
  expect_equal(tail(plots3.30.01$file,1), "data_plot2.png")
})

test_that("SS_plots runs on simple_3.30.12 model", {
  plots3.30.12 <- SS_plots(simple3.30.12, datplot=TRUE)
  expect_equal(tail(plots3.30.12$file,1), "data_plot2.png")
})

###############################################################################
# testing SSsummarize, SSplotComparisons, and SStableComparisons
###############################################################################

test_that("SSsummarize and SSplotComparisons both work", {
  # run summarize function
  simple_summary <- SSsummarize(list(simple3.24, simple3.30.01, simple3.30.12))

  # plot comparisons of results
  comparison_plots <- SSplotComparisons(simple_summary, png=TRUE,
                                        plotdir=example_path)
  # confirm that function finished
  expect_equal(comparison_plots, "finished comparison plots")

  # make table of comparisons
  simple_table <- SStableComparisons(simple_summary)
  # confirm that output produces a data.frame
  # with 3 variables (label, model1, model2)
  expect_output(str(simple_table), "variables")
  
})

###############################################################################
# testing read/write dat functions for 3.24
###############################################################################

test_that("SS_readdat and SS_writedat both work for 3.24", {
  # read data file
  simple3.24_dat <- SS_readdat(file = file.path(example_path,"simple_3.24/simple.dat"),
                               version="3.24")
  # write data file
  SS_writedat(datlist = simple3.24_dat,
              outfile = file.path(example_path, "simple_3.24/testdat_3.24.ss"),
              version = "3.24",
              faster = FALSE)

  # write data file with faster option
  SS_writedat(datlist = simple3.24_dat,
              outfile = file.path(example_path, "simple_3.24/fastdat_3.24.ss"),
              version = "3.24",
              faster = TRUE)
})

###############################################################################
# testing read/write dat functions for 3.30.01
###############################################################################

test_that("SS_readdat and SS_writedat both work for 3.30.01", {
  # read data file
  simple3.30.01_dat <- SS_readdat(file = file.path(example_path,"simple_3.30.01/simple.dat"),
                               version="3.30")
  # write data file
  SS_writedat(datlist = simple3.30.01_dat,
              outfile = file.path(example_path, "simple_3.30.01/testdat_3.30.01.ss"),
              faster = FALSE)

  # write data file with faster option
  SS_writedat(datlist = simple3.30.01_dat,
              outfile = file.path(example_path, "simple_3.30.01/fastdat_3.30.01.ss"),
              faster = TRUE)
})

###############################################################################
# testing read/write dat functions for 3.30.12
###############################################################################

test_that("SS_readdat and SS_writedat both work for 3.30.12", {
  # read data file
  simple3.30.12_dat <- SS_readdat(file = file.path(example_path,"simple_3.30.12/simple.dat"),
                               version="3.30")
  # write data file
  SS_writedat(datlist = simple3.30.12_dat,
              outfile = file.path(example_path, "simple_3.30.12/testdat_3.30.12.ss"),
              faster = FALSE)

  # write data file with faster option
  SS_writedat(datlist = simple3.30.12_dat,
              outfile = file.path(example_path, "simple_3.30.12/fastdat_3.30.12.ss"),
              faster = TRUE)
})
