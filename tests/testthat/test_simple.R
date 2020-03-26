###############################################################################
### automated tests of r4ss package
###############################################################################

context("Basic r4ss functions")

example_path <- system.file("extdata", package = "r4ss")
# create a temporary directory location to write files to. 
# to view the temp_path location, you can add a browser() statement after 
# creating the temp_path directory and see what temp_path is. R should write 
# to the same location for the same R session (if you restart R, temp_path will)
# change.
temp_path <- file.path(tempdir(), "test_simple")
dir.create(temp_path, showWarnings = FALSE)
# remove all artifacts created from testing. (developers: simply comment out 
# the line below if you want to keep artifacts for troubleshooting purposes)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

###############################################################################
# testing SS_output 
###############################################################################
simple3.24 <- SS_output(file.path(example_path,"simple_3.24"), verbose = FALSE, 
                        printstats = FALSE)
test_that("SS_output runs on simple_3.24 model", {
  expect_equal(tail(names(simple3.24),1), "inputs")
})

simple3.30.01 <- SS_output(file.path(example_path,"simple_3.30.01"), 
                           verbose = FALSE, printstats = FALSE)
test_that("SS_output runs on simple_3.30.01 model", {
  expect_equal(tail(names(simple3.30.01),1), "inputs")
})

simple3.30.12 <- SS_output(file.path(example_path,"simple_3.30.12"),
                           verbose = FALSE, printstats = FALSE)
test_that("SS_output runs on simple_3.30.12 model", {
  expect_equal(tail(names(simple3.30.12),1), "inputs")
})

simple3.30.13 <- SS_output(file.path(example_path,"simple_3.30.13"),
                           verbose = FALSE, printstats = FALSE)
test_that("SS_output runs on simple_3.30.13 model", {
  expect_equal(tail(names(simple3.30.13),1), "inputs")
})

test_that("SS_output generates warning when MCMC folder is missing", {
  expect_warning(SS_output(dir=tail(dir(example_path, full.names = TRUE),1),
   dir.mcmc="mcmc", warn=FALSE, printstats=FALSE, hidewarn=TRUE, verbose=FALSE)
  )
})

###############################################################################
# Test an internal function used by SS_output
###############################################################################
test_that("get_ncol finds the correct number of columns for SS_output", {
  reportfiles <- dir(example_path, pattern = "^Report", recursive = TRUE, 
                     full.names = TRUE)
  results <- mapply(r4ss:::get_ncol, reportfiles)
  expect_vector(results)
  expect(all(results %in% 55:56), 
    "Optimum width of Report.sso wasn't 55 or 56.")
})

###############################################################################
# specific tests for elements in the list created by SS_output
###############################################################################
test_that("SS_output list: Kobe looks right", {
  expect_true(all(simple3.24$Kobe$Year %in% 1950:2050 ))
  expect_true(all(simple3.30.01$Kobe$Year %in% 1950:2050 ))
  expect_true(all(simple3.30.12$Kobe$Year %in% 1950:2050 ))
  expect_true(all(simple3.30.13$Kobe$Year %in% 1950:2050 ))
})

###############################################################################
# testing SS_plots with models loaded above
###############################################################################

test_that("SS_plots runs on simple_3.24 model", {
  plots3.24 <- SS_plots(simple3.24, dir = temp_path, printfolder = "plots_3.24", 
                        verbose = FALSE)
  expect_equal(tail(plots3.24$file, 1), "parameterchecks.html")
})

test_that("SS_plots runs on simple_3.30.01 model", {
  plots3.30.01 <- SS_plots(simple3.30.01, dir = temp_path, 
                           printfolder = "plots_3.30.01", verbose = FALSE)
  expect_equal(tail(plots3.30.01$file, 1), "parameterchecks.html")
})

test_that("SS_plots runs on simple_3.30.12 model", {
  plots3.30.12 <- SS_plots(simple3.30.12, dir = temp_path, 
                           printfolder = "plots_3.30.12", verbose = FALSE)
  expect_equal(tail(plots3.30.12$file, 1), "parameterchecks.html")
})

test_that("SS_plots runs on simple_3.30.13 model", {
  plots3.30.13 <- SS_plots(simple3.30.13, dir = temp_path, 
                           printfolder = "plots_3.30.13", verbose = FALSE)
  expect_equal(tail(plots3.30.13$file, 1), "parameterchecks.html")
})

###############################################################################
# testing SSsummarize, SSplotComparisons, and SStableComparisons
###############################################################################

test_that("SSsummarize and SSplotComparisons both work", {
  # run summarize function
  simple_summary <- SSsummarize(list(simple3.24, simple3.30.01,
                                     simple3.30.12, simple3.30.13))

  # plot comparisons of results
  comparison_plots <- SSplotComparisons(simple_summary, png=TRUE,
                                        plotdir=temp_path, verbose = FALSE)
  # confirm that function finished
  expect_equal(comparison_plots, "finished comparison plots")

  # make table of comparisons
  simple_table <- SStableComparisons(simple_summary, verbose = FALSE)
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
                               version="3.24", verbose = FALSE)
  # write data file
  SS_writedat(datlist = simple3.24_dat,
              outfile = file.path(temp_path, "testdat_3.24.ss"),
              version = "3.24",
              faster = FALSE, verbose = FALSE)
  expect_true(file.exists(file.path(temp_path, "testdat_3.24.ss")))

  # write data file with faster option
  suppressWarnings(SS_writedat(datlist = simple3.24_dat,
                   outfile = file.path(temp_path, "fastdat_3.24.ss"),
                   version = "3.24",
                   faster = TRUE, verbose = FALSE))
  expect_true(file.exists(file.path(temp_path, "fastdat_3.24.ss")))
})

###############################################################################
# testing read/write dat functions for 3.30.01
###############################################################################

test_that("SS_readdat and SS_writedat both work for 3.30.01", {
  # read data file - supress warnings b/c warning number of columns of results
  # is not a multiple of vector length (arg 2) can be safely ignored
  suppressWarnings(
    simple3.30.01_dat <- SS_readdat(
      file = file.path(example_path,"simple_3.30.01/simple.dat"),
      version="3.30", verbose = FALSE)
  )
  # write data file
  SS_writedat(datlist = simple3.30.01_dat,
              outfile = file.path(temp_path, "testdat_3.30.01.ss"),
              faster = FALSE, verbose = FALSE)
  expect_true(file.exists(file.path(temp_path, "testdat_3.30.01.ss")))

  # write data file with faster option - suppress warnings b/c they can be 
  # safely ignored.
  suppressWarnings(SS_writedat(datlist = simple3.30.01_dat,
                    outfile = file.path(temp_path, "fastdat_3.30.01.ss"),
                    faster = TRUE, verbose = FALSE))
  expect_true(file.exists(file.path(temp_path, "fastdat_3.30.01.ss")))
})

###############################################################################
# testing read/write dat functions for 3.30.12
###############################################################################

test_that("SS_readdat and SS_writedat both work for 3.30.12", {
  # read data file
  simple3.30.12_dat <- SS_readdat(file = file.path(example_path,"simple_3.30.12/simple_data.ss"),
                               version="3.30", verbose = FALSE)
  # write data file
  SS_writedat(datlist = simple3.30.12_dat,
              outfile = file.path(temp_path, "testdat_3.30.12.ss"),
              faster = FALSE, verbose = FALSE)
  expect_true(file.exists(file.path(temp_path, "testdat_3.30.12.ss")))
  # write data file with faster option
  suppressWarnings(SS_writedat(datlist = simple3.30.12_dat,
                    outfile = file.path(temp_path, "fastdat_3.30.12.ss"),
                    faster = TRUE, verbose = FALSE))
  expect_true(file.exists(file.path(temp_path, "fastdat_3.30.12.ss")))
})

###############################################################################
# testing read/write dat functions for 3.30.13
###############################################################################

test_that("SS_readdat and SS_writedat both work for 3.30.13", {
  # read data file
  simple3.30.13_dat <- SS_readdat(file = file.path(example_path,"simple_3.30.13/simple_data.ss"),
                               version="3.30", verbose = FALSE)
  # write data file
  SS_writedat(datlist = simple3.30.13_dat,
              outfile = file.path(temp_path, "testdat_3.30.13.ss"),
              faster = FALSE, verbose = FALSE)
  expect_true(file.exists(file.path(temp_path, "testdat_3.30.13.ss")))
  # write data file with faster option
  suppressWarnings(SS_writedat(datlist = simple3.30.13_dat,
                    outfile = file.path(temp_path, "fastdat_3.30.13.ss"),
                    faster = TRUE, verbose = FALSE))
  expect_true(file.exists(file.path(temp_path, "fastdat_3.30.13.ss")))
})


###############################################################################
# testing read/write forecast functions for 3.30.13
###############################################################################

test_that("SS_readforecast and SS_writeforecast both work for 3.30.13", {
  # read forecast file
  simple3.30.13_forecast <-
    SS_readforecast(file = file.path(example_path,"simple_3.30.13/forecast.ss"),
                    version="3.30", verbose = FALSE)

  # write forecast file
  suppressWarnings(SS_writeforecast(mylist = simple3.30.13_forecast,
                     dir = temp_path,
                     file = "testforecast_3.30.13.ss",
                     overwrite = TRUE, verbose = FALSE))
  expect_true(file.exists(file.path(temp_path, "testforecast_3.30.13.ss")))
  # mock a file with forecast option 0
  fore_0 <- simple3.30.13_forecast
  fore_0[["Forecast"]] <- 0
  
  # write short and long versions (and check that write okay)
  SS_writeforecast(mylist = fore_0, dir = temp_path, file = "fore_0_long.ss", 
                   verbose = FALSE, writeAll = TRUE)
  SS_writeforecast(mylist = fore_0, dir = temp_path, file = "fore_0_short.ss", 
                   verbose = FALSE, writeAll = FALSE)
  expect_true(file.exists(file.path(temp_path, "fore_0_long.ss")))
  expect_true(file.exists(file.path(temp_path, "fore_0_short.ss")))
  # make sure SS_readforecast can read short and long versions
  fore_0_read_short <-
    SS_readforecast(file = file.path(temp_path, "fore_0_short.ss"),
                    version = "3.30", verbose = FALSE, readAll = FALSE)
  expect_length(fore_0_read_short, 11)
  
  expect_warning(fore_0_read_all_long <-
    SS_readforecast(file = file.path(temp_path,"fore_0_long.ss"),
                    version="3.30", verbose = FALSE, readAll = TRUE), 
    "Forecast = 0 should always be used with 1 forecast year")
  expect_length(fore_0_read_all_long, 34)
  
  fore_0_read_some_long <-
    SS_readforecast(file = file.path(temp_path,"fore_0_long.ss"),
                    version="3.30", verbose = FALSE, readAll = FALSE)
  expect_length(fore_0_read_some_long, 11)
  
  # make sure warning provided if try to read all with a short file
  expect_warning(fore_0_read_all_short <-
    SS_readforecast(file = file.path(temp_path,"fore_0_short.ss"),
                    version="3.30", verbose = FALSE, readAll = TRUE), 
    "readAll selected as TRUE, but lines beyond Forecast are not present")
  expect_length(fore_0_read_all_short, 11)
  # check that warning provided if writeAll = TRUE, but only a short version
  # available
  expect_warning(SS_writeforecast(mylist = fore_0_read_short,
                                  dir = temp_path, 
                                  file = "fore_0_read_short.ss", 
                                  verbose = FALSE, 
                                  writeAll = TRUE),
    "Even though writeAll == TRUE, cannot write past list element Forecast")
  expect_true(file.exists(file.path(temp_path, "fore_0_read_short.ss")))
})
