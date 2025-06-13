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
temp_path <- file.path(tempdir(), "test_basics")
dir.create(temp_path, showWarnings = FALSE)
# remove all artifacts created from testing. (developers: simply comment out
# the line below if you want to keep artifacts for troubleshooting purposes)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

###############################################################################
# testing SS_output
###############################################################################

simple_small <- SS_output(
  file.path(example_path, "simple_small"),
  verbose = FALSE,
  printstats = FALSE
)
test_that("SS_output() runs on simple_small model", {
  expect_equal(tail(names(simple_small), 1), "inputs")
})

# tests where stuff is missing
# missing MCMC folder
test_that("SS_output() generates warning when MCMC folder is missing", {
  expect_warning(SS_output(
    dir = file.path(example_path, "simple_small"),
    dir.mcmc = "mcmc",
    warn = FALSE,
    printstats = FALSE,
    hidewarn = TRUE,
    verbose = FALSE
  ))
})

# missing a bunch of files
simple_small <- SS_output(
  file.path(example_path, "simple_small"),
  verbose = FALSE,
  printstats = FALSE,
  compfile = "wrong file",
  covarfile = "wrong file",
  forefile = "wrong file",
  wtfile = "wrong file",
  warnfile = "wrong file"
)
test_that("SS_output() runs when lots of inputs are unavailable", {
  expect_equal(tail(names(simple_small), 1), "inputs")
})

###############################################################################
# Test get_ncol() used by SS_output()
###############################################################################
test_that("get_ncol() finds the correct number of columns for SS_output()", {
  reportfiles <- dir(
    example_path,
    pattern = "^Report",
    recursive = TRUE,
    full.names = TRUE
  )
  results <- mapply(r4ss:::get_ncol, reportfiles)
  expect_true(is.atomic(results))
  # 55 and 56 are values for simple_3.24 and simple_3.30.XX models
  # 61 is the value for simple_small
  expect(
    all(results %in% c(55, 56, 61)),
    "Optimum width of Report.sso wasn't 55, 56, or 61"
  )
})

###############################################################################
# specific tests for elements in the list created by SS_output
###############################################################################
test_that("SS_output() list: Kobe looks right", {
  expect_true(all(simple_small$Kobe$Year %in% 2011:2032))
})

###############################################################################
# testing SS_plots with models loaded above
###############################################################################
test_that("SS_plots runs on simple_small model", {
  plots_simple_small <- SS_plots(
    simple_small,
    dir = temp_path,
    printfolder = "plots_simple_small",
    verbose = FALSE
  )
  expect_equal(tail(plots_simple_small$file, 1), "parameterchecks.html")
})

###############################################################################
# testing SSsummarize, SSplotComparisons, and SStableComparisons
###############################################################################

test_that("SSsummarize and SSplotComparisons both work", {
  # run summarize function (repeating same model twice)
  simple_summary <- SSsummarize(list(
    simple_small,
    simple_small
  ))

  # plot comparisons of results
  comparison_plots <- SSplotComparisons(
    simple_summary,
    png = TRUE,
    plotdir = temp_path,
    verbose = FALSE,
    indexUncertainty = TRUE
  )
  # confirm that function finished
  # (present of plot 19 depends on having SmryBio in derived quants)
  if (all(is.na(simple_summary[["SmryBioSD"]][["Label"]]))) {
    expect_equal(length(comparison_plots), 18)
  } else {
    expect_equal(length(comparison_plots), 19)
  }

  # make table of comparisons
  simple_table <- SStableComparisons(simple_summary, verbose = FALSE)
  # confirm that output produces a data.frame
  # with 3 variables (label, model1, model2)
  expect_output(str(simple_table), "variables")
})

###############################################################################
# testing read/write dat functions for simple_small
###############################################################################

test_that("SS_readdat() and SS_writedat() both work for simple_small", {
  # read data file
  simple_small_dat <- SS_readdat(
    file = file.path(example_path, "simple_small/data.ss"),
    verbose = FALSE
  )
  # write data file
  SS_writedat(
    datlist = simple_small_dat,
    outfile = file.path(temp_path, "testdat.ss"),
    verbose = FALSE
  )
  expect_true(file.exists(file.path(temp_path, "testdat.ss")))
})

# Note: for blank lines, SS_readdat() just warns, while SS_writedat() removes.
test_that("SS_readdat()/SS_writedat() removes lines of 0 age comps from data file", {
  datfile <- readLines(file.path(example_path, "simple_small/data.ss"))
  comp_lines_age <- grep(
    "^-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
    datfile
  )[1]
  datfile <- append(
    datfile,
    values = "2022  7 2  3 0 2 1 -1 25  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
    after = comp_lines_age - 1
  )
  writeLines(datfile, file.path(temp_path, "zero_comps.dat"))
  dat <- expect_warning(
    SS_readdat(file.path(temp_path, "zero_comps.dat"), verbose = FALSE),
    "Lines of all zero age comp found"
  )
  expect_warning(SS_writedat(
    dat,
    file.path(temp_path, "comps_rm_age.dat"),
    verbose = FALSE
  ))
  dat2 <- SS_readdat(file.path(temp_path, "comps_rm_age.dat"))
  expect_true(nrow(dat[["agecomp"]]) == (nrow(dat2[["agecomp"]]) + 1))
})

test_that("SS_readdat()/SS_writedat() removes lines of 0 length comps from data file", {
  datfile <- readLines(file.path(example_path, "simple_small/data.ss"))
  comp_lines_len <- grep(
    "-9999 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
    datfile
  )
  datfile <- append(
    datfile,
    values = " 2022 7 2 3 0 50  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
    after = comp_lines_len - 1
  )
  writeLines(datfile, file.path(temp_path, "zero_comps_len.dat"))
  dat <- expect_warning(
    SS_readdat(file.path(temp_path, "zero_comps_len.dat"), verbose = FALSE),
    "Lines of all zero length comp found"
  )
  expect_warning(SS_writedat(
    dat,
    file.path(temp_path, "comps_rm_len.dat"),
    verbose = FALSE
  ))
  dat2 <- SS_readdat(file.path(temp_path, "comps_rm_len.dat"), verbose = FALSE)
  expect_true(nrow(dat[["lencomp"]]) == (nrow(dat2[["lencomp"]]) + 1))
})


###############################################################################
# testing read/write forecast functions
###############################################################################

test_that("SS_readforecast() and SS_writeforecast() both work", {
  # read forecast file
  simple_small_forecast <-
    SS_readforecast(
      file = file.path(example_path, "simple_small/forecast.ss"),
      verbose = FALSE
    )

  # write forecast file
  suppressWarnings(SS_writeforecast(
    mylist = simple_small_forecast,
    dir = temp_path,
    file = "testforecast.ss",
    overwrite = TRUE,
    verbose = FALSE
  ))
  expect_true(file.exists(file.path(temp_path, "testforecast.ss")))
  # mock a file with forecast option 0
  fore_0 <- simple_small_forecast
  fore_0[["Forecast"]] <- 0

  # write short and long versions (and check that write okay)
  SS_writeforecast(
    mylist = fore_0,
    dir = temp_path,
    file = "fore_0_long.ss",
    verbose = FALSE,
    writeAll = TRUE
  )
  SS_writeforecast(
    mylist = fore_0,
    dir = temp_path,
    file = "fore_0_short.ss",
    verbose = FALSE,
    writeAll = FALSE
  )
  expect_true(file.exists(file.path(temp_path, "fore_0_long.ss")))
  expect_true(file.exists(file.path(temp_path, "fore_0_short.ss")))

  # make sure SS_readforecast can read short and long versions
  fore_0_read_short <-
    SS_readforecast(
      file = file.path(temp_path, "fore_0_short.ss"),
      verbose = FALSE,
      readAll = FALSE
    )
  expect_length(fore_0_read_short, 11)

  expect_warning(
    fore_0_read_all_long <-
      SS_readforecast(
        file = file.path(temp_path, "fore_0_long.ss"),
        verbose = FALSE,
        readAll = TRUE
      ),
    "Forecast = 0 should always be used with 1 forecast year"
  )
  expect_length(fore_0_read_all_long, 34)

  fore_0_read_some_long <-
    SS_readforecast(
      file = file.path(temp_path, "fore_0_long.ss"),
      verbose = FALSE,
      readAll = FALSE
    )
  expect_length(fore_0_read_some_long, 11)

  # make sure warning provided if try to read all with a short file
  expect_warning(
    fore_0_read_all_short <-
      SS_readforecast(
        file = file.path(temp_path, "fore_0_short.ss"),
        verbose = FALSE,
        readAll = TRUE
      ),
    "readAll selected as TRUE, but lines beyond Forecast are not present"
  )
  expect_length(fore_0_read_all_short, 11)
  # check that warning provided if writeAll = TRUE, but only a short version
  # available
  expect_warning(
    SS_writeforecast(
      mylist = fore_0_read_short,
      dir = temp_path,
      file = "fore_0_read_short.ss",
      verbose = FALSE,
      writeAll = TRUE,
      overwrite = TRUE
    ),
    "Even though writeAll == TRUE, r4ss"
  )
  expect_true(file.exists(file.path(temp_path, "fore_0_read_short.ss")))
})

###############################################################################
# testing read/write starter functions
###############################################################################

test_that("SS_readstarter() and SS_writestarter() both work", {
  # read data file
  start <- SS_readstarter(
    file = file.path(example_path, "simple_small/starter.ss"),
    verbose = FALSE
  )
  # write data file
  SS_writestarter(
    start,
    dir = temp_path,
    file = "teststarter.ss",
    verbose = FALSE
  )
  expect_true(file.exists(file.path(temp_path, "teststarter.ss")))
})
