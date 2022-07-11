###############################################################################
### tests of older ss3 version (3.24, 3.30.01, 3.30.12, 3.30.13)
### this file should not be included in the CRAN version
### because they depend on model files that are too large
###############################################################################

context("Basic r4ss functions")

example_path <- system.file("extdata", package = "r4ss")
# create a temporary directory location to write files to.
# to view the temp_path location, you can add a browser() statement after
# creating the temp_path directory and see what temp_path is. R should write
# to the same location for the same R session (if you restart R, temp_path will)
# change.
temp_path <- file.path(tempdir(), "test_older_ss3")
dir.create(temp_path, showWarnings = FALSE)
# remove all artifacts created from testing. (developers: simply comment out
# the line below if you want to keep artifacts for troubleshooting purposes)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

###############################################################################
# testing SS_output
###############################################################################
simple3.24 <- SS_output(file.path(example_path, "simple_3.24"),
  verbose = FALSE,
  printstats = FALSE
)
test_that("SS_output() runs on simple_3.24 model", {
  expect_equal(tail(names(simple3.24), 1), "inputs")
})

simple3.30.01 <- SS_output(file.path(example_path, "simple_3.30.01"),
  verbose = FALSE, printstats = FALSE
)
test_that("SS_output() runs on simple_3.30.01 model", {
  expect_equal(tail(names(simple3.30.01), 1), "inputs")
})

simple3.30.12 <- SS_output(file.path(example_path, "simple_3.30.12"),
  verbose = FALSE, printstats = FALSE
)
test_that("SS_output() runs on simple_3.30.12 model", {
  expect_equal(tail(names(simple3.30.12), 1), "inputs")
})

simple3.30.13 <- SS_output(file.path(example_path, "simple_3.30.13"),
  verbose = FALSE, printstats = FALSE
)
test_that("SS_output() runs on simple_3.30.13 model", {
  expect_equal(tail(names(simple3.30.13), 1), "inputs")
})


###############################################################################
# specific tests for elements in the list created by SS_output
###############################################################################
test_that("SS_output() list: Kobe looks right", {
  expect_true(all(simple3.24$Kobe$Year %in% 1950:2050))
  expect_true(all(simple3.30.01$Kobe$Year %in% 1950:2050))
  expect_true(all(simple3.30.12$Kobe$Year %in% 1950:2050))
  expect_true(all(simple3.30.13$Kobe$Year %in% 1950:2050))
})

###############################################################################
# testing SS_plots with models loaded above
###############################################################################

test_that("SS_plots runs on simple_3.24 model", {
  plots3.24 <- SS_plots(simple3.24,
    dir = temp_path, printfolder = "plots_3.24",
    verbose = FALSE
  )
  expect_equal(tail(plots3.24$file, 1), "parameterchecks.html")
})

test_that("SS_plots runs on simple_3.30.01 model", {
  plots3.30.01 <- SS_plots(simple3.30.01,
    dir = temp_path,
    printfolder = "plots_3.30.01", verbose = FALSE
  )
  expect_equal(tail(plots3.30.01$file, 1), "parameterchecks.html")
})

test_that("SS_plots runs on simple_3.30.12 model", {
  plots3.30.12 <- SS_plots(simple3.30.12,
    dir = temp_path,
    printfolder = "plots_3.30.12", verbose = FALSE
  )
  expect_equal(tail(plots3.30.12$file, 1), "parameterchecks.html")
})

test_that("SS_plots runs on simple_3.30.13 model", {
  plots3.30.13 <- SS_plots(simple3.30.13,
    dir = temp_path,
    printfolder = "plots_3.30.13", verbose = FALSE
  )
  expect_equal(tail(plots3.30.13$file, 1), "parameterchecks.html")
})


###############################################################################
# testing SSsummarize, SSplotComparisons, and SStableComparisons
###############################################################################

test_that("SSsummarize and SSplotComparisons both work", {
  # run summarize function
  simple_summary <- SSsummarize(list(
    simple3.24, simple3.30.01,
    simple3.30.12, simple3.30.13
  ))

  # plot comparisons of results
  comparison_plots <- SSplotComparisons(simple_summary,
    png = TRUE,
    plotdir = temp_path, verbose = FALSE,
    indexUncertainty = TRUE
  )
  # confirm that function finished
  expect_equal(length(comparison_plots), 17)

  # make table of comparisons
  simple_table <- SStableComparisons(simple_summary, verbose = FALSE)
  # confirm that output produces a data.frame
  # with 3 variables (label, model1, model2)
  expect_output(str(simple_table), "variables")
})

###############################################################################
# testing read/write dat functions for 3.24
###############################################################################

test_that("SS_readdat() and SS_writedat() both work for 3.24", {
  # read data file
  simple3.24_dat <- SS_readdat(
    file = file.path(example_path, "simple_3.24/simple.dat"),
    version = "3.24", verbose = FALSE
  )
  # write data file
  SS_writedat(
    datlist = simple3.24_dat,
    outfile = file.path(temp_path, "testdat_3.24.ss"),
    version = "3.24", verbose = FALSE
  )
  expect_true(file.exists(file.path(temp_path, "testdat_3.24.ss")))
})

###############################################################################
# testing read/write dat functions for 3.30.01
###############################################################################

test_that("SS_readdat() and SS_writedat() both work for 3.30.01", {
  # read data file - supress warnings b/c warning number of columns of results
  # is not a multiple of vector length (arg 2) can be safely ignored
  suppressWarnings(
    simple3.30.01_dat <- SS_readdat(
      file = file.path(example_path, "simple_3.30.01/simple.dat"),
      verbose = FALSE
    )
  )
  # write data file
  SS_writedat(
    datlist = simple3.30.01_dat,
    outfile = file.path(temp_path, "testdat_3.30.01.ss"),
    verbose = FALSE
  )
  expect_true(file.exists(file.path(temp_path, "testdat_3.30.01.ss")))
})

###############################################################################
# testing read/write dat functions for 3.30.12
###############################################################################

test_that("SS_readdat() and SS_writedat() both work for 3.30.12", {
  # read data file
  simple3.30.12_dat <- SS_readdat(
    file = file.path(example_path, "simple_3.30.12/simple_data.ss"),
    verbose = FALSE
  )
  # write data file
  SS_writedat(
    datlist = simple3.30.12_dat,
    outfile = file.path(temp_path, "testdat_3.30.12.ss"),
    verbose = FALSE
  )
  expect_true(file.exists(file.path(temp_path, "testdat_3.30.12.ss")))
})

###############################################################################
# testing read/write dat functions for 3.30.13
###############################################################################

test_that("SS_readdat() and SS_writedat() both work for 3.30.13", {
  # read data file
  simple3.30.13_dat <- SS_readdat(
    file = file.path(example_path, "simple_3.30.13/simple_data.ss"),
    verbose = FALSE
  )
  # write data file
  SS_writedat(
    datlist = simple3.30.13_dat,
    outfile = file.path(temp_path, "testdat_3.30.13.ss"),
    verbose = FALSE
  )
  expect_true(file.exists(file.path(temp_path, "testdat_3.30.13.ss")))
})


###############################################################################
# testing read/write starter functions for 3.24
###############################################################################

test_that("SS_readstater and SS_writestarter both work for 3.24", {
  # read data file
  start <- SS_readstarter(
    file = file.path(example_path, "simple_3.24/starter.ss"),
    verbose = FALSE
  )
  # write data file
  SS_writestarter(start,
    dir = temp_path,
    file = "teststarter_3.24.ss",
    verbose = FALSE
  )
  expect_true(file.exists(file.path(temp_path, "teststarter_3.24.ss")))
})

###############################################################################
# testing read/write starter functions for 3.24
###############################################################################

context("Read and write control file for 3.24")

# Note: these tests are not very extensive. They only check that a list is
# successfully created.

# read/write in a temporary dir so that the state is not disrupted if tests
# exit early.
tmp_path <- file.path(tempdir(check = TRUE), "test-control")
dir.create(tmp_path, showWarnings = FALSE)
example_path <- system.file("extdata", package = "r4ss")
file.copy(example_path, tmp_path, recursive = TRUE)
# clean up
on.exit(unlink(tmp_path, recursive = TRUE))

# paths to the model files
sim_3.24 <- file.path(tmp_path, "extdata", "simple_3.24")

test_that("SS_readctl() and SS_writectl() work for 3.24", {
  # check exits on error when no datafile provided
  expect_error(
    SS_readctl(file.path(sim_3.24, "simple.ctl"),
      verbose = FALSE, version = "3.24"
    ),
    "Cannot find data file specified in datlist"
  )

  # read data file b/c necessary input to read control
  dat_3.24 <- SS_readdat(file.path(sim_3.24, "simple.dat"),
    verbose = FALSE, version = "3.24"
  )
  # read the control file
  ctl_3.24 <- SS_readctl(
    file = file.path(sim_3.24, "simple.ctl"),
    verbose = FALSE,
    use_datlist = TRUE,
    datlist = dat_3.24,
    version = "3.24"
  )
  expect_type(ctl_3.24, "list")
  # check write control
  if (file.exists(file.path(sim_3.24, "testctl.ss"))) {
    file.remove(file.path(sim_3.24, "testctl.ss"))
  }
  SS_writectl(
    ctllist = ctl_3.24,
    version = "3.24",
    verbose = FALSE,
    overwrite = TRUE,
    outfile = file.path(sim_3.24, "testctl.ss")
  )
  expect_true(file.exists(file.path(sim_3.24, "testctl.ss")))
})

test_that("SS_readctl() and SS_writectl() work for 3.24 when datlist = FALSE", {
  # read data file b/c necessary input to read control
  dat_3.24 <- SS_readdat(file.path(sim_3.24, "simple.dat"),
    verbose = FALSE, version = "3.24"
  )
  # read the control file
  ctl_3.24 <- SS_readctl(
    file = file.path(sim_3.24, "simple.ctl"),
    verbose = FALSE,
    use_datlist = FALSE,
    nseas = dat_3.24[["nseas"]],
    N_areas = dat_3.24[["N_areas"]],
    Nages = dat_3.24[["Nages"]],
    Nsexes = dat_3.24[["Nsexes"]],
    Nfleet = dat_3.24[["Nfleet"]],
    Nsurveys = dat_3.24[["Nsurveys"]],
    N_CPUE_obs = dat_3.24[["N_cpue"]],
    version = "3.24",
    Do_AgeKey = FALSE
  )
  expect_type(ctl_3.24, "list")
  # check write control
  if (file.exists(file.path(sim_3.24, "testctl.ss"))) {
    file.remove(file.path(sim_3.24, "testctl.ss"))
  }
  SS_writectl(
    ctllist = ctl_3.24,
    verbose = FALSE,
    overwrite = TRUE,
    version = "3.24",
    outfile = file.path(sim_3.24, "testctl.ss")
  )
  expect_true(file.exists(file.path(sim_3.24, "testctl.ss")))
})

test_that(paste0(
  "SS_readctl_3.24(), SS_writectl_3.24(), SS_readdat_3.24(), and ",
  " SS_writedat_3.24() work using funs directly"
), {
  # read data file b/c necessary input to read control
  dat_3.24 <- SS_readdat_3.24(file.path(sim_3.24, "simple.dat"),
    verbose = FALSE
  )
  # test write dat
  if (file.exists(file.path(sim_3.24, "testdat.ss"))) {
    file.remove(file.path(sim_3.24, "testdat.ss"))
  }
  SS_writedat_3.24(dat_3.24, file.path(sim_3.24, "testdat.ss"))
  expect_true(file.exists(file.path(sim_3.24, "testdat.ss")))
  # read the control file so that test can run on it
  ctl_3.24 <- SS_readctl_3.24(
    file.path(sim_3.24, "simple.ctl"),
    verbose = FALSE,
    use_datlist = TRUE,
    datlist = dat_3.24
  )
  expect_type(ctl_3.24, "list")
  # check write control
  if (file.exists(file.path(sim_3.24, "testctl.ss"))) {
    file.remove(file.path(sim_3.24, "testctl.ss"))
  }
  SS_writectl_3.24(
    ctllist = ctl_3.24,
    verbose = FALSE,
    overwrite = FALSE,
    outfile = file.path(sim_3.24, "testctl.ss")
  )
  expect_true(file.exists(file.path(sim_3.24, "testctl.ss")))
})

test_that("var adj and q translator fxns, works", {
  # read data file b/c necessary input to read control
  sim_small <- file.path(tmp_path, "extdata", "simple_small")
  dat_small <- SS_readdat(file.path(sim_small, "data.ss"),
    verbose = FALSE
  )
  # read the control file so that test can run on it
  ctl_small <- SS_readctl(
    file.path(sim_small, "control.ss"),
    verbose = FALSE,
    use_datlist = TRUE,
    datlist = dat_small
  )
  # check that the variance adj translator works (functionality was in
  # SS_readctl_3.30, but then refactored to its own fxn to avoid confusion.)
  var_adj_3.24 <- translate_3.30_to_3.24_var_adjust(
    Variance_adjustment_list = ctl_small[["Variance_adjustment_list"]],
    Nfleets = ctl_small[["Nfleets"]]
  )
  expect_true(nrow(var_adj_3.24) == 6)
  expect_true(ncol(var_adj_3.24) == ctl_small[["Nfleets"]])
  q_3.24 <- translate_3.30_to_3.24_Q_setup(
    Q_options = ctl_small[["Q_options"]],
    Nfleets = ctl_small[["Nfleets"]]
  )
  expect_true(nrow(q_3.24) == ctl_small[["Nfleets"]])
})

# clean up
unlink(tmp_path, recursive = TRUE)
