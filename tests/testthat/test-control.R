context("Read and write SS control file r4ss functions")

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
sim_3.30.13 <- file.path(tmp_path, "extdata", "simple_3.30.13")
sim_3.24 <- file.path(tmp_path, "extdata", "simple_3.24")

test_that("SS_readctl and SS_writectl works for 3.30.13", {
  # check exits on error when no datafile provided
  expect_error(SS_readctl(file.path(sim_3.30.13, "simple_control.ss"), 
                          verbose = FALSE), 
               "Cannot find data file specified in datlist")
  
  # read data file b/c necessary input to read control
  dat_3.30.13 <- SS_readdat(file.path(sim_3.30.13, "simple_data.ss"),
    verbose = FALSE
  )
  # read the control file so that test can run on it
  ctl_3.30.13 <- SS_readctl(
    file.path(sim_3.30.13, "simple_control.ss"),
    verbose = FALSE,
    datlist = dat_3.30.13
  )
  expect_type(ctl_3.30.13, "list")
  # check write control
  if (file.exists(file.path(sim_3.30.13, "testctl.ss"))) {
    file.remove(file.path(sim_3.30.13, "testctl.ss"))
  }
  SS_writectl(
    ctllist = ctl_3.30.13,
    verbose = FALSE,
    overwrite = FALSE,
    outfile = file.path(sim_3.30.13, "testctl.ss")
  )
  expect_true(file.exists(file.path(sim_3.30.13, "testctl.ss")))
})

test_that("SS_readctl and SS_writectl works for 3.30.13 when not reading from data", {
  # read data file b/c necessary input to read control
  dat_3.30.13 <- SS_readdat(file.path(sim_3.30.13, "simple_data.ss"),
    verbose = FALSE
  )

  # read the control file so that test can run on it
  ctl_3.30.13 <- SS_readctl(
    file.path(sim_3.30.13, "simple_control.ss"),
    verbose = FALSE,
    use_datlist = FALSE,
    nseas = dat_3.30.13[["nseas"]],
    N_areas = dat_3.30.13[["N_areas"]],
    Nages = dat_3.30.13[["Nages"]],
    Nsexes = dat_3.30.13[["Ngenders"]],
    Nfleets = dat_3.30.13[["Nfleets"]],
    N_rows_equil_catch = NULL,
    Do_AgeKey = FALSE
  )

  expect_type(ctl_3.30.13, "list")
  # check write control
  if (file.exists(file.path(sim_3.30.13, "testctl.ss"))) {
    file.remove(file.path(sim_3.30.13, "testctl.ss"))
  }
  SS_writectl(
    ctllist = ctl_3.30.13,
    verbose = FALSE,
    overwrite = FALSE,
    outfile = file.path(sim_3.30.13, "testctl.ss")
  )
  expect_true(file.exists(file.path(sim_3.30.13, "testctl.ss")))
})

test_that(paste0(
  "SS_readctl_3,30, SS_writectl_3.30, SS_readdat_3.30, and ",
  " SS_writedat_3.30 works for 3.30.13 using funs directly"
), {
  # read data file b/c necessary input to read control
  dat_3.30.13 <- SS_readdat_3.30(file.path(sim_3.30.13, "simple_data.ss"),
    verbose = FALSE
  )
  # test write dat
  if (file.exists(file.path(sim_3.30.13, "testdat.ss"))) {
    file.remove(file.path(sim_3.30.13, "testdat.ss"))
  }
  SS_writedat_3.30(dat_3.30.13, file.path(sim_3.30.13, "testdat.ss"))
  expect_true(file.exists(file.path(sim_3.30.13, "testdat.ss")))
  # read the control file so that test can run on it
  ctl_3.30.13 <- SS_readctl_3.30(
    file.path(sim_3.30.13, "simple_control.ss"),
    verbose = FALSE,
    use_datlist = TRUE,
    datlist = dat_3.30.13
  )
  expect_type(ctl_3.30.13, "list")
  # check write control
  if (file.exists(file.path(sim_3.30.13, "testctl.ss"))) {
    file.remove(file.path(sim_3.30.13, "testctl.ss"))
  }
  SS_writectl_3.30(
    ctllist = ctl_3.30.13,
    verbose = FALSE,
    overwrite = FALSE,
    outfile = file.path(sim_3.30.13, "testctl.ss")
  )
  expect_true(file.exists(file.path(sim_3.30.13, "testctl.ss")))
})

test_that("SS_readctl and SS_writectl works for 3.24", {
  # check exits on error when no datafile provided
  expect_error(SS_readctl(file.path(sim_3.24, "simple.ctl"), 
                          verbose = FALSE, version = "3.24"), 
               "Cannot find data file specified in datlist")
  
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
    verbose = FALSE,
    overwrite = TRUE,
    outfile = file.path(sim_3.24, "testctl.ss")
  )
  expect_true(file.exists(file.path(sim_3.24, "testctl.ss")))
})

test_that("SS_readctl and SS_writectl works for 3.24 when datlist = FALSE", {
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
    Nsexes = dat_3.24[["Ngenders"]],
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
    outfile = file.path(sim_3.24, "testctl.ss")
  )
  expect_true(file.exists(file.path(sim_3.24, "testctl.ss")))
})

test_that(paste0(
  "SS_readctl_3.24, SS_writectl_3.24, SS_readdat_3.24, and ",
  " SS_writedat_3.24 works using funs directly"
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

test_that("var adj and q translator fxns, works for 3.30.13", {
  # read data file b/c necessary input to read control
  dat_3.30.13 <- SS_readdat(file.path(sim_3.30.13, "simple_data.ss"),
    verbose = FALSE
  )
  # read the control file so that test can run on it
  ctl_3.30.13 <- SS_readctl(
    file.path(sim_3.30.13, "simple_control.ss"),
    verbose = FALSE,
    use_datlist = TRUE,
    datlist = dat_3.30.13
  )
  # check that the variance adj translator works (functionality was in
  # SS_readctl_3.30, but then refactored to its own fxn to avoid confusion.)
  var_adj_3.24 <- translate_3.30_to_3.24_var_adjust(
    Variance_adjustment_list = ctl_3.30.13[["Variance_adjustment_list"]],
    Nfleets = ctl_3.30.13[["Nfleets"]]
  )
  expect_true(nrow(var_adj_3.24) == 6)
  expect_true(ncol(var_adj_3.24) == ctl_3.30.13[["Nfleets"]])
  q_3.24 <- translate_3.30_to_3.24_Q_setup(
    Q_options = ctl_3.30.13[["Q_options"]],
    Nfleets = ctl_3.30.13[["Nfleets"]]
  )
  expect_true(nrow(q_3.24) == ctl_3.30.13[["Nfleets"]])
})

# clean up
unlink(tmp_path, recursive = TRUE)
