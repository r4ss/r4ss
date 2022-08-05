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
sim_small <- file.path(tmp_path, "extdata", "simple_small")

test_that("SS_readctl() and SS_writectl() work under various conditions", {
  # check exits on error when wrong datafile provided
  expect_error(
    SS_readctl(file.path(sim_small, "control.ss"),
      datlist = "wrong_file.ss",
      verbose = FALSE
    ),
    "Cannot find data file specified in datlist"
  )

  # read data file b/c necessary input to read control
  dat_small <- SS_readdat(file.path(sim_small, "data.ss"),
    verbose = FALSE
  )
  # read the control file so that test can run on it
  ctl_small <- SS_readctl(
    file.path(sim_small, "control.ss"),
    verbose = FALSE,
    datlist = dat_small
  )
  expect_type(ctl_small, "list")
  # check write control
  if (file.exists(file.path(sim_small, "testctl.ss"))) {
    file.remove(file.path(sim_small, "testctl.ss"))
  }
  SS_writectl(
    ctllist = ctl_small,
    verbose = FALSE,
    overwrite = FALSE,
    outfile = file.path(sim_small, "testctl.ss")
  )
  expect_true(file.exists(file.path(sim_small, "testctl.ss")))
})

test_that("SS_readctl() and SS_writectl() work when not reading from data", {
  # read data file b/c necessary input to read control
  dat_small <- SS_readdat(file.path(sim_small, "data.ss"),
    verbose = FALSE
  )

  # read the control file so that test can run on it
  ctl_small <- SS_readctl(
    file.path(sim_small, "control.ss"),
    verbose = FALSE,
    use_datlist = FALSE,
    nseas = dat_small[["nseas"]],
    N_areas = dat_small[["N_areas"]],
    Nages = dat_small[["Nages"]],
    Nsexes = dat_small[["Nsexes"]],
    Nfleets = dat_small[["Nfleets"]],
    N_rows_equil_catch = NULL,
    Do_AgeKey = FALSE
  )

  expect_type(ctl_small, "list")
  # check write control
  if (file.exists(file.path(sim_small, "testctl.ss"))) {
    file.remove(file.path(sim_small, "testctl.ss"))
  }
  SS_writectl(
    ctllist = ctl_small,
    verbose = FALSE,
    overwrite = FALSE,
    outfile = file.path(sim_small, "testctl.ss")
  )
  expect_true(file.exists(file.path(sim_small, "testctl.ss")))
})

test_that(paste0(
  "SS_readctl_3.30(), SS_writectl_3.30(), SS_readdat_3.30(), and ",
  " SS_writedat_3.30() works for simple_small using funs directly"
), {
  # read data file b/c necessary input to read control
  dat_small <- SS_readdat_3.30(file.path(sim_small, "data.ss"),
    verbose = FALSE
  )
  # test write dat
  if (file.exists(file.path(sim_small, "testdat.ss"))) {
    file.remove(file.path(sim_small, "testdat.ss"))
  }
  SS_writedat_3.30(dat_small, file.path(sim_small, "testdat.ss"))
  expect_true(file.exists(file.path(sim_small, "testdat.ss")))
  # read the control file so that test can run on it
  ctl_small <- SS_readctl_3.30(
    file.path(sim_small, "control.ss"),
    verbose = FALSE,
    use_datlist = TRUE,
    datlist = dat_small
  )
  expect_type(ctl_small, "list")
  # check write control
  if (file.exists(file.path(sim_small, "testctl.ss"))) {
    file.remove(file.path(sim_small, "testctl.ss"))
  }
  SS_writectl_3.30(
    ctllist = ctl_small,
    verbose = FALSE,
    overwrite = FALSE,
    outfile = file.path(sim_small, "testctl.ss")
  )
  expect_true(file.exists(file.path(sim_small, "testctl.ss")))
})

# clean up
unlink(tmp_path, recursive = TRUE)
