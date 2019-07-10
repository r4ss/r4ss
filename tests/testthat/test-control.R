context("Read and write SS control file r4ss functions")

# Note: these tests are not very extensive. They only check that a list is
# successfully created. 

# read/write in a temporary dir so that the state is not disrupted if tests 
# exit early.
tmp_path <- file.path(tempdir(check = TRUE), "test-control")
dir.create(tmp_path, showWarnings = FALSE)
example_path <- system.file("extdata", package = "r4ss")
file.copy(example_path, tmp_path, recursive = TRUE)


# paths to the model files
sim_3.30.13 <- file.path(tmp_path, "extdata", "simple_3.30.13")
sim_3.24 <- file.path(tmp_path, "extdata", "simple_3.24")

test_that("SS_readctl and SS_writectl works for 3.30.13", {
  # read data file b/c necessary input to read control
  dat_3.30.13 <- SS_readdat(file.path(sim_3.30.13, "simple_data.ss"), 
                            verbose = FALSE)
  # read the control file so that test can run on it
  ctl_3.30.13 <- SS_readctl(
    file.path(sim_3.30.13, "simple_control.ss"),
    verbose = FALSE,
    use_datlist = TRUE, 
    datlist = dat_3.30.13)
  expect_type(ctl_3.30.13, "list")
  #check write control
  if(file.exists(file.path(sim_3.30.13,"testctl.ss"))){
    file.remove(file.path(sim_3.30.13,"testctl.ss"))
  }
  SS_writectl(ctllist = ctl_3.30.13, 
              verbose = FALSE, 
              overwrite = FALSE,
              outfile = file.path(sim_3.30.13,"testctl.ss"))
  expect_true(file.exists(file.path(sim_3.30.13,"testctl.ss")))
})
test_that("SS_readctl and SS_writectl works for 3.24", {
  # read data file b/c necessary input to read control
  dat_3.24 <- SS_readdat(file.path(sim_3.24, "simple.dat"), 
                         verbose = FALSE, version = "3.24")
  # read the control file
  ctl_3.24 <- SS_readctl(file = file.path(sim_3.24, "simple.ctl"),
                         verbose = FALSE, 
                         use_datlist = TRUE, 
                         datlist = dat_3.24,
                         version = "3.24")
  expect_type(ctl_3.24, "list")
  #check write control
  if(file.exists(file.path(sim_3.24,"testctl.ss"))){
    file.remove(file.path(sim_3.24,"testctl.ss"))
  }
  SS_writectl(ctllist = ctl_3.24, 
              verbose = FALSE, 
              overwrite = TRUE,
              outfile = file.path(sim_3.24, "testctl.ss"))
  expect_true(file.exists(file.path(sim_3.24, "testctl.ss")))
})

#clean up
unlink(tmp_path, recursive = TRUE)