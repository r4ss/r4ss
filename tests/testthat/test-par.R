context("Test read and write par functions")
# read/write in a temporary dir so that the state is not disrupted if tests 
# exit early.
tmp_path <- file.path(tempdir(check = TRUE), "test-par")
dir.create(tmp_path, showWarnings = FALSE)
example_path <- system.file("extdata", package = "r4ss")
file.copy(example_path, tmp_path, recursive = TRUE)
#clean up
on.exit(unlink(tmp_path, recursive = TRUE))

# paths to the model files
sim_3.30.13 <- file.path(tmp_path, "extdata", "simple_3.30.13")
sim_3.24 <- file.path(tmp_path, "extdata", "simple_3.24")

test_that("SS_readpars functions work ", {
  
  par_3.30 <- SS_readpar_3.30(parfile = file.path(sim_3.30.13, "ss3.par"), 
                datsource = file.path(sim_3.30.13, "simple_data.ss"),
                ctlsource = file.path(sim_3.30.13, "simple_control.ss"),
                verbose = FALSE)
  SS_writepar_3.30(par_3.30, file.path(sim_3.30.13, "ss_test.par"),
                   verbose = FALSE)
  expect_true(file.exists(file.path(sim_3.30.13, "ss_test.par")))
  par_3.30_read_2 <- SS_readpar_3.30(parfile = file.path(sim_3.30.13, 
                                                         "ss_test.par"), 
                       datsource = file.path(sim_3.30.13, "simple_data.ss"),
                       ctlsource = file.path(sim_3.30.13, "simple_control.ss"),
                       verbose = FALSE)
  expect_equal(par_3.30, par_3.30_read_2)
  par_3.24 <- SS_readpar_3.24(parfile = file.path(sim_3.24, "ss3.par"), 
                datsource = file.path(sim_3.24, "simple.dat"),
                ctlsource = file.path(sim_3.24, "simple.ctl"),
                verbose = FALSE)
  SS_writepar_3.24(par_3.24, file.path(sim_3.24, "ss_test.par"),
                   verbose = FALSE)
  expect_true(file.exists(file.path(sim_3.24, "ss_test.par")))
  par_3.24_read_2 <- SS_readpar_3.24(parfile = file.path(sim_3.24, "ss_test.par"), 
                       datsource = file.path(sim_3.24, "simple.dat"),
                       ctlsource = file.path(sim_3.24, "simple.ctl"),
                       verbose = FALSE)
  expect_equal(par_3.24, par_3.24_read_2)
})

#clean up
unlink(tmp_path, recursive = TRUE)
