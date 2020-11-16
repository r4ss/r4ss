context("Test the ss_tune_comps function")
# note: to get these to run, please put an ss exe named "ss.exe" in
# inst/extdata/simple_3.30.13 . Otherwise, most of the tests will be skipped.

# do runs in a temporary dir so that the state is not disrupted if tests 
# exit early.
tmp_path <- file.path(tempdir(check = TRUE), "test-runs")
dir.create(tmp_path, showWarnings = FALSE)
example_path <- system.file("extdata", package = "r4ss")
file.copy(example_path, tmp_path, recursive = TRUE)
# runs_path avoids repeated use of "extdata" that would have to be added 
# if using tmp_path directly
runs_path <- file.path(tmp_path, "extdata")
#clean up
on.exit(unlink(tmp_path, recursive = TRUE))

test_that(" ss tune comps works when just want to return the Francis table", {
  replist <- suppressWarnings(SS_output(dir = file.path(runs_path, "simple_3.30.13"), 
                                        verbose = FALSE, hidewarn = TRUE, printstats = FALSE))
  test <- SS_tune_comps(replist = replist, option = "Francis",
                        niters_tuning = 0, 
                        dir = file.path(runs_path, "simple_3.30.13"),
                        verbose = FALSE)
  expect_true(nrow(test) == 4)
  expect_true(is.data.frame(test))
  # write new table to file
  table <- SS_varadjust(dir=file.path(runs_path, "simple_3.30.13"),
                        ctlfile = "simple_control.ss",
                          newctlfile="new_control_varadjust.ss",
                          newtable=test, overwrite=FALSE)
  expect_true(file.exists(file.path(runs_path, "simple_3.30.13", 
                                    "new_control_varadjust.ss")))
  dat <- SS_readdat(file.path(runs_path, "simple_3.30.13", "simple_data.ss"),
                    verbose = FALSE)
  ctl_varadjust <- SS_readctl(
    file.path(runs_path, "simple_3.30.13", "new_control_varadjust.ss"),
    use_datlist = TRUE,
    datlist = dat,
    verbose = FALSE)
  expect_true(ctl_varadjust[["DoVar_adjust"]] == 1)
  expect_true(!is.null(ctl_varadjust[["Variance_adjustment_list"]]))
})


test_that("ss tune comps works with francis", {
  skip_if(all(file.info(dir(file.path(runs_path, "simple_3.30.13"),
                            full.names = TRUE))$exe == "no"),
          message = "skipping test that requires SS executable"
  )
  test <- SS_tune_comps(replist = NULL, fleets='all',
                option = "Francis", niters_tuning = 1,
                init_run = FALSE, dir = file.path(runs_path, "simple_3.30.13"),
                model = "ss", allow_up_tuning = FALSE,
                exe_in_path = FALSE, verbose = FALSE, extras = "-nohess")
  expect_length(test, 2)
})

test_that(" ss tune comps works with MI and up tuning", {
  skip_if(all(file.info(dir(file.path(runs_path, "simple_3.30.13"),
                            full.names = TRUE))$exe == "no"),
          message = "skipping test that requires SS executable"
  )
  test <- SS_tune_comps(replist = NULL, fleets='all',
                        option = "MI", niters_tuning = 1,
                        init_run = FALSE, dir = file.path(runs_path, "simple_3.30.13"),
                        model = "ss", allow_up_tuning = TRUE,
                        exe_in_path = FALSE, verbose = FALSE)
  expect_length(test, 2)
})

test_that(" ss tune comps works with DM", {
  skip_if(all(file.info(dir(file.path(runs_path, "simple_3.30.13"),
                            full.names = TRUE))$exe == "no"),
          message = "skipping test that requires SS executable"
  )
  test <- SS_tune_comps(replist = NULL, fleets='all',
                        option = "DM", niters_tuning = 1,
                        init_run = FALSE,
                        dir = file.path(runs_path, "simple_3.30.13"),
                        model = "ss", extras = "-nohess",
                        exe_in_path = FALSE, verbose = FALSE)
  expect_length(test, 2)
  # add check that varaiance adjustment is gone
  dat <- SS_readdat(file.path(runs_path, "simple_3.30.13", "simple_data.ss"),
                    verbose = FALSE)
  ctl <- SS_readctl(file.path(runs_path, "simple_3.30.13", "simple_control.ss"),
                    use_datlist = TRUE,
                    datlist = dat,
                    verbose = FALSE)
  expect_true(ctl[["DoVar_adjust"]] == 0)
  expect_null(ctl[["Variance_adjustment_list"]])
})

test_that("ss tune comps works with none", {
  skip_if(all(file.info(dir(file.path(runs_path, "simple_3.30.13"),
                            full.names = TRUE))$exe == "no"),
          message = "skipping test that requires SS executable"
  )
  test <- SS_tune_comps(option = "none",
                        dir = file.path(runs_path, "simple_3.30.13"),
                        verbose = FALSE)
  #make sure includes both Francis and MI weighting
  expect_true(length(grep("Francis", colnames(test))) > 0)
  expect_true(length(grep("MI", colnames(test))) > 0)
})

test_that(" ss tune comps works with multiple iterations", {
  skip_if(all(file.info(dir(file.path(runs_path, "simple_3.30.13"),
                            full.names = TRUE))$exe == "no"),
          message = "skipping test that requires SS executable"
  )
  test <- SS_tune_comps(replist = NULL, fleets='all',
                        option = "MI", niters_tuning = 2,
                        init_run = FALSE, dir = file.path(runs_path, "simple_3.30.13"),
                        model = "ss", allow_up_tuning = TRUE,
                        exe_in_path = FALSE, verbose = FALSE)
  expect_length(test, 2)
  expect_length(test$tuning_table_list, 2)
  expect_length(test$weights, 2)
})