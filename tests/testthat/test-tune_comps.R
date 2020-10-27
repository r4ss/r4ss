context("Test the ss_tune_comps function")
# note: to get these to run, please put an ss exe named "ss.exe" in
# inst/extdata/simple_3.30.13 . Otherwise, 3 of thee tests will be skipped.

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


test_that(" ss tune comps works with francis", {
  skip_if(all(file.info(dir(file.path(runs_path, "simple_3.30.13"),
                            full.names = TRUE))$exe == "no"),
          message = "skipping test that requires SS executable"
  )
  test <- SS_tune_comps(replist = NULL, fleets='all', 
                option = "Francis", niters_tuning = 1,
                init_run = FALSE, dir = file.path(runs_path, "simple_3.30.13"),
                model = "ss", allow_up_tuning = FALSE,
                exe_in_path = FALSE, verbose = FALSE)
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

test_that(" ss tune comps works when just want to return the Francis table", {
  replist <- SS_output(dir = file.path(runs_path, "simple_3.30.13"), 
                       verbose = FALSE, hidewarn = TRUE, printstats = FALSE)
  test <- SS_tune_comps(replist = replist, option = "Francis",
                        niters_tuning = 0, 
                        dir = file.path(runs_path, "simple_3.30.13"),
                        verbose = FALSE)
  expect_length(test, 1)
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
                        model = "ss",
                        exe_in_path = FALSE, verbose = FALSE)
  expect_length(test, 2)
})