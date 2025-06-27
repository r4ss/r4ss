context("Test the tune_comps function")
# note: to get these to run, please put an ss exe named "ss3.exe" in
# inst/extdata/simple_small . Otherwise, most of the tests will be skipped.

# do runs in a temporary dir so that the state is not disrupted if tests
# exit early.
tmp_path <- file.path(tempdir(check = TRUE), "test-runs")
dir.create(tmp_path, showWarnings = FALSE)
example_path <- system.file("extdata", package = "r4ss")
file.copy(example_path, tmp_path, recursive = TRUE)
# runs_path avoids repeated use of "extdata" that would have to be added
# if using tmp_path directly
runs_path <- file.path(tmp_path, "extdata")
# clean up
on.exit(unlink(tmp_path, recursive = TRUE))

test_that("get_last_phase works", {
  start <- r4ss::SS_readstarter(
    file.path(runs_path, "simple_small", "starter.ss"),
    verbose = FALSE
  )
  dat <- r4ss::SS_readdat(
    file.path(runs_path, "simple_small", start$datfile),
    verbose = FALSE
  )
  ctl <- r4ss::SS_readctl(
    file.path(runs_path, "simple_small", start$ctlfile),
    use_datlist = TRUE,
    datlist = dat,
    verbose = FALSE
  )
  last_phase <- get_last_phase(ctl)
  expect_true(last_phase == 4) # based on last known value.
})

test_that(" tune_comps() works when just want to return the Francis table", {
  replist <- suppressWarnings(SS_output(
    dir = file.path(runs_path, "simple_small"),
    verbose = FALSE,
    hidewarn = TRUE,
    printstats = FALSE
  ))
  test <- tune_comps(
    replist = replist,
    option = "Francis",
    niters_tuning = 0,
    dir = file.path(runs_path, "simple_small"),
    verbose = FALSE
  )
  expect_true(nrow(test) == 4)
  expect_true(is.data.frame(test))
  # write new table to file
  table <- SS_varadjust(
    dir = file.path(runs_path, "simple_small"),
    ctlfile = "control.ss",
    newctlfile = "new_control_varadjust.ss",
    newtable = test,
    overwrite = FALSE
  )
  expect_true(file.exists(file.path(
    runs_path,
    "simple_small",
    "new_control_varadjust.ss"
  )))
  dat <- SS_readdat(
    file.path(runs_path, "simple_small", "data.ss"),
    verbose = FALSE
  )
  ctl_varadjust <- SS_readctl(
    file.path(runs_path, "simple_small", "new_control_varadjust.ss"),
    use_datlist = TRUE,
    datlist = dat,
    verbose = FALSE
  )
  expect_true(ctl_varadjust[["DoVar_adjust"]] == 1)
  expect_true(!is.null(ctl_varadjust[["Variance_adjustment_list"]]))
})


test_that("tune_comps() works with francis", {
  skip_if(
    (!file.exists(file.path(runs_path, "simple_small", "ss3"))) &
      (!file.exists(file.path(runs_path, "simple_small", "ss3.exe"))),
    message = "skipping test that requires SS3 executable"
  )
  test <- tune_comps(
    replist = NULL,
    fleets = "all",
    option = "Francis",
    niters_tuning = 1,
    init_run = FALSE,
    dir = file.path(runs_path, "simple_small"),
    allow_up_tuning = FALSE,
    verbose = FALSE,
    extras = "-nohess"
  )
  expect_length(test, 2)
})

test_that("tune_comps() works with MI and up tuning", {
  skip_if(
    (!file.exists(file.path(runs_path, "simple_small", "ss3"))) &
      (!file.exists(file.path(runs_path, "simple_small", "ss3.exe"))),
    message = "skipping test that requires SS3 executable"
  )
  test <- tune_comps(
    replist = NULL,
    fleets = "all",
    option = "MI",
    niters_tuning = 1,
    init_run = FALSE,
    dir = file.path(runs_path, "simple_small"),
    allow_up_tuning = TRUE,
    verbose = FALSE
  )
  expect_length(test, 2)
})

test_that("tune_comps() works with DM", {
  skip_if(
    (!file.exists(file.path(runs_path, "simple_small", "ss3"))) &
      (!file.exists(file.path(runs_path, "simple_small", "ss3.exe"))),
    message = "skipping test that requires SS3 executable"
  )
  test <- tune_comps(
    replist = NULL,
    fleets = "all",
    option = "DM",
    niters_tuning = 1,
    init_run = FALSE,
    dir = file.path(runs_path, "simple_small"),
    extras = "-nohess",
    verbose = FALSE
  )
  expect_length(test, 2)
  # add check that varaiance adjustment is gone
  dat <- SS_readdat(
    file.path(runs_path, "simple_small", "data.ss"),
    verbose = FALSE
  )
  ctl <- SS_readctl(
    file.path(runs_path, "simple_small", "control.ss"),
    use_datlist = TRUE,
    datlist = dat,
    verbose = FALSE
  )
  expect_true(ctl[["DoVar_adjust"]] == 0)
  expect_null(ctl[["Variance_adjustment_list"]])
})

test_that("tune_comps() works with none", {
  skip_if(
    (!file.exists(file.path(runs_path, "simple_small", "ss3"))) &
      (!file.exists(file.path(runs_path, "simple_small", "ss3.exe"))),
    message = "skipping test that requires SS3 executable"
  )
  test <- tune_comps(
    option = "none",
    dir = file.path(runs_path, "simple_small"),
    verbose = FALSE
  )
  # make sure includes both Francis and MI weighting
  expect_true(length(grep("Francis", colnames(test))) > 0)
  expect_true(length(grep("MI", colnames(test))) > 0)
})

test_that("tune_comps() works with multiple iterations", {
  skip_if(
    (!file.exists(file.path(runs_path, "simple_small", "ss3"))) &
      (!file.exists(file.path(runs_path, "simple_small", "ss3.exe"))),
    message = "skipping test that requires SS3 executable"
  )
  test <- tune_comps(
    replist = NULL,
    fleets = "all",
    option = "MI",
    niters_tuning = 2,
    init_run = FALSE,
    dir = file.path(runs_path, "simple_small"),
    allow_up_tuning = TRUE,
    verbose = FALSE
  )
  expect_length(test, 2)
  expect_length(test$tuning_table_list, 2)
  expect_length(test$weights, 2)
})
