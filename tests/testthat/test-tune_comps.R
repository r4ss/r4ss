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

test_that("tune_comps() works with Francis_via_DM without running model", {
  local_path <- file.path(tempdir(check = TRUE), "test-runs-francis-via-dm")
  dir.create(local_path, showWarnings = FALSE)
  file.copy(example_path, local_path, recursive = TRUE)
  local_runs_path <- file.path(local_path, "extdata")
  on.exit(unlink(local_path, recursive = TRUE), add = TRUE)

  replist <- suppressWarnings(SS_output(
    dir = file.path(local_runs_path, "simple_small"),
    verbose = FALSE,
    hidewarn = TRUE,
    printstats = FALSE
  ))

  test <- tune_comps(
    replist = replist,
    fleets = "all",
    option = "Francis_via_DM",
    niters_tuning = 0,
    init_run = FALSE,
    dir = file.path(local_runs_path, "simple_small"),
    verbose = FALSE
  )
  expect_length(test, 2)
  expect_true(is.data.frame(test$weights))
  expect_true("target_log_theta" %in% names(test$weights))

  dat <- SS_readdat(
    file.path(local_runs_path, "simple_small", "data.ss"),
    verbose = FALSE
  )
  ctl <- SS_readctl(
    file.path(local_runs_path, "simple_small", "control.ss"),
    use_datlist = TRUE,
    datlist = dat,
    verbose = FALSE
  )
  expect_true(!is.null(ctl[["dirichlet_parms"]]))
  expect_true(all(ctl[["dirichlet_parms"]][, "PHASE"] < 0))
  expect_true(all(
    dat[["len_info"]][, "CompError"][dat[["len_info"]][, "ParmSelect"] > 0] == 1
  ))
  expect_true(all(
    dat[["age_info"]][, "CompError"][dat[["age_info"]][, "ParmSelect"] > 0] == 1
  ))
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

# # run locally to allow this test to run:
# download_models(dir = runs_path)
test_that("tune_comps() options 'Francis' and 'Francis_via_DM' produce similar results", {
  skip_if(
    (!dir.exists(file.path(runs_path, "models"))),
    message = "skipping test that requires models directory"
  )

  # get vector of models (if dir exists)
  models <- dir(file.path(runs_path, "models"), full.names = TRUE)
  models <- models[!grepl("DM", models)] # exclude DM model because it has no iterative variance adjustment
  ncores <- min(parallelly::availableCores(omit = 1), length(models))
  # run in parallel
  future::plan(future::multisession, workers = ncores)
  on.exit(future::plan(future::sequential), add = TRUE)

  results <- future.apply::future_lapply(models, function(model) {
    test1 <- tune_comps(
      replist = NULL,
      fleets = "all",
      option = "Francis",
      niters_tuning = 3,
      init_run = TRUE,
      dir = model,
      allow_up_tuning = FALSE,
      verbose = FALSE
    )

    model2 <- paste(model, "Francis_via_DM", sep = "_")
    copy_SS_inputs(
      dir.old = model,
      dir.new = model2,
      copy_par = TRUE,
      copy_exe = TRUE
    )
    # run tuning again in same directory using the Francis_via_DM option and compare results
    test2 <- tune_comps(
      replist = NULL,
      fleets = "all",
      option = "Francis_via_DM",
      niters_tuning = 3,
      init_run = TRUE,
      dir = model2,
      allow_up_tuning = FALSE,
      verbose = FALSE
    )

    # compare the final fraction unfished from the two models
    mod1 <- r4ss::SS_output(dir = model, verbose = FALSE)
    mod2 <- r4ss::SS_output(dir = model2, verbose = FALSE)

    list(
      test1_len = length(test1),
      test1_tuning_table_len = length(test1$tuning_table_list),
      test1_weights_len = length(test1$weights),
      test2_len = length(test2),
      test2_tuning_table_len = length(test2$tuning_table_list),
      test2_weights_len = length(test2$weights),
      francis_weights = test1$weights[[3]],
      francis_via_dm_weights = test2$target_multiplier[[3]],
      depletion1 = mod1$current_depletion,
      depletion2 = mod2$current_depletion
    )
  }, future.seed = TRUE)

  for (result in results) {
    expect_equal(result$test1_len, 2)
    expect_equal(result$test1_tuning_table_len, 2)
    expect_equal(result$test1_weights_len, 2)
    expect_equal(result$test2_len, 2)
    expect_equal(result$test2_tuning_table_len, 2)
    expect_equal(result$test2_weights_len, 2)

    # compare the weights from the two methods
    expect_equal(
      result$francis_weights,
      result$francis_via_dm_weights,
      tolerance = 0.01
    )

    expect_equal(
      result$depletion1,
      result$depletion2,
      tolerance = 0.01
    )
  }
})
