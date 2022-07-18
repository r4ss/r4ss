### automated tests of r4ss package
context("r4ss functions that require executables to run")

# do runs in a temporary dir so that the state is not disrupted if tests
# exit early.
tmp_path <- file.path(tempdir(check = TRUE), "test-runs")
dir.create(tmp_path, showWarnings = FALSE)
example_path <- system.file("extdata", package = "r4ss")
file.copy(example_path, tmp_path, recursive = TRUE)
# runs_path avoids repeated use of "extdata" that would have to be added
# if using tmp_path directly
runs_path <- file.path(tmp_path, "extdata")
path_simple_small <- file.path(runs_path, "simple_small")
# clean up
on.exit(unlink(tmp_path, recursive = TRUE))

test_that("check_exe() fails or succeeds as expected", {
  # skip if no executable in model path
  skip_if((!file.exists(file.path(path_simple_small, "ss"))) &
    (!file.exists(file.path(path_simple_small, "ss.exe"))),
  message = "skipping test that requires SS executable"
  )
  # error when no exe found
  expect_error(check_exe())
  # returns path (along with exe name) when exe found
  check_exe_results <- check_exe(dir = path_simple_small)
  expect_equal(check_exe_results[["path"]], path_simple_small)
})

test_that("SS_doRetro() runs on simple_small model", {
  # skip if no executable in model path
  skip_if((!file.exists(file.path(path_simple_small, "ss"))) &
    (!file.exists(file.path(path_simple_small, "ss.exe"))),
  message = "skipping test that requires SS executable"
  )
  SS_doRetro(
    dir = path_simple_small,
    oldsubdir = "", newsubdir = "retrospectives", years = 0:-2
  )
  retro_subdirs <- file.path(
    path_simple_small, "retrospectives",
    paste0("retro", c("0", "-1", "-2"))
  )
  retro_ran <- lapply(
    retro_subdirs,
    function(d) file.exists(file.path(d, "Report.sso"))
  )
  expect_true(all(unlist(retro_ran) == TRUE))

  # read model output from the retrospectives
  retroModels <- SSgetoutput(
    dirvec = file.path(
      path_simple_small, "retrospectives",
      paste0("retro", 0:-2)
    )
  )
  # summarize the model output
  retroSummary <- SSsummarize(retroModels)
  # set the fector of ending years
  endyrvec <- retroSummary$endyrs + 0:-2
  SSplotComparisons(retroSummary,
    endyrvec = endyrvec,
    legendlabels = paste("Data", 0:-2, "years")
  )
  # calculate Mohn's rho values
  # TODO: add better tests for mohns rho. Some values aren't calcualted b/c they
  # are missing in the summaries for this model run.
  mohns_rho <- SSmohnsrho(retroSummary)
  expect_length(mohns_rho, 12)
})


test_that("SS_RunJitter runs on simple_small model", {
  # skip if no executable in model path
  skipexe <- (!file.exists(file.path(path_simple_small, "ss"))) &
    (!file.exists(file.path(path_simple_small, "ss.exe")))
  dir.jit <- file.path(path_simple_small, "jitter")
  expect_true(copy_SS_inputs(
    dir.old = path_simple_small,
    dir.new = dir.jit,
    create.dir = TRUE,
    overwrite = TRUE,
    copy_exe = !skipexe,
    copy_par = FALSE,
    verbose = FALSE
  ))
  if (!skipexe & .Platform[["OS.type"]] == "unix") {
    file.copy(
      from = file.path(path_simple_small, "ss"),
      to = file.path(dir.jit, "ss")
    )
  }
  # run jitters
  if (skipexe) {
    # error expected when no exe found
    expect_error(SS_RunJitter(
      mydir = dir.jit, Njitter = 2, jitter_fraction = 0.1,
      printlikes = FALSE, verbose = TRUE,
    ))
    # starter file shouldn't have changed if exe check failed
    starter <- SS_readstarter(file.path(dir.jit, "starter.ss"), verbose = FALSE)
    expect_equal(starter$jitter_fraction, 0)
  } else {
    likesaved <- SS_RunJitter(
      mydir = dir.jit, Njitter = 2, jitter_fraction = 0.1,
      printlikes = FALSE, verbose = TRUE, show_in_console = FALSE
    )
    # confirm that likelihoods were returned by function
    expect_true(is.vector(likesaved) & length(likesaved) == 2)
    expect_equal(likesaved[1], likesaved[2])
    # confirm starter file change
    starter <- SS_readstarter(file.path(dir.jit, "starter.ss"), verbose = FALSE)
    expect_equal(starter$jitter_fraction, 0.1)
  }
  expect_equal(starter$init_values_src, 0)
  unlink(dir.jit, recursive = TRUE)
})

###############################################################################

test_that("profile functions run on simple_small model", {
  # skip if no executable in model path
  skip_if((!file.exists(file.path(path_simple_small, "ss"))) &
    (!file.exists(file.path(path_simple_small, "ss.exe"))),
  message = "skipping test that requires SS executable"
  )
  dir.prof <- file.path(path_simple_small, "profile")
  copy_SS_inputs(
    dir.old = path_simple_small,
    dir.new = dir.prof,
    create.dir = TRUE,
    overwrite = TRUE,
    copy_exe = TRUE,
    copy_par = TRUE,
    verbose = TRUE
  )
  # b/c current copy ss inputs wont copy the exe for mac or linux
  if (.Platform[["OS.type"]] == "unix") {
    file.copy(
      from = file.path(path_simple_small, "ss"),
      to = file.path(dir.prof, "ss")
    )
  }
  starter <- SS_readstarter(file.path(dir.prof, "starter.ss"))
  # Make use the par file as a starting point
  starter$ctlfile <- "control_modified.ss"
  # write modified starter file
  SS_writestarter(starter, dir = dir.prof, overwrite = TRUE)
  # run profile
  prof.table <- SS_profile(
    dir = dir.prof,
    masterctlfile = "control.ss",
    string = "R0", profilevec = c(8.5, 9)
  )
  # read model output
  prof.out <- SSgetoutput(dirvec = dir.prof, keyvec = 1:2)
  # summarize output
  summary.out <- SSsummarize(prof.out)
  # run plotting functions
  plotprofile.out <- SSplotProfile(summary.out)
  pinerplot.out <- PinerPlot(summary.out)

  # check that the minimum of the total likelihood is 0
  # (also serves to indicate that the function didn't crash and
  # there are no NA values in these columns)
  expect_equal(min(plotprofile.out$TOTAL), 0)
  expect_equal(min(pinerplot.out$ALL), 0)
})

test_that("Run an SS3 model and read the hessian", {
  # skip if no executable in model path
  skip_if((!file.exists(file.path(path_simple_small, "ss"))) &
    (!file.exists(file.path(path_simple_small, "ss.exe"))),
  message = "skipping test that requires SS executable"
  )
  copy_results <- copy_SS_inputs(
    dir.old = path_simple_small,
    dir.new = file.path(tmp_path, "test_mod_run"), copy_exe = TRUE
  )
  expect_true(copy_results)
  run_results <- run(dir = file.path(tmp_path, "test_mod_run"))
  expect_true(run_results[["results"]] == "ran model")
  hes <- getADMBHessian(
    dir = file.path(tmp_path, "test_mod_run"),
    FileName = "admodel.hes"
  )
  expect_length(hes, 4)
  expect_true(is.matrix(hes[["hes"]]))
})


# clean up (should delete the temporary directory in which everything was run)
unlink(tmp_path, recursive = TRUE)
