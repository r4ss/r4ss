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
# clean up
on.exit(unlink(tmp_path, recursive = TRUE))

# testing SS_doRetro
test_that("SS_doRetro runs on simple_3.24 model", {
  path_3.24 <- file.path(runs_path, "simple_3.24")
  skip_if((!file.exists(file.path(path_3.24, "ss"))) &
    (!file.exists(file.path(path_3.24, "ss.exe"))),
  message = "skipping test that requires SS executable"
  )
  SS_doRetro(
    masterdir = path_3.24,
    oldsubdir = "", newsubdir = "retrospectives", years = 0:-2
  )
  retro_subdirs <- file.path(
    path_3.24, "retrospectives",
    paste0("retro", c("0", "-1", "-2"))
  )
  retro_ran <- lapply(
    retro_subdirs,
    function(d) file.exists(file.path(d, "Report.sso"))
  )
  expect_true(all(unlist(retro_ran) == TRUE))
})

test_that("SS_doRetro runs on simple_3.30.12 model", {
  path_3.30.12 <- file.path(runs_path, "simple_3.30.12")
  skip_if((!file.exists(file.path(path_3.30.12, "ss"))) &
    (!file.exists(file.path(path_3.30.12, "ss.exe"))),
  message = "skipping test that requires SS executable"
  )
  SS_doRetro(
    masterdir = path_3.30.12,
    oldsubdir = "", newsubdir = "retrospectives", years = 0:-2
  )
  retro_subdirs <- file.path(
    path_3.30.12, "retrospectives",
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
      path_3.30.12, "retrospectives",
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


test_that("SS_RunJitter runs on newest simple model", {
  path_simple <- tail(dir(runs_path, full.names = TRUE), 1)
  skipexe <- (!file.exists(file.path(path_simple, "ss"))) &
    (!file.exists(file.path(path_simple, "ss.exe")))
  dir.jit <- file.path(path_simple, "jitter")
  expect_true(copy_SS_inputs(
    dir.old = path_simple,
    dir.new = dir.jit,
    create.dir = TRUE,
    overwrite = TRUE,
    copy_exe = !skipexe,
    copy_par = FALSE,
    verbose = FALSE
  ))
  if (!skipexe & .Platform[["OS.type"]] == "unix") {
    file.copy(
      from = file.path(path_simple, "ss"),
      to = file.path(dir.jit, "ss")
    )
  }
  # run jitters
  if (skipexe) {
    expect_error(SS_RunJitter(
      mydir = dir.jit, Njitter = 2, jitter_fraction = 0.1,
      printlikes = FALSE
    ))
    starter <- SS_readstarter(file.path(dir.jit, "starter.ss"), verbose = FALSE)
    expect_equal(starter$jitter_fraction, 0)
  } else {
    likesaved <- SS_RunJitter(
      mydir = dir.jit, Njitter = 2, jitter_fraction = 0.1,
      printlikes = FALSE
    )
    expect_true(is.vector(likesaved) & length(likesaved) == 2)
    expect_equal(likesaved[1], likesaved[2])
    starter <- SS_readstarter(file.path(dir.jit, "starter.ss"), verbose = FALSE)
    expect_equal(starter$jitter_fraction, 0.1)
  }
  expect_equal(starter$init_values_src, 0)
  unlink(dir.jit, recursive = TRUE)
})

###############################################################################

test_that("profile functions run on simple_3.30.12 model", {
  path_3.30.12 <- file.path(runs_path, "simple_3.30.12")
  skip_if((!file.exists(file.path(path_3.30.12, "ss"))) &
    (!file.exists(file.path(path_3.30.12, "ss.exe"))),
  message = "skipping test that requires SS executable"
  )
  dir.prof <- file.path(path_3.30.12, "profile")
  copy_SS_inputs(
    dir.old = path_3.30.12,
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
      from = file.path(path_3.30.12, "ss"),
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
    masterctlfile = "simple_control.ss",
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
  path_3.30.12 <- file.path(runs_path, "simple_3.30.12")
  skip_if((!file.exists(file.path(path_3.30.12, "ss"))) &
    (!file.exists(file.path(path_3.30.12, "ss.exe"))),
  message = "skipping test that requires SS executable"
  )
  copy_results <- copy_SS_inputs(
    dir.old = path_3.30.12,
    dir.new = file.path(tmp_path, "test_mod_run"), copy_exe = TRUE
  )
  expect_true(copy_results)
  run_results <- run(dir = file.path(tmp_path, "test_mod_run"))
  expect_true(run_results[["results"]] == "ran model")
  hes <- getADMBHessian(
    File = file.path(tmp_path, "test_mod_run"),
    FileName = "admodel.hes"
  )
  expect_length(hes, 4)
  expect_true(is.matrix(hes[["hes"]]))
})


# clean up (should delete the temporary directory in which everything was run)
unlink(tmp_path, recursive = TRUE)
