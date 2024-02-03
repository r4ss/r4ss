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
retro_years <- 0:-2 # defining here so available in multiple tests
# clean up
on.exit(unlink(tmp_path, recursive = TRUE))

###############################################################################

test_that("check_exe() fails or succeeds as expected", {
  # skip if no executable in model path
  skip_if(
    (!file.exists(file.path(path_simple_small, "ss3"))) &
      (!file.exists(file.path(path_simple_small, "ss3.exe"))),
    message = "skipping test that requires SS3 executable"
  )
  # error when no exe found
  expect_error(check_exe("bad_exe_name"))
  # returns path (along with exe name) when exe found
  check_exe_results <- check_exe(dir = path_simple_small)
  expect_equal(check_exe_results[["path"]], path_simple_small)
})

###############################################################################

test_that("retro() and populate_multiple_folders() both work", {
  # skip if no executable in model path
  skip_if(
    (!file.exists(file.path(path_simple_small, "ss3"))) &
      (!file.exists(file.path(path_simple_small, "ss3.exe"))),
    message = "skipping test that requires SS3 executable"
  )
  retro(
    dir = path_simple_small,
    oldsubdir = "", newsubdir = "retrospectives", years = retro_years,
    show_in_console = FALSE
  )
  retro_subdirs <- file.path(
    path_simple_small, "retrospectives",
    paste0("retro", retro_years)
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
      paste0("retro", retro_years)
    )
  )
  # summarize the model output
  retroSummary <- SSsummarize(retroModels)
  # set the fector of ending years
  endyrvec <- retroSummary$endyrs + retro_years
  SSplotComparisons(retroSummary,
    endyrvec = endyrvec,
    legendlabels = paste("Data", retro_years, "years")
  )
  # calculate Mohn's rho values
  # TODO: add better tests for mohns rho. Some values aren't calcualted b/c they
  # are missing in the summaries for this model run.
  mohns_rho <- SSmohnsrho(retroSummary)
  expect_length(mohns_rho, 12)

  # Additional tests of populate_multiple_folders()
  #
  # These are unrelated to the retro() tests but test
  # options that require exe and/or model output to run
  # unlike the tests in test-populate_multiple_folders.R

  # test with exe.dir = TRUE and copy_par = TRUE option
  folders_copied <- populate_multiple_folders(
    outerdir.old = file.path(path_simple_small, "retrospectives"),
    outerdir.new = file.path(path_simple_small, "retrospectives_copy"),
    exe.dir = TRUE,
    copy_par = TRUE
  )
  # confirm files got reported as copied
  expect_true(all(folders_copied[["results.files"]]))
  # results.exe is NA when copied by copy_SS_inputs() instead of from a
  # central location
  expect_true(all(folders_copied[["results.exe"]]))
  # confirm number of subdirectories is correct
  expect_true(nrow(folders_copied) == length(retro_years))
  # check for .par file (either ss.par or ss3.par)
  expect_true(!is.na(get_par_name(path_simple_small)))

  # test exe.dir as a path and use_ss_new = TRUE
  folders_copied2 <- populate_multiple_folders(
    outerdir.old = file.path(path_simple_small, "retrospectives"),
    outerdir.new = file.path(path_simple_small, "retrospectives_copy2"),
    use_ss_new = TRUE,
    copy_par = FALSE,
    exe.dir = path_simple_small
  )
  expect_true(all(folders_copied2[["results.files"]]))
  expect_true(all(folders_copied2[["results.exe"]]))
})

###############################################################################

test_that("jitter runs on simple_small model", {
  # skip if no executable in model path
  skipexe <- (!file.exists(file.path(path_simple_small, "ss3"))) &
    (!file.exists(file.path(path_simple_small, "ss3.exe")))
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
  # run jitters
  if (skipexe) {
    # error expected when no exe found
    expect_error(jitter(
      dir = dir.jit, Njitter = 2, jitter_fraction = 0.1,
      printlikes = FALSE, verbose = TRUE,
    ))
    # starter file shouldn't have changed if exe check failed
    starter <- SS_readstarter(file.path(dir.jit, "starter.ss"), verbose = FALSE)
    expect_equal(starter$jitter_fraction, 0)
  } else {
    likesaved <- jitter(
      dir = dir.jit, Njitter = 2, jitter_fraction = 0.1,
      printlikes = FALSE, verbose = TRUE, show_in_console = FALSE
    )
    # confirm that likelihoods were returned by function
    expect_true(is.vector(likesaved) & length(likesaved) == 2)
    expect_equal(likesaved[1], likesaved[2])
    # confirm starter file change
    starter <- SS_readstarter(file.path(dir.jit, "starter.ss"), verbose = FALSE)
    expect_equal(starter$jitter_fraction, 0.1)

    # check jitter output
    jitter_output <- SSgetoutput(dir.jit, keyvec = c(1:2))
    jitter_summary <- SSsummarize(jitter_output)
    expect_equal(length(grep("replist", colnames(jitter_summary[["likelihoods"]][1, ]))), 2)
    expect_equal(length(grep("replist", colnames(jitter_summary[["pars"]]))), 2)
  }
  expect_equal(starter$init_values_src, 0)
  unlink(dir.jit, recursive = TRUE)
})

###############################################################################

test_that("profile functions run on simple_small model", {
  # skip if no executable in model path
  skip_if(
    (!file.exists(file.path(path_simple_small, "ss3"))) &
      (!file.exists(file.path(path_simple_small, "ss3.exe"))),
    message = "skipping test that requires SS3 executable"
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
  starter <- SS_readstarter(file.path(dir.prof, "starter.ss"))
  # Make use the par file as a starting point
  starter$ctlfile <- "control_modified.ss"
  # write modified starter file
  SS_writestarter(starter, dir = dir.prof, overwrite = TRUE)
  # run profile
  prof.table <- profile(
    dir = dir.prof,
    oldctlfile = "control.ss",
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

###############################################################################

test_that("Run an SS3 model and read the hessian", {
  # skip if no executable in model path
  skip_if(
    (!file.exists(file.path(path_simple_small, "ss3"))) &
      (!file.exists(file.path(path_simple_small, "ss3.exe"))),
    message = "skipping test that requires SS3 executable"
  )
  copy_results <- copy_SS_inputs(
    dir.old = path_simple_small,
    dir.new = file.path(tmp_path, "test_mod_run"), copy_exe = TRUE
  )
  expect_true(copy_results)
  run_results <- run(dir = file.path(tmp_path, "test_mod_run"))
  expect_true(run_results == "ran model")
  hes <- getADMBHessian(
    hesfile = file.path(tmp_path, "test_mod_run", "admodel.hes")
  )
  expect_length(hes, 4)
  expect_true(is.matrix(hes[["hes"]]))
})

###############################################################################


# clean up (should delete the temporary directory in which everything was run)
unlink(tmp_path, recursive = TRUE)
