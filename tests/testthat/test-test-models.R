context("Read output and make plots for all test-models")

test_that("test-models work with SS_output() and SS_plots()", {
  skip_if(!file.exists(system.file("extdata", "models", package = "r4ss")),
    message = "No 'models' folder in 'extdata'"
  )
  # skip if no executable in simple_small path
  # (should have been loaded there by
  # .github\workflows\r4ss-extra-tests.yml)

  # find simple_small
  dir_exe <- system.file("extdata", "simple_small", package = "r4ss")
  skip_if(
    (!file.exists(file.path(dir_exe, "ss3")) &
      !file.exists(file.path(dir_exe, "ss3.exe"))),
    message = paste("skipping test: no exe called 'ss' found in", dir_exe)
  )
  # temporary directory
  mod_path <- file.path(tempdir(check = TRUE), "test-test-models")
  on.exit(unlink(mod_path, recursive = TRUE), add = TRUE)
  dir.create(mod_path, showWarnings = FALSE)
  # copy all test models to temporary directory
  orig_mod_path <- system.file("extdata", "models", package = "r4ss")
  file.copy(orig_mod_path, mod_path, recursive = TRUE)
  all_mods <- list.dirs(file.path(mod_path, "models"),
    full.names = TRUE,
    recursive = FALSE
  )

  # run models without estimation and then run r4ss functions
  message("Will run SS_output() and SS_plots() on models:\n  ", paste(basename(all_mods),
    collapse = ",\n  "
  ))

  #' Run test models with the purpose of being called using the furrr package to
  #' run in parallel.
  #'
  #' @param models list of test models to run
  run_models <- function(models){
    message("Now running without estimation: ", basename(models))
    run(models, exe = file.path(dir_exe, "ss3"), extras = "-stopph 0 -nohess")

    #### Check for presence of Report.sso
    if (!"Report.sso" %in% dir(models)) {
      warning("No Report.sso file in ", models)
    } else {
      #### Checks related to SS_output()
      message("Running SS_output()")
      out <- SS_output(models, verbose = FALSE, printstats = FALSE)
      # expect_true(is.list(out))
      # expect_equal(tail(names(out), 1), "inputs")

      #### Checks related to SS_plots()
      message("Running SS_plots()")
      plots <- SS_plots(out, verbose = FALSE)
      # expect_true("data_plot2.png" %in% plots$file)
    }
  }


ncores <- parallelly::availableCores(omit = 1)
future::plan(future::multisession, workers = ncores)
  
furrr::future_map(.x = all_mods, .f = function(x, dir_exe){
  message("Now running without estimation: ", basename(x))
    run(x, exe = file.path(dir_exe, "ss3"), extras = "-stopph 0 -nohess")
},
dir_exe = dir_exe)

out <- furrr::future_map(.x = all_mods, .f = function(x){
  if (!"Report.sso" %in% dir(x)) {
      warning("No Report.sso file in ", x)
    } else {
      #### Checks related to SS_output()
      message("Running SS_output()")
      SS_output(x, verbose = FALSE, printstats = FALSE)
    }
})

expect_true(all(unlist(lapply(out, is.list))))
expect_true(length(out) == length(all_mods))
expect_setequal(unlist(lapply(out, function(x){tail(names(x), 1)})), "inputs")
})
