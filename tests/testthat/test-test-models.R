context("Read output and make plots for all test-models")

test_that("test-models work with SS_output() and SS_plots()", {
  skip_if(
    !file.exists(system.file("extdata", "models", package = "r4ss")),
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
    message = paste("skipping test: no exe called 'ss3' found in", dir_exe)
  )
  # temporary directory
  mod_path <- file.path(tempdir(check = TRUE), "test-test-models")
  on.exit(unlink(mod_path, recursive = TRUE), add = TRUE)
  dir.create(mod_path, showWarnings = FALSE)
  # copy all test models to temporary directory
  orig_mod_path <- system.file("extdata", "models", package = "r4ss")
  file.copy(orig_mod_path, mod_path, recursive = TRUE)
  all_mods <- list.dirs(
    file.path(mod_path, "models"),
    full.names = TRUE,
    recursive = FALSE
  )

  # run models without estimation and then run r4ss functions
  message(
    "Will run SS_output() and SS_plots() on models:\n  ",
    paste(basename(all_mods), collapse = ",\n  ")
  )

  ncores <- min(2, parallelly::availableCores(omit = 1))
  future::plan(future::multisession, workers = ncores)

  # Combined model run and result extraction
  results <- furrr::future_map(
    .x = all_mods,
    .f = function(x, dir_exe) {
      exe_path <- file.path(dir_exe, "ss3")
      # Run the model
      run(x, exe = exe_path, extras = "-stopph 0 -nohess")
      # Check for output
      if (!"Report.sso" %in% dir(x)) {
        warning("No Report.sso file in ", x)
        return(list(success = FALSE, model = basename(x)))
      }
      # Extract output and plots
      out <- tryCatch(
        SS_output(x, verbose = FALSE, printstats = FALSE),
        error = function(e) e
      )
      plots <- tryCatch(
        SS_plots(out, verbose = FALSE),
        error = function(e) e
      )
      list(success = is.list(out), model = basename(x), output = out, plots = plots)
    },
    dir_exe = dir_exe
  )
  
  # Check outputs
  expect_true(all(purrr::map_lgl(results, ~ .x$success)))
  expect_true(length(results) == length(all_mods))
  expect_setequal(
    unlist(purrr::map(results, ~ if(is.list(.x$output)) tail(names(.x$output), 1))),
    "inputs"
  )
  expect_true(all(purrr::map_lgl(results, ~ {
    is.list(.x$plots) && "data_plot2.png" %in% .x$plots$file
  })))

  # Run table_all sequentially for stability
  for (res in results) {
    if (is.list(res$output)) table_all(res$output, verbose = TRUE)
  }
})
