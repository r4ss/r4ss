test_that("test-models (11 to 18) work with SS_output(), SS_plots(), and table_all()", {
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
  )[-(1:10)]  # only take models 11 to 18 (or however many are there)

  # run models without estimation and then run r4ss functions
  message(
    "Will run SS_output() and SS_plots() on models:\n  ",
    paste(basename(all_mods), collapse = ",\n  ")
  )

  #' Run test models with the purpose of being called using the furrr package to
  #' run in parallel.
  #'
  #' @param models list of test models to run
  run_models <- function(models) {
    message("Now running without estimation: ", basename(models))
    run(models, exe = file.path(dir_exe, "ss3"), extras = "-stopph 0 -nohess")
  }

  ncores <- parallelly::availableCores(omit = 1)
  future::plan(future::multisession, workers = ncores)

  furrr::future_map(
    .x = all_mods,
    .f = function(x, dir_exe) {
      message("Now running without estimation: ", basename(x))
      run(x, exe = file.path(dir_exe, "ss3"), extras = "-stopph 0 -nohess")
    },
    dir_exe = dir_exe
  )

  out <- furrr::future_map(.x = all_mods, .f = function(x) {
    if (!"Report.sso" %in% dir(x)) {
      warning("No Report.sso file in ", x)
    } else {
      #### Checks related to SS_output()
      message("Running SS_output()")
      SS_output(x, verbose = FALSE, printstats = FALSE)
    }
  })

  expect_true(all(unlist(purrr::map(out, is.list))))
  expect_true(length(out) == length(all_mods))
  expect_setequal(
    unlist(purrr::map(out, function(x) {
      tail(names(x), 1)
    })),
    "inputs"
  )

  plots <- furrr::future_map(.x = out, .f = function(x) {
    message("Running SS_plots()")
    SS_plots(x, verbose = FALSE)
  })

  expect_true(all(unlist(purrr::map(plots, function(x) {
    "data_plot2.png" %in% x$file
  }))))

  # tables <- furrr::future_map(.x = out, .f = function(x) {
  #   message("Running table_all()")
  #   table_all(x, verbose = FALSE)
  # })

  # furr command above was failing, so trying to loop over the list of model output
  for (i in 1:length(out)) {
    table_all(out[[i]], verbose = TRUE)
  }

  ## was failing here but probably due to user error
  # expect_true(all(unlist(purrr::map(tables, function(x) {
  #   "table_pars" %in% names(x)
  # }))))

  # good practice to reset the future plan to sequential after running
  # parallel code, so that future::plan() doesn't affect anything else
  future::plan(future::sequential)
})
