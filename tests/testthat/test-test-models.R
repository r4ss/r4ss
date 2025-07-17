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

  # make empty list to store output
  all_output <- rep(list(NULL), length(all_mods))

  # run models
  for (i in seq_along(all_mods)) {
    cli::cli_alert_info(
      "Now running without estimation: {basename(all_mods[i])}"
    )
    run(
      all_mods[i],
      exe = file.path(dir_exe, "ss3"),
      extras = "-stopph 0 -nohess"
    )

    if (!"Report.sso" %in% dir(all_mods[i])) {
      cli::cli_alert_warning("No Report.sso file in {all_mods[i]}")
    } else {
      #### Checks related to SS_output()
      message("Running SS_output()")
      all_output[[i]] <- SS_output(
        all_mods[[i]],
        verbose = FALSE,
        printstats = FALSE
      )
    }
  }

  # confirm that there are no NULL outputs
  expect_true(all(!sapply(all_output, is.null)))

  expect_true(all(unlist(lapply(all_output, function(x) {
    tail(names(x), 1) == "inputs"
  }))))

  for (i in seq_along(all_output)) {
    cli::cli_alert_info("Running SS_plots() for model {basename(all_mods[i])}")
    SS_plots(all_output[[i]], verbose = FALSE)
    expect_true("data_plot2.png" %in% file.path(all_mods[i], "plots"))
  }

  for (i in seq_along(all_output)) {
    cli::cli_alert_info("Running table_all() for model {basename(all_mods[i])}")
    table_all(all_output[[i]], verbose = TRUE)
    expect_true("table_parcounts.rda" %in% file.path(all_mods[i], "tables"))
  }
})
