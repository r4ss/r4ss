context("Read output and make plots for all test-models")

# temporary directory
mod_path <- file.path(tempdir(check = TRUE), "test-test-models")
download_models(dir = mod_path)
mods <- list.dirs(
  file.path(mod_path, "models"),
  full.names = FALSE,
  recursive = FALSE
)

# run models
for (i in seq_along(all_mods)) {
  test_that(
    glue::glue(
      "run SS_output() and SS_plots() on test model: {mods[i]}"
    ),
    {
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
      # copy all test models to temporary directory
      orig_mod_path <- system.file("extdata", "models", package = "r4ss")
      orig_all_mods <- list.dirs(
        file.path(mod_path, "models"),
        full.names = TRUE,
        recursive = FALSE
      )
      mod <- basename(orig_all_mods[i])
      file.copy(orig_all_mods[i], file.path(mod_path, mod))

      cli::cli_alert_info(
        "Now running without estimation: {mod}"
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
        output <- SS_output(
          all_mods[i],
          verbose = FALSE,
          printstats = FALSE
        )
      }
      expect_true(!is.null(output))
      expect_true("inputs" %in% names(output))

      cli::cli_alert_info(
        "Running SS_plots() for model {mod}"
      )
      SS_plots(output, verbose = FALSE)
      expect_true("data_plot2.png" %in% dir(file.path(all_mods[i], "plots")))

      cli::cli_alert_info(
        "Running table_all() for model {mod}"
      )
      table_all(output, verbose = TRUE)
      expect_true(
        "table_parcounts.rda" %in% dir(file.path(all_mods[i], "tables"))
      )
    }
  ) # end testthat
} # end loop over models
