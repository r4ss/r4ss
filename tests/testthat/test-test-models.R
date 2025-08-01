context("Read output and make plots for all test-models")

# download models to a temporary directory
models_path <- file.path(tempdir(check = TRUE), "test-test-models")
download_models(dir = models_path)
mods <- list.dirs(
  file.path(models_path, "models"),
  full.names = FALSE,
  recursive = FALSE
)

# run models
for (i in seq_along(mods)) {
  test_that(
    glue::glue(
      "run SS_output() and SS_plots() on test model: {mods[i]}"
    ),
    {
      # find simple_small
      dir_exe <- system.file("extdata", "simple_small", package = "r4ss")
      skip_if(
        (!file.exists(file.path(dir_exe, "ss3")) &
          !file.exists(file.path(dir_exe, "ss3.exe"))),
        message = paste("skipping test: no exe called 'ss3' found in", dir_exe)
      )
      mod <- basename(mods[i])
      mod_path <- file.path(models_path, "models", mod)

      cli::cli_alert_info(
        "Now running without estimation: {mod}"
      )
      run(
        mod_path,
        exe = file.path(dir_exe, "ss3"),
        extras = "-stopph 0 -nohess"
      )

      if (!"Report.sso" %in% dir(mod_path)) {
        cli::cli_alert_warning("No Report.sso file in {mod_path}")
      } else {
        #### Checks related to SS_output()
        message("Running SS_output()")
        output <- SS_output(
          mod_path,
          verbose = FALSE,
          printstats = FALSE
        )
      }
      expect_true(exists("output"))
      if (exists("output")) {
        expect_true("inputs" %in% names(output))

        cli::cli_alert_info(
          "Running SS_plots() for model {mod}"
        )
        # make low-resolution plots to save time
        SS_plots(output, verbose = FALSE, res = 50)
        expect_true("data_plot2.png" %in% dir(file.path(mod_path, "plots")))

        cli::cli_alert_info(
          "Running table_all() for model {mod}"
        )
        table_all(output, verbose = TRUE)
        expect_true(
          "table_parcounts.rda" %in% dir(file.path(mod_path, "tables"))
        )
      }
    }
  ) # end testthat
} # end loop over models
