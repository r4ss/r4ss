context("Read output and make plots for all test-models")

# Keep downloaded models and generated outputs outside the repository.
models_path <- file.path(tempdir(check = TRUE), "test-test-models")
download_models(dir = models_path)
mods <- list.dirs(
  file.path(models_path, "models"),
  full.names = FALSE,
  recursive = FALSE
)

# Give each model its own test so failures identify the affected configuration.
for (i in seq_along(mods)) {
  test_that(
    glue::glue(
      "run SS_output(), SS_plots(), and ss3_data_to_fims() on test model: {mods[i]}"
    ),
    {
      # Reuse the SS3 executable bundled with simple_small; skip if unavailable.
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
      # Regenerate reports and .ss_new inputs at the initial parameter values.
      # Avoid estimation and Hessian calculations to keep integration tests fast.
      run(
        mod_path,
        exe = file.path(dir_exe, "ss3"),
        extras = "-stopph 0 -nohess",
        skipfinished = FALSE
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
          "Running ss3_data_to_fims() for model {mod}"
        )
        # Read SS3's generated inputs, including weight-at-age where available.
        # Reuse the parsed output for any empirical age-to-length conversion.
        inputs <- SS_read(dir = mod_path, ss_new = TRUE, read_wtatage = TRUE)
        fims_data <- ss3_data_to_fims(
          ss3_inputs = inputs,
          ss3_output = output
        )
        # Check conversion for every model, regardless of its composition types.
        # These assertions check the FIMS data schema, not execution of FIMS.
        expect_s3_class(fims_data, "data.frame")
        expect_named(
          fims_data,
          c(
            "type",
            "fleet",
            "age",
            "length",
            "timing",
            "observed",
            "unit",
            "uncertainty"
          )
        )
        expect_gt(nrow(fims_data), 0)

        # Check CAAL coordinates only when the converter resolves restricted
        # population-length bounds into conditional age-at-length output.
        if ("caal_comp" %in% fims_data[["type"]]) {
          # CAAL output needs both coordinates, unlike marginal age compositions.
          caal <- fims_data[fims_data[["type"]] == "caal_comp", ]
          expect_gt(nrow(caal), 0)
          expect_true(all(!is.na(caal[["age"]])))
          expect_true(all(!is.na(caal[["length"]])))
        }

        cli::cli_alert_info(
          "Running SS_plots() for model {mod}"
        )
        # Use low resolution and check a representative file was written.
        SS_plots(output, verbose = FALSE, res = 50)
        expect_true("data_plot2.png" %in% dir(file.path(mod_path, "plots")))

        cli::cli_alert_info(
          "Running table_all() for model {mod}"
        )
        # Exercise table generation and verify a representative saved table.
        table_all(output, verbose = TRUE)
        expect_true(
          "table_parcounts.rda" %in% dir(file.path(mod_path, "tables"))
        )
      }
    }
  ) # end testthat
} # end loop over models
