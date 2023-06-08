context("Read output and make plots for all test-models")

test_that("test-models work with SS_output() and SS_plots()", {
  skip_if(!file.exists(system.file("extdata", "models", package = "r4ss")))
  # skip if no executable in simple_small path
  # (should have been loaded there by 
  # .github\workflows\r4ss-extra-tests.yml)
  skip_if(
    (!file.exists("inst/extdata/simple_small/ss"))) &
      (!file.exists("inst/extdata/simple_small/ss.exe"))),
    message = "skipping test that requires SS3 executable"
  )
  mod_path <- file.path(tempdir(check = TRUE), "test-test-models")
  on.exit(unlink(mod_path, recursive = TRUE), add = TRUE)
  dir.create(mod_path, showWarnings = FALSE)
  orig_mod_path <- system.file("extdata", "models", package = "r4ss")
  file.copy(orig_mod_path, mod_path, recursive = TRUE)
  all_mods <- list.dirs(file.path(mod_path, "models"),
    full.names = TRUE,
    recursive = FALSE
  )
  message("Will run SS_output() and SS_plots() on models:\n  ", paste(basename(all_mods),
    collapse = ",\n  "
  ))
  for (m in all_mods) {
    message("Now running without estimation: ", basename(m))
    run(m, exe = "inst/extdata/simple_small/ss", extras = "-stopph 0 -nohess")

    #### Checks related to SS_output()
    message("Running SS_output()")
    out <- SS_output(m, verbose = FALSE, printstats = FALSE)
    expect_true(is.list(out))
    expect_equal(tail(names(simple_small), 1), "inputs")

    #### Checks related to SS_plots()
    message("Running SS_plots()")
    plots <- SS_plots(out, verbose = FALSE)
    expect_equal(tail(plots$file, 1), "parameterchecks.html")
  }
})
