context("Generalized read and write function tests")

test_that("models can be read and written", {
  skip_if(!file.exists(system.file("extdata", "models", package = "r4ss")))
  mod_path <- file.path(tempdir(check = TRUE), "test-readwrite")
  on.exit(unlink(mod_path, recursive = TRUE), add = TRUE)
  dir.create(mod_path, showWarnings = FALSE)
  orig_mod_path <- system.file("extdata", "models", package = "r4ss")
  file.copy(orig_mod_path, mod_path, recursive = TRUE)
  all_mods <- list.dirs(file.path(mod_path, "models"),
    full.names = TRUE,
    recursive = FALSE
  )
  message("Will read and write models:\n  ", paste(basename(all_mods),
                                              collapse = ",\n  "))
  for (m in all_mods) {
    message("Now reading model ", basename(m))

    #### Checks related to SS_read* functions
    start <- SS_readstarter(file = file.path(m, "starter.ss"), verbose = FALSE)
    fore <- SS_readforecast(file.path(m, "forecast.ss"), verbose = FALSE)
    dat <- SS_readdat(file.path(m, start[["datfile"]]), verbose = FALSE)
    ctl <- SS_readctl(file.path(m, start[["ctlfile"]]), datlist = dat)
    expect_true(is.list(start))
    expect_true(is.list(fore))
    expect_true(is.list(dat))
    expect_true(is.list(ctl))

    #### Checks related to SS_read
    allfiles <- SS_read(m)
    # Check that the dir call was added to the list
    expect_equal(m, allfiles[["dir"]])
    # Check that the core names are in the list with input files
    expect_true(all(
      c("dat", "ctl", "start", "fore") %in% names(allfiles)
    ))
    # Check that the length of the starter list matches
    # the length of the native starter object
    expect_equal(length(start), length(allfiles[["start"]]))

    #### Checks related to SS_write* functions
    files <- file.path(m, c("starter.ss", "forecast.ss", start[["datfile"]],
                            start[["ctlfile"]]))
    # remove files
    lapply(files, function(x) file.remove(x))

    # write files individually
    message("Now writing model ", basename(m))
    SS_writestarter(start, dir = m, verbose = FALSE)
    SS_writeforecast(fore, dir = m, verbose = FALSE)
    SS_writedat(dat, outfile = file.path(m, start[["datfile"]]), verbose = FALSE)
    SS_writectl(ctl, outfile = file.path(m, start[["ctlfile"]]), verbose = FALSE)

    # confirm that they got written
    lapply(files, function(x) expect_true(file.exists(x)))

    # remove files again
    lapply(files, function(x) file.remove(x))
    # remove wtatage file if it exists
    if (file.exists(file.path(m, "wtatage.ss"))) {
      file.remove(file.path(m, "wtatage.ss"))
    }

    # write all files at once
    SS_write(inputlist = allfiles, dir = allfiles[["dir"]])
    # confirm that they got written
    lapply(files, function(x) expect_true(file.exists(x)))
  }

  # todo: run the models with no est to make sure the written files work with SS
})
