context("Generalized read and write function tests")

test_that("models can be read and written", {
  skip_if(!file.exists(system.file("extdata", "models", package = "r4ss")))
  mod_path <- file.path(tempdir(check = TRUE), "test-readwrite")
  on.exit(unlink(mod_path, recursive = TRUE), add = TRUE)
  dir.create(mod_path, showWarnings = FALSE)
  orig_mod_path <- system.file("extdata", "models", package = "r4ss")
  file.copy(orig_mod_path, mod_path, recursive = TRUE)
  all_mods <- list.dirs(
    file.path(mod_path, "models"),
    full.names = TRUE,
    recursive = FALSE
  )
  # modify a complex model to use use the .par file so that it
  # tests reading and writing the par file
  tag_mod <- all_mods[grepl("tagging_mirrored_sel", all_mods)]
  expect_true(length(tag_mod) == 1)
  if (length(tag_mod) == 1) {
    tag_mod_par <- file.path(dirname(tag_mod), "tagging_mirrored_sel_par")
    # copy input files
    copy_SS_inputs(
      dir.old = tag_mod,
      dir.new = tag_mod_par,
      copy_par = TRUE
    )
    # read starter file
    start <- SS_readstarter(file.path(tag_mod_par, "starter.ss"))
    # tell it to read the par file
    start$init_values_src <- 1
    # write changes to starter
    SS_writestarter(
      mylist = start,
      dir = tag_mod_par,
      overwrite = TRUE
    )
    # add new model to vector of all models
    all_mods <- c(all_mods, tag_mod_par)
  }

  message(
    "Will read and write models:\n  ",
    paste(basename(all_mods), collapse = ",\n  ")
  )
  for (m in all_mods) {
    message("Now reading model ", basename(m))

    #### Checks related to SS_read* functions
    start <- SS_readstarter(file = file.path(m, "starter.ss"), verbose = FALSE)
    fore <- SS_readforecast(file.path(m, "forecast.ss"), verbose = FALSE)
    dat <- SS_readdat(file.path(m, start[["datfile"]]), verbose = FALSE)
    ctl <- SS_readctl(file.path(m, start[["ctlfile"]]), datlist = dat)
    parfile <- get_par_name(m)
    par <- SS_readpar_3.30(
      file.path(m, parfile),
      datsource = dat,
      ctlsource = ctl,
      verbose = FALSE
    )
    expect_true(is.list(start))
    expect_true(is.list(fore))
    expect_true(is.list(dat))
    expect_true(is.list(ctl))
    expect_true(is.list(par))

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
    files <- file.path(
      m,
      c(
        "starter.ss",
        "forecast.ss",
        start[["datfile"]],
        start[["ctlfile"]],
        parfile
      )
    )
    # remove files
    lapply(files, function(x) file.remove(x))

    # write files individually
    message("Now writing model ", basename(m))
    SS_writestarter(start, dir = m, verbose = FALSE)
    SS_writeforecast(fore, dir = m, verbose = FALSE)
    SS_writedat(
      dat,
      outfile = file.path(m, start[["datfile"]]),
      verbose = FALSE
    )
    SS_writectl(
      ctl,
      outfile = file.path(m, start[["ctlfile"]]),
      verbose = FALSE
    )
    par2 <- SS_writepar_3.30(
      par,
      outfile = file.path(m, parfile),
      verbose = FALSE
    )

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
    lapply(files[basename(files) != parfile], function(x) {
      expect_true(file.exists(x))
    })
  }

  # todo: run the models with no est to make sure the written files work with SS
})

test_that("empty files lead to NULL or error", {
  expect_null((SS_readwtatage("nonexistentfile.ss", verbose = FALSE)))
  expect_message((SS_readwtatage("nonexistentfile.ss")))
  # Have to catch warning because as of 3rd edition of {testthat},
  # `expect_*` can only trap one expectation and the following leads to
  # both an error and a warning.
  expect_error(suppressWarnings(
    SS_readstarter("nonexistentfile.ss", verbose = FALSE)
  ))
})

test_that("SS_read works with a raw github URL", {
  skip_if_offline(host = "github.com")
  list_objs <- SS_read(
    dir = "https://raw.githubusercontent.com/nmfs-ost/ss3-test-models/main/models/Simple"
  )
  expect_true(is.list(list_objs))
  expect_equal(
    names(list_objs),
    c(
      "dir",
      "path",
      "dat",
      "ctl",
      "start",
      "fore"
    )
  )
})
