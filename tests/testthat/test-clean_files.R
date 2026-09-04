test_that("clean_files lists only matching top-level files", {
  model_dir <- tempfile("clean_files_")
  dir.create(model_dir)
  on.exit(unlink(model_dir, recursive = TRUE), add = TRUE)
  matching <- c(
    "admodel.p01", "admodel.b01", "admodel.r01", "admodel.std",
    "gradient.1", "SIS_table.sso", "ParmTrace.sso", "runnumber.ss"
  )
  retained <- c("starter.ss", "Report.sso", "admodel.par", "gradient")
  file.create(file.path(model_dir, c(matching, retained)))
  dir.create(file.path(model_dir, "nested"))
  file.create(file.path(model_dir, "nested", "admodel.std"))

  expect_message(
    listed <- clean_files(model_dir, action = "list"),
    "Successfully deleted 8 matching files"
  )

  expect_setequal(listed, matching)
  expect_true(all(file.exists(file.path(model_dir, c(matching, retained)))))
  expect_true(file.exists(file.path(model_dir, "nested", "admodel.std")))
})

test_that("clean_files deletes matching files by default", {
  model_dir <- tempfile("clean_files_")
  dir.create(model_dir)
  on.exit(unlink(model_dir, recursive = TRUE), add = TRUE)
  matching <- c("admodel.cov", "POSTERIORS.SSO", "variance")
  retained <- "data.ss"
  file.create(file.path(model_dir, c(matching, retained)))

  expect_message(
    deleted <- clean_files(model_dir),
    "Successfully deleted 3 matching files"
  )

  expect_setequal(deleted, matching)
  expect_false(any(file.exists(file.path(model_dir, matching))))
  expect_true(file.exists(file.path(model_dir, retained)))
})

test_that("clean_files preserves MCMC files when posteriors are populated", {
  model_dir <- tempfile("clean_files_")
  dir.create(model_dir)
  on.exit(unlink(model_dir, recursive = TRUE), add = TRUE)
  writeLines(rep("posterior sample", 101), file.path(model_dir, "posteriors.sso"))
  protected <- c(
    "posteriors.sso", "posterior_obj_func.sso", "posterior_vectors.sso",
    "admodel.psv"
  )
  file.create(file.path(model_dir, protected[-1]))
  file.create(file.path(model_dir, "admodel.std"))

  expect_message(
    deleted <- clean_files(model_dir),
    "skipping deletion of MCMC-related files"
  )

  expect_equal(deleted, "admodel.std")
  expect_true(all(file.exists(file.path(model_dir, protected))))
  expect_false(file.exists(file.path(model_dir, "admodel.std")))
})

test_that("clean_files reports when no files match", {
  model_dir <- tempfile("clean_files_")
  dir.create(model_dir)
  on.exit(unlink(model_dir, recursive = TRUE), add = TRUE)

  expect_message(
    found <- clean_files(model_dir, action = "list"),
    "No matching files found to delete"
  )
  expect_length(found, 0L)
})

test_that("clean_files validates inputs", {
  expect_error(clean_files(tempdir(), action = "move"), "action")
  expect_error(clean_files(file.path(tempdir(), "missing")), "does not exist")
})