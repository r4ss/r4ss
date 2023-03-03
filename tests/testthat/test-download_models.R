# setup
temp_dir <- tempdir()
save_models_dir <- file.path(temp_dir, "save_mods")
# create directories and use on exit statements to ensure cleanup at the end.
dir.create(save_models_dir)
on.exit(unlink(save_models_dir, recursive = TRUE), add = TRUE)
if (!dir.exists("inst")) {
  dir.create("inst")
  dir.create("inst/extdata")
  on.exit(unlink("inst", recursive = TRUE), add = TRUE)
}
if (!dir.exists("inst/extdata")) {
  dir.create("inst/extdata")
  on.exit(unlink("inst/extdata", recursive = TRUE), add = TRUE)
}

test_that("download_models with defaults work", {
  download_models()
  expect_true(file.exists("inst/extdata/models"))
  expect_true(file.exists("inst/extdata/models/Simple"))
  expect_true(file.exists("inst/extdata/models/Simple/ss.par"))
})

test_that("download_models with different dir works", {
  download_models(dir = save_models_dir)
  expect_true(file.exists(file.path(save_models_dir, "models")))
  expect_true(file.exists(file.path(save_models_dir, "models", "Simple")))
  expect_true(file.exists(file.path(save_models_dir, "models", "Simple", "ss.par")))
})

test_that("download_models() works when the dir doesn't exist", {
  temp_dir_missing_dir <- file.path(save_models_dir, "missing_dir", "missing_subdir")
  # create it
  worked <- download_models(dir = temp_dir_missing_dir)
  expect_true(worked)
  expect_true(file.exists(file.path(temp_dir_missing_dir, "models")))
  expect_true(file.exists(file.path(temp_dir_missing_dir, "models", "Simple")))
  expect_true(file.exists(file.path(temp_dir_missing_dir, "models", "Simple", "ss.par")))
})

# (note that this generates 2 warnings, and I'm not sure how to suppress them!)
test_that("download_models() fails when the branch doesn't exist", {
  temp_dir_branch_dne <- file.path(save_models_dir, "diff_branch_dne")
  dir.create(temp_dir_branch_dne)
  expect_error(
    download_models(
      dir = temp_dir_branch_dne,
      branch = "not_existing_branch"
    ),
    "not be downloaded"
  )
})
