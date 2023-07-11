# example_path <- system.file("extdata", package = "r4ss")
temp_path <- file.path(tempdir(), "test_ss3_exe")
dir.create(temp_path, showWarnings = FALSE)
# remove all artifacts created from testing. (developers: simply comment out
# the line below if you want to keep artifacts for troubleshooting purposes)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

test_that("executables are downloading default latest version", {
  download_loc <- get_ss3_exe(dir = temp_path)
  download_filepath <- gsub(".*: ", "", download_loc)
  exe_name <- gsub(paste0(temp_path, "/"), "", download_filepath, fixed = TRUE)
  dir_temp <- file.path(temp_path, exe_name)
  file.remove(download_filepath)

  expect_equal(dir_temp, download_filepath)
})

test_that("executables are downloading with version", {
  download_loc <- get_ss3_exe(dir = temp_path, version = "v3.30.18")
  download_filepath <- gsub(".*: ", "", download_loc)
  exe_name <- gsub(paste0(temp_path, "/"), "", download_filepath, fixed = TRUE)
  dir_temp <- file.path(temp_path, exe_name)
  file.remove(download_filepath)

  expect_equal(dir_temp, download_filepath)
})

test_that("executables are able to run simple_small model", {
  simple_small <- system.file("extdata/simple_small", package = "r4ss")
  file.copy(simple_small, temp_path, recursive = TRUE)
  path <- file.path(temp_path, "simple_small")
  path <- normalizePath(path, "/")
  get_ss3_exe(dir = file.path(temp_path, "simple_small"))
  download_exe <- list.files(pattern = "ss3|ss_win.exe", temp_path)
  if(download_exe == "ss3"){
    r4ss::run(dir = path, exe = "ss3", skipfinished = FALSE)
  } else{
    r4ss::run(dir = path, exe = "ss_win", skipfinished = FALSE)
  }
  file_date <- file.mtime(file.path(temp_path, "simple_small/Report.sso"))
  file_date <- gsub(" .*", "",file_date)
  expect_true(file_date == Sys.Date())
})
