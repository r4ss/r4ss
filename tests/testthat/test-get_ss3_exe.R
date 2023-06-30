# example_path <- system.file("extdata", package = "r4ss")
temp_path <- file.path(tempdir(), "test_ss3_exe")
dir.create(temp_path, showWarnings = FALSE)
# remove all artifacts created from testing. (developers: simply comment out
# the line below if you want to keep artifacts for troubleshooting purposes)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

test_that("executables are downloading", {
  download_loc <- get_ss3_exe(dir = temp_path)
  download_filepath <- gsub(".*: ","", download_loc)
  exe_name <- gsub(paste0(temp_path,"/"),"", download_filepath)
  dir_temp <- file.path(temp_path, exe_name)
  file.remove(download_filepath)
  
  expect_equal(dir_temp, download_filepath)
})