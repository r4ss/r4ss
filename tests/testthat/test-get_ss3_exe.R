test_that("executables are downloading", {
  download_loc <- get_ss3_exe()
  download_filepath <- gsub(".*: ","", download_loc)
  exe_name <- gsub(paste0(getwd(),"/"),"", download_filepath)
  dir_wd <- file.path(getwd(),exe_name)
  
  testthat::expect_equal(dir_wd, download_filepath)
})