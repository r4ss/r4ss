# contains small helper functions that can be used across multiple functions in
# the r4ss pkg


#' Get the name of the data .ss_new file in a directory
#'
#' In previous versions of Stock Synthesis,
#' the file new data file was named `data.ss_new`.
#' `_echo` was added to the name when the file was parsed into three separate
#' files.
#' @param dir Relative or absolute path to a directory
#' @return A string with the name of the data .ss_new file. If not found, will
#'  be NA. Both of strings are searched for using `dir(pattern = )` and
#'  if both exist, then `data_echo.ss_new` is returned.
#'
get_dat_new_name <- function(dir) {
  datname <- tail(
    dir(path = dir, pattern = "data_*e*c*h*o*\\.ss_new"),
    1
  )
  ifelse(length(datname) == 0, NA, datname)
}
