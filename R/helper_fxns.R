# contains small helper functions that can be used across multiple functions in
# the r4ss pkg


#' Get the name of the data .ss_new file in a directory
#' 
#' @param dir Relative or absolute path to a directory
#' @return A string with the name of the data .ss_new file. If not found, will
#'  be NA.
get_dat_new_name <- function(dir) {
  datname <- 
    ifelse(file.exists(file.path(dir, "data.ss_new")), 
           "data.ss_new", "data_echo.ss_new")
  if(!file.exists(file.path(dir, "data_echo.ss_new"))) {
    datname <- as.character(NA)
    warning("Neither data.ss_new or data_echo.ss_new found in dir ", dir)
  }
  datname
}
