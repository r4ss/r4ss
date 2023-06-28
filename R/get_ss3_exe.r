#' Download latest Stock Synthesis (SS3) executable.
#'
#' Downloads the latest release SS3 executable according to operating system.
#'
#' @param dir The directory that you would like the executable downloaded to.
#' @return A string of the full file path to the downloaded executable
#' @author Elizabeth F. Gugliotti
#' @export
#' @import gh
#' @examples
#' \dontrun{
#' get_ss3_exe()
#' }
#' @description The `get_ss3_exe()` function gets the latest release tag from
#' github using the {gh} package and uses that tag to download the executable
#' for the appropriate operating system to the directory `dir` (if dir = NULL,
#' then the executable is downloaded to the working directory).

get_ss3_exe <- function(dir = NULL){
  latest_release <- gh::gh("GET /repos/nmfs-stock-synthesis/stock-synthesis/releases/latest", page = 1)
  tag <- latest_release$tag_name

    if(is.null(dir)){
      dir <- getwd()
      message("No directory provided, will download executable to working directory")
      }

    if(.Platform$OS.type == "windows"){
        if(.Platform$r_arch == "x32"){
          warning(
            "Stock Synthesis binary is not available for 32-bit ",
            .Platform$OS.type, ".")
            } else {
              url <- paste0(
                "https://github.com/nmfs-stock-synthesis/stock-synthesis/releases/download/",
                tag, "/ss_win.exe")
              utils::download.file(url, destfile=file.path(dir, "ss3.exe"))
              download_location <- file.path(dir, "ss3.exe")
              return(paste0("The stock synthesis executable was downloaded to: ",
                      download_location))
              }
      } else {
        if(substr(R.version$os, 1, 6) == "darwin") {
          url <- paste0("https://github.com/nmfs-stock-synthesis/stock-synthesis/releases/download/", tag, "/ss_osx")
          utils::download.file(url, destfile=file.path(dir, "ss3"))
          download_location <- file.path(dir, "ss3")
          return(paste0("The stock synthesis executable was downloaded to: ",
                      download_location))
        } else {
            if (R.version$os == "linux-gnu") {
              url <- paste0("https://github.com/nmfs-stock-synthesis/stock-synthesis/releases/download/", tag, "/ss_linux")
              utils::download.file(url, destfile=file.path(dir, "ss3"))
              download_location <- file.path(dir, "ss3")
              return(paste0("The stock synthesis executable was downloaded to: ",
                      download_location))
              } else {
                warning(
                  "Stock Synthesis binary is not available for ", R.version$os,".")
            }
          }
        }
}