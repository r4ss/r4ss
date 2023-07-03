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
#' get_ss3_exe_no_version()
#' }
#' @description The `get_ss3_exe_no_version()` function gets the latest release version from
#' github using the {gh} package and uses that tag to download the executable
#' for the appropriate operating system to the directory `dir` (if dir = NULL,
#' then the executable is downloaded to the working directory).

get_ss3_exe_no_version <- function(dir = NULL){
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


#' Download Stock Synthesis (SS3) executable.
#'
#' Downloads the SS3 executable according to operating system.
#'
#' @param dir The directory that you would like the executable downloaded to.
#' @param version A character string of the tag of the version that of the 
#' executable to download. Ex. 'v3.30.20' or 'v3.30.18'
#' @return A string of the full file path to the downloaded executable
#' @author Elizabeth F. Gugliotti
#' @export
#' @import gh
#' @examples
#' \dontrun{
#' get_ss3_exe()
#' }
#' @description The `get_ss3_exe()` function gets either the version entered or 
#' the latest release version from GitHub using the {gh} package and uses that tag 
#' to download the executable for the appropriate operating system to the 
#' directory `dir` (if dir = NULL, then the executable is downloaded to the 
#' working directory). To view the version tags available go to 
#' https://github.com/nmfs-stock-synthesis/stock-synthesis/tags

get_ss3_exe <- function(dir = NULL, version = NULL){
  if(is.null(version)){
    latest_release <- gh::gh("GET /repos/nmfs-stock-synthesis/stock-synthesis/releases/latest", page = 1)
    tag <- latest_release$tag_name 
  } else {
    all_tags <- gh::gh("GET /repos/nmfs-stock-synthesis/stock-synthesis/tags")
    df_tags <- as.data.frame(do.call(rbind, all_tags))
    tags <- unlist(df_tags$name)
    
    if(!version %in% tags){
      warning("The version you entered is invalid or not in the right format, 
              please go to https://github.com/nmfs-stock-synthesis/stock-synthesis/tags 
              to get a correct version or version format")
    } else{
      tag <- version
    }
  }
  
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