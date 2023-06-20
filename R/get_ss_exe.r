#' Download latest Stock Synthesis (SS3) executable.
#'
#' Downloads the latest release SS3 executable for the operating system 
#' specified.
#' 
#' @param os The operating system as "linux", "windows", or "mac"
#' @param dir The directory that you would like the executable downloaded to.
#' @return The file path to the downloaded executable
#' @author Elizabeth F. Gugliotti
#' @export
#' @examples
#' \dontrun{
#' get_ss_exe(os = "windows", dir = getwd())
#' check_exe()
#' # check for executable with a different name in the PATH
#' check_exe(exe = "ss_win")
#' # check for executable in a specific directory
#' check_exe(exe = "ss_linux", dir = "~/ss/ss_v3.30.19.01")
#' }
#' 
#' @description The `get_ss_exe()` function gets the latest release tag from
#' github using the {gh} package and uses that tag to download the executable
#' for the appropriate operating system to the directory `dir` (if dir = NULL,
#' then the executable is downloaded to the working directory). The operating
#' system is specified by `os` where the os options are "windows", "linux", or
#' "mac". The file path for the downloaded executable is returned.

get_ss_exe <- function(os, dir){
    if(dir = NULL){
        dir = getwd()
    }

    if(os %in% c("windows", "linux", "mac")){

        latest_release <- gh::gh("GET /repos/nmfs-stock-synthesis/stock-synthesis/releases/latest", page = 1)
        tag <- latest_release$tag_name

        if(os == "windows"){
            url <- paste0("https://github.com/nmfs-stock-synthesis/stock-synthesis/releases/download/", tag, "/ss_win.exe")
            download.file(url, destfile=file.path(dir, "ss.exe"))
        }
        if(os == "linux"){
            url <- paste0("https://github.com/nmfs-stock-synthesis/stock-synthesis/releases/download/", tag, "/ss_linux.exe")
            download.file(url, destfile=file.path(dir, "ss.exe"))
        }
        if(os == "mac"){
            url <- paste0("https://github.com/nmfs-stock-synthesis/stock-synthesis/releases/download/", tag, "/ss_osx.exe")
            download.file(url, destfile=file.path(dir, "ss.exe"))
        
      # return the file path to the executable
        return(path_to_exe = file.path(dir, "ss.exe"))
        }
    } else{
        stop("Please enter a valid os system: os = 'windows', os = 'linux', or os = 'mac'. ")
    }
}

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}