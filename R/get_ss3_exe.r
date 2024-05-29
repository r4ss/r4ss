#' Download the Stock Synthesis (SS3) executable
#'
#' Downloads the SS3 executable according to specified version and the user
#' operating system.
#'
#' @param dir The directory that you would like the executable downloaded to.
#' @param version A character string of the executable version tag to download
#' (e.g.'v3.30.20' or 'v3.30.18'). A list of tags is available at
#' https://github.com/nmfs-ost/ss3-source-code/tags
#' @return A string of the file path to the downloaded executable
#' @author Elizabeth F. Gugliotti
#' @export
#' @import gh
#' @examples
#' \dontrun{
#' get_ss3_exe()
#' get_ss3_exe(version = "v3.30.18")
#' }
#' @description The `get_ss3_exe()` function uses the {gh} package to get either
#' the latest release (if version = NULL) or the specified version of the Stock
#' Synthesis executable for the appropriate operating system to the directory `dir`
#' (if dir = NULL, then the executable is downloaded to the working directory).
#' To view the version tags available go to
#' https://github.com/nmfs-ost/ss3-source-code/tags

get_ss3_exe <- function(dir = NULL, version = NULL) {
  # Get latest release if version not specified
  if (is.null(version)) {
    latest_release <- gh::gh("GET /repos/nmfs-ost/ss3-source-code/releases/latest", page = 1)
    tag <- latest_release[["tag_name"]]
  } else {
    # Otherwise get specified version
    all_tags <- gh::gh("GET /repos/nmfs-ost/ss3-source-code/tags")
    df_tags <- as.data.frame(do.call(rbind, all_tags))
    tags <- unlist(df_tags[["name"]])

    if (!version %in% tags) {
      warning("The version tag that you entered is invalid or not in the right format,
              please go to https://github.com/nmfs-ost/ss3-source-code/tags
              to get a correct version tag or version tag format")
    } else {
      tag <- version
    }
  }

    if (is.null(dir)) {
      dir <- getwd()
      message("No directory provided, the executable will be downloaded to the working directory")
    }

    if (!dir.exists(dir)) {
      stop("Directory doesn't exist: ", dir)
    }

    if (.Platform[["OS.type"]] == "windows") {
      if(!grepl("Schannel",curl::curl_version()$ssl_version)){
        stop(
          "Please do the following:\n",
          "1. Run write('CURL_SSL_BACKEND=openssl', file = '~/.Renviron', append = TRUE) in your R session\n",
          "2. Restart your R session\n",
          "3. Run curl::curl_version()$ssl_version and confirm the return is OpenSSL/1.1.1 (Schannel) (note the numbers may be different)\n",
          "4. Try remotes::install_github() (e.g., devtools::install_github('tidyverse/dplyr')). It should work and continue to work in future R sessions.")
      }
      if (.Platform[["r_arch"]] == "x32") { # nocov start
        warning(
          "Stock Synthesis binary is not available for 32-bit ",
          .Platform[["OS.type"]], "."
        )
      } else {
        url <- paste0(
          "https://github.com/nmfs-ost/ss3-source-code/releases/download/",
          tag, "/ss3_win.exe"
        )
        try_ss <- tryCatch(
          suppressWarnings(utils::download.file(url, destfile = file.path(dir, "ss3.exe"), mode = "wb")),
          error = function(e) "ss3 name not right for this version, trying ss"
        )

        if (try_ss == "ss3 name not right for this version, trying ss") {
          url <- paste0(
            "https://github.com/nmfs-ost/ss3-source-code/releases/download/",
            tag, "/ss_win.exe"
          )
          utils::download.file(url, destfile = file.path(dir, "ss3.exe"), mode = "wb")
        }
        download_location <- file.path(dir, "ss3.exe")
        message(paste0(
          "The stock synthesis executable for Windows ", tag, " was downloaded to: ",
          download_location
        ))
      }
    } else {
      if (substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]] == "aarch64") {
        url <- paste0(
          "https://github.com/nmfs-ost/ss3-source-code/releases/download/",
          tag, "/ss3_osx_arm64"
        )
        try_arm64 <- tryCatch(
          suppressWarnings(utils::download.file(url, destfile = file.path(dir, "ss3"), mode = "wb")),
          error = function(e) "ss3 executable not available for macOS arm64 architecture
              computers for versions prior to v.3.30.22.1"
        )

        if (try_arm64 == "ss3 executable not available for macOS arm64 architecture computers for
          versions prior to v.3.30.22.1") {
          print(try_arm64)
        } else {
          Sys.chmod(paths = file.path(dir, "ss3"), mode = "0700")
          download_location <- file.path(dir, "ss3")
          message(paste0(
            "The stock synthesis executable for Mac ", tag, " was downloaded to: ",
            download_location
          ))
        }
      } else {
        if (substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]] == "x86_64") {
          url <- paste0(
            "https://github.com/nmfs-ost/ss3-source-code/releases/download/",
            tag, "/ss3_osx"
          )
          try_ss <- tryCatch(
            suppressWarnings(utils::download.file(url, destfile = file.path(dir, "ss3"), mode = "wb")),
            error = function(e) "ss3 name not right for this version, trying ss"
          )

          if (try_ss == "ss3 name not right for this version, trying ss") {
            url <- paste0(
              "https://github.com/nmfs-ost/ss3-source-code/releases/download/",
              tag, "/ss_osx"
            )
            utils::download.file(url, destfile = file.path(dir, "ss3"), mode = "wb")
          }
          Sys.chmod(paths = file.path(dir, "ss3"), mode = "0700")
          download_location <- file.path(dir, "ss3")
          message(paste0(
            "The stock synthesis executable for Mac ", tag, " was downloaded to: ",
            download_location
          ))
        } else {
          if (R.version[["os"]] == "linux-gnu") {
            url <- paste0(
              "https://github.com/nmfs-ost/ss3-source-code/releases/download/",
              tag, "/ss3_linux"
            )
            try_ss <- tryCatch(
              suppressWarnings(utils::download.file(url, destfile = file.path(dir, "ss3"), mode = "wb")),
              error = function(e) "ss3 name not right for this version, trying ss"
            )

            if (try_ss == "ss3 name not right for this version, trying ss") {
              url <- paste0(
                "https://github.com/nmfs-ost/ss3-source-code/releases/download/",
                tag, "/ss_linux"
              )
              utils::download.file(url, destfile = file.path(dir, "ss3"), mode = "wb")
            }
            Sys.chmod(paths = file.path(dir, "ss3"), mode = "0700")
            Sys.chmod(paths = dir, mode = "0777")
            download_location <- file.path(dir, "ss3")
            message(paste0(
              "The stock synthesis executable for Linux ", tag, " was downloaded to: ",
              download_location
            ))
          } else {
            stop(
              "The Stock Synthesis executable is not available for ", R.version[["os"]], "."
            ) # nocov end
          }
        }
      }
    }
    return(invisible(download_location))
  } 
}
