#' Updates r4ss files to newest versions on web.
#' 
#' Sources files containing R functions r4ss package from the google code
#' repository. These may often be newer than those available form CRAN mirrors.
#' It is probably wise to run this function every time you load the r4ss
#' library.
#' 
#' @param local A local directory from which to source the files instead of
#' getting them from the web.
#' @param save If TRUE, then copy files from web to local directory, then
#' source from this same local directory
#' @param revision Either "newest" (the default), or an optional revision
#' number of the files to source. These numbers are found within the list of
#' changes to the r4ss code at \url{http://code.google.com/p/r4ss/source/list}.
#' If you're using an out-of-date version of SS, or some recent update to the
#' code isn't working for your model, an older revision may help. Otherwise, we
#' recommend using the newest revision.
#' @author Ian Taylor
#' @keywords file
#' @examples
#' 
#' \dontrun{
#' update_r4ss_files()
#' # getting file names from http://r4ss.googlecode.com/svn/trunk/ 
#' # most recent change: Today (6 hours ago) 
#' # 64 files found in http://r4ss.googlecode.com/svn/trunk/ 
#' #   sourcing IOTCmove.R
#' #   sourcing RebuildPlot.R
#' #   sourcing SSFishGraph.R
#' #   sourcing SS_changepars.R
#' #   sourcing SS_fitbiasramp.R
#' #   sourcing SS_makedatlist.R
#' #   ...
#' #   sourcing stackpoly.R
#' #   sourcing update_r4ss_files.R
#' # update complete.
#' 
#' # copy files from web to local directory and then source them
#' update_r4ss_files(local='c:/SS/R/r4ss_files/',save=T)
#' 
#' # source files from a local directory (i.e. if no network available)
#' update_r4ss_files(local='c:/SS/R/r4ss_files/',save=F)
#' 
#' # update the updater function to get the new options:
#' source("http://r4ss.googlecode.com/svn/trunk/update_r4ss_files.R")
#' 
#' # get version 523 (for latest version, no "revision" input is needed)
#' update_r4ss_files(revision=523) 
#' }
#' 
update_r4ss_files <- function (local = NULL, save = FALSE, revision = "newest"){

  getwebnames <- function() {
    changes <- readLines("http://code.google.com/p/r4ss/source/list")
    line <- changes[grep("detail?", changes)[6]]
    cat("most recent change:", strsplit(strsplit(line, ">")[[1]][3], 
                                        "<")[[1]][1], "\n")
    current_revision <- as.numeric(strsplit(strsplit(line, 
                 "detail?r=", fixed = TRUE)[[1]][2], "\">")[[1]][1])
    cat("current revision number:", current_revision, "\n")
    if (revision == "newest") {
      webdir <- "http://r4ss.googlecode.com/svn/trunk/"
    }
    else {
      if (is.numeric(revision) && revision <= current_revision) {
        webdir <- paste("http://r4ss.googlecode.com/svn-history/r", 
                        revision, "/trunk/", sep = "")
      }
      else {
        stop("'revision' input should either be 'newest', or an integer <", 
             current_revision)
      }
    }
    cat("getting file names from", webdir, "\n")
    lines <- readLines(webdir, warn = F)
    filenames <- lines[grep("\"*.R\"", lines)]
    for (i in 1:length(filenames)) filenames[i] <- strsplit(filenames[i], 
                                                            "\">")[[1]][2]
    for (i in 1:length(filenames)) filenames[i] <- strsplit(filenames[i], 
                                                            "</a>")[[1]][1]
    return(list(filenames = filenames, webdir = webdir))
  }
  getwebfiles <- function(fileinfo) {
    filenames <- fileinfo$filenames
    webdir <- fileinfo$webdir
    n <- length(filenames)
    cat(n, "files found\n")
    if (save){
      if(is.null(local)) local <- getwd()
      cat("saving all files to", local, "\n")
      cat("  saving...\n   ")
    }else{
      cat("  sourcing...\n   ")
    }
    for (i in 1:n) {
      webfile <- paste(webdir, filenames[i], sep = "/")
      if (filenames[i] == "update_r4ss_files.R") 
        webfile <- "http://r4ss.googlecode.com/svn/trunk/update_r4ss_files.R"
      if (save) {
        localfile <- paste(local, filenames[i], sep = "/")
        temp <- readLines(webfile)
        writeLines(temp, localfile)
        cat(filenames[i], ",",ifelse(i==n | i%%4==0,"\n   "," "), sep = "")
      }
      else {
        cat(filenames[i], ",",ifelse(i==n | i%%4==0,"\n   "," "), sep = "")
        source(webfile)
      }
      flush.console()
    }
  }
  getlocalfiles <- function(local) {
    filenames <- dir(local, pattern = "*.R$")
    n <- length(filenames)
    cat(n, "files found in", local, "\n")
    cat("  sourcing...\n")
    for (i in 1:n) {
      cat(filenames[i], ",",ifelse(i==n | i%%4==0,"\n   "," "), sep = "")
      source(paste(local, filenames[i], sep = "/"))
      flush.console()
    }
  }
  if (is.null(local)) {
    fileinfo <- getwebnames()
    getwebfiles(fileinfo)
  }
  else {
    if (save) {
      fileinfo <- getwebnames()
      getwebfiles(fileinfo)
    }
    getlocalfiles(local)
  }
  cat("\n  r4ss update complete.\n")
}
