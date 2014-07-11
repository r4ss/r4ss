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
#' @param GitHub Get files from GitHub (as opposed to Google Code)
#' @param override Override the message about how you should get code from github?
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
update_r4ss_files <- function (local = NULL, save = FALSE, revision = "newest",
                               GitHub = TRUE, override = FALSE){
  if(GitHub && revision!="newest") {
    stop("There's not yet an option to source older revisions from GitHub")
  }

  check <- function(){
    # function to produce messages if needed
    if (!override) {
      if ("package:r4ss" %in% search() &&
          as.numeric(as.character(packageVersion("r4ss"))) >= 1.22) {
        cat('r4ss has moved from Google Code to GitHub.\n',
            'Among other benefits, this allows an alternative to update_r4ss_files.\n',
            'If you install the "devtools" package, you can get updated code as\n',
            'a complete package by running the following command:\n',
            '  devtools::install_github("r4ss/r4ss")\n',
            'to override this message, use the argument "override=TRUE"\n')
      }
    }
  }
  getGitHubNames <- function() {
    # a function to get the list of file names from GitHub

    # get most recent change
    commits <- getURL("https://github.com/r4ss/r4ss/commit/master")
    commits <- strsplit(commits, split="relative-time\">", fixed=TRUE)[[1]][2]
    commits <- strsplit(commits, split="</time", fixed=TRUE)[[1]][1]
    cat("most recent change:", commits, "\n")

    # get website which contains a list of all files
    myFile <- getURL("https://github.com/r4ss/r4ss/tree/master/R",.opts = list(ssl.verifypeer = FALSE))
    changes <- readLines(textConnection(myFile))
    # find the lines that have the .R files in them
    files <- changes[grep("blob/master", changes)]
    # trim off the extra stuff to get just the .R file names
    filenames <- NULL
    N <- length(files)
    for(ifile in 1:N){
        trim.front <- strsplit(files[ifile],"title=\"",fixed=TRUE)[[1]][2]
        trim.back <- strsplit(trim.front,"\"",fixed=TRUE)[[1]][1]
        filenames <- c(filenames, trim.back)
    }
    # make sure all files end in .R
    good <- grep(".R$",toupper(filenames))
    # report any bad files that don't end in .R
    bad <- setdiff(1:N, good)
    if(length(bad)>0){
        cat("Skipping the following files that don't end in '.R':\n")
        cat(paste(" ",filenames[bad],"\n",sep=""))
    }
    return(list(filenames = filenames[good],
                webdir = "https://raw.githubusercontent.com/r4ss/r4ss/master/R"))
  }

  getwebnames <- function() {
    # older function to get the list of file names from Google Code
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
    # get files from the web using names supplied by previous function
    filenames <- fileinfo$filenames
    webdir <- fileinfo$webdir
    n <- length(filenames)
    cat(n, "files found\n")
    if (save){
      # check for presence of local directory
      if(is.null(local)) local <- getwd()
      isdir <- file.info(local)$isdir
      if (is.na(isdir) || !isdir) {
        stop("\nlocal directory needs to be created to save files into:\n  ", local,"\n")
      }
      cat("saving all files to", local, "\n")
      cat("  saving...\n   ")
    }else{
      cat("  sourcing...\n   ")
    }

    # loop over files
    for (i in 1:n) {
      webfile <- file.path(webdir, filenames[i])
      if(GitHub) {
        if (save) { # if saving to local directory from GitHub
          localfile <- paste(local, filenames[i], sep = "/")
          myFile <- getURL(webfile, .opts = list(ssl.verifypeer = FALSE))
          writeLines(myFile, localfile)
          cat(filenames[i], ",",ifelse(i==n | i%%4==0,"\n   "," "), sep = "")
        } else { # if NOT saving to local directory from GitHub
          cat(filenames[i], ",",ifelse(i==n | i%%4==0,"\n   "," "), sep = "")
          myFile <- getURL(webfile, .opts = list(ssl.verifypeer = FALSE))
          source(textConnection(myFile))
        }
      } else { # if NOT sourcing from GitHub
        if (filenames[i] == "update_r4ss_files.R") {
          webfile <- "http://r4ss.googlecode.com/svn/trunk/update_r4ss_files.R"
        }
        if (save) {  # if saving to local directory from Google
          localfile <- paste(local, filenames[i], sep = "/")
          temp <- readLines(webfile)
          writeLines(temp, localfile)
          cat(filenames[i], ",",ifelse(i==n | i%%4==0,"\n   "," "), sep = "")
        } else {  # if NOT saving to local directory from Google
          cat(filenames[i], ",",ifelse(i==n | i%%4==0,"\n   "," "), sep = "")
          source(webfile)
        }
      } # end if NOT sourcing from GitHub
      flush.console()
    }
  }
  getlocalfiles <- function(local) {
    # function to source all files ending in .R from chosen directory
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

  # now do stuff using functions defined above
  if (is.null(local)) { # if not using local folders, source from web
    check()
    if (override){
      if (GitHub) {
        fileinfo <- getGitHubNames()
      } else {
        fileinfo <- getwebnames()
      }
      #print(fileinfo)
      getwebfiles(fileinfo)
      cat("\n  r4ss update complete.\n")
    }
  } else { # if using local folders...
    if (save) { # and saving from web
      check()
      if(override){
        if (GitHub) {
          fileinfo <- getGitHubNames()
        } else {
          fileinfo <- getwebnames()
        }
        getwebfiles(fileinfo)
      }
    }
    # source from local folder (whether newly saved or not)
    getlocalfiles(local)
    cat("\n  r4ss update complete.\n")
  }
}
