update_r4ss_files <- function (local = NULL, save = FALSE,
                               revision = "newest",env="updated_r4ss_files",pos=2){

  #if env is an environment simply wirte to that. If not, create a new environment
  if(!is.environment(env)) {  
    if(!is.character(env))
      stop("'env' must be an environment or a character vector.",
           "Use default if not sure to assign objects into global workspace\n")
    if(is.character(env)) {
      cat('Creating environment "',env,'", to store updated r4ss files.\n',sep="")
      cat('To put the files in the global environment, set "env=.GlobalEnv"\n')
      env <- attach(NULL,pos=pos,name=env)
    }
  }
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
    if (save) 
      cat("saving all files to", local, "\n")
    for (i in 1:n) {
      webfile <- paste(webdir, filenames[i], sep = "/")
      if (filenames[i] == "update_r4ss_files.R") 
        webfile <- "http://r4ss.googlecode.com/svn/trunk/update_r4ss_files.R"
      if (save) {
        localfile <- paste(local, filenames[i], sep = "/")
        temp <- readLines(webfile)
        writeLines(temp, localfile)
        cat("  writing ", localfile, "\n", sep = "")
      }
      else {
        cat("  sourcing ", filenames[i], "\n", sep = "")
        source(webfile,local=env)
      }
      flush.console()
    }
  }
  getlocalfiles <- function(local) {
    filenames <- dir(local, pattern = "*.R$")
    n <- length(filenames)
    cat(n, "files found in", local, "\n")
    for (i in 1:n) {
      cat("  sourcing ", filenames[i], "\n", sep = "")
      source(paste(local, filenames[i], sep = "/"),local=env)
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
  cat("update complete.\n")
}
