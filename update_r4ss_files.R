update_r4ss_files <- function(){
  # read and parse the HTML file that will list all the file names
  getfilenames <- function(webdir){
    cat("sourcing updated functions from",webdir,"\n")
    lines <- readLines(webdir,warn=F)
    filenames <- lines[grep('"*.R"',lines)]
    for(i in 1:length(filenames)) filenames[i] <- strsplit(filenames[i],"\">")[[1]][2]   # split along: ">
    for(i in 1:length(filenames)) filenames[i] <- strsplit(filenames[i],"</a>")[[1]][1] # split along: </a>

    # getting the files
    n <- length(filenames)
    cat(n,"files found:\n")
    for(i in 1:n){
      cat("  sourcing ",filenames[i],"\n",sep="")
      source(paste(webdir,filenames[i],sep="/"))
      flush.console()
    }
  }

  getfilenames("http://r4ss.googlecode.com/svn/trunk/")
  cat("update complete.\n")
}
