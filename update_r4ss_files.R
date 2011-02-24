update_r4ss_files <- function(local=NULL){
  # read and parse the HTML file that will list all the file names
  getfilenames <- function(webdir){
    changes <- readLines('http://code.google.com/p/r4ss/source/list')
    line <- changes[grep("detail?",changes)[6]]
    cat("sourcing updated functions from",webdir,"\n")
    cat("most recent change:",strsplit(strsplit(line,">")[[1]][3],"<")[[1]][1],"\n")
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

  getlocalfiles <- function(localdir){
    filenames <- dir(localdir,pattern="*.R$")
    n <- length(filenames)
    cat(n,"files found:\n")
    for(i in 1:n){
      cat("  sourcing ",filenames[i],"\n",sep="")
      source(paste(localdir,filenames[i],sep="/"))
      flush.console()
    }
  }

  if(is.null(local)){
    getfilenames("http://r4ss.googlecode.com/svn/trunk/")
  }else{
    getlocalfiles(local)
  }
  cat("update complete.\n")
}
