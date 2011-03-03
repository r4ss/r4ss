update_r4ss_files <- function(local=NULL,save=FALSE){
  # read and parse the HTML file that will list all the file names
  webdir <- "http://r4ss.googlecode.com/svn/trunk/"

  getwebnames <- function(){
    changes <- readLines('http://code.google.com/p/r4ss/source/list')
    line <- changes[grep("detail?",changes)[6]]
    cat("getting file names from",webdir,"\n")
    cat("most recent change:",strsplit(strsplit(line,">")[[1]][3],"<")[[1]][1],"\n")

    lines <- readLines(webdir,warn=F)
    filenames <- lines[grep('"*.R"',lines)]
    for(i in 1:length(filenames)) filenames[i] <- strsplit(filenames[i],"\">")[[1]][2]   # split along: ">
    for(i in 1:length(filenames)) filenames[i] <- strsplit(filenames[i],"</a>")[[1]][1] # split along: </a>
    return(filenames)
  }

  getwebfiles <- function(filenames){
    # getting the files
    n <- length(filenames)
    cat(n,"files found in",webdir,"\n")
    if(save) cat("saving all files to",local,"\n")
    for(i in 1:n){
      webfile <- paste(webdir,filenames[i],sep="/")
      if(save){
        localfile <- paste(local,filenames[i],sep="/")
        temp <- readLines(webfile)
        writeLines(temp,localfile)
        cat("  writing ",localfile,"\n",sep="")
      }else{
        cat("  sourcing ",filenames[i],"\n",sep="")
        source(webfile)
      }
      flush.console()
    }
  }

  getlocalfiles <- function(local){
    filenames <- dir(local,pattern="*.R$")
    n <- length(filenames)
    cat(n,"files found in",local,"\n")
    for(i in 1:n){
      cat("  sourcing ",filenames[i],"\n",sep="")
      source(paste(local,filenames[i],sep="/"))
      flush.console()
    }
  }

  if(is.null(local)){
    filenames <- getwebnames()
    getwebfiles(filenames)
  }else{
    if(save){
      filenames <- getwebnames()
      getwebfiles(filenames)
    }
    getlocalfiles(local)
  }
  cat("update complete.\n")
}

