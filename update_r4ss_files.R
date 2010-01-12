update_r4ss_files <- function(){
  # read and parse the HTML file that will list all the file names
  webdir <- "http://r4ss.googlecode.com/svn/trunk/"
  print(paste("sourcing updated functions from",webdir),quote=F)
  lines <- readLines(webdir,warn=F)
  filenames <- lines[grep('"*.R"',lines)]
  for(i in 1:length(filenames)) filenames[i] <- strsplit(filenames[i],"\">")[[1]][2]   # split along: \">
  for(i in 1:length(filenames)) filenames[i] <- strsplit(filenames[i],"</a>")[[1]][1] # split along: </a>

  # getting the files
  for(i in 1:length(filenames)){
    print(paste("sourcing",filenames[i]),quote=F)
    source(paste(webdir,filenames[i],sep=""))
  }
  print("update complete.",quote=F)
}
