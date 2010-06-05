update_r4ss_files <- function(trunk=T,branches=F){
  # read and parse the HTML file that will list all the file names
  getfilenames <- function(webdir){
    print(paste("sourcing updated functions from",webdir),quote=F)
    lines <- readLines(webdir,warn=F)
    filenames <- lines[grep('"*.R"',lines)]
    for(i in 1:length(filenames)) filenames[i] <- strsplit(filenames[i],"\">")[[1]][2]   # split along: ">
    for(i in 1:length(filenames)) filenames[i] <- strsplit(filenames[i],"</a>")[[1]][1] # split along: </a>

    # getting the files
    for(i in 1:length(filenames)){
      print(paste("sourcing",filenames[i]),quote=F)
      source(paste(webdir,filenames[i],sep="/"))
    }
  }
  if(trunk){
      getfilenames("http://r4ss.googlecode.com/svn/trunk/")
      getfilenames("http://r4ss.googlecode.com/svn/trunk/individual_plot_functions/")
  }
  if(branches){
      getfilenames("http://r4ss.googlecode.com/svn/branches/input_file_objects")
      getfilenames("http://r4ss.googlecode.com/svn/branches/individual_plot_functions")
      getfilenames("http://r4ss.googlecode.com/svn/branches/simulations")
  }

  print("update complete.",quote=F)
}
