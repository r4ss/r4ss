SS_splitdat <-
function(
                        inpath     = 'working_directory' ,
                        outpath    = 'working_directory' ,
                        inname     = 'data.ss_new'       ,
                        outpattern = 'BootData'          ,
                        number     = F                   ,
                        verbose    = T                   ,
                        fillblank  = T                   ,
                        MLE        = T                   ,
                        notes      = ""
                        )
{
  # this is a function to split bootstrap aggregated in the data.ss_new file
  # which is output from Stock Synthesis into individual data files.
  if(inpath=="working_directory") inpath=getwd()
  if(outpath=="working_directory") outpath=getwd()

  infile    <- paste(inpath,inname,sep='/')
  filelines <- readLines(infile)
  if(fillblank)  filelines[filelines==""] <- "#"

  string    <- '#_bootstrap file'
  starts    <- grep(string, filelines)
  ends      <- c(starts[-1]-1,length(filelines)-1)
  MLEstring <- '#_expected values with no error added'
  MLEstart  <- grep(MLEstring, filelines)
  MLEend    <- starts[1]-1

  if(!MLE){
    for(i in 1:length(starts)) {
      outfile <- paste(outpath,'/',outpattern,ifelse(number,i,''),'.ss',sep='')
      outline <- paste('# Data file created from',infile,'to',outfile)
      if(verbose) print(outline,quote=F)
      writeLines(c(outline,filelines[starts[i]:ends[i]]),outfile)
    }
  }else{
    outfile <- paste(outpath,'/',outpattern,'.ss',sep='')
    if(notes!="") notes <- paste("#C",notes) else notes <- NULL
    notes <- c(notes,paste('#C MLE data file created from',infile,'to',outfile))
    if(verbose) print(paste('MLE data file created from',infile,'to',outfile),quote=F)
    writeLines(c(notes,filelines[MLEstart:MLEend]),outfile)
  }
}

