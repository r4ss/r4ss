SS_splitdat <-
  function(inpath     = 'working_directory',
           outpath    = 'working_directory',
           inname     = 'data.ss_new',
           outpattern = 'BootData',
           number     = FALSE,
           verbose    = TRUE,
           fillblank  = TRUE,
           MLE        = TRUE,
           inputs     = FALSE,
           notes      = ""
           )
{
  # this is a function to split bootstrap aggregated in the data.ss_new file
  # which is output from Stock Synthesis into individual data files.
  if(MLE & inputs) stop("can't have both 'MLE' and 'inputs' = TRUE")
  
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

  if(MLE & length(MLEstart)==0) stop("no MLE values in ",inname,"\n  change 'N bootstrap datafiles' in starter.ss to 2 or greater")
  inputstring <- '#_observed data'
  inputstart  <- grep(inputstring, filelines)
  if(length(MLEstart)==0) inputend <- length(filelines) else inputend <- MLEstart-1
  if(length(inputstart)==0) stop("no values in ",inname,"\n  change 'N bootstrap datafiles' in starter.ss to 1 or greater")

  
  if(!MLE & !inputs){
    if(length(starts)==0) stop("no bootstrap values in ",inname,"\n  change 'N bootstrap datafiles' in starter.ss to 3 or greater")
    for(i in 1:length(starts)) {
      outfile <- paste(outpath,'/',outpattern,ifelse(number,i,''),'.ss',sep='')
      outline <- paste('# Data file created from',infile,'to',outfile)
      if(verbose) cat(outline,"\n")
      writeLines(c(outline,filelines[starts[i]:ends[i]]),outfile)
    }
  }else{
    if(MLE){
      outfile <- paste(outpath,'/',outpattern,'.ss',sep='')
      if(notes!="") notes <- paste("#C",notes) else notes <- NULL
      notes <- c(notes,paste('#C MLE data file created from',infile,'to',outfile))
      if(verbose) cat('MLE data file created from',infile,'to',outfile,"\n")
      writeLines(c(notes,filelines[MLEstart:MLEend]),outfile)
    }
    if(inputs){
      outfile <- paste(outpath,'/',outpattern,'.ss',sep='')
      if(notes!="") notes <- paste("#C",notes) else notes <- NULL
      notes <- c(notes,paste('#C data file created from',infile,'to',outfile))
      if(verbose) cat('file with copies of input data created from',infile,'to',outfile,"\n")
      writeLines(c(notes,filelines[inputstart:inputend]),outfile)
    }
  }
}

