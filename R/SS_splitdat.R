#' Split apart bootstrap data to make input file.
#' 
#' A function to split apart bootstrap data files created in data.ss_new.  To
#' get bootstraps, the input "N bootstrap file to produce" in starter.ss needs
#' to be 3 or greater.
#' 
#' 
#' @param inpath Directory containing the input file. By default the working
#' directory given by getwd() is used. Default="working_directory".
#' @param outpath Directory into which the output file will be written.
#' Default="working_directory".
#' @param inname File name of input data file to be split.
#' Default="Data.SS_New".
#' @param outpattern File name of output data file. Default="BootData".
#' @param number Append bootstrap number to the file name chosen in
#' \code{outpattern}? Default=F.
#' @param verbose Provide richer command line info of function progress?
#' Default=TRUE.
#' @param fillblank Replace blank lines with "#". Helps with running on linux.
#' Default=TRUE.
#' @param MLE Grab the maximum likelihood values from the second block in
#' Data.SS_New (instead of bootstrap values or copies of inputs)? Default=TRUE.
#' @param inputs Grab the copy of the input values values from the first block
#' in Data.SS_New (instead of MLE or bootstrap values)? Default=F.
#' @param notes Notes to the top of the new file (comment indicator "#C" will
#' be added). Default="".
#' @author Ian Taylor
#' @export
#' @keywords data manip
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

