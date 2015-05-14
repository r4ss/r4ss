#' Modify variance and sample size adjustments in the control file
#'
#' Function has not been fully tested yet
#'
#' @param dir Directory with control file to change.
#' @param ctlfile Control file name. Default="control.ss_new".
#' @param newctlfile Name of new control file to be written.
#' Default="control_modified.ss".
#' @param keyword Keyword to use as reference for start of section on
#' variance adjustments
#' @param newtable Optional table of new variance adjustment values
#' @param newrow Optional vector of new variance adjustment values for a particular row
#' @param rownumber Which of the 6 rows to replace with 'newrow' if present?
#' @param maxcols Maximum number of columns to search among (may need to
#' increase from default if you have a huge number of fleets)
#' @param overwrite Overwrite file if it exists?
#' @param verbose TRUE/FALSE switch for amount of detail produced by function.
#' Default=TRUE.
#' @author Ian Taylor
#' @seealso \code{\link{SS_parlines}}, \code{\link{SS_changepars}}
#' @export
#'
SS_varadjust <- function(dir="C:/myfiles/mymodels/myrun/",
                         ctlfile="control.ss_new",
                         newctlfile="control_modified.ss",
                         keyword="Variance_adjustments",
                         newtable=NULL, newrow=NULL, rownumber=NULL,
                         maxcols=100, overwrite=FALSE,
                         verbose=TRUE){
  # check for consistency of inputs
  if(!is.null(newtable)){
    if(!is.null(newrow)){
      stop("You can't input both 'newtable' and 'newrow'")
    }
    if(!is.data.frame(newtable) || nrow(newtable)!=6){
      stop("Input 'newtable' must be a data.frame with 6 rows")
    }
  }
  if(!is.null(newrow) & is.null(rownumber)){
    stop("Input 'newrow' requires the input 'rownumber' (an integer from 1 to 6)")
  }
  if(!is.null(rownumber) && !rownumber %in% 1:6){
    stop("Input 'rownumber' should be an integer specifying which of the 6 rows\n",
         "of the variance adjustment table will be replaced with 'newrow'")
  }
  # combine directory and filenames
  if(!is.null(dir)){
    ctlfile <- file.path(dir, ctlfile)
    newctlfile <- file.path(dir, newctlfile)
  }
  # read control file as a set of strings
  ctl_lines <- readLines(ctlfile)
  # find line matching keyword and complain if 0 or 2+ lines found
  keyword_line <- grep(keyword, ctl_lines)
  if(length(keyword_line)!=1){
    stop("keyword input",keyword,"found",length(keyword_line),"times.\n",
         "It should be a unique string immediately before variance adjustments.")
  }
  # read control file as a table of values
  ctl <- read.table(file=ctlfile,col.names=1:maxcols,skip=keyword_line,
                    nrows=10, fill=TRUE,
                    quote="",colClasses="character",comment.char="",
                    blank.lines.skip=FALSE)
  # save warnings settings and then turn off "NAs introduced" warning
  old_warn <- options()$warn
  options(warn=-1)
  # subset first 6 numeric rows
  numeric_rows <- which(!is.na(as.numeric(ctl[,1])))
  good_rows <- numeric_rows[1:6]
  ctl <- ctl[good_rows,]
  # loop over columns, converting to numeric, checking for non-numeric values
  nfleets <- NULL
  for(icol in 1:maxcols){
    ctl[,icol] <- as.numeric(ctl[,icol])
    # first time that an NA value appears, record that column number
    if(is.null(nfleets) && any(is.na(ctl[,icol]))){
      nfleets <- icol-1
    }
  }
  #returning to old warning value
  options(warn=old_warn)
  # subset numeric columns only
  ctl <- ctl[,1:nfleets]
  # add header
  colnames(ctl) <- paste("Fleet",1:nfleets,sep="")
  # add labels to each rows (based on labels in control.ss_new)
  ctl <- data.frame(ctl, label=c("#_add_to_survey_CV",
                             "#_add_to_discard_stddev",
                             "#_add_to_bodywt_CV",
                             "#_mult_by_lencomp_N",
                             "#_mult_by_agecomp_N",
                             "#_mult_by_size-at-age_N"))
  cat("Existing table of variance adjustments:\n")
  print(ctl)
  
  # absolute position of the rows to change
  good_rows_absolute <- keyword_line + good_rows

  # this command will hopefully prevent getting stuck with all
  # output written to the file after the function crashes before closing connection
  on.exit({if(sink.number()>0) sink()})

  # check for existence of file and warn if present and overwrite=FALSE
  if(file.exists(newctlfile)){
    if(!overwrite){
      stop("File exists and input 'overwrite'=FALSE:\n      ",newctlfile,"\n")
    }else{
      file.remove(newctlfile)
    }
  }

  # open connection to file
  if(verbose) cat("opening connection to",newctlfile,"\n")
  zz <- file(newctlfile, open="at")
  sink(zz)
  # change maximum number of columns
  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  options(width=5000,max.print=9999999)

  printdf <- function(dataframe){
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    print(dataframe, row.names=FALSE, strip.white=TRUE)
  }

  ### write file
  # stuff prior to variance adjustments
  writeLines(ctl_lines[1:(min(good_rows_absolute)+1)])
  # table of variance adjustments
  printdf(ctl)
  # stuff after variance adjustments
  writeLines(ctl_lines[(max(good_rows_absolute)+1):length(ctl_lines)])

  # return maximum number of columns
  options(width=oldwidth,max.print=oldmax.print)
  # close connection
  sink()
  close(zz)
  if(verbose){
    cat("file written to", newctlfile, "\n")
  }
  # return table of values
  return(invisible(ctl))
} # end function

