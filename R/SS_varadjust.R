#' Modify variance and sample size adjustments in the control file
#' 
#' Incomplete function in process of development.
#' 
#' @param dir Directory with control file to change.
#' @param ctlfile Control file name. Default="control.ss_new".
#' @param newctlfile Name of new control file to be written.
#' Default="control_modified.ss".
#' @param keyword Keyword to use as reference for start of section on
#' variance adjustments
#' @param maxcols Maximum number of columns to search among (may need to
#' increase from default if you have a huge number of fleets)
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
                         maxcols=50, 
                         verbose=TRUE)
{
  # read control file as a table of values
  if(!is.null(dir)) ctlfile <- file.path(dir,'control.ss_new')
  ctl_lines <- readLines(ctlfile)
  keyword_line <- grep(keyword, ctl_lines)
  if(length(keyword_line)!=1){
    stop("keyword input",keyword,"found",length(keyword_line),"times.\n",
         "It should be a unique string immediately before variance adjustments.")
  }
  ctl <- read.table(file=ctlfile,col.names=1:maxcols,skip=keyword_line,
                    fill=TRUE,
                    quote="",colClasses="character",comment.char="",
                    blank.lines.skip=FALSE)
  numeric_rows <- which(!is.na(as.numeric(ctl[,1])))
  # subset first 6 numeric rows
  ctl <- ctl[numeric_rows[1:6],]
  # loop over columns, converting to numeric, checking for non-numeric values
  nfleets <- NULL
  for(icol in 1:maxcols){
    ctl[,icol] <- as.numeric(ctl[,icol])
    # first time that an NA value appears, record that column number
    if(is.null(nfleets) && any(is.na(ctl[,icol]))){
      nfleets <- icol-1
    }
  }
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
  return(ctl)
  
  ## ctl_num <- matrix(NA,nrows,ncols) # copy of ctl converted to numerical values or NA
  ## num_cnt <- rep(NA,nrows)          # count of number of numerical values in each row
  ## num_cnt7 <- rep(NA,nrows)         # count of number of numerical values in first 7 values of each row
  ## num_cnt14 <- rep(NA,nrows)        # count of number of numerical values in first 14 values of each row
  ## options(warn = -1)                # temporarily turn off "Warning: NAs introduced by coercion"
  ## for(irow in 1:nrows){
  ##   ctl_num[irow,] <- as.numeric(ctl[irow,])
  ##   num_cnt[irow] <- sum(!is.na(ctl_num[irow,]))
  ##   num_cnt7[irow] <- sum(!is.na(ctl_num[irow,1:7]))
  ##   num_cnt14[irow] <- sum(!is.na(ctl_num[irow,1:14]))
  ## }
  ## options(warn = 1)                 # turn warnings back on
  ## parlines7  <- ctl[num_cnt7==7 & is.na(ctl_num[,8]), ]
  ## parlines14 <- ctl[num_cnt14==14 & is.na(ctl_num[,15]), ]

  ## parlines7   <- parlines7[, 1:9]
  ## parlines14  <- parlines14[,1:16]
  
  ## namesvec7  <- c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE", "Label", "Label2")
  ## namesvec14 <- c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE",
  ##                 "env-var", "use_dev", "dev_minyr", "dev_maxyr", "dev_stddev", "Block", "Block_Fxn",
  ##                 "Label", "Label2")

  ## names(parlines7 ) <- namesvec7
  ## names(parlines14) <- namesvec14

  ## if(print14){
  ##   parlines7 <- cbind(parlines7[,1:7],matrix(NA,nrow=1,ncol=7),parlines7[,8:9])
  ##   names(parlines7) <- namesvec14
  ##   parlines <- rbind(parlines7,parlines14)
  ## }else{
  ##   parlines <- rbind(parlines7,parlines14[,c(1:7,15:16)])
  ## }
  
  ## parlines$Label[parlines$Label=="#"] <- parlines$Label2[parlines$Label=="#"]
  ## parlines <- parlines[,names(parlines)!="Label2"] # dropping the Label2 column
  
  ## parlines$Linenum <- as.numeric(rownames(parlines))
  ## parlines <- parlines[order(parlines$Linenum),]
  ## for(i in 1:7) parlines[,i] <- as.numeric(parlines[,i])

  ## if(active) parlines <- parlines[parlines$PHASE > 0,]
  ## return(parlines)
} # end function

