SS_parlines <-
function(
  ctlfile="C:/myfiles/mymodels/myrun/control.ss_new",
  dir=NULL, verbose=T, active=F)
{

################################################################################
#
# SS_parlines October 5, 2009.
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: To identify the line numbers and parameter labels in a Stock Synthesis control file
# Written: Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
# Returns: Table of line numbers and parameter labels
# Notes:   See users guide for documentation: http://code.google.com/p/r4ss/wiki/
# Required packages: none
#
################################################################################

  # read control file
  if(!is.null(dir)) ctlfile <- paste(dir,'control.ss_new',sep='/')
  ncols = 100 # !!this should by more dynamic--if it's too small, the function dies
  ctl <- read.table(file=ctlfile,col.names=1:ncols,fill=T,
    quote="",colClasses="character",comment.char="", blank.lines.skip=F)

  nrows <- nrow(ctl)
  #print(nrows)
  ctl_num <- matrix(NA,nrows,ncols) # copy of ctl converted to numerical values or NA
  num_cnt <- rep(NA,nrows)          # count of number of numerical values in each row
  num_cnt7 <- rep(NA,nrows)         # count of number of numerical values in first 7 values of each row
  num_cnt14 <- rep(NA,nrows)        # count of number of numerical values in first 14 values of each row
  options(warn = -1)                # temporarily turn off "Warning: NAs introduced by coercion"
  for(irow in 1:nrows){
    ctl_num[irow,] <- as.numeric(ctl[irow,])
    num_cnt[irow] <- sum(!is.na(ctl_num[irow,]))
    num_cnt7[irow] <- sum(!is.na(ctl_num[irow,1:7]))
    num_cnt14[irow] <- sum(!is.na(ctl_num[irow,1:14]))
  }
  options(warn = 1)                 # turn warnings back on
  parlines7  <- ctl[num_cnt7==7 & is.na(ctl_num[,8]), ]
  parlines14 <- ctl[num_cnt14==14 & is.na(ctl_num[,15]), ]

  parlines7  <- parlines7[,c(1:7,8:9)]
  parlines14 <- parlines14[,c(1:7,15:16)]
  
  namesvec <- c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE", "Label", "Label2")

  names(parlines7 ) <- namesvec
  names(parlines14) <- namesvec

  parlines <- rbind(parlines7,parlines14)
  parlines$Label[parlines$Label=="#"] <- parlines$Label2[parlines$Label=="#"]
  parlines <- parlines[,1:8] # dropping the Label2 column
  
  parlines$Linenum <- as.numeric(rownames(parlines))
  parlines <- parlines[order(parlines$Linenum),]
  for(i in 1:7) parlines[,i] <- as.numeric(parlines[,i])

  if(active) parlines <- parlines[parlines$PHASE > 0,]
  return(parlines)
} # end function

