#' Get parameter lines from Stock Synthesis control file
#'
#' A simple function which takes as input the full path and filename of a
#' control file for input to Stock Synthesis. Ideally, a Control.SS_New file
#' will be used, so that it represents what SS thinks the inputs are, and not
#' what the user thinks the inputs are.
#'
#' It returns a table which should contain one line for each parameter in the
#' model. Currently, only the first 7 values are returned, because all
#' parameters have those values. In the future, extended parameter lines could
#' be returned.
#'
#' Parameter lines are identified as those which have 7 or 14 numeric elements
#' followed by a non-numeric element. It's possible that this system could
#' break down under certain circumstances
#'
#'
#' @param ctlfile File name of control file including path.
#' @param dir Alternative input of path, where file is assumed to be
#' "control.ss_new". Default=NULL.
#' @param version SS version number. Currently only "3.24" or "3.30" are supported,
#' either as character or numeric values (noting that numeric 3.30  = 3.3).
#' @param verbose TRUE/FALSE switch for amount of detail produced by function.
#' Default=TRUE.
#' @param active Should only active parameters (those with positive phase) be
#' output? Default=FALSE.
#' @author Ian Taylor
#' @seealso \code{\link{SS_changepars}, \code{\link{SS_readctl}},
#' \code{\link{SS_readctl_3.24}}}
#' @export
#' @examples
#'
#' \dontrun{
#' parlines <- SS_parlines(ctlfile='c:/ss/Simple/Control.SS_New')
#' head(parlines)
#' #       LO    HI     INIT PRIOR PR_type   SD PHASE              Label Line_num
#' # 42  0.05  0.15  0.10000  0.10       0  0.8    -3  NatM_p_1_Fem_GP_1       42
#' # 43  0.05  0.15  0.10000  0.10       0  0.8    -3  NatM_p_2_Fem_GP_1       43
#' # 44  1.00 45.00 32.28100 36.00       0 10.0     2 L_at_Amin_Fem_GP_1       44
#' # 45 40.00 90.00 71.34260 70.00       0 10.0     4 L_at_Amax_Fem_GP_1       45
#' # 46  0.05  0.25  0.15199  0.15       0  0.8     4 VonBert_K_Fem_GP_1       46
#' # 47  0.05  0.25  0.10000  0.10       0  0.8    -3  CV_young_Fem_GP_1       47
#' }
#'
SS_parlines <- function(ctlfile="control.ss_new", dir=NULL,
                        version="3.24", verbose=TRUE, active=FALSE){

  # function to read parameter lines in Stock Synthesis control files
  if(!(version=="3.24" | version=="3.30" | version==3.3)){
    # turns out 3.30 != "3.30" in R
    stop('version must be either 3.24 or 3.30')
  }

  # read control file
  if(!is.null(dir)) ctlfile <- file.path(dir,'control.ss_new')
  ncols = 150 # !!this should by more dynamic--if it's too small, the function dies
  ctl <- read.table(file=ctlfile,col.names=1:ncols,fill=TRUE,
                    quote="",colClasses="character",comment.char="", blank.lines.skip=FALSE)

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

  parlines7   <- parlines7[, 1:9]
  parlines14  <- parlines14[,1:16]

  # get column names
  if(version=="3.24"){
    namesvec7  <- c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE",
                    "Label", "Label2")
    namesvec14 <- c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE",
                    "env-var", "use_dev", "dev_minyr", "dev_maxyr", "dev_stddev", "Block", "Block_Fxn",
                    "Label", "Label2")
    names(parlines7 ) <- namesvec7
  }
  if(version=="3.30"){
    namesvec7 <- c("LO", "HI", "INIT", "PRIOR", "PR_SD", "PR_type", "PHASE",
                   "Label", "Label2")
    namesvec14 <- c("LO", "HI", "INIT", "PRIOR", "PR_SD", "PR_type", "PHASE",
                    "env-var", "use_dev", "dev_mnyr", "dev_mxyr", "dev_PH", "Block", "Blk_Fxn",
                    "Label", "Label2")
  }
  names(parlines14) <- namesvec14

  # combine 7 and 14 column lines
  parlines <- parlines14
  # check for presence of short parameter lines
  if(nrow(parlines7) > 0){
    # add filler columns to short parameter lines
    parlines7 <- cbind(parlines7[,1:7],matrix(NA,nrow=1,ncol=7),parlines7[,8:9])
    names(parlines7) <- namesvec14
    parlines <- rbind(parlines7,parlines14)
  }

  # sort out labels
  parlines$Label[parlines$Label=="#"] <- parlines$Label2[parlines$Label=="#"]
  parlines <- parlines[,names(parlines)!="Label2"] # dropping the Label2 column

  # make line number numeric
  parlines$Linenum <- as.numeric(rownames(parlines))
  # sort by order found in control file
  parlines <- parlines[order(parlines$Linenum),]

  # make other values numeric
  for(i in 1:(ncol(parlines)-2)){
    parlines[,i] <- as.numeric(parlines[,i])
  }
  # filter for active parameters only
  if(active){
    parlines <- parlines[parlines$PHASE > 0,]
  }
  return(parlines)
} # end function

