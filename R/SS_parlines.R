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
#' @param verbose TRUE/FALSE switch for amount of detail produced by function.
#' Default=TRUE.
#' @param active Should only active parameters (those with positive phase) be
#' output? Default=FALSE.
#' @param print14 Print 14 columns or just 7 (TRUE/FALSE)
#' @author Ian Taylor
#' @seealso \code{\link{SS_changepars}}
#' @export
#' @keywords data manip
#' @examples
#' 
#' \dontrun{
#' x <- SS_parlines(ctlfile='Y:/ss/SSv3.03a/Simple/Control.SS_New')
#' head(x)
#' #       LO    HI     INIT PRIOR PR_type   SD PHASE              Label Line_num
#' # 42  0.05  0.15  0.10000  0.10       0  0.8    -3  NatM_p_1_Fem_GP_1       42
#' # 43  0.05  0.15  0.10000  0.10       0  0.8    -3  NatM_p_2_Fem_GP_1       43
#' # 44  1.00 45.00 32.28100 36.00       0 10.0     2 L_at_Amin_Fem_GP_1       44
#' # 45 40.00 90.00 71.34260 70.00       0 10.0     4 L_at_Amax_Fem_GP_1       45
#' # 46  0.05  0.25  0.15199  0.15       0  0.8     4 VonBert_K_Fem_GP_1       46
#' # 47  0.05  0.25  0.10000  0.10       0  0.8    -3  CV_young_Fem_GP_1       47
#' }
#' 
SS_parlines <-
function(
  ctlfile="C:/myfiles/mymodels/myrun/control.ss_new",
  dir=NULL, verbose=TRUE, active=FALSE, print14=FALSE)
{
  # read control file
  if(!is.null(dir)) ctlfile <- file.path(dir,'control.ss_new')
  ncols = 100 # !!this should by more dynamic--if it's too small, the function dies
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
  
  namesvec7  <- c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE", "Label", "Label2")
  namesvec14 <- c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE",
                  "env-var", "use_dev", "dev_minyr", "dev_maxyr", "dev_stddev", "Block", "Block_Fxn",
                  "Label", "Label2")

  names(parlines7 ) <- namesvec7
  names(parlines14) <- namesvec14

  if(print14){
    parlines7 <- cbind(parlines7[,1:7],matrix(NA,nrow=1,ncol=7),parlines7[,8:9])
    names(parlines7) <- namesvec14
    parlines <- rbind(parlines7,parlines14)
  }else{
    parlines <- rbind(parlines7,parlines14[,c(1:7,15:16)])
  }
  
  parlines$Label[parlines$Label=="#"] <- parlines$Label2[parlines$Label=="#"]
  parlines <- parlines[,names(parlines)!="Label2"] # dropping the Label2 column
  
  parlines$Linenum <- as.numeric(rownames(parlines))
  parlines <- parlines[order(parlines$Linenum),]
  for(i in 1:7) parlines[,i] <- as.numeric(parlines[,i])

  if(active) parlines <- parlines[parlines$PHASE > 0,]
  return(parlines)
} # end function

