SS_parlines <- function(
  ctlfile="C:\\myfiles\\mymodels\\myrun\\Control.SS_New",
  verbose=T)
{

################################################################################
#
# SS_parlines January 25, 2009.
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
  ncols = 30
  ctl <- read.table(file=ctlfile,col.names=1:ncols,fill=T,
    quote="",colClasses="character",comment.char="", blank.lines.skip=F)
  nrows <- nrow(ctl)
  ctl_num <- matrix(NA,nrows,ncols) # copy of ctl converted to numerical values or NA
  num_cnt <- rep(NA,nrows)          # count of number of numerical values in each row
  options(warn = -1)                # temporarily turn off "Warning: NAs introduced by coercion"
  for(irow in 1:nrows){
    ctl_num[irow,] <- as.numeric(ctl[irow,])
    num_cnt[irow] <- sum(!is.na(ctl_num[irow,]))
  }
  options(warn = 1)                 # turn warnings back on
  badlines <- c("#_Cond","#Cond")
  ctl_pars7  <- ctl[num_cnt==7  & !(ctl[,1] %in% badlines) & ctl[,7]!="#",]
  ctl_pars14 <- ctl[num_cnt==14 & !(ctl[,1] %in% badlines) & ctl[,7]!="#",]

  ctl_pars7  <- ctl_pars7[,c(1:7,9)]
  ctl_pars14 <- ctl_pars14[,c(1:7,16)]
  namesvec <- c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE", "Label")

  names(ctl_pars7 ) <- namesvec
  names(ctl_pars14) <- namesvec

  ctl_pars <- rbind(ctl_pars7,ctl_pars14)
  ctl_pars$Line_num <- as.numeric(rownames(ctl_pars))
  ctl_pars <- ctl_pars[order(ctl_pars$Line_num),]
  for(i in 1:7) ctl_pars[,i] <- as.numeric(ctl_pars[,i])

  return(ctl_pars)
} # end function


############################################################
############################################################

Plot_Prior <- function(
  ctlfile='c:/path/controlfilename.SS',
  activeonly=T,nrows='default',ncols='default',
  maxrows=4, maxcols=4)
{
  ################################################################################
  #
  # Plot_Prior
  # February 9, 2009.
  # This function comes with no warranty or guarantee of accuracy
  #
  # Purpose: To make a multi-figure plot of prior distributions
  #          from a Stock Synthesis control file
  # Written: Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
  # Returns: Plots of prior distributions used in Stock Synthesis model
  # Notes:   requires SS_profile
  # Required packages: none
  #
  ################################################################################

  # define subfunctions
  Get_Prior <- function(T,Pmin,Pmax,Pr,Psd,Pval)
  {
    # function to calculate prior values is direct translation of code in SSv3
    Pconst=0.0001
    if(T==-1) # no prior
    {
      Prior_Like = rep(0.,length(Pval));
    }
    if(T==0) # normal
    {
      Prior_Like = 0.5*((Pval-Pr)/Psd)^2;
    }
    if(T==1)  # symmetric beta    value of Psd must be >0.0
    {
      mu=-(Psd*(log( (Pmax+Pmin)*0.5 - Pmin))) - (Psd*(log(0.5)));
      Prior_Like = -(mu+ (Psd*(log(Pval-Pmin+Pconst)))+(Psd*(log(1.-((Pval-Pmin-Pconst)/(Pmax-Pmin))))));
    }
    if(T==2)  # CASAL's Beta;  check to be sure that Aprior and Bprior are OK before running SS2!
    {
      mu=(Pr-Pmin) / (Pmax-Pmin);  # CASAL's v
      tau=(Pr-Pmin)*(Pmax-Pr)/(Psd^2)-1.0;
      Bprior=tau*mu;  Aprior=tau*(1-mu);  # CASAL's m and n
      if(Bprior<=1.0 | Aprior <=1.0) {print(" bad Beta prior ");}
      Prior_Like =  (1.0-Bprior)*log(Pconst+Pval-Pmin) + (1.0-Aprior)*log(Pconst+Pmax-Pval)
      -(1.0-Bprior)*log(Pconst+Pr-Pmin) - (1.0-Aprior)*log(Pconst+Pmax-Pr);
    }
    return(Prior_Like)
  }

  PlotPrior <- function(T,Pmin,Pmax,Pr,Psd,main="")
  {
   x <- seq(Pmin,Pmax,length=200)
    negL_prior <- Get_Prior(T=T,Pmin=Pmin,Pmax=Pmax,Pr=Pr,Psd=Psd,Pval=x)
    prior <- exp(-1*negL_prior)
    plot(x,prior,type='l',lwd=3,ylim=c(0,1.1*max(prior)),xaxs='i',yaxs='i',
      xlab='',ylab='',main=main)
  }

  parlines <- SS_parlines(ctlfile=ctlfile)
  if(activeonly) parlines <- parlines[parlines$PHASE > 0,]

  npanels <- nrow(parlines)
  if(nrows=='default') nrows <- min(ceiling(sqrt(npanels)), maxrows)
  if(ncols=='default') ncols <- min(ceiling(npanels/nrows), maxcols)

  if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
  windows(record=T)

  par(mfcol=c(nrows,ncols),mar=c(2,2,4,1),oma=c(2,2,0,0))

  npars <- nrow(parlines)
  for(ipar in 1:npars)
  {
    irow <- parlines[ipar,]
    PlotPrior(T=irow$PR_type,Pmin=irow$LO,Pmax=irow$HI,Pr=irow$PRIOR,Psd=irow$SD,main=irow$Label)
    abline(v=irow$INIT,col=2,lwd=3)
  }
  mtext('Parameter value',side=1,line=0.5,outer=T)
  mtext('Prior density',side=2,line=0.5,outer=T)
}
