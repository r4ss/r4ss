# this file contains several functions, some of which depend on each-other.
# SS_parlines:   identify the line numbers and parameter labels in a Stock Synthesis control file
# SS_PlotPriors: make a multi-figure plot of prior distributions from a Stock Synthesis control file
# SS_splitdat:   split bootstrap files aggregated in the Data.SS_New file
# SS_profile:    change one or more parameter values in the Control file for SSv3
# SS_recdevs:    add newly generated stochastic recruitment deviation inputs to the Control file for SSv3
SS_parlines <- function(
  ctlfile="C:\\myfiles\\mymodels\\myrun\\Control.SS_New",
  verbose=T, active=F)
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

  parlines7  <- parlines7[,c(1:7,9)]
  parlines14 <- parlines14[,c(1:7,16)]
  namesvec <- c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE", "Label")

  names(parlines7 ) <- namesvec
  names(parlines14) <- namesvec

  parlines <- rbind(parlines7,parlines14)
  parlines$Line_num <- as.numeric(rownames(parlines))
  parlines <- parlines[order(parlines$Line_num),]
  for(i in 1:7) parlines[,i] <- as.numeric(parlines[,i])

  if(active) parlines <- parlines[parlines$PHASE > 0,]
  return(parlines)
} # end function


############################################################
############### NEXT FUNCTION ##############################
############################################################

SS_PlotPriors <- function(
  ctlfile='c:/path/controlfilename.SS',read=T,
  activeonly=T,nrows='default',ncols='default',
  maxrows=4,maxcols=4,new=T,returntable=T,
  rownum=c(),strings=c(),oneline=c())
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
  GetPrior <- function(T,Pmin,Pmax,Pr,Psd,Pval)
  {
    # function to calculate prior values is direct translation of code in SSv3
    Pconst <- 0.0001
    if(T==-1) # no prior
    {
      Prior_Like <- rep(0.,length(Pval));
    }
    if(T==0) # normal
    {
      Prior_Like <- 0.5*((Pval-Pr)/Psd)^2;
    }
    if(T==1)  # symmetric beta    value of Psd must be >0.0
    {
      mu <- -(Psd*(log( (Pmax+Pmin)*0.5 - Pmin))) - (Psd*(log(0.5)));
      Prior_Like <- -(mu+ (Psd*(log(Pval-Pmin+Pconst)))+(Psd*(log(1.-((Pval-Pmin-Pconst)/(Pmax-Pmin))))));
    }
    if(T==2)  # CASAL's Beta;  check to be sure that Aprior and Bprior are OK before running SS2!
    {
      mu <- (Pr-Pmin) / (Pmax-Pmin);  # CASAL's v
      tau <- (Pr-Pmin)*(Pmax-Pr)/(Psd^2)-1.0;
      Bprior <- tau*mu;  Aprior <- tau*(1-mu);  # CASAL's m and n
      if(Bprior<=1.0 | Aprior <=1.0) {print(" bad Beta prior ");}
      Prior_Like <- (1.0-Bprior)*log(Pconst+Pval-Pmin) + (1.0-Aprior)*log(Pconst+Pmax-Pval)
      -(1.0-Bprior)*log(Pconst+Pr-Pmin) - (1.0-Aprior)*log(Pconst+Pmax-Pr);
    }
    return(Prior_Like)
  } # end GetPrior

  MakePlot <- function(T,Pmin,Pmax,Pr,Psd,main="")
    {
      x <- seq(Pmin,Pmax,length=200)
      negL_prior <- GetPrior(T=T,Pmin=Pmin,Pmax=Pmax,Pr=Pr,Psd=Psd,Pval=x)
      prior <- exp(-1*negL_prior)
      plot(x,prior,type='l',lwd=3,ylim=c(0,1.1*max(prior)),xaxs='i',yaxs='i',
           xlab='',ylab='',main=main)
    } # end MakePlot

  ## get parameter lines
  if(read==T & is.null(oneline)){
    parlines <- SS_parlines(ctlfile=ctlfile)

    ## subset as requested
    if(!is.null(strings)){
      goodlines <- NULL
      for(i in 1:length(strings))
        goodlines <- c(goodlines,grep(strings[i],parlines$Label))
      goodlines <- sort(unique(goodlines))
      parlines <- parlines[goodlines,]
    }
    if(!is.null(rownum)) parlines <- parlines[parlines$Line_num %in% as.character(rownum),]
    if(activeonly) parlines <- parlines[parlines$PHASE > 0,]
  }else{
    if(length(oneline)%in%c(7,8))
      {
        parlines <- as.data.frame(matrix(as.numeric(oneline[1:7]),1,7))
        parlines$Label <- ifelse(length(oneline)==7, "", oneline[8])
      }else{
        return("input 'oneline' needs to have 7 number + (optionally) one label string")
      }
    names(parlines) <- c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE", "Label")
  }
  npars <- nrow(parlines)

  if(nrows=='default') nrows <- min(ceiling(sqrt(npars)), maxrows)
  if(ncols=='default') ncols <- min(ceiling(npars/nrows), maxcols)

  ## make plot
  if(new)
  {
    if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
    windows(record=T)
  }

  par(mfcol=c(nrows,ncols),mar=c(2,2,4,1),oma=c(2,2,0,0))
  for(ipar in 1:npars)
  {
    irow <- parlines[ipar,]
    MakePlot(T=irow$PR_type,Pmin=irow$LO,Pmax=irow$HI,Pr=irow$PRIOR,Psd=irow$SD,main=irow$Label)
    abline(v=irow$INIT,col=2,lwd=3)
  }
  mtext('Parameter value',side=1,line=0.5,outer=T)
  mtext('Prior density',side=2,line=0.5,outer=T)

  if(returntable) return(parlines)
}


############################################################
############### NEXT FUNCTION ##############################
############################################################

SS_splitdat <- function(
                        inpath     = 'working_directory' ,
                        outpath    = 'working_directory' ,
                        inname     = 'Data.SS_New'       ,
                        outpattern = 'BootData'          ,
                        number     = F                   ,
                        verbose    = T                   ,
                        fillblank  = T                   ,
                        MLE        = T                   ,
                        notes      = ""
                        )
{
  # this is a function to split bootstrap aggregated in the Data.SS_New file
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
      outfile <- paste(outpath,'/',outpattern,ifelse(number,i,''),'.SS',sep='')
      outline <- paste('# Data file created from',infile,'to',outfile)
      if(verbose) print(outline,quote=F)
      writeLines(c(outline,filelines[starts[i]:ends[i]]),outfile)
    }
  }else{
    outfile <- paste(outpath,'/',outpattern,'.SS',sep='')
    if(notes!="") notes <- paste("#C",notes) else notes <- NULL
    notes <- c(notes,paste('#C MLE data file created from',infile,'to',outfile))
    if(verbose) print(paste('MLE data file created from',infile,'to',outfile),quote=F)
    writeLines(c(notes,filelines[MLEstart:MLEend]),outfile)
  }
}

############################################################
############### NEXT FUNCTION ##############################
############################################################

SS_profile <- function(
         dir="C:\\myfiles\\mymodels\\myrun\\",
         ctlfile="Control.SS_New",
         newctlfile="Control_Modified.SS",
         linenums=NULL, newvals=NULL, estimate=F,
         verbose=T
         )
{
################################################################################
#
# SS_profile November 21, 2008.
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: To change one or more parameter values in the Control file for SSv3
# Written: Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
# Returns: writes a new control file and returns a table of the changes made
# Notes:   See users guide for documentation: http://code.google.com/p/r4ss/wiki/
# Required packages: none
#
################################################################################

  # read control file
  ctl = readLines(paste(dir,ctlfile,sep="/"))
  ctlsubset <- ctl[linenums]

  # define objects to store changes
  newctlsubset <- NULL
  cmntvec <- NULL
  nvals <- length(linenums)
  oldvals <- oldphase <- newphase <- rep(NA,nvals)

  # check inputs
  if(length(newvals)!=nvals) return("'linenums' and 'newvals' should have the same number of elements")
  if(!(length(estimate) %in% c(1,nvals))) return("'estimate' should have 1 element or same number as 'linenums'")
  if(length(estimate)==1) estimate <- rep(estimate, nvals)

  # loop over line numbers to replace parameter values
  for(i in 1:nvals)
  {
    splitline <- strsplit(ctlsubset[i], "#")[[1]]
    cmnt <- paste("#",paste(splitline[-1],collapse="#"),sep='')
    cmntvec <- c(cmntvec, cmnt)
    vecstrings <- strsplit(splitline[1]," +")[[1]]
    vec <- as.numeric(vecstrings[vecstrings!=""])
    if(max(is.na(vec))==1) return(paste("There's a problem with a non-numeric value in line",linenums[i]))
    oldvals[i] <- vec[3]
    vec[3] <- newvals[i]
    oldphase[i] <- as.numeric(vec[7])
    if(estimate[i]){
      vec[7] <- abs(oldphase[i])
    }else{
      vec[7] <- -abs(oldphase[i])
    }
    newphase[i] <- vec[7]
    newline <- paste("",paste(vec, collapse=" "), cmnt)
    newctlsubset <- rbind(newctlsubset, newline)
  }
  # write new file
  newctl <- ctl
  newctl[linenums] <- newctlsubset
  writeLines(newctl, paste(dir,newctlfile,sep="/"))
  if(verbose) print(paste('wrote new file to',newctlfile))
  # output table of changes
  if(verbose) return(data.frame(oldvals, newvals, oldphase, newphase, comment=cmntvec))

} # end function


############################################################
############### NEXT FUNCTION ##############################
############################################################

# source('C:/SS/R/modifying_inputs/SS_recdevs.R')

SS_recdevs <- function(
         fyr=NA, lyr=NA, ctl=NULL, recdevs=NULL,
         dir="working_directory",
         ctlfile="Control.SS_New",
         newctlfile="Control_Modified.SS",
         verbose=T, writectl=T, returnctl=F,
         newmaxbias=NULL
         )
{

################################################################################
#
# SS_recdevs November 21, 2008.
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: Add newly generated stochastic recruitment deviation inputs to the Control file for SSv3
# Written: Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
# Returns: writes a new control file and/or returns a character vector of all lines of the control file
# Notes:   See users guide for documentation: http://code.google.com/p/r4ss/wiki/
# Required packages: none
#
################################################################################
  current_wd <- getwd()
  if(dir!="working_directory") setwd(dir)

  # define a general function for reading values from control file
  readfun <- function(string, maxlen=Inf)
  {
    line1 <- grep(string,ctl)
    if(length(line1)<1) stop("no line contains the phrase, '",string,"'",sep="")
    if(length(line1)>1) stop("more than one line contains the phrase, '",string,"'",sep="")

    splitline <- strsplit(ctl[line1], "#")[[1]]
    vecstrings <- strsplit(splitline[1]," +")[[1]]
    vec <- as.numeric(vecstrings[vecstrings!=""])
    if(length(vec) > maxlen)
      stop(paste("this line has more than ",maxlen," value",c("s","")[1+(maxlen==1)],": ",ctl[line1],sep=""))
    return(vec)
  } # end readfun

  # read control file if ctl is not supplied
  if(is.null(ctl)) ctl = readLines(ctlfile)

  # get sigma R
  sigmaR <- readfun("SR_sigmaR")[3]

  # make sure model includes recdevs and get some information
  do_recdev <- readfun("do_recdev", maxlen=1)
  if(do_recdev!=1) return("do_recdev should be set to 1")
  Nrecdevs <- lyr-fyr+1
  phase <- readfun("recdev phase", maxlen=1)
  advanced <- readfun("read 11 advanced options", maxlen=1)
  if(advanced!=1) stop("advanced options must be turned on in control file")
  if(phase>0){
    newphase <- -abs(phase)
    if(verbose) print(paste("making recdev phase to negative:",newphase),quote=F)
    ctl[grep("recdev phase",ctl)] <- paste(newphase,"#_recdev phase")
  }

  # turn on read_recdevs
  key1 <- grep("read_recdevs",ctl)
  ctl[key1] <- paste(Nrecdevs,"#_read_recdevs")

  # check for keyword at start of following section
  key2 <- grep("Fishing Mortality info",ctl)
  if(length(key2)==0){
    print("The phrase 'Fishing Mortality info' does not occur after the recdev section.",quote=F)
    print("Format of control file may be messy.",quote=F)
  }else{
    key2==key2[1]
  }

  # generate new recdevs
  if(!is.null(recdevs)){
    if(length(recdevs)!=Nrecdevs){
      stop(paste("input 'recdevs' has length=",length(recdevs)," but Nrecdevs=lyr-fyr+1=",Nrecdevs,sep=""))
    }else{
      newdevs <- recdevs
    }
  }else{
    newdevs <- rnorm(n=Nrecdevs)
  }
  newdevs <- sigmaR*newdevs/sd(newdevs)
  # build new recdev section
  newsection <- c(
    "#_end of advanced SR options"          ,
    ""                                      ,
    "# read specified recr devs"            ,
    "#_Yr Input_value"
  )
  #newsection <-c(newsection, rep("",(key2-key1-1)-length(newsection))) # preserve length of file

  for(i in 1:Nrecdevs) newsection[4+i] <-
    paste((fyr:lyr)[i], c("  "," ")[1+(newdevs[i]<0)], newdevs[i], " #_stochastic_recdev_with_sigmaR=", sigmaR, sep="")

  ctl <- c(ctl[1:key1],newsection,ctl[key2:length(ctl)])
  #ctl[(key1+1):(key2-1)] <- newsection

  # if maxbias is input, then replace
  ctl[grep("max_bias",ctl)] <- paste(newmaxbias,"#_max_bias_adj_in_MPD")
  
  # write and/or return the modified control file
  if(writectl){
    writeLines(ctl,newctlfile)
    if(verbose) print(paste("wrote new file:",newctlfile),quote=F)
  }
  #reset working directory
  setwd(current_wd)
  if(returnctl) return(ctl)
} # end function

