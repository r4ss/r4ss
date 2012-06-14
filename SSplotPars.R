SSplotPars <-
  function(
    dir="c:/path/", repfile="Report.sso",
    postfile="posteriors.sso", showpost=TRUE, showprior=TRUE,
    showmle=TRUE, showinit=TRUE, showrecdev=TRUE, priorinit=TRUE,
    priorfinal=TRUE, showlegend=TRUE, fitrange=FALSE, xaxs="i",
    xlim=NULL, ylim=NULL, verbose=TRUE, nrows=3, ncols=3,
    new=TRUE, pdf=FALSE, pwidth=7, pheight=7, punits="in",
    ptsize=12, returntable=FALSE, strings=c(), exact=FALSE,
    newheaders=NULL, burn=0, thin=1,
    ctlfile="control.ss_new")
{
  ################################################################################
  #
  # SSplotPars
  # This function comes with no warranty or guarantee of accuracy
  #
  # Purpose: To make a multi-figure plot of prior distributions
  #          from a Stock Synthesis control file
  # Written: Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
  # Returns: Plots of prior distributions used in Stock Synthesis model
  # Notes:   hosted at http://code.google.com/p/r4ss/
  # Required packages: none
  #
  ################################################################################

  if(verbose){
    cat("Check for new code and report problems at http://code.google.com/p/r4ss/\n")
  }

  # define subfunction
  GetPrior <- function(Ptype,Pmin,Pmax,Pr,Psd,Pval){
    # function to calculate prior values is direct translation of code in SSv3

    # Ptype used to be a numeric value and is now a character string
    # hopefully this function is robust to either option
    Ptype2 <- NA
    if(is.character(Ptype)){
      if(Ptype=="No_prior") Ptype2 <- -1
      if(Ptype=="Normal") Ptype2 <- 0
      if(Ptype=="Sym_Beta") Ptype2 <- 1
      if(Ptype=="Full_Beta") Ptype2 <- 2
      if(Ptype=="Log_Norm") Ptype2 <- 3
    }else{
      Ptype2 <- Ptype
    }
    # fix cases where numeric value was read as character (from older SS versions, I think)
    if(is.na(Ptype2)) Ptype2 <- as.numeric(Ptype)
    if(is.na(Ptype2)) cat("problem with prior type interpretation. Ptype:",Ptype," Ptype2:",Ptype2,"\n")
    
    Pconst <- 0.0001
    if(Ptype2==-1){ # no prior
      Prior_Like <- rep(0.,length(Pval));
    }
    if(Ptype2==0){ # normal
      Prior_Like <- 0.5*((Pval-Pr)/Psd)^2;
    }
    if(Ptype2==1){  # symmetric beta    value of Psd must be >0.0
      mu <- -(Psd*(log( (Pmax+Pmin)*0.5 - Pmin))) - (Psd*(log(0.5)));
      Prior_Like <- -(mu+ (Psd*(log(Pval-Pmin+Pconst)))+(Psd*(log(1.-((Pval-Pmin-Pconst)/(Pmax-Pmin))))));
    }
    if(Ptype2==2){  # CASAL's Beta;  check to be sure that Aprior and Bprior are OK before running SS2!
      mu <- (Pr-Pmin) / (Pmax-Pmin);  # CASAL's v
      tau <- (Pr-Pmin)*(Pmax-Pr)/(Psd^2)-1.0;
      Bprior <- tau*mu;  Aprior <- tau*(1-mu);  # CASAL's m and n
      if(Bprior<=1.0 | Aprior <=1.0) {cat(" bad Beta prior\n");}
      Prior_Like <- (1.0-Bprior)*log(Pconst+Pval-Pmin) + (1.0-Aprior)*log(Pconst+Pmax-Pval)
        -(1.0-Bprior)*log(Pconst+Pr-Pmin) - (1.0-Aprior)*log(Pconst+Pmax-Pr);
    }
    if(Ptype2==3){  # lognormal
      Prior_Like <- 0.5*((log(Pval)-Pr)/Psd)^2
    }
    return(Prior_Like)
  } # end GetPrior

  fullpostfile <- paste(dir,postfile,sep="/")
  fullrepfile  <- paste(dir,repfile,sep="/")
  fullctlfile  <- paste(dir,ctlfile,sep="/")

  postfileinfo <- file.info(fullpostfile)$size
  repfileinfo  <- file.info(fullrepfile)$size
  ctlfileinfo  <- file.info(fullctlfile)$size

  if(is.na(repfileinfo)) stop("Missing rep file:",fullrepfile)
  if(repfileinfo==0) stop("Empty rep file:",fullrepfile)
  
  goodctl <- TRUE
  if(is.na(ctlfileinfo)){
    cat("Missing control.ss_new file. Assuming recdev limits are -5 & 5.\n")
    goodctl <- FALSE
  }else{
    if(ctlfileinfo==0){
      cat("Empty control.ss_new file. Assuming recdev limits are -5 & 5.\n")
      goodctl <- FALSE
    }
  }
  if(showpost & is.na(postfileinfo)){
    cat("Missing posteriors file: ",postfile,", changing input to 'showpost=FALSE'\n",sep="")
    showpost <- FALSE
  }
  if(showpost & !is.na(postfile) & postfileinfo==0){
    cat("Empty posteriors file: ",postfile,", changing input to 'showpost=FALSE'\n",sep="")
    showpost <- FALSE
  }

  ## get posteriors
  if(showpost & !is.na(postfileinfo) & postfileinfo>0){
    test <- readLines(fullpostfile,n=10) # test for presence of file with at least 10 rows
    if(length(test)>5){
      posts <- read.table(fullpostfile,header=TRUE)
      names(posts)[names(posts)=="SR_LN.R0."] <- "SR_LN(R0)"
      # remove burn-in and thin the posteriors if requested
      posts <- posts[seq(burn+1,nrow(posts),thin), ]
    }else{
      cat("Posteriors file has too few rows, changing input to 'showpost=FALSE'\n")
      showpost <- FALSE
    }
  }
  ## get parameter estimates
  if(!is.na(repfileinfo) & repfileinfo>0){
    replines <- readLines(fullrepfile,n=2000)
    parstart <- grep("PARAMETERS",replines)[2]
    parend <- grep("DERIVED_QUANTITIES",replines)[2]
    nrows2 <- parend - parstart - 3
    partable <- read.table(fullrepfile,header=FALSE,nrows=nrows2,skip=parstart,as.is=TRUE,
                           fill=TRUE,row.names=paste(1:nrows2),col.names=1:60)
    partable <- partable[,1:15]
    temp <- as.character(partable[1,])
    # this command was necessary for some intermediate version of SS (but I forget which)
    #temp <- c(temp[1:12],"PR_type_code",temp[13:14])
    names(partable) <- temp
    partable <- partable[-1,]
    rownames(partable) <- 1:nrow(partable)
    # check for presence of line which would represent the end of the table
    test <- grep("Number_of_active_parameters",partable$Num)
    if(length(test)>0) partable <- partable[1:(test-1),]
    # clean up contents of the table and make certain columns numeric or character
    partable[partable=="_"] <- NA
    partable$Active_Cnt <- as.numeric(as.character(partable$Active_Cnt))
    partable$Label <- as.character(partable$Label)
    for(i in (1:ncol(partable))[!names(partable) %in% c("Label","Status","PR_type")] ){
      partable[,i] <- as.numeric(as.character(partable[,i]))
    }
  }
  # subset for only active parameters
  allnames <- partable$Label[!is.na(partable$Active_Cnt)]

  ## get list of subset names if vector "strings" is supplied
  if(!is.null(strings)){
    goodnames <- NULL
    if(exact) goodnames <- allnames[allnames %in% strings]
    else for(i in 1:length(strings))
       goodnames <- c(goodnames,grep(strings[i],allnames,value=TRUE))
    goodnames <- unique(goodnames)
    cat("parameters matching input vector 'strings':\n")
    print(goodnames)
    if(length(goodnames)==0){
      cat("No active parameters match input vector 'strings'.\n")
      return()
    }
  }else{
    goodnames <- allnames
  }
  badpars <- grep("Impl_err_",goodnames)
  if(length(badpars)>0) goodnames <- goodnames[-badpars]
  stds <- partable$Parm_StDev[partable$Label %in% goodnames]

  if(showmle & (min(is.na(stds))==1 || min(stds, na.rm=TRUE) <= 0)){
    cat("Some parameters have std. dev. values in Report.sso equal to 0.\n",
        "  Asymptotic uncertainty estimates will not be shown.\n",
        "  Try re-running the model with the Hessian but no MCMC.\n")
  }

  # Recruitment Devs
  recdevmin <- -5
  recdevmin <- 5
  recdevlabels <- c("Early_RecrDev_","Early_InitAge_","Main_InitAge_",
                    "Main_RecrDev_","ForeRecr_","Late_RecrDev_")
  if(showrecdev & goodctl){
    ctllines <- readLines(fullctlfile)
    iline <- grep("#min rec_dev",ctllines)
    if(length(iline)==1){
      # advanced options stuff
      recdevmin  <- as.numeric(strsplit(ctllines[iline],  " #")[[1]][1])
      recdevmax  <- as.numeric(strsplit(ctllines[iline+1]," #")[[1]][1])
      readrecdev <- as.numeric(strsplit(ctllines[iline+2]," #")[[1]][1])
      if(is.na(readrecdev) | readrecdev==1)
        cat("This function does not yet display recdev values read from ctl file.\n")
    }
  }else{
    goodnames <- goodnames[!substr(goodnames,1,9) %in% substr(recdevlabels,1,9)]
  }
  npars <- length(goodnames)
  ### debugging tools:
  ## print(goodnames)
  ## return(partable[partable$Label %in% goodnames,])

  # make plot
  if(verbose & is.null(xlim)){
    if(fitrange){
      cat("Plotting range is scaled to fit parameter estimates.\n",
          "  Change input to 'fitrange=FALSE' to get full parameter range.\n")
    }else{
      cat("Plotting range is equal to input limits on parameters.\n",
          "  Range can be scaled to fit estimates by setting input 'fitrange=TRUE'.\n")
    }
  }

  ## make plot
  if(new & !pdf){
    if(length(grep('linux',version$os)) > 0) OS <- "Linux"
    if(length(grep('mingw',version$os)) > 0) OS <- "Windows"
    # need appropriate line to support Mac operating systems
    if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
    if(OS=="Windows") windows(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
    if(OS=="Linux") X11(width=pwidth,height=pheight,pointsize=ptsize)
    if(OS=="Mac") quartz(width=pwidth,height=pheight,pointsize=ptsize)
  }
  if(pdf){
    pdffile <- paste(dir,"/SSplotPars_",format(Sys.time(),'%d-%h-%Y_%H.%M' ),".pdf",sep="")
    pdf(file=pdffile,width=pwidth,height=pheight)
    if(verbose) cat("PDF file with plots will be: ",pdffile,"\n")
  }

  par(mfcol=c(nrows,ncols),mar=c(2,1,2,1),oma=c(2,2,0,0))
  if(verbose) cat("Making plots of parameters:\n")
  for(ipar in 1:npars){
    # grab name and full parameter line
    parname <- goodnames[ipar]
    if(verbose) cat("    ",parname,"\n")
    parline <- partable[partable$Label==parname,]

    # grab values
    initval <- parline$Init
    finalval <- parline$Value
    parsd <- parline$Parm_StDev

    Pmin <- parline$Min
    Pmax <- parline$Max
    Ptype <- parline$PR_type
    Psd <- parline$Pr_SD
    Pr <- parline$Prior

    if(substr(parname,1,9) %in% substr(recdevlabels,1,9)){
      initval <- 0
      Pmin <- recdevmin
      Pmax <- recdevmax
      Ptype <- 0
      Pr <- 0
      Psd <- partable$Value[partable$Label=="SR_sigmaR"]
    }

    x <- seq(Pmin,Pmax,length=5000) # x vector for subsequent calcs

    # make empty holders for future information
    ymax <- 0 # upper y-limit in plot
    xmin <- NULL # lower x-limit in plot
    xmax <- NULL # upper x-limit in plot

    # get prior
    negL_prior <- GetPrior(Ptype=Ptype,Pmin=Pmin,Pmax=Pmax,Pr=Pr,Psd=Psd,Pval=x)
    prior <- exp(-1*negL_prior)
    # prior likelihood at initial and final values
    ## priorinit <- exp(-1*GetPrior(Ptype=Ptype,Pmin=Pmin,Pmax=Pmax,Pr=Pr,Psd=Psd,Pval=initval))
    ## priorfinal <- exp(-1*GetPrior(Ptype=Ptype,Pmin=Pmin,Pmax=Pmax,Pr=Pr,Psd=Psd,Pval=finalval))
    if(showprior){
      prior <- prior/(sum(prior)*mean(diff(x)))
      ymax <- max(ymax,max(prior),na.rm=TRUE) # update ymax
    }

    # get normal distribution associated with ADMB's estimate
    # of the parameter's asymptotic std. dev.
    if(showmle){
      if(!is.na(parsd) && parsd>0){
        mle <- dnorm(x,finalval,parsd)
        mlescale <- 1/(sum(mle)*mean(diff(x)))
        mle <- mle*mlescale
        ymax <- max(ymax,max(mle),na.rm=TRUE) # update ymax
        # update x range
        xmin <- qnorm(0.001,finalval,parsd)
        xmax <- qnorm(0.999,finalval,parsd)
      }else{
        xmin <- xmax <- finalval
      }
    }

    # get posterior
    goodpost <- FALSE
    if(showpost){
      jpar <- (1:ncol(posts))[names(posts)==parname]
      if(length(jpar)==1){
        post <- posts[,jpar]
        xmin <- min(xmin, quantile(post,0.001)) # update x range
        xmax <- max(xmax, quantile(post,0.999)) # update x range
        goodpost <- TRUE
      }else{
        cat("Error! parameter '",parname,"', not found in '",postfile,"'.\n",sep="")
      }
    }

    # get x-range
    if(is.null(xlim)){
      if(fitrange & ((!is.na(parsd) && parsd!=0) | showpost)){
        # if rescaling limits,
        # make sure initial value is inside limits
        if(showinit){
          xmin <- min(initval,xmin)
          xmax <- max(initval,xmax)
        }
        # keep range inside parameter limits
        xmin <- max(Pmin,xmin)
        xmax <- min(Pmax,xmax)
      }else{
        # or use parameter limits
        xmin <- Pmin
        xmax <- Pmax
      }
      xlim2 <- c(xmin,xmax)
    }else{
      xlim2 <- xlim
    }

    # get histogram for posterior based on x-range
    if(showpost & goodpost){
      jpar <- (1:ncol(posts))[names(posts)==parname]
      post <- posts[,jpar]
      breakvec <- seq(xmin,xmax,length=50)
      if(min(breakvec) > min(post)) breakvec <- c(min(post),breakvec)
      if(max(breakvec) < max(post)) breakvec <- c(breakvec,max(post))
      posthist <- hist(post,plot=FALSE,breaks=breakvec)
      ymax <- max(ymax,max(posthist$density),na.rm=FALSE) # update ymax
    }

    # make plot
    if(is.null(newheaders)) header <- parname else header <- newheaders[ipar]
    if(is.null(ylim)) ylim2 <- c(0,1.1*ymax) else ylim2 <- ylim
    plot(0,type="n",xlim=xlim2,ylim=ylim2,xaxs=xaxs,yaxs="i",
         xlab="",ylab="",main=header,cex.main=1,axes=FALSE)
    axis(1)
    # axis(2) # don't generally show y-axis values because it's just distracting

    # add stuff to plot
    colval <- "grey"
    if(showpost & goodpost) plot(posthist,add=TRUE,freq=FALSE,col=colval,border=colval)
    if(showprior) lines(x,prior,lwd=2,lty=1)
    if(showmle){
      if(!is.na(parsd) && parsd>0){
        lines(x,mle,col="blue",lwd=2)
        lines(rep(finalval,2),c(0,dnorm(finalval,finalval,parsd)*mlescale),col="blue")
      }else{
        abline(v=finalval,col="blue")
      }
    }
    if(showinit){
      par(xpd=NA) # stop clipping
      points(initval,-0.02*ymax,col="red",pch=17,cex=1.2)
      par(xpd=FALSE)  # restore original value
    }
    ##     if(printlike) mtext(side=3,line=0.2,cex=.8,adj=0,paste("prob@init =",round(priorinit,3)))
    ##     if(printlike) mtext(side=3,line=0.2,cex=.8,adj=1,paste("prob@final =",round(priorfinal,3)))
    box()

    if(max(par("mfg")[1:2])==1){ # first panel on page
      mtext("Parameter value",side=1,line=0.5,outer=TRUE)
      mtext("Density",side=2,line=0.5,outer=TRUE)
      if(showlegend){
        showvec <- c(showprior,showmle,showpost,showinit)
        legend("topleft",cex=1.2,bty="n",pch=c(NA,NA,15,17)[showvec],
               lty=c(1,1,NA,NA)[showvec],lwd=c(2,1,NA,NA)[showvec],
               col=c("black","blue","grey","red")[showvec],
               pt.cex=c(1,1,2,1)[showvec],
               legend=c("prior","max. likelihood","posterior","initial value")[showvec])
      } # end legend
    } # end first panel stuff
  } # end loop over parameters
  if(pdf) dev.off() # close PDF file if it was open
  if(returntable) return(partable[partable$Label %in% goodnames,])
}

