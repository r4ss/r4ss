SSv3_plotpars <-
function(
    dir="c:/path/",
    repfile="Report.sso",
    postfile="posteriors.sso",
    showpost=T,showprior=T,showmle=T,showinit=T,
    showrecdev=T,priorinit=T,priorfinal=T,
    showlegend=T,fitrange=F,xaxs="i",verbose=T,
    nrows=3,ncols=3,new=T,pdf=F,
    pwidth=7,pheight=7,punits="in",ptsize=12,
    returntable=F,strings=c(),burn=0,thin=1,
    ctlfile="control.ss_new")
{
  ################################################################################
  #
  # SSv3_plotpars
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

  codedate <- "December 21, 2009"

  if(verbose){
    print(paste("R function updated:",codedate),quote=F)
    print("Check for new code and report problems at http://code.google.com/p/r4ss/",quote=F)
  }

  # define subfunction
  GetPrior <- function(Ptype,Pmin,Pmax,Pr,Psd,Pval){
    # function to calculate prior values is direct translation of code in SSv3
    Pconst <- 0.0001
    if(Ptype==-1){ # no prior
      Prior_Like <- rep(0.,length(Pval));
    }
    if(Ptype==0){ # normal
      Prior_Like <- 0.5*((Pval-Pr)/Psd)^2;
    }
    if(Ptype==1){  # symmetric beta    value of Psd must be >0.0
      mu <- -(Psd*(log( (Pmax+Pmin)*0.5 - Pmin))) - (Psd*(log(0.5)));
      Prior_Like <- -(mu+ (Psd*(log(Pval-Pmin+Pconst)))+(Psd*(log(1.-((Pval-Pmin-Pconst)/(Pmax-Pmin))))));
    }
    if(Ptype==2){  # CASAL's Beta;  check to be sure that Aprior and Bprior are OK before running SS2!
      mu <- (Pr-Pmin) / (Pmax-Pmin);  # CASAL's v
      tau <- (Pr-Pmin)*(Pmax-Pr)/(Psd^2)-1.0;
      Bprior <- tau*mu;  Aprior <- tau*(1-mu);  # CASAL's m and n
      if(Bprior<=1.0 | Aprior <=1.0) {print(" bad Beta prior ");}
      Prior_Like <- (1.0-Bprior)*log(Pconst+Pval-Pmin) + (1.0-Aprior)*log(Pconst+Pmax-Pval)
        -(1.0-Bprior)*log(Pconst+Pr-Pmin) - (1.0-Aprior)*log(Pconst+Pmax-Pr);
    }
    if(Ptype==3){  # lognormal
      Prior_Like <- 0.5*((log(Pval)-Pr)/Psd)^2
    }
    return(Prior_Like)
  } # end GetPrior

  fullpostfile <- paste(dir,postfile,sep="/")
  fullrepfile  <- paste(dir,repfile,sep="/")
  fullctlfile  <- paste(dir,ctlfile,sep="/")

  print(fullpostfile)
  postfileinfo <- file.info(fullpostfile)$size
  repfileinfo  <- file.info(fullrepfile)$size
  ctlfileinfo  <- file.info(fullctlfile)$size

  if(is.na(repfileinfo)){
    print(paste("Error! Missing rep file:",fullrepfile),quote=F)
    return()
  }
  if(repfileinfo==0){
    print(paste("Error! Empty rep file:",fullrepfile),quote=F)
    return()
  }
  goodctl <- T
  if(is.na(ctlfileinfo)){
    print("Error! Missing control.ss_new file. Assuming recdev limits are -5 & 5.",quote=F)
    goodctl <- F
  }
  if(ctlfileinfo==0){
    print("Error! Empty control.ss_new file. Assuming recdev limits are -5 & 5.",quote=F)
    goodctl <- F
  }

  if(showpost & is.na(postfileinfo)){
    print(paste("Error! Missing post file:",fullpostfile),quote=F)
    print(      "       changing input to 'showpost=F'",quote=F)
    showpost <- F
  }
  if(showpost & !is.na(postfile) & postfileinfo==0){
    print(paste("Error! Empty post file:",fullpostfile),quote=F)
    print(      "       changing input to 'showpost=F'",quote=F)
    showpost <- F
  }

  ## get posteriors
  if(showpost & !is.na(postfileinfo) & postfileinfo>0){
    posts <- read.table(fullpostfile,head=T)
    posts <- posts[seq(burn+1,nrow(posts),thin), ] # remove burn-in and thin the posteriors
  }
  ## get parameter estimates
  if(!is.na(repfileinfo) & repfileinfo>0){
    replines <- readLines(fullrepfile,n=2000)
    parstart <- grep("PARAMETERS",replines)[2]
    parend <- grep("DERIVED_QUANTITIES",replines)[2]
    partable <- read.table(fullrepfile,head=T,nrows=parend-parstart-3,skip=parstart,as.is=T,fill=T)
    partable[partable=="_"] <- NA
    partable$Active_Cnt <- as.numeric(as.character(partable$Active_Cnt))
    partable$Label <- as.character(partable$Label)
    for(i in (1:ncol(partable))[!names(partable) %in% c("Label","Status")] ){
      partable[,i] <- as.numeric(as.character(partable[,i]))
    }
  }
  # subset for only active parameters
  allnames <- partable$Label[!is.na(partable$Active_Cnt)]

  ## get list of subset names if vector "strings" is supplied
  if(!is.null(strings)){
    goodnames <- NULL
    for(i in 1:length(strings)) goodnames <- c(goodnames,allnames[grep(strings[i],allnames)])
    goodnames <- unique(goodnames)
    print("parameters matching input vector 'strings':",quote=F)
    print(goodnames)
    if(length(goodnames)==0){
      print("No active parameters match input vector 'strings'.",quote=F)
      return()
    }
  }else{
    goodnames <- allnames
  }

  if(showmle & min(partable$Parm_StDev[partable$Label %in% goodnames]) <= 0){
    print("Some parameters have std. dev. values in Report.sso equal to 0.",quote=F)
    print("  Asymptotic uncertainty estimates will not be shown.",quote=F)
  }

  # remove RecrDevs temporarily until I add code to fill in the prior stuff
  recdevmin <- -5
  recdevmin <- 5
  recdevlabels <- c("Early_RecrDev_","Early_InitAge_","Main_InitAge_","Main_RecrDev_","ForeRecr_","Late_RecrDev_")
  if(showrecdev & goodctl){
    ctllines <- readLines(fullctlfile)
    iline <- grep("#min rec_dev",ctllines)
    if(length(iline)==1){
      # advanced options stuff
      recdevmin  <- as.numeric(strsplit(ctllines[iline],  " #")[[1]][1])
      recdevmax  <- as.numeric(strsplit(ctllines[iline+1]," #")[[1]][1])
      readrecdev <- as.numeric(strsplit(ctllines[iline+2]," #")[[1]][1])
      if(is.na(readrecdev) | readrecdev==1) print("This function does not yet display recdev values read from ctl file",quote=F)
    }
  }else{
    goodnames <- goodnames[!substr(goodnames,1,9) %in% substr(recdevlabels,1,9)]
  }
  npars <- length(goodnames)
  ### debugging tools:
  ## print(goodnames)
  ## return(partable[partable$Label %in% goodnames,])

  # make plot
  if(verbose){
    if(fitrange){
      print("Plotting range is scaled to fit parameter estimates.",quote=F)
      print("  Change input to 'fitrange=F' to get full parameter range.",quote=F)
    }else{
      print("Plotting range is equal to input limits on parameters.",quote=F)
      print("  Range can be scaled to fit estimates by setting input 'fitrange=T'.",quote=F)
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
    pdffile <- paste(dir,"/SSplotpars_",format(Sys.time(),'%d-%h-%Y_%H.%M' ),".pdf",sep="")
    pdf(file=pdffile,width=pwidth,height=pheight,punits="in")
    if(verbose) print(paste("PDF file with plots will be: ",pdffile,sep=""),quote=F)
  }

  par(mfcol=c(nrows,ncols),mar=c(2,1,2,1),oma=c(2,2,0,0))
  for(ipar in 1:npars){
    # grab name and full parameter line
    parname <- goodnames[ipar]
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
    ymax <- NULL # upper y-limit in plot
    xmin <- NULL # lower x-limit in plot
    xmax <- NULL # upper x-limit in plot

    # get prior
    negL_prior <- GetPrior(Ptype=Ptype,Pmin=Pmin,Pmax=Pmax,Pr=Pr,Psd=Psd,Pval=x)
    prior <- exp(-1*negL_prior)
    # prior likelihood at initial and final values
    priorinit <- exp(-1*GetPrior(Ptype=Ptype,Pmin=Pmin,Pmax=Pmax,Pr=Pr,Psd=Psd,Pval=initval))
    priorfinal <- exp(-1*GetPrior(Ptype=Ptype,Pmin=Pmin,Pmax=Pmax,Pr=Pr,Psd=Psd,Pval=finalval))
    if(showprior){
      prior <- prior/(sum(prior)*mean(diff(x)))
      ymax <- max(ymax,max(prior)) # update ymax
    }

    # get normal distribution associated with ADMB's estimate of the parameter's asymptotic std. dev.
    if(showmle){
      if(parsd>0){
        mle <- dnorm(x,finalval,parsd)
        mlescale <- 1/(sum(mle)*mean(diff(x)))
        mle <- mle*mlescale
        ymax <- max(ymax,max(mle)) # update ymax
      }
      # update x range
      xmin <- qnorm(0.001,finalval,parsd)
      xmax <- qnorm(0.999,finalval,parsd)
    }

    # get posterior
    goodpost <- F
    if(showpost){
      jpar <- (1:ncol(posts))[names(posts)==parname]
      if(length(jpar)==1){
        post <- posts[,jpar]
        xmin <- min(xmin, quantile(post,0.001)) # update x range
        xmax <- max(xmax, quantile(post,0.999)) # update x range
        goodpost <- T
      }else{
        print(paste("Error! parameter '",parname,"', not found in '",postfile,"'.",sep=""))
      }
    }

    # get x-range
    if(fitrange & (parsd!=0 | showpost)){
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
    xlim <- c(xmin,xmax)

    # get histogram for posterior based on x-range
    if(showpost & goodpost){
      jpar <- (1:ncol(posts))[names(posts)==parname]
      post <- posts[,jpar]
      breakvec <- seq(xmin,xmax,length=50)
      if(min(breakvec) > min(post)) breakvec <- c(min(post),breakvec)
      if(max(breakvec) < max(post)) breakvec <- c(breakvec,max(post))
      posthist <- hist(post,plot=F,breaks=breakvec)
      ymax <- max(ymax,max(posthist$density),na.rm=F) # update ymax
    }

    # make plot
    plot(0,type="n",xlim=xlim,ylim=c(0,1.1*ymax),xaxs=xaxs,yaxs="i",
         xlab="",ylab="",main=parname,cex.main=1,axes=F)
    axis(1)
    # axis(2) # don't generally show y-axis values because it's just distracting

    # add stuff to plot
    colval <- "grey"
    if(showpost & goodpost) plot(posthist,add=T,freq=F,col=colval,border=colval)
    if(showprior) lines(x,prior,lwd=2,lty=1)
    if(showmle){
      if(parsd>0){
        lines(x,mle,col="blue",lwd=2)
        lines(rep(finalval,2),c(0,dnorm(finalval,finalval,parsd)*mlescale),col="blue")
      }else{
        abline(v=finalval,col="blue")
      }
    }
    if(showinit){
      par(xpd=NA) # stop clipping
      points(initval,-0.02*ymax,col="red",pch=17,cex=1.2)
      par(xpd=F)  # restore original value
    }
    ##     if(printlike) mtext(side=3,line=0.2,cex=.8,adj=0,paste("prob@init =",round(priorinit,3)))
    ##     if(printlike) mtext(side=3,line=0.2,cex=.8,adj=1,paste("prob@final =",round(priorfinal,3)))
    box()

    if(max(par("mfg")[1:2])==1){ # first panel on page
      mtext("Parameter value",side=1,line=0.5,outer=T)
      mtext("Density",side=2,line=0.5,outer=T)
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

