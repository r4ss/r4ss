#' Plot distributions of priors, posteriors, and estimates.
#'
#' Make multi-figure plots of prior, posterior, and estimated asymptotic
#' parameter distributions. MCMC not required to make function work.
#'
#'
#' @param replist List produced by \code{\link{SS_output}}.
#' @param xlab Label on horizontal axis.
#' @param ylab Label on vertical axis.
#' @param showmle Show MLE estimate and asymptotic variance estimate with blue
#' lines?
#' @param showprior Show prior distribution as black line?
#' @param showpost Show posterior distribution as bar graph if MCMC results
#' are available in \code{replist}?
#' @param showinit Show initial value as red triangle?
#' @param showdev Include devs in the plot?
#' @param showlegend Show the legend?
#' @param fitrange Fit range tightly around MLE & posterior distributions,
#' instead of full parameter range?
#' @param xaxs Parameter input for x-axis. See \code{?par} for more info.
#' @param xlim Optional x-axis limits to be applied to all plots.
#' Otherwise, limits are based on the model results.
#' @param ylim Optional y-axis limits to be applied to all plots.
#' Otherwise, limits are based on the model results.
#' @param verbose Controls amount of text output (maybe).
#' @param nrows How many rows in multi-figure plot.
#' @param ncols How many columns in multi-figure plot.
#' @param ltyvec Vector of line types used for lines showing MLE and prior
#' distributions and the median of the posterior distribution.
#' @param colvec Vector of colors used for lines and polygons showing MLE,
#' initial value, prior, posterior, and median of the posterior.
#' @param new Open new window for plotting?
#' @param pdf Write to PDF file instead of R GUI?
#' @param pwidth Default width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param pheight Default height width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param punits Units for \code{pwidth} and \code{pheight}. Can be "px"
#' (pixels), "in" (inches), "cm" or "mm". Default="in".
#' @param ptsize Point size for plotted text in plots printed to files (see
#' help("png") in R for details). Default=12.
#' @param returntable Return table of parameter info? Default=FALSE.
#' @param strings Subset parameters included in the plot using substring from
#' parameter names (i.e. "SR" will get "SR_LN(R0)" and "SR_steep" if they are both
#' estimated quantities in this model).
#' @param exact Should strings input match parameter names exactly?  Otherwise
#' substrings are allowed.
#' @param newheaders Optional vector of headers for each panel to replace the
#' parameter names.
#' @author Ian G. Taylor, Cole C. Monnahan
#' @export
#' @examples
#'
#' \dontrun{
#' pars <- SSplotPars(dir='c:/SS/Simple/')
#'
#' # strings can be partial match
#' pars <- SSplotPars(dir='c:/SS/Simple/',strings=c("steep"))
#' }
#'
SSplotPars <-
  function(
      replist,
      xlab="Parameter value",ylab="Density",
      showmle=TRUE, showpost=TRUE, showprior=TRUE, showinit=TRUE,
      showdev=FALSE,
      # priorinit=TRUE, priorfinal=TRUE,
      showlegend=TRUE, fitrange=FALSE, xaxs="i",
      xlim=NULL, ylim=NULL, verbose=TRUE, nrows=3, ncols=3,
      ltyvec=c(1, 1, 3, 4),
      colvec=c("blue", "red", "black", "gray60", rgb(0, 0, 0, .5)),
      new=TRUE, pdf=FALSE, pwidth=6.5, pheight=5.0, punits="in",
      ptsize=10, returntable=FALSE, strings=c(), exact=FALSE,
      newheaders=NULL)
{
  # define subfunction
  GetPrior <- function(Ptype,Pmin,Pmax,Pr,Psd,Pval){
    # function to calculate prior values is direct translation of code in SS

    if(is.na(Ptype)){
      warning("problem with prior type interpretation. Ptype:", Ptype)
    }

    Pconst <- 0.0001
    # no prior
    if(Ptype == "No_prior"){
      Prior_Like <- rep(0.,length(Pval));
    }
    
    # normal
    if(Ptype == "Normal"){
      Prior_Like <- 0.5*((Pval-Pr)/Psd)^2;
    }
    
    # symmetric beta    value of Psd must be >0.0
    if(Ptype == "Sym_Beta"){
      mu <- -(Psd*(log( (Pmax+Pmin)*0.5 - Pmin))) - (Psd*(log(0.5)));
      Prior_Like <- -(mu+ (Psd*(log(Pval-Pmin+Pconst))) +
                        (Psd*(log(1.-((Pval-Pmin-Pconst)/(Pmax-Pmin))))));
    }
    
    # CASAL's Beta;  check to be sure that Aprior and Bprior are OK before running SS2!
    if(Ptype == "Full_Beta"){
      mu <- (Pr-Pmin) / (Pmax-Pmin);  # CASAL's v
      tau <- (Pr-Pmin)*(Pmax-Pr)/(Psd^2)-1.0;
      Bprior <- tau*mu;  Aprior <- tau*(1-mu);  # CASAL's m and n
      if(Bprior<=1.0 | Aprior <=1.0) {
        warning("bad Beta prior");
      }
      Prior_Like <- (1.0-Bprior)*log(Pconst+Pval-Pmin) +
        (1.0-Aprior)*log(Pconst+Pmax-Pval) -
          (1.0-Bprior)*log(Pconst+Pr-Pmin) -
            (1.0-Aprior)*log(Pconst+Pmax-Pr);
    }
    
    # lognormal
    if(Ptype == "Log_Norm"){
      Prior_Like <- 0.5*((log(Pval)-Pr)/Psd)^2
    }
    
    # lognormal with bias correction (from Larry Jacobson)
    if(Ptype == "Log_Norm_w/biasadj"){
      if(Pmin>0.0){
        Prior_Like <- 0.5*((log(Pval)-Pr+0.5*Psd^2)/Psd)^2;
      }else{
        warning("cannot do prior in log space for parm with min <=0.0")
      }
    }

    # gamma  (from Larry Jacobson)
    if(Ptype == "Gamma"){
      warning("Gamma prior not implemented yet")
      #### need to work out the following code, including replacing "gammln"
      ##   warnif <- 1e-15;
      ##   if(Pmin<0.0){
      ##     cat("Lower bound for gamma prior must be >=0.  Suggestion ",warnif*10.0,"\n")
      ##   }else{
      ##     # Gamma is defined over [0,+inf) but x=zero causes trouble for some mean/variance combos.
      ##     if(Pval < warnif){
      ##       cat("Pval too close to zero in gamma prior - can not guarantee reliable calculations.\n",
      ##           "Suggest rescaling data (e.g. * 1000)?\n")
      ##     }else{
      ##       scale <- (Psd^2)/Pr;  #  gamma parameters by method of moments
      ##       shape <- Pr/scale;
      ##       Prior_Like <- -shape*log(scale)-gammln(shape)+(shape-1.0)*log(Pval)-Pval/scale;
      ##     }
      ##   }
    }

    return(Prior_Like)
  } # end GetPrior

  # check input
  if(!"parameters" %in% names(replist)){
    stop("'replist' input needs to be a list created by the SS_output function")
  }

  parameters <- replist$parameters

  # subset for only active parameters
  allnames <- parameters$Label[!is.na(parameters$Active_Cnt)]

  ## get list of subset names if vector "strings" is supplied
  if(!is.null(strings)){
    goodnames <- NULL
    if(exact){
      goodnames <- allnames[allnames %in% strings]
    }else{
      for(i in 1:length(strings)){
        goodnames <- c(goodnames,grep(strings[i],allnames,value=TRUE))
      }
    }
    goodnames <- unique(goodnames)
    if(verbose){
      message("Active parameters matching input vector 'strings':")
      print(goodnames)
    }
    if(length(goodnames)==0){
      warning("No active parameters match input vector 'strings'.")
      return()
    }
  }else{
    goodnames <- allnames
    if(length(goodnames)==0){
      warning("No active parameters.")
      return()
    }
  }

  # skip 
  skip <- grep("Impl_err_",goodnames)
  if(length(skip)>0){
    goodnames <- goodnames[-skip]
    message("Skipping 'Impl_err_' parameters which don't have bounds reported")
  }

  # Recruitment devs
  recdevlabels <- c("Early_RecrDev_","Early_InitAge_","Main_InitAge_",
                    "Main_RecrDev_","ForeRecr_","Late_RecrDev_")
  if(!showdev){
    for(ilabel in 1:length(recdevlabels)){
      skip <- grep(recdevlabels[ilabel], goodnames)
      if(length(skip) > 0){
        goodnames <- goodnames[-skip]
      }
    }
  }

  # other devs
  otherdevlabels <- c("DEVrwalk", "DEVadd", "DEVmult", "ARDEV")
  if(!showdev){
    # remove 
    for(ilabel in 1:length(otherdevlabels)){
      skip <- grep(otherdevlabels[ilabel], goodnames)
      if(length(skip) > 0){
        ## print names being removed
        #print(goodnames[skip])
        goodnames <- goodnames[-skip]
      }
    }
  }else{
    # warn if any of these are present
    if(length(grep('DEVrwalk', x=goodnames))>0 |
       length(grep('DEVadd', x=goodnames))>0 |
       length(grep('DEVmult', x=goodnames))>0 |
       length(grep('ARDEV', x=goodnames))>0){
      warning('Parameter deviates are not fully implemented in this function.\n',
              'Prior and bounds unavailable so these are skipped and\n',
              'fitrange is set to TRUE for those parameters.')
    }
  }

  # get vector of standard deviations and test for NA or 0 values
  stds <- parameters$Parm_StDev[parameters$Label %in% goodnames]

  if(showmle & (all(is.na(stds)) || min(stds, na.rm=TRUE) <= 0)){
    message("Some parameters have std. dev. values in Report.sso equal to 0.\n",
            "  Asymptotic uncertainty estimates will not be shown.\n",
            "  Try re-running the model with the Hessian but no MCMC.")
  }
  
  npars <- length(goodnames)
  if(is.null(strings)){
    message("Plotting distributions for ", npars, " parameters.")
  }
  
  # make plot
  if(verbose & is.null(xlim)){
    if(fitrange){
      message("Plotting range is scaled to fit parameter estimates.\n",
              "  Change input to 'fitrange=FALSE' to get full parameter range.")
    }else{
      message("Plotting range is equal to input limits on parameters.\n",
              "  Range can be scaled to fit estimates by setting input 'fitrange=TRUE'.")
    }
  }

  ## make plot
  if(new & !pdf){
    ### Note: the following line has been commented out because it was identified
    ###       by Brian Ripley as "against CRAN policies".
    #if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
    dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
  }
  if(pdf){
    pdffile <- file.path(dir, paste0("SSplotPars_",
                                     format(Sys.time(),'%d-%b-%Y_%H.%M' ),".pdf"))
    pdf(file=pdffile,width=pwidth,height=pheight)
    if(verbose){
      message("PDF file with plots will be: ",pdffile)
    }
  }

  if(new){
    par(mfcol=c(nrows,ncols),mar=c(2,1,2,1),oma=c(2,2,0,0))
  }
  if(verbose){
    message("Making plots of parameters:")
  }

  # loop over parameters to make individual pots
  for(ipar in 1:npars){
    # grab name and full parameter line
    parname <- goodnames[ipar]

    if(verbose){
      message("    ",parname)
    }
    parline <- parameters[parameters$Label==parname,]

    # grab values
    initval <- parline$Init
    finalval <- parline$Value
    parsd <- parline$Parm_StDev

    Pmin <- parline$Min
    Pmax <- parline$Max
    Ptype <- parline$Pr_type
    Psd <- parline$Pr_SD
    Pr <- parline$Prior

    ## if(is.na(initval)){
    ##   initval <- 0
    ## }
    ## if(is.na(Pmin)){
    ##   Pmin <- -5
    ##   Pmax <- 5
    ## }
    if(is.na(Ptype) || Ptype == "dev"){
      Ptype <- "Normal"
      Pr <- 0
    }
    # add bounds and sigma for recdevs
    if(substr(parname,1,9) %in% substr(recdevlabels,1,9)){
      Psd <- parameters$Value[parameters$Label=="SR_sigmaR"]
    }

    ## Devations on parameters (either random-walk, additive, or multiplicative,
    ## or the new semi-parametric selectivity devs)
    ## are a special case (as opposed to rec devs)
    ## For now the prior gets skipped because the sigma wasn't reported by SS 3.24
    ## In SS version 3.30, the sigma is available as a parameter,
    ## so just needs to be matched up with the deviations
    ## also note that the "dev" column in parameters can be used here instead
    isdev <- FALSE
    if(length(grep('DEVrwalk', x=parname))>0 |
       length(grep('DEVadd', x=parname))>0 |
       length(grep('DEVmult', x=parname))>0 |
       length(grep('ARDEV', x=parname))>0){
      initval <- 0
      isdev <- TRUE
    }

    # make empty holders for future information
    ymax <- 0 # upper y-limit in plot
    xmin <- NULL # lower x-limit in plot
    xmax <- NULL # upper x-limit in plot

    ## get prior if not a dev (which don't yet have prior/penalty configured)
    if(!isdev){
      x <- seq(Pmin,Pmax,length=5000) # x vector for subsequent calcs
      negL_prior <- GetPrior(Ptype=Ptype,Pmin=Pmin,Pmax=Pmax,Pr=Pr,Psd=Psd,Pval=x)
      prior <- exp(-1*negL_prior)
    } else {
      x <- finalval+seq(-4*parsd, 4*parsd, length=5000)
    }
    #### note from Ian (12/21/2018): I have no memory of why the prior likelihood
    #### text was added and then commented out. I'm not sure it would add value
    #### to the plot, so will leave it commented out
    ####
    ### prior likelihood at initial and final values
    ## priorinit <- exp(-1*GetPrior(Ptype=Ptype,Pmin=Pmin,Pmax=Pmax,Pr=Pr,Psd=Psd,Pval=initval))
    ## priorfinal <- exp(-1*GetPrior(Ptype=Ptype,Pmin=Pmin,Pmax=Pmax,Pr=Pr,Psd=Psd,Pval=finalval))
    if(!isdev & showprior){
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

    # get mcmc results from replist created by SS_output
    mcmc <- replist$mcmc
    if(showpost && is.null(mcmc)){
      warning("$mcmc not found in input 'replist', changing input to 'showpost=FALSE'")
      showpost <- FALSE
    }
    if(showpost && length(mcmc) < 20){
      message("mcmc output has fewer than 20 rows, changing input to 'showpost=FALSE'")
      showpost <- FALSE
    }

    goodpost <- FALSE
    if(showpost){
      # modify parname to remove parentheses as done by read.table
      postparname <- parname
      if(substring(parname, 1, 1) == "_"){
        postparname <- paste0("X", postparname)
      }
      postparname <- gsub("(", ".", postparname, fixed=TRUE)
      postparname <- gsub(")", ".", postparname, fixed=TRUE)
      postparname <- gsub("%", ".", postparname, fixed=TRUE)
      # restore R0 parameter label to match correction in SSgetMCMC
      postparname[postparname=="SR_LN.R0."] <- "SR_LN(R0)"

      # figure out which column of the mcmc output
      jpar <- (1:ncol(mcmc))[names(mcmc)==postparname]
      if(length(jpar)==1){
        post <- mcmc[,jpar]
        xmin <- min(xmin, quantile(post,0.001)) # update x range
        xmax <- max(xmax, quantile(post,0.999)) # update x range
        goodpost <- TRUE
      }else{
        warning("parameter '",postparname,"', not found in posteriors.")
      }
    }

    # get x-range
    if(is.null(xlim)){
      if(fitrange & ((!is.na(parsd) && parsd!=0) | showpost)){
        # if rescaling limits,
        # make sure initial value is inside limits
          if(showinit){
              ## xmin or xmax may be NA if its a rdevwalk parameter
              xmin <- min(initval,xmin, na.rm=TRUE)
              xmax <- max(initval,xmax, na.rm=TRUE)
          }
        # keep range inside parameter limits
        xmin <- max(Pmin,xmin, na.rm=TRUE)
        xmax <- min(Pmax,xmax, na.rm=TRUE)
      }else{
          ## or use parameter limits, unless case of rdevwalk which as none
          ## then revert back to those calculated above
        if(!isdev) xmin <- Pmin
        if(!isdev) xmax <- Pmax
      }
      xlim2 <- c(xmin,xmax)
    }else{
      xlim2 <- xlim
    }

    # get histogram for posterior based on x-range
    if(showpost & goodpost){
      jpar <- (1:ncol(mcmc))[names(mcmc)==postparname]
      post <- mcmc[,jpar]
      breakvec <- seq(xmin,xmax,length=50)
      if(min(breakvec) > min(post)) breakvec <- c(min(post),breakvec)
      if(max(breakvec) < max(post)) breakvec <- c(breakvec,max(post))
      posthist <- hist(post,plot=FALSE,breaks=breakvec)
      postmedian <- median(post)
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
    colval <- colvec[4]
    # posterior with median
    if(showpost & goodpost){
      plot(posthist,add=TRUE,freq=FALSE,col=colval,border=colval)
      abline(v=postmedian,col=colvec[5],lwd=2,lty=ltyvec[3])
    }
    # prior
    if(!isdev & showprior){
      lines(x,prior,lwd=2,lty=ltyvec[2])
    }
    # MLE
    if(showmle){
      # full normal distribution if uncertainty is present
      if(!is.na(parsd) && parsd>0){
        lines(x,mle,col=colvec[1],lwd=1,lty=ltyvec[1])
        lines(rep(finalval,2),c(0,dnorm(finalval,finalval,parsd)*mlescale),
              col=colvec[1],lty=ltyvec[1])
      }else{
        # just point estimate otherwise
        abline(v=finalval, col=colvec[1], lty=ltyvec[1])
      }
    }
    # marker for initial value
    if(showinit){
      par(xpd=NA) # stop clipping
      points(initval,-0.02*ymax,col=colvec[2],pch=17,cex=1.2)
      par(xpd=FALSE)  # restore original value
    }
    ##     if(printlike) mtext(side=3,line=0.2,cex=.8,adj=0,paste("prob@init =",round(priorinit,3)))
    ##     if(printlike) mtext(side=3,line=0.2,cex=.8,adj=1,paste("prob@final =",round(priorfinal,3)))
    box()

    # add margin text and legend
    if(max(par("mfg")[1:2])==1){ # first panel on page
      mtext(xlab,side=1,line=0.5,outer=TRUE)
      mtext(ylab,side=2,line=0.5,outer=TRUE)
      if(showlegend){
        showvec <- c(showprior,showmle,showpost,showpost,showinit)
        legend("topleft",cex=1.2,bty="n",pch=c(NA,NA,15,NA,17)[showvec],
               lty=c(ltyvec[2],ltyvec[1],NA,ltyvec[3],NA)[showvec],lwd=c(2,1,NA,2,NA)[showvec],
               col=c(colvec[3],colvec[1],colvec[4],colvec[5],colvec[2])[showvec],
               pt.cex=c(1,1,2,1,1)[showvec],
               legend=c("prior","max. likelihood","posterior",
                   "posterior median","initial value")[showvec])
      } # end legend
    } # end first panel stuff
  } # end loop over parameters
  if(pdf){
    dev.off() # close PDF file if it was open
  }
  if(returntable){
    return(parameters[parameters$Label %in% goodnames,])
  }
}

