#' Estimate bias adjustment for recruitment deviates
#' 
#' Uses standard error of estimated recruitment deviates to estimate the 5
#' controls for the bias adjustment in Stock Synthesis
#' 
#' 
#' @param replist Object created using \code{\link{SS_output}}
#' @param verbose Controls the amount of output to the screen.  Default=FALSE.
#' @param startvalues A vector of 5 values for the starting points in the
#' minimization. Default=NULL.
#' @param method A method to apply to the 'optim' function. See ?optim for
#' options. Default="BFGS". By default, optim is not used, and the optimization
#' is based on the input \code{altmethod}.
#' @param twoplots Make a two-panel plot showing devs as well as transformed
#' uncertainty, or just the second panel in the set?  Default=TRUE.
#' @param transform An experimental option to treat the transform the 5
#' quantities to improve minimization. Doesn't work well. Default=FALSE.
#' @param plot Plot to active plot device?
#' @param print Print to PNG files?
#' @param plotdir Directory where PNG files will be written. By default it will
#' be the directory where the model was run.
#' @param shownew Include new estimated bias adjustment values on top of values
#' used in the model? (TRUE/FALSE)
#' @param oldctl Optional name of existing control file to modify.
#' Default=NULL.
#' @param newctl Optional name of new control file to create from old file with
#' estimated bias adjustment values. Default=NULL.
#' @param altmethod Optimization tool to use in place of optim, either "nlminb"
#' or "psoptim". If not equal to either of these, then optim is used.
#' @param pwidth Default width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param pheight Default height width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param punits Units for \code{pwidth} and \code{pheight}. Can be "px"
#' (pixels), "in" (inches), "cm" or "mm". Default="in".
#' @param ptsize Point size for plotted text in plots printed to files (see
#' help("png") in R for details). Default=12.
#' @param res Resolution of plots printed to files. Default=300.
#' @param cex.main Character expansion for plot titles.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_output}}
#' @references Methot, R.D. and Taylor, I.G., 2011. Adjusting for bias due to
#' variability of estimated recruitments in fishery assessment models.  Can. J.
#' Fish. Aquat. Sci., 68:1744-1760.
#' @keywords data manip hplot
SS_fitbiasramp <-
function(replist, verbose=FALSE, startvalues=NULL, method="BFGS", twoplots=TRUE,
         transform=FALSE, plot=TRUE, print=FALSE, plotdir="default",shownew=TRUE,
         oldctl=NULL, newctl=NULL, altmethod="nlminb",
         pwidth=6.5, pheight=5.0, punits="in", ptsize=10, res=300, cex.main=1){
  ##################
  # function to estimate bias adjustment ramp
  # for Stock Synthesis v3.11 - v3.21
  # by Ian Taylor
  # April 14, 2011
  #
  # Usage: run function with input that is an object from SS_output
  #        from http://code.google.com/p/r4ss/
  #
  ##################

  # note, method is choices that go into optim:
  #  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")
  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  if(!is.list(replist) | !(as.numeric(substr(replist$SS_version,5,8)) > 3.11)){
    stop("this function needs an input object created by SS_output from SS version 3.11 or greater")
  }
  if(replist$inputs$covar==FALSE){
    stop("you need to have covar=TRUE in the input to the SS_output function")
  }
  parameters <- replist$parameters
  startyr    <- replist$startyr
  recruit    <- replist$recruit
  sigma_R_in <- replist$sigma_R_in
  rmse_table <- replist$rmse_table
  if(plotdir=="default") plotdir <- replist$inputs$dir

  if(!is.numeric(rmse_table$RMSE)){
    stop("Input list element 'rmse_table' has non-numeric 'RMSE' column.")
  }
  if(max(rmse_table$RMSE)==0){
    stop("No bias adjustment needed. Root mean squared error of recruit devs is 0.")
  }
  
  if(is.null(startvalues)){
      nonfixedyrs <- recruit$year[recruit$era!="Fixed"]
      mainyrs <- recruit$year[recruit$era=="Main"]
      startvalues <- c(min(nonfixedyrs),
                       min(mainyrs) + .3*diff(range(mainyrs)),
                       max(mainyrs) - .1*diff(range(mainyrs)),
                       max(nonfixedyrs),
                       .7)
  }
  if(verbose) cat("startvalues =",paste(startvalues,collapse=", "),"\n")

  makeoffsets <- function(values){
      # a function to transform parameters into offsets from adjacent values
      newvalues <- NULL
      newvalues[3] <- values[3]
      newvalues[2] <- log(values[3]-values[2])
      newvalues[1] <- log(values[2]-values[1])
      newvalues[4] <- log(values[4]-values[3])
      newvalues[5] <- log(values[5]/(1-values[5]))
      return(newvalues)
  }
  removeoffsets <- function(newvalues){
      # a function to undo the transformation caused by makeoffsets
      values <- NULL
      values[3]   <- newvalues[3]
      values[2]   <- values[3]-exp(newvalues[2])
      values[1]   <- values[2]-exp(newvalues[1])
      values[4]   <- values[3]+exp(newvalues[4])
      values[5]   <- 1/(1+exp(-newvalues[5]))
      return(values)
  }

  if(transform){
    startvalues <- makeoffsets(startvalues)
  }
  if(verbose & transform) cat("transformed startvalues =",paste(startvalues,collapse=", "),"\n")

  biasadjfit <- function(pars,yr,std,sigmaR,transform,eps=.1){
    # calculate the goodness of the fit of the estimated ramp and values to the model output
    biasadj <- biasadjfun(yr=yr,vec=pars, transform=transform)$biasadj
    compare <- 1 - (std/sigmaR)^2

    # penalty similar to that employed by posfun in ADMB
    penfun <- function(xsmall,xbig,eps=.1){
      pen <- 0
      if(xbig < xsmall+eps) pen <- pen + (xbig - (xsmall+eps))^2
      return(pen)
    }
    penalty <- 0
    # penalize year values out of order
    penalty <- penalty + penfun(pars[2],pars[3])
    penalty <- penalty + penfun(pars[1],pars[2])
    penalty <- penalty + penfun(pars[3],pars[4])
    # penalize values outside range of years
    penalty <- penalty + penfun(pars[3],max(yr),eps=0)
    penalty <- penalty + penfun(min(yr),pars[2],eps=0)
    penalty <- penalty + penfun(0,pars[5],eps=0)
    penalty <- penalty + penfun(pars[5],1,eps=0)
    # penalize extreme values
    penalty <- penalty + penfun(min(yr)-diff(range(yr)),pars[1],eps=0)
    penalty <- penalty + penfun(pars[4],max(yr)+diff(range(yr)),eps=0)

    fit <- sum((biasadj-compare)^2) + penalty
    return(fit)
  }

  getrecdevs <- function(replist){
    # get info on recruitment devs from the model output
    par_mat   <- replist$parameters
    par_start <- grep("SR_autocorr",par_mat$Label)+1
    par_end   <- grep("InitF",par_mat$Label)[1]-1
    if(is.na(par_end)){
      # InitF parameters have gone away in SSv3.3 (at least for one model)
      par_end <- grep("Impl_err",par_mat$Label)[1]-1
    }
    rowrange <- par_start:par_end
    Impl_err_rows <- grep("Impl_err",par_mat$Label)
    if(length(Impl_err_rows)>0)
      rowrange <- (grep("SR_autocorr",par_mat$Label)+1):(Impl_err_rows[1]-1)
    yr <- par_mat$Label[rowrange]
    yr <- strsplit(yr,"_")
    yr2 <- rep(NA,length(yr))
    for(i in 1:length(yr)){
      if(yr[[i]][2]!="InitAge") yr2[i] <- as.numeric(yr[[i]][length(yr[[i]])])
    }
    # if label is something like "Main_InitAge_19" then the year
    # is the minimum of the years from the normal RecrDev labels
    minyr2 <- min(yr2) 
    for(i in (1:length(yr))[is.na(yr2)]){
      if(yr[[i]][2]=="InitAge") yr2[i] <- minyr2 - as.numeric(yr[[i]][length(yr[[i]])])
    }
    
    yr2[is.na(yr2)] <- min(yr2,na.rm=T) - sum(is.na(yr2)):1
    val <- par_mat$Value[rowrange]
    std <- par_mat$Parm_StDev[rowrange]
    return(data.frame(yr=yr2,val=val,std=std))
  }

  optimfun <- function(yr,std,startvalues){
    # run the optimizationt to find best fit values
    if(altmethod=="nlminb"){
      biasopt <- nlminb(start=startvalues, objective=biasadjfit, gradient = NULL,
                        hessian = NULL, scale = 1, control = list(maxit=1000),
                        lower = c(-Inf,-Inf,-Inf,-Inf,0), upper = Inf,
                        yr=yr,std=std,sigmaR=sigma_R_in,transform=transform)
    }
    if(altmethod=="psoptim"){
      #### the following commands no longer needed since packages are required by r4ss
      ## require(pso)
      biasadjfit(pars=startvalues,yr=yr,std=std,sigmaR=sigma_R_in,transform=transform)
      biasopt <- psoptim(par=startvalues,fn=biasadjfit,yr=yr,std=std,
                       sigmaR=sigma_R_in,transform=transform,
                       control=list(maxit=1000,trace=TRUE),lower=rep(-1e6,5),upper=rep(1e6,5))
    }
    if(!(altmethod %in% c("nlminb","psoptim"))){
      biasopt <- optim(par=startvalues,fn=biasadjfit,yr=yr,std=std,
                       sigmaR=sigma_R_in,transform=transform,
                       method=method,control=list(maxit=1000))
    }
    return(biasopt)
  }

  biasadjfun <- function(yr,vec,transform=transform){
    # calculate the bias adjustment for every year as a function of the parameters
    if(transform) vec2 <- removeoffsets(vec) else vec2 <- vec
    last_no     <- vec2[1]
    first_full  <- vec2[2]
    last_full   <- vec2[3]
    first_no    <- vec2[4]
    max_biasadj <- vec2[5]

    biasadj <- rep(NA,length(yr))
    for(i in 1:length(yr)){
      y <- yr[i]

      if(y<=last_no){
        biasadj[i]=0.;
      }else{
        if(y<=first_full){
          biasadj[i] <- max_biasadj*(y-last_no)/(first_full-last_no);
        }else{
          if(y<=last_full){
            biasadj[i]=max_biasadj;
          }else{
            if(y<=first_no){
              biasadj[i]=max_biasadj*(1 - (y-last_full)/(first_no-last_full));
            }else{
              biasadj[i]=0.;
            }}}}}
    return(data.frame(yr=yr,biasadj=biasadj))
  }

  recdevs <- getrecdevs(replist)
  recdevs <- recdevs[!is.na(recdevs$std),]

  yr <- recdevs$yr
  val <- recdevs$val
  std <- recdevs$std

  # test for presence of estimated recruitment deviations
  if(max(val)==0 | length(val)==0){
    if(verbose) cat("no rec devs estimated in this model\n")
    return()
  }

  recdev_hi <- val + 1.96*std
  recdev_lo <- val - 1.96*std

  ylim <- range(recdev_hi,recdev_lo)
  cat('Now estimating alternative recruitment bias adjustment fraction...\n')
  newbias <- optimfun(yr=yr,std=std,startvalues=startvalues)

  plotbiasadj <- function(){
    if(twoplots){
      par(mfrow=c(2,1),mar=c(2,5,1,1),oma=c(3,0,0,0))
      plot(yr,yr,type='n',xlab="Year",
           ylab='Recruitment deviation',ylim=ylim)
      abline(h=0,col="grey")
      arrows(yr,recdev_lo,yr,recdev_hi,length=0.03,code=3,angle=90,lwd=1.2)
      points(yr,val,pch=16)
    }

    yvals <- 1-(std/sigma_R_in)^2
    plot(yr,yvals,xlab="Year",
         ylab='',
         ylim=range(min(yvals,0),1,1.3),type="b",yaxs='i')
    abline(h=0,col="grey")
    abline(h=1,col="grey")
    mtext(side=2,line=2.5,expression(1 - italic(SE(hat(r[y]))^2 / sigma[R])^2))
    
    # bias correction (2nd axis, scaled by ymax)
    if(shownew) lines(biasadjfun(yr,newbias[[1]],transform=transform),col=4,lwd=3,lty=1)
    legendlines <- 1
    if(shownew) legendlines <- 1:2
    lines(recruit$year,recruit$biasadj,col=2,lwd=3,lty=2)
    legend('topleft',col=c(2,4)[legendlines],lwd=3,lty=(2:1)[legendlines],
           inset=.01,cex=.9,bg=rgb(1,1,1,.8),box.col=NA,
           legend=c('bias adjust in model','estimated alternative')[legendlines])
    mtext(side=1,line=3,'Year')
  }

  #names
  names <- c(
  "#_last_early_yr_nobias_adj_in_MPD",
  "#_first_yr_fullbias_adj_in_MPD",
  "#_last_yr_fullbias_adj_in_MPD",
  "#_first_recent_yr_nobias_adj_in_MPD",
  "#_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)")
  
  newvals <- newbias[[1]]
  if(transform) newvals <- removeoffsets(newvals)
  newvals <- round(newvals,4)
  df <- data.frame(value=newvals,label=names)

  if(newbias$convergence!=0){
      cat("Problem with convergence, here is output from 'optim':\n")
      cat("##############################\n")
      print(newbias)
      cat("##############################\n")
  }

  cat('Estimated values:\n')
  print(format(df,justify="left"),row.names=FALSE)

  if(plot) plotbiasadj()
  if(print){
    file <- paste(plotdir,"/recruit_fit_bias_adjust.png",sep="")
    caption <-
      paste("Points are transformed variances. Red line shows current settings for",
            "for bias adjustment specified in control file.",
            "Blue line shows least squares estimate of alternative bias adjustment",
            "relationship for recruitment deviations (which may or may not be an",
            "improvement. For more information, see<br> \n",
            "<blockquote>Methot, R.D. and Taylor, I.G., 2011. Adjusting for bias ",
            "due to variability of estimated recruitments in fishery assessment ",
            "models. <i>Can. J. Fish. Aquat. Sci.</i>, 68:1744-1760.",
            "</blockquote><br> \n",
            "Estimated alternative inputs to SS control file associated ",
            "with blue line in figure: \n<pre>",
            sep="")
    for(iline in 1:4){
      caption <- paste(caption, format(round(df$value[iline],1),nsmall=1), "   ",
                       df$label[iline], " \n", sep="")
    }
    caption <- paste(caption, df$value[5], "   ", df$label[5], sep="")
    caption <- paste(caption,"  </pre>")
    
    plotinfo <- pngfun(file=file, caption=caption)
    plotbiasadj()
    dev.off()
  }

  
  if(!is.null(oldctl) & !is.null(newctl)){
    # modify a control file to include estimates if file names are provided
    ctlfile <- readLines(oldctl)
    # look for certain comments in file
    spot1 <- grep('last_early_yr',ctlfile)
    spot2 <- grep('max_bias_adj_in_MPD',ctlfile)
    if(spot1!=spot2-4) stop('error related to maxbias inputs in ctl file')
    # replace values
    ctlfile[spot1:spot2] <- newvals
    # write new file
    writeLines(ctlfile,newctl)
    cat('wrote new file to',newctl,'with values',paste(newvals,collapse=" "),"\n")
  }
  if(!is.null(plotinfo)) plotinfo$category <- "RecDev"
  return(invisible(list(newbias=newbias, df=df, plotinfo=plotinfo)))
}

