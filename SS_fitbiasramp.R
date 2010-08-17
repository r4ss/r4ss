SS_fitbiasramp <-
function(replist, verbose=FALSE, startvalues=NULL, method="BFGS", twoplots=TRUE,
         transform=FALSE, png=FALSE, pdf=FALSE, oldctl=NULL, newctl=NULL,
         pwidth=7, pheight=7, punits="in", ptsize=12, res=300){
  ##################
  # function to estimate bias adjustment ramp
  # for Stock Synthesis v3.10b
  # by Ian Taylor
  # June 28, 2010
  #
  # Usage: run function with input that is an object from SS_output
  #        from http://code.google.com/p/r4ss/
  #
  ##################

  # note, method is choices that go into optim:
  #  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")

  if(!is.list(replist) | !(substr(replist$SS_version,1,8) %in% c("SS-V3.04","SS-V3.1-","SS-V3.10"))){
    print("!error: this function needs an input object created by SS_output from a SSv3.10 model")
    return()
  }
  if(replist$inputs$covar==FALSE){
    print("!error, you need to have covar=TRUE in the input to the SS_output function",quote=FALSE)
    return()
  }
  parameters <- replist$parameters
  startyr    <- replist$startyr
  recruit    <- replist$recruit
  sigma_R_in <- replist$sigma_R_in

  if(is.null(startvalues)){
      nonfixedyrs <- recruit$year[recruit$era!="Fixed"]
      mainyrs <- recruit$year[recruit$era=="Main"]
      startvalues <- c(min(nonfixedyrs),
                       min(mainyrs) + .3*diff(range(mainyrs)),
                       max(mainyrs) - .1*diff(range(mainyrs)),
                       max(nonfixedyrs),
                       .7)
  }
  if(verbose) print(paste("startvalues =",paste(startvalues,collapse=", ")),quote=FALSE)

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
  if(verbose & transform) print(paste("transformed startvalues =",paste(startvalues,collapse=", ")),quote=FALSE)

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
    parmat <- replist$parameters
    rowrange <- (grep("SR_autocorr",parmat$Label)+1):(grep("InitF",parmat$Label)[1]-1)
    yr <- parmat$Label[rowrange]
    yr <- strsplit(yr,"_")
    yr2 <- matrix(NA,length(yr),2)
    yr2 <- rep(NA,length(yr))
    for(i in 1:length(yr)) yr2[i] <- as.numeric(yr[[i]][length(yr[[i]])])
    yr2[is.na(yr2)] <- min(yr2,na.rm=T) - sum(is.na(yr2)):1
    val <- parmat$Value[rowrange]
    std <- parmat$Parm_StDev[rowrange]
    return(data.frame(yr=yr2,val=val,std=std))
  }

  optimfun <- function(yr,std,startvalues){
    # run the optimizationt to find best fit values
    biasopt <- optim(par=startvalues,fn=biasadjfit,yr=yr,std=std,
                     sigmaR=sigma_R_in,transform=transform,
                     method=method,control=list(maxit=1000))
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

  
  if(max(val)==0 | length(val)==0){
    if(verbose) print("no rec devs estimated in this model",quote=FALSE)
    return()
  }else{
    recdev_hi <- val + 1.96*std
    recdev_lo <- val - 1.96*std

    ylim <- range(recdev_hi,recdev_lo)

    if(png!=FALSE & pdf!=FALSE){ print("must have either png or pdf equal to FALSE",quote=FALSE)
                         return()
                       }
    if(png!=FALSE) png(file=png,width=pwidth,height=pheight,
                   units=punits,res=res,pointsize=ptsize)
    if(pdf!=FALSE) pdf(file=pdf,width=pwidth,height=pheight,
                   pointsize=ptsize)
    if(twoplots){
      par(mfrow=c(2,1),mar=c(2,5,1,1),oma=c(3,0,0,0))
      plot(yr,yr,type='n',xlab="Year",
           ylab='Recruitment deviation',ylim=ylim)
      abline(h=0,col="grey")
      arrows(yr,recdev_lo,yr,recdev_hi,length=0.03,code=3,angle=90,lwd=1.2)
      points(yr,val,pch=16)
    }
  }

  print('estimating alternative recruitment bias adjustment fraction...',quote=FALSE)
  newbias <- optimfun(yr=yr,std=std,startvalues=startvalues)

  yvals <- 1-(std/sigma_R_in)^2
  plot(yr,yvals,xlab="Year",
       ylab='',
       ylim=range(0,1,1.3),type="b",yaxs='i')
  abline(h=0,col="grey")
  abline(h=1,col="grey")
  mtext(side=2,line=2.5,expression(1 - italic(SE(hat(r[y]))^2 / sigma[R])^2))

  #names
  names <- c(
  "#_last_early_yr_nobias_adj_in_MPD",
  "#_first_yr_fullbias_adj_in_MPD",
  "#_last_yr_fullbias_adj_in_MPD",
  "#_first_recent_yr_nobias_adj_in_MPD",
  "#_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)")

  # bias correction (2nd axis, scaled by ymax)
  lines(biasadjfun(yr,newbias[[1]],transform=transform),col=4,lwd=3,lty=1)
  lines(recruit$year,recruit$biasadj,col=2,lwd=3,lty=2)
  legend('topleft',col=c(2,4),lwd=3,lty=2:1,inset=.01,cex=.9,bg=rgb(1,1,1,.8),box.col=NA,
         leg=c('bias adjust in model','estimated alternative'))
  mtext(side=1,line=3,'Year')

  if(pdf!=FALSE | png!=FALSE) dev.off()

  newvals <- newbias[[1]]
  if(transform) newvals <- removeoffsets(newvals)
  newvals <- round(newvals,4)
  df <- data.frame(value=newvals,label=names)

  if(newbias$convergence!=0){
      print("Problem with convergence, here is output from 'optim':",quote=FALSE)
      print("##############################",quote=FALSE)
      print(newbias)
      print("##############################",quote=FALSE)
  }


  print('Estimated values:',quote=FALSE)
  print(format(df,justify="left"),row.names=FALSE)

  if(!is.null(oldctl) & !is.null(newctl)){
    # modify a control file to include estimates if file names are provided
    ctlfile <- readLines(oldctl)
    # look for certain comments in file
    spot1 <- grep('last_early_yr',ctlfile)
    spot2 <- grep('max_bias_adj_in_MPD',ctlfile)
    if(spot1!=spot2-4) print('error! related to maxbias inputs in ctl file')
    # replace values
    ctlfile[spot1:spot2] <- newvals
    # write new file
    writeLines(ctlfile,newctl)
    print(paste('wrote new file to',newctl,'with values',paste(newvals,collapse=" ")),quote=FALSE)
  }
  return(invisible(newbias))
}

