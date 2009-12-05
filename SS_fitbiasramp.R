SS_fitbiasramp <-
function(replist){
  ##################
  # function to estimate bias adjustment ramp
  # for Stock Synthesis v3.04b
  # by Ian Taylor
  # December 2, 2009
  #
  # Usage: run function with input that is an object from SSv3_output
  #        from http://code.google.com/p/r4ss/
  #
  ##################
  if(!is.list(replist) | substr(replist$SS_version,1,8)!="SS-V3.04"){
    print("!error: this function needs an input object created by SSv3_output from a SSv3.04 model")
    return()
  }
  if(replist$inputs$covar==F){
    print("!error, you need to have covar=T in the input to the SSv3_output function",quote=F)
    return()
  }
  parameters <- replist$parameters
  startyr    <- replist$startyr
  recruit    <- replist$recruit
  sigma_R_in <- replist$sigma_R_in

  startvalues <- c(min(recruit$year),
                   range(recruit$year[recruit$biasadj==max(recruit$biasadj)]),
                   max(recruit$year),
                   max(recruit$biasadj))

  biasadjfit <- function(pars,yr,std,sigmaR){
    biasadj <- biasadjfun(yr,pars)$biasadj
    compare <- 1 - (std/sigmaR)^2
    fit <- sum((biasadj-compare)^2)
    return(fit)
  }

  getrecdevs <- function(replist){
    parmat <- replist$parameters
    rowrange <- (grep("SR_autocorr",parmat$Label)+1):(grep("InitF",parmat$Label)[1]-1)
    val <- parmat$Value[rowrange]
    std <- parmat$Parm_StDev[rowrange]
    return(data.frame(val=val,std=std))
  }

  optimfun <- function(yr,std,startvalues){
    biasopt <- optim(par=startvalues,fn=biasadjfit,yr=yr,std=std,
                     sigmaR=sigma_R_in,method="L-BFGS-B")
    return(biasopt)
  }

  biasadjfun <- function(yr,vec){
    last_no     <- vec[1]
    first_full  <- vec[2]
    last_full   <- vec[3]
    first_no    <- vec[4]
    max_biasadj <- vec[5]

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
  val <- recdevs$val
  std <- recdevs$std

  if(max(val)==0 | length(val)==0){
    if(verbose) print("no rec devs estimated in this model",quote=F)
    return()
  }else{

    recdev_hi <- val + 1.96*std
    recdev_lo <- val - 1.96*std
    Yr <- recruit$year
    ylim <- range(recdev_hi,recdev_lo)

    par(mfrow=c(2,1),mar=c(2,5,1,1),oma=c(3,0,0,0))
    plot(Yr,Yr,type='n',xlab="Year",
         ylab='Recruitment deviation',ylim=ylim)
    abline(h=0,col="grey")
    arrows(Yr,recdev_lo,Yr,recdev_hi,length=0.03,code=3,angle=90,lwd=1.2)
    points(Yr,val,pch=16)
  }

  print('estimating...',quote=F)
  newbias <- optimfun(yr=Yr,std=std,startvalues=startvalues)

  yvals <- 1-(std/sigma_R_in)^2
  plot(Yr,yvals,xlab="Year",
       ylab=expression(1 - italic(SE(hat(R[i]))^2 / sigma[R])^2),
       ylim=range(0,1,1.3),type="b",yaxs='i')
  abline(h=0,col="grey")
  abline(h=1,col="grey")

  #names
  names <- c(
  "#_last_early_yr_nobias_adj_in_MPD",
  "#_first_yr_fullbias_adj_in_MPD",
  "#_last_yr_fullbias_adj_in_MPD",
  "#_first_recent_yr_nobias_adj_in_MPD",
  "#_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)")

  # bias correction (2nd axis, scaled by ymax)
  lines(biasadjfun(Yr,newbias[[1]]),col=4,lwd=3,lty=1)
  lines(recruit$year,recruit$biasadj,col=2,lwd=3,lty=2)
  legend('topleft',col=c(2,4),lwd=3,lty=2:1,inset=.01,cex=.9,bg=rgb(1,1,1,.8),box.col=NA,
         leg=c('bias adjust in model','estimated alternative'))
  mtext(side=1,line=3,'Year')
  newvals <- newbias[[1]]
  newvals <- round(newvals,4)
  df <- data.frame(value=newvals,label=names)
  print('Estimate values:',quote=F)
  print(format(df,justify="left"),row.names=F)
#  return(df)
}

