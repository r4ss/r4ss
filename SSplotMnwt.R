SSplotMnwt <-
  function(replist, subplots=1:2,
           plot=TRUE, print=FALSE,
           fleets="all",
           fleetnames="default",
           datplot=FALSE,
           labels=c("Year",  #1
           "discard",        #2
           "retained catch", #3
           "whole catch",    #4
           "Mean individual body weight (kg)", #5
           "Mean weight in", #6
           "for fleet"),     #7
           col1="blue", col2="red",
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,
           cex.main=1,
           plotdir="default", verbose=TRUE)
{
  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  # get stuff from replist
  mnwgt         <- replist$mnwgt
  FleetNames    <- replist$FleetNames
  DF_mnwgt      <- replist$DF_mnwgt

  if(fleetnames[1]=="default") fleetnames <- FleetNames
  if(plotdir=="default") plotdir <- replist$inputs$dir

  # mean body weight observations ###
  if(!is.na(mnwgt)[1]){
    for(fleetname in unique(mnwgt$Fleet)){
      usemnwgt <- mnwgt[mnwgt$Fleet==fleetname & mnwgt$Obs>0,]
      usemnwgt$Mkt <- usemnwgt$Mkt
      for(j in unique(mnwgt$Mkt)){
        yr <- usemnwgt$Yr[usemnwgt$Mkt==j]
        ob <- usemnwgt$Obs[usemnwgt$Mkt==j]
        cv <- usemnwgt$CV[usemnwgt$Mkt==j]
        ex <- usemnwgt$Exp[usemnwgt$Mkt==j]
        xmin <- min(yr)-3
        xmax <- max(yr)+3
        liw <- -ob*cv*qt(0.025,DF_mnwgt) # quantile of t-distribution
        uiw <- ob*cv*qt(0.975,DF_mnwgt) # quantile of t-distribution
        liw[(ob-liw)<0] <- ob[(ob-liw)<0] # no negative limits
        ymax <- max(ob + uiw)
        ymax <- max(ymax,ex)
        titlepart <- labels[2]
        if(j==2) titlepart <- labels[3]
        if(j==0) titlepart <- labels[4]
        ptitle <- paste(labels[6],titlepart,labels[7],fleetname,sep=" ")
        ylab <- labels[5]

        # wrap up plot command in function
        bdywtfunc <- function(addfit){
          plotCI(x=yr,y=ob,uiw=uiw,liw=liw,xlab=labels[1],main=ptitle,
                 ylo=0,col=col2,sfrac=0.001,ylab=ylab,lty=1,
                 xlim=c(xmin,xmax),cex.main=cex.main,ymax=ymax)
          abline(h=0,col="grey")
          if(addfit) points(yr,ex,col=col1,cex=2,pch="-")
        }

        # make plots
        if(!datplot) subplots <- setdiff(subplots,1) # don't do subplot 1 if datplot=FALSE
        for(isubplot in subplots){ # loop over subplots (data only or with fit)
          if(plot) bdywtfunc(addfit=addfit)
          if(print){
            file <- paste(plotdir,"bodywtfit_flt",fleetname,".png",sep="")
            caption <- ptitle
            plotinfo <- pngfun(file=file, caption=caption)
            bdywtfunc(addfit=addfit)
            dev.off()
          }
        } # end loop over subplots
      } # end loop over market categories
    } # end loop over fleets
  ##   if(verbose) cat("Finished mean body weight plot\n")
  ## }else{ # if mean weight data exists
  ##   if(verbose) cat("No mean body weight data to plot\n")
  }
  if(!is.null(plotinfo)) plotinfo$category <- "Mnwt"
  return(invisible(plotinfo))
}
