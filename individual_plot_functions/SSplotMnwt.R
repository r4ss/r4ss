SSplotMnwt <-
  function(replist,add=FALSE,plot=TRUE,print=FALSE,
           fleets="all",
           fleetnames="default",
           labels=c("Year",  #1
           "discard",        #2
           "retained catch", #3
           "whole catch",    #4
           "Mean individual body weight (kg)", #5
           "Mean weight in", #6
           "for fleet"),     #7
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,
           plotdir="default",
           verbose=TRUE)
{
  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  # get stuff from replist
  mnwgt         <- replist$mnwgt
  FleetNames    <- replist$FleetNames

  if(fleetnames[1]=="default") fleetnames <- FleetNames
  if(plotdir=="default") plotdir <- replist$inputs$dir

  # average body weight observations ###
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
        ptitle <- paste(lables[6],titlepart,labels[7],fleetname,sep=" ")
        ylab <- labels[4]
        bdywtfunc <- function(){
          plotCI(x=yr,y=ob,uiw=uiw,liw=liw,xlab=labels[1],main=ptitle,ylo=0,col="red",sfrac=0.001,z=ymax,ylab=ylab,lty=1,xlim=c(xmin,xmax))
          abline(h=0,col="grey")
          points(yr,ex,col="blue",cex=2,pch="-")}
        if(plot) bdywtfunc()
        if(print){
          png(file=paste(plotdir,"9_bodywtfit",fleetname,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
          bdywtfunc()
          dev.off()}
      } # loop over market categories
    } # loop over fleets
    if(verbose) print("Finished average body weight plot",quote=FALSE)
  }else{ # if mean weight data exists
    if(verbose) print("No average body weight data to plot",quote=F)
  }
  flush.console()
} # end if 10 in plot or print
