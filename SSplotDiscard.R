SSplotDiscard <-
  function(replist,add=FALSE,plot=TRUE,print=FALSE,
           plotdir="default",
           fleets="all",
           fleetnames="default",
           labels=c("Year",
           "Discard fraction",
           "Total discards",
           "for"),
           yhi=1,
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           verbose=TRUE)
{
  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  # get stuff from replist
  discard         <- replist$discard
  FleetNames      <- replist$FleetNames
  DF_discard      <- replist$DF_discard
  discard_type    <- replist$discard_type

  if(fleetnames[1]=="default") fleetnames <- FleetNames
  if(plotdir=="default") plotdir <- replist$inputs$dir

  # if discards exist
  if(length(discard)>1){
    for(fleetname in unique(discard$Fleet)){
      usedisc <- discard[discard$Fleet==fleetname,]
      yr <- as.numeric(usedisc$Yr)
      ob <- as.numeric(usedisc$Obs)
      std <- as.numeric(usedisc$Std_use)
      if(DF_discard == -2){ # lognormal with std as interpreted as the standard error (in log space) of the observation
        liw <- ob - qlnorm(0.025,log(ob),std)
        uiw <- qlnorm(0.975,log(ob),std) - ob
      }
      if(DF_discard == -1){ # normal with std as std
        liw <- ob - qnorm(0.025,ob,std)
        uiw <- qnorm(0.975,ob,std) - ob
      }
      if(DF_discard == 0){  # normal with std interpreted as CV
        liw <- ob - qnorm(0.025,ob,std*ob)
        uiw <- qnorm(0.975,ob,std*ob) - ob
      }
      if(DF_discard > 0){ # t-distribution with DF_discard = degrees of freedom
        liw <- -std*qt(0.025,DF_discard) # quantile of t-distribution
        uiw <- std*qt(0.975,DF_discard) # quantile of t-distribution
      }
      liw[(ob-liw)<0] <- ob[(ob-liw)<0] # no negative limits
      xlim <- c((min(yr)-3),(max(yr)+3))
      if(grepl("as_fraction",discard_type)){
        # discards as a fraction
        title <- paste("Discard fraction for",fleetname)
        ylab <- "Discard fraction"
      }else{
        # discards in same units as catch, or in numbers (should distinguish in the future)
        title <- paste("Total discard for",fleetname)
        ylab <- "Total discards"
      }
      dfracfunc <- function(){
        plotCI(x=yr,y=ob,z=0,uiw=uiw,liw=liw,ylab=ylab,xlab=labels[1],main=title,ylo=0,yhi=yhi,col="red",sfrac=0.001,lty=1,xlim=xlim,ymax=max(usedisc$Exp,na.rm=TRUE))
        abline(h=0,col="grey")
        points(yr,usedisc$Exp,col="blue",pch="-",cex=2)
      }
      if(plot) dfracfunc()
      if(print) {
        png(file=paste(dir,"discfracfit",fleetname,".png",sep=""),width=pwidth,height=pheight)
        dfracfunc()
        dev.off()
      }
    } # discard series
    if(verbose) print("Finished discard plot",quote=FALSE)
  }else{ # if discards
    if(verbose) print("No discard data to plot",quote=FALSE)
  }
} # end of function
