SSplotCatch <-
  function(replist,subplots=1:9,add=FALSE,areas=1,
           plot=TRUE,print=FALSE,
           fleetlty=1, fleetpch=1,
           fleetcols="default", fleetnames="default",
           lwd=1, areacols="default", areanames="default",
           forecastplot=TRUE,
           plotdir="default",showlegend=TRUE,
           legendloc="topleft",
           xlab="Year",
           labels=c("Harvest rate/Year", #1
             "Continuous F",             #2
             "Landings",                 #3
             "Total catch",              #4
             "Predicted Discards",       #5  # should add units
             "Discard fraction",         #6  # need to add by weight or by length
             "(mt)",                     #7
             "(numbers x1000)"),         #8
           catchasnumbers=FALSE,
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12)
{

  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  F_method                       <- replist$F_method
  timeseries                     <- replist$timeseries
  nseasons                       <- replist$nseasons
  nareas                         <- replist$nareas
  nfleets                        <- replist$nfleets
  nfishfleets                    <- replist$nfishfleets
  endyr                          <- replist$endyr
  FleetNames                     <- replist$FleetNames

  if(fleetnames[1]=="default") fleetnames <- FleetNames
  if(plotdir=="default") plotdir <- replist$inputs$dir

  if(length(fleetlty)<nfishfleets) fleetlty <- rep(fleetlty,nfishfleets)
  if(length(fleetpch)<nfishfleets) fleetpch <- rep(fleetpch,nfishfleets)

  if(fleetcols[1]=="default"){
    fleetcols <- rich.colors.short(nfishfleets)
    if(nfishfleets > 2) fleetcols <- rich.colors.short(nfishfleets+1)[-1]
  }

  if(areacols[1]=="default"){
    areacols  <- rich.colors.short(nareas)
    if(nareas > 2) areacols <- rich.colors.short(nareas+1)[-1]
  }

  if(catchasnumbers){
    labels[3] <- paste(labels[3],labels[8])
    labels[4] <- paste(labels[4],labels[8])
  }else{
    labels[3] <- paste(labels[3],labels[7])
    labels[4] <- paste(labels[4],labels[7])
  }


  # time series (but no forecast) quantities used for multiple plots
  timeseries$Yr <- timeseries$Yr + (timeseries$Seas-1)/nseasons
  ts <- timeseries[timeseries$Yr <= endyr+1,]

  # harvest rates
  if(F_method==1){
    Fstring <- "Hrate:_"
    ylabF <- labels[1]
  }else{ # for either continuous F or hybrid F (methods 2 and 3)
    Fstring <- "F:_"
    ylabF <- labels[2]
  }

  ### total landings (retained) & catch (encountered)
  goodrows <- ts$Area==1 & ts$Era %in% c("INIT","TIME")
  catchyrs <- ts$Yr[goodrows] # T/F indicator of the lines for which we want to plot catch
  if(catchasnumbers){
    retmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar("retain(N)"))=="retain(N)"])
    totcatchmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar("enc(N)"))=="enc(N)"])
  }else{
    retmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
    totcatchmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar("enc(B)"))=="enc(B)"])
  }
  totobscatchmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar("obs_cat"))=="obs_cat"])
  Hratemat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar(Fstring))==Fstring])

  # add total across areas
  if(nareas > 1){
    for(iarea in 2:nareas){
      arearows <- ts$Area==iarea & ts$Era %in% c("INIT","TIME")
      if(catchasnumbers){
        retmat <- retmat + as.matrix(ts[arearows, substr(names(ts),1,nchar("retain(N)"))=="retain(N)"])
        totcatchmat <- totcatchmat + as.matrix(ts[arearows, substr(names(ts),1,nchar("enc(N)"))=="enc(N)"])
      }else{
        retmat <- retmat + as.matrix(ts[arearows, substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
        totcatchmat <- totcatchmat + as.matrix(ts[arearows, substr(names(ts),1,nchar("enc(B)"))=="enc(B)"])
      }
      totobscatchmat <- totobscatchmat + as.matrix(ts[arearows, substr(names(ts),1,nchar("obs_cat"))=="obs_cat"])
      Hratemat  <- Hratemat  + as.matrix(ts[arearows, substr(names(ts),1,nchar(Fstring))==Fstring])
    }
  }

  # ghost is a fleet with no catch (or a survey for these purposes)
  ghost <- rep(TRUE,nfleets)
  ghost[(1:nfishfleets)[colSums(totcatchmat)>0]] <- FALSE
  discmat <- totcatchmat - retmat

  discfracmat <- discmat/totcatchmat
  discfracmat[totcatchmat==0] <- 0

  # generic function to plot catch, landings, discards or harvest rates
  linefunc <- function(ymat,ylab,addtotal=TRUE){
    if(addtotal & nfishfleets>1){
      ytotal <- rowSums(ymat)
      ymax <- max(ytotal)
    }else{
      ytotal <- rep(NA,nrow(ymat))
      ymax <- max(ymat)
    }
    plot(catchyrs, ytotal, ylim=c(0,ymax), xlab=xlab, ylab=ylab, type="o")
    abline(h=0,col="grey")
    abline(h=1,col="grey")
    for(f in 1:nfishfleets){
      if(max(ymat[,f])>0){
        lines(catchyrs, ymat[,f], type="o", col=fleetcols[f],
              lty=fleetlty[f], lwd=lwd, pch=fleetpch[f])
      }
    }
    if(showlegend & nfishfleets!=1){
      if(nfishfleets>1 & addtotal){
        legend(legendloc, lty=fleetlty[!ghost], lwd=lwd, pch=c(1,fleetpch[!ghost]),
               col=c("black",fleetcols[!ghost]), legend=c("Total",fleetnames[!ghost]), bty="n")
      }else{
        legend(legendloc, lty=fleetlty[!ghost], lwd=lwd, pch=fleetpch[!ghost], col=fleetcols[!ghost], legend=fleetnames[!ghost], bty="n")
      }
    }
  } # end linefunc

  # function for stacked polygons
  stackfunc <- function(ymat,ylab){
    ## call to embedded, modified function
    stackpoly(x=catchyrs, y=ymat, border="black",
              xlab=xlab, ylab=ylab, col=fleetcols)
    if(showlegend) legend(legendloc, fill=fleetcols[!ghost], legend=fleetnames[!ghost], bty="n")
  } # end stackfunc

  makeplots <- function(subplot){
    if(subplot==1) linefunc(ymat=retmat, ylab=labels[3], addtotal=TRUE)
    if(subplot==2 & nfishfleets>1) stackfunc(ymat=retmat, ylab=labels[3])
    # if observed catch doesn't equal estimated, make plot to compare
    if(subplot==3 & diff(range(retmat-totobscatchmat))>0){
      linefunc(ymat=retmat, ylab=paste("Observed and expected",labels[3]), addtotal=TRUE)
      for(f in 1:nfishfleets){
        if(max(totobscatchmat[,f])>0){
          lines(catchyrs, totobscatchmat[,f], type="o", col=fleetcols[f],
                lty=3, lwd=lwd, pch=4)
        }
      }
      legend(legendloc, lty=c(fleetlty[!ghost],rep(3,sum(!ghost))), lwd=lwd, pch=c(fleetpch[!ghost],rep(4,sum(!ghost))), col=fleetcols[!ghost], legend=c(fleetnames[!ghost],paste(fleetnames[!ghost],"obs.")), bty="n")
    }
    if(max(discmat)>0){
      if(subplot==4) linefunc(ymat=totcatchmat, ylab=labels[4], addtotal=TRUE)
      if(subplot==5 & nfishfleets>1) stackfunc(ymat=totcatchmat, ylab=labels[4])
      if(subplot==6) linefunc(ymat=discmat,ylab=labels[5], addtotal=TRUE)
      if(subplot==7 & nfishfleets>1) stackfunc(ymat=discmat,ylab=labels[5])
      if(subplot==8) linefunc(ymat=discfracmat,ylab=labels[6], addtotal=FALSE)
    }
    if(subplot==9) linefunc(ymat=Hratemat, ylab=ylabF, addtotal=FALSE)
  } # end makeplots

  if(plot) for(isubplot in 1:9) makeplots(isubplot)
  namesvec <- paste(plotdir,c("06_landings.png",             #1
                              "06_landings_stacked.png",     #2
                              "06_obs_vs_exp_landings.png",  #3
                              "06_totcatch.png",             #4
                              "06_discards.png",             #5
                              "06_discardfraction.png",      #6
                              "06_totcatch_stacked.png",     #7
                              "06_discards_stacked.png",     #8
                              "06_harvestrates.png"),sep='/')#9
  if(print) for(isubplot in 1:9){
    pngfun(namesvec[isubplot])
    makeplots(isubplot)
    dev.off()
  }
}
