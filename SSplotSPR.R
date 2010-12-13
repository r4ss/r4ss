SSplotSPR <-
  function(replist,add=FALSE,plot=TRUE,print=FALSE,
           uncertainty=TRUE,
           subplots=1:4,
           col1="black",col2="blue",col3="green3",col4="red",
           sprtarg=0.4, btarg=0.4,
           labels=c("Year", #1
             "SPR",         #2
             "1-SPR"),      #3
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           plotdir="default",
           verbose=TRUE)
{
  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
  if(plotdir=="default") plotdir <- replist$inputs$dir

  sprseries             <- replist$sprseries
  timeseries            <- replist$timeseries
  derived_quants        <- replist$derived_quants
  nsexes                <- replist$nsexes
  nseasons              <- replist$nseasons
  endyr                 <- replist$endyr
  managementratiolabels	<- replist$managementratiolabels

  sprfunc <- function(){
    plot(sprseries$Year,sprseries$spr,xlab=labels[1],ylab=labels[2],ylim=c(0,max(1,max(sprseries$spr[!is.na(sprseries$spr)]))),type="o",col=col2)
    if(sprtarg>0) abline(h=sprtarg,col=col4,lty=2)
    abline(h=0,col="grey")
    abline(h=1,col="grey")}
  if(1 %in% subplots){
    if(plot) sprfunc()
    if(print){
      pngfun(file=paste(plotdir,"11_sprseries.png",sep=""))
      sprfunc()
      dev.off()
    }
  }

  if(nseasons>1) cat("Skipped 1-SPR series plot because it's not yet configured for multi-season models\n")
  if(nseasons==1){ # temporary disable until code cleanup
    sprfunc2 <- function(){
      plot(sprseries$Year,(1-sprseries$spr),xlab=labels[1],ylab=labels[3],ylim=c(0,1),type="o",col=col2)
      if(sprtarg>0) abline(h=(1-sprtarg),col=col4,lty=2)
      abline(h=0,col="grey")
      abline(h=1,col="grey")}

    if(2 %in% subplots){
      if(plot) sprfunc2()
      if(print){
        pngfun(file=paste(plotdir,"11_1minussprseries.png",sep=""))
        sprfunc2()
        dev.off()
      }
    }

    if(uncertainty & sprtarg>0){
      sprratiostd <- derived_quants[substring(derived_quants$LABEL,1,8)=="SPRratio",]
      sprratiostd$Yr <- as.numeric(substring(sprratiostd$LABEL,10))
      sprratiostd$period <- "fore"
      sprratiostd$period[sprratiostd$Yr<=(endyr)] <- "time"
      sprratiostd$upper <- sprratiostd$Value + 1.96*sprratiostd$StdDev
      sprratiostd$lower <- sprratiostd$Value - 1.96*sprratiostd$StdDev
      ylab <- managementratiolabels[1,2]
      ylim=c(0,max(1,sprratiostd$upper[sprratiostd$period=="time"]))
      sprfunc3 <- function(){
        plot(sprratiostd$Yr[sprratiostd$period=="time"],sprratiostd$Value[sprratiostd$period=="time"],xlab=labels[1],ylim=ylim,ylab=ylab,type="o",col=col2)
        abline(h=0,col="grey")
        abline(h=1,col=col4)
        text((min(sprratiostd$Yr)+4),(1+0.02),"Management target",adj=0)
        lines(sprratiostd$Yr[sprratiostd$period=="time"],sprratiostd$upper[sprratiostd$period=="time"],col=col2,lty="dashed")
        lines(sprratiostd$Yr[sprratiostd$period=="time"],sprratiostd$lower[sprratiostd$period=="time"],col=col2,lty="dashed")
      }
      if(3 %in% subplots){
        if(plot) sprfunc3()
        if(print){
          pngfun(file=paste(plotdir,"11_sprratiointerval.png",sep=""))
          sprfunc3()
          dev.off()
        }
      }
    }

    if(btarg<=0 | sprtarg<=0){
      cat("skipped SPR phase plot (in group 11) because btarg or sprtarg <= 0\n")
    }else{
      timeseries$Yr <- timeseries$Yr + (timeseries$Seas-1)/nseasons
      ts <- timeseries[timeseries$Area==1 & timeseries$Yr <= endyr+1,] #!subsetting to area 1 only. This should be generalized
      tsyears <- ts$Yr[ts$Seas==1]
      tsspaw_bio <- ts$SpawnBio[ts$Seas==1]
      if(nsexes==1) tsspaw_bio <- tsspaw_bio/2
      depletionseries <- tsspaw_bio/tsspaw_bio[1]
      reldep <- depletionseries[tsyears %in% sprseries$Year]/btarg
      relspr <- (1-sprseries$spr)/(1-sprtarg)
      xmax <- 1.1*max(reldep)
      ymax <- 1.1*max(1,relspr[!is.na(relspr)])
      phasefunc <- function(){
        plot(reldep,relspr,xlab="B/Btarget",xlim=c(0,xmax),ylim=c(0,ymax),ylab="(1-SPR)/(1-SPRTarget)",type="o",col=col2)
        abline(h=0,col="grey")
        abline(v=0,col="grey")
        lines(reldep,relspr,type="o",col=col2)
        points(reldep[length(reldep)],relspr[length(relspr)],col=col4,pch=19)
        abline(h=1,col=col4,lty=2)
        abline(v=1,col=col4,lty=2)}

      if(4 %in% subplots){
        if(plot) phasefunc()
        if(print){
          pngfun(file=paste(plotdir,"11_sprphase.png",sep=""))
          phasefunc()
          dev.off()
        }
      }

      if(verbose) cat("Finished SPR plots\n")
    }
    flush.console()
  } # end temporary multi-season disable of section if nseasons>1
}
