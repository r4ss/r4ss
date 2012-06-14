SSplotSpawnrecruit <-
  function(replist,subplot=1:2,add=FALSE,plot=TRUE,print=FALSE,xlim=NULL,ylim=NULL,
           xlab="Spawning biomass (mt)",
           ylab="Recruitment (1,000s)",
           plotdir="default",
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           verbose=TRUE,line1="blue",line2="green3",line3="black",
           minyr="default",textmindev=0.5,
           ptcol="red",virg=TRUE,init=FALSE,forecast=FALSE)
{
  # plot of spawner recruit curve

  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  recruit <- replist$recruit

  if(plotdir=="default") plotdir <- replist$inputs$dir
  if(minyr=="default") minyr <- min(recruit$year)

  recruit <- recruit[recruit$era %in% c("Early","Main","Fixed","Late",
                                        ifelse(forecast,"Forecast",NA)) &
                     recruit$year>=minyr,]
  
  timeseries <- replist$timeseries

  if(is.null(ylim)) ylim=c(0, max(recruit$pred_recr, recruit$exp_recr, recruit$adjusted))
  x <- recruit$spawn_bio
  if(is.null(xlim)) xlim=c(0, max(x))
  recruitfun <- function(text=FALSE){
    if(!add) plot(0,type='n',ylim=ylim,xlim=xlim,xlab=xlab,ylab=ylab)
    lines(x[order(x)],recruit$with_env[order(x)],col=line1)
    abline(h=0,col="grey")
    abline(v=0,col="grey")
    # lines(x[order(x)],recruit$adjusted[order(x)],col=line2)
    lines(x[order(x)],recruit$exp_recr[order(x)],lwd=2,col=line3)
    lines(x,recruit$adjusted,col=line2)
    points(x,recruit$pred_recr,col=ptcol)
    if(text){
      # only label values with larger devs (in abs value)
      show <- abs(recruit$dev) > textmindev
      show[1] <- show[length(show)] <- TRUE  # also include first & last years
      text(x[show],recruit$pred_recr[show],labels=recruit$year[show], pos=2, cex=.7)
    }
    if(virg) points(sum(timeseries$SpawnBio[timeseries$Era=="VIRG"]),
                    sum(timeseries$Recruit_0[timeseries$Era=="VIRG"]),
                    pch='+',cex=1.5)
    if(init) points(sum(timeseries$SpawnBio[timeseries$Era=="INIT"]),
                    sum(timeseries$Recruit_0[timeseries$Era=="INIT"]),
                    pch='x',cex=1.5)

  }
  if(plot){
    if(1 %in% subplot) recruitfun()
    if(2 %in% subplot) recruitfun(text=TRUE)
  }    
  if(print){
    if(1 %in% subplot){
      file <- paste(plotdir,"/SR_curve.png",sep="")
      caption <- "Spawner-recruit curve"
      plotinfo <- pngfun(file=file, caption=caption)
      recruitfun()
      dev.off()
    }
    if(2 %in% subplot){
      file <- paste(plotdir,"/SR_curve2.png",sep="")
      caption <- paste("Spawner-recruit curve with labels on first, last, and years with (log) deviations >",textmindev)
      plotinfo <- pngfun(file=file, caption=caption)
      recruitfun(text=TRUE)
      dev.off()
    }
  }
  if(!is.null(plotinfo)) plotinfo$category <- "S-R"
  return(invisible(plotinfo))
}
