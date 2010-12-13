SSplotSpawnrecruit <-
  function(replist,add=FALSE,plot=TRUE,print=FALSE,xlim=NULL,ylim=NULL,
           xlab="Spawning biomass (mt)",
           ylab="Recruitment (1,000s)",
           plotdir="default",
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           verbose=TRUE,line1="blue",line2="green",line3="black",
           ptcol="red",virg=TRUE,init=FALSE)
{
  # plot of spawner recruit curve

  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
  if(plotdir=="default") plotdir <- replist$inputs$dir

  recruit <- replist$recruit
  recruit <- recruit[recruit$era %in% c("Main","Fixed","Late"),]
  timeseries <- replist$timeseries

  if(is.null(ylim)) ylim=c(0, max(recruit$pred_recr, recruit$exp_recr, recruit$adjusted))
  x <- recruit$spawn_bio
  if(is.null(xlim)) xlim=c(0, max(x))
  recruitfun <- function(){
    if(!add) plot(0,type='n',ylim=ylim,xlim=xlim,xlab=xlab,ylab=ylab)
    lines(x[order(x)],recruit$with_env[order(x)],col=line1)
    abline(h=0,col="grey")
    abline(v=0,col="grey")
    # lines(x[order(x)],recruit$adjusted[order(x)],col=line2)
    lines(x[order(x)],recruit$exp_recr[order(x)],lwd=2,col=line3)
    lines(x,recruit$adjusted,col=line2)
    points(x,recruit$pred_recr,col=ptcol)

    if(virg) points(sum(timeseries$SpawnBio[timeseries$Era=="VIRG"]),
                    sum(timeseries$Recruit_0[timeseries$Era=="VIRG"]),
                    pch='+',cex=1.5)
    if(init) points(sum(timeseries$SpawnBio[timeseries$Era=="INIT"]),
                    sum(timeseries$Recruit_0[timeseries$Era=="INIT"]),
                    pch='x',cex=1.5)
  }
  if(plot) recruitfun()
  if(print){
    pngfun(file=paste(plotdir,"12_srcurve.png",sep=""))
    recruitfun()
    dev.off()}
  if(verbose) cat("Finished plot 12: Spawner-recruit curve\n")
}
