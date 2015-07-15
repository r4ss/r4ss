#Example from Widow assessment
# indCols <- c(rich.colors.short(max(base$cpue$Fleet)))
# indColsAlpha <- adjustcolor(indCols,alpha.f=0.7)

# doPNG <- T
# ht<-4;wd=6.5
# legCex <- 1

# #Bottom Trawl
# ind <- c(1)
# if(doPNG) {png(file.path(figDir,"Results/BottomTrawlFits.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
# if(!doPNG) {windows(height=ht,width=wd)}
# par(las=1,mar=c(5, 4, 1, 1) + 0.1,cex.axis=0.9)
# plotIndices(base, ind, cols=indCols[ind],div=1, xlim=c(1983,2000),ylim=c(20,800), alpha=0.6,
#              xlab="Year",ylab="CPUE index",log="y", legX="bottomleft", legCex=legCex)
# if(doPNG) {dev.off()}

# #Hake and Foreign
# ind <- c(3,9)
# if(doPNG) {png(file.path(figDir,"Results/HakeCpueFits.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
# if(!doPNG) {windows(height=ht,width=wd)}
# par(las=1,mar=c(5, 4, 1, 1) + 0.1,cex.axis=0.9)
# plotIndices(base, ind, cols=indCols[ind],div=1, xlim=c(1976,1999),ylim=c(0.03,15), alpha=0.6,
#              xlab="Year",ylab="CPUE index",log="y", legX="bottomright", shift=c(-0.1,0.1), legCex=legCex)
# if(doPNG) {dev.off()}

#Some thoughts:
### would be nice if it could use the readDat to only plot the data.
### should a change in q have a broken line?
### plot input and output confidence intervals to show effect of additional variation



plotIndices <- function(x, fleets, xlim=NULL, ylim=NULL,
                        cols="default", alpha=1, div=1, legX=NULL, legY=NULL, legCex=1,
                        plotExpect=T, shift=rep(0,length(fleets)),
                        scale=c("none","mean","max"), ...) {

    #I may want to figure out how to plot one series that has a break in q as two series
    #how to plot the input CI and the output CI with additional variance
    if(is.character(cols[1])) {
        if(cols[1]=="default") {
            cols <- rich.colors.short(length(fleets))
        }
    }
    colsAlpha <- adjustcolor(cols, alpha.f=alpha)
    cpue <- x$cpue[x$cpue$Fleet %in% fleets,]
    if(is.null(xlim)) xlim <- range(cpue$Yr)
        #I don't think this si working right????????
    if(is.null(ylim)) xlim <- range(c(qlnorm(.025,meanlog=log(tmp$Obs/div),sdlog=tmp$SE),
                                      qlnorm(.975,meanlog=log(tmp$Obs/div),sdlog=tmp$SE)))

    plot(0.001, type='n', xlim=xlim, xaxs='i', ylim=ylim, yaxs='i', ...)

    for(i in 1:length(fleets)) {
      tmp <- cpue[cpue$Fleet == fleets[i],]
      if(is.character(scale)) {  #if they enter a number, you can scale specifically by that
        tmp$Obs <- switch(scale[1],
                            none=tmp$Obs,
                            mean=tmp$Obs/mean(tmp$Obs),
                            max=tmp$Obs/max(tmp$Obs))
      }
      xx <- tmp$Yr + shift[i]
      segments(x0 = xx,
             y0=qlnorm(.025,meanlog=log(tmp$Obs/div),sdlog=tmp$SE),
             y1=qlnorm(.975,meanlog=log(tmp$Obs/div),sdlog=tmp$SE),
             lwd=2, lend=1, col=colsAlpha[i])
      points(xx,tmp$Obs/div,col=cols[i],cex=1.3,pch=20)
      if(plotExpect) lines(xx,tmp$Exp/div,col=cols[i],lwd=3)
    }
    if(!is.null(legX)) {legend(x=legX, y=legY, legend=unique(cpue$FleetName),
                               lty=1, pch=20, col=cols, cex=legCex)}
}