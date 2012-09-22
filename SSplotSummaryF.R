SSplotSummaryF <- function(replist,yrs="all",Ftgt=NA,ylab="Summary Fishing Mortality") {
    #plots the summary F (or harvest rate) as set up in the starter file
    #needs a lot of work to be generalized
    if(yrs=="all") {yrs <- replist$startyr:replist$endyr}
    Ftot <- replist$derived_quants[match(paste("F_",yrs,sep=""),replist$derived_quants$LABEL),]
    uppFtot <- Ftot$Value + 1.96*Ftot$StdDev
    lowFtot <- Ftot$Value - 1.96*Ftot$StdDev
    Fmax <- max(c(uppFtot,Ftgt+0.01),na.rm=T)
    plot(as.numeric(substring(Ftot$LABEL,3,6)),Ftot$Value,pch=16,type="p",xlab="Year",ylab=ylab,ylim=c(0,Fmax),cex.lab=1.0,cex.axis=1.0,cex=0.7)
    abline(h=Ftgt,col="red")
    segments(as.numeric(substring(Ftot$LABEL,3,6)),uppFtot,as.numeric(substring(Ftot$LABEL,3,6)),lowFtot,col=gray(0.5))
}
