SSplotMovementRates <-
  function(replist=NULL, colvec="default", ylim="default",
           legend=TRUE, legendloc="topleft", moveseas=1, cex.main=1)
{
  accuage <- replist$accuage
  move <- replist$movement
  move2 <- move[move$Seas==moveseas &
                move$Source_area!=move$Dest_area,]
  if(nrow(move2)==0){
    cat("Skipping movement rate plot: no movement in season",moveseas,"\n")
  }else{
    move3 <- move2[,-(1:6)]
    
    if(colvec[1]=="default") colvec=rich.colors.short(nrow(move2))
    if(ylim[1]=="default") ylim=c(0,1.1*max(move))
    matplot(0:accuage,t(move3),
            type='l',lwd=3,lty=1,col=colvec,
            ylab="Movement rate",xlab="Age (years)",
            main=paste("Movement rates\n(fraction moving per year in season ",moveseas,")",sep=""),
            cex.main=cex.main)
    abline(h=0,col='grey')
    if(legend){
      legend(legendloc,lwd=3,bty="n",
             col=colvec,
             legend=paste("area",move2$Source_area,"to area",move2$Dest_area)
             )
    }
  }
}
