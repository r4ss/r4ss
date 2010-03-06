DoProjectPlots<-function(dirn="C:/NWFSC/NWFSC25D/",fileN=c("res.csv"),Titles="",ncols=200,Plots=1:25,Options=c(1:9),LegLoc="bottomright")
{
 windows(record=T)

#  ==================================================================================================

Net_Spawn_Graph<-function(UUU,Amin,Amax,Title)
{
 Ipnt <- which(UUU=="# Age Fecu")+1

 Xvals <- as.double(UUU[Ipnt:(Ipnt+Amax*5-Amin),1])
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Amax*5-Amin),2])
 par(mfrow=c(2,2))
 plot(Xvals,Yvals,xlab="Age (years)",ylab="Net Spawning Output",lty=1,type='l',lwd=2,xaxs="i",yaxs="i",ylim=c(0,1.05*max(Yvals)))
 title(Title)

}

#  ==================================================================================================

RecruitmentPlots<-function(UUU,Title)
{
 par(mfrow=c(2,2))

 Ipnt <- which(UUU=="# Recruitments")+2
 Npnt <- as.double(UUU[Ipnt-1,1])
 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2])
 plot(Xvals,Yvals,xlab="Year",ylab="Recruitment",lty=1,type='l',lwd=2,yaxs="i",ylim=c(0,1.05*max(Yvals)))
 title(Title)

 Ipnt <- which(UUU=="# Recruits-per-spawner")+2
 Npnt <- as.double(UUU[Ipnt-1,1])
 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2])
 plot(Xvals,Yvals,xlab="Year",ylab="Recruits \\ Spawning Output",lty=1,type='l',lwd=2,yaxs="i",ylim=c(0,1.05*max(Yvals)))

}
#  ==================================================================================================

B0Dist<-function(UUU,Title)
{
 par(mfrow=c(2,2))

 Ipnt <- which(UUU=="# B0 Dist")+1
 Xvals <- as.double(UUU[Ipnt:(Ipnt+19),1])
 Yvals <- as.double(UUU[Ipnt:(Ipnt+19),2])
 plot(Xvals,Yvals,xlab=expression(B[0]),ylab="Relative Density",type='n',yaxs="i",ylim=c(0,1.05*max(Yvals)))
 Inc <- (Xvals[2]-Xvals[1])/2
 for (II in 1:20)
  {
   xx <- c(Xvals[II]-Inc,Xvals[II]-Inc,Xvals[II]+Inc,Xvals[II]+Inc)
   yy <- c(0,Yvals[II],Yvals[II],0)
   polygon(xx,yy,col="gray")
  }
 title(Title)

}

#  ==================================================================================================

RecHist<-function(UUU,Title)
{
 par(mfrow=c(2,2))

 Ipnt <- which(UUU=="# Recovery Histogram")+2
 Npnt <- as.double(UUU[Ipnt-1,1])
 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2])
 plot(Xvals,Yvals,xlab=expression(T[min] -Y[init]),ylab="Proportion of Simulations",type='n',yaxs="i",ylim=c(0,1.05*max(Yvals)))
 Inc <- (Xvals[2]-Xvals[1])/2
 for (II in 1:Npnt)
  {
   xx <- c(Xvals[II]-Inc,Xvals[II]-Inc,Xvals[II]+Inc,Xvals[II]+Inc)
   yy <- c(0,Yvals[II],Yvals[II],0)
   polygon(xx,yy,col="gray")
  }
 title(Title)

 Npnt <- as.double(UUU[Ipnt-1,2])
 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),4])
 plot(Xvals,Yvals,xlab=expression(T[max] -Y[init]),ylab="Proportion of Simulations",type='n',yaxs="i",ylim=c(0,1.05*max(Yvals)))
 Inc <- (Xvals[2]-Xvals[1])/2
 for (II in 1:Npnt)
  {
   xx <- c(Xvals[II]-Inc,Xvals[II]-Inc,Xvals[II]+Inc,Xvals[II]+Inc)
   yy <- c(0,Yvals[II],Yvals[II],0)
   polygon(xx,yy,col="gray")
  }

}

# =============================================================================================================

AltStrategies<-function(UUU,Options,Title)
{
 par(mfrow=c(2,2))

 Ipnt <- which(UUU=="# Recovery Histogram")
 Tmax <- as.double(UUU[Ipnt-1,1])
 Yinit <-as.double(UUU[Ipnt-7,1])
 MinRev <-as.double(UUU[Ipnt-10,1])
 Tmin <- MinRev+Yinit

 Ipnt <- which(UUU=="# Summary 1")+3
 Npnt <- as.double(UUU[Ipnt-2,1])

 titles <- UUU[Ipnt-1,3:11]

 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),3])
 plot(Xvals,Yvals,xlab="Year",ylab="Probability Above Target",type='n',yaxs="i",ylim=c(0,1.05))
 for (II in 1:9)
  if (II %in% Options)
   {
    Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2+II])
    lines(Xvals,Yvals,lty=II)
   }
 abline(h=0.5,lwd=3)
 abline(v=Tmin,lwd=1,lty=2)
 abline(v=Tmax,lwd=1,lty=2)
 title(Title)

 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 ymax <- 0
 for (II in 1:9)
  if (II %in% Options)
   {
    Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2+10+II])
    if (max(Yvals)  > ymax) ymax <- max(Yvals)
   }
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2+10+1])
 plot(Xvals,Yvals,xlab="Year",ylab="Catch (t)",type='n',yaxs="i",ylim=c(0,1.05*ymax))
 for (II in 1:9)
  if (II %in% Options)
   {
    Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2+10+II])
    lines(Xvals,Yvals,lty=II)
   }

 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 ymax <- 0
 for (II in 1:9)
  if (II %in% Options)
   {
    Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2+20+II])
    if (max(Yvals)  > ymax) ymax <- max(Yvals)
   }
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2+20+1])
 plot(Xvals,Yvals,xlab="Year",ylab="Spawning Output \\ Target",type='n',yaxs="i",ylim=c(0,1.05*ymax))
 for (II in 1:9)
  if (II %in% Options)
   {
    Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2+20+II])
    lines(Xvals,Yvals,lty=II)
   }

 plot(0,0,xlab="",ylab="",axes=F)
 legend(LegLoc,legend=titles[Options],lty=Options,cex=1)

}
# =============================================================================================================

IndividualPlots<-function(UUU,Title)
{
 par(mfrow=c(2,2))

 Ipnt <- which(UUU=="# Individual")+2
 Npnt <- as.double(UUU[Ipnt-1,1])

 PlotA(UUU,0,"Spawning Output \\ Target",Ipnt,Npnt)
 title(Title)
 PlotA(UUU,6,"Catch (t)",Ipnt,Npnt)
 PlotA(UUU,12,"Recruitment",Ipnt,Npnt)
 PlotA(UUU,18,expression(paste("Fishing Mortality ", (yr^-1))),Ipnt,Npnt)
 PlotA(UUU,24,"Exploitable Biomass",Ipnt,Npnt)
 PlotA(UUU,30,"Cumulative (discounted) Catch (t)",Ipnt,Npnt)
 PlotA(UUU,36,"Spawning Biomass",Ipnt,Npnt)

 Jpnt <- which(UUU=="# Recruitments")-8
 B0 <- as.double(UUU[Jpnt,1])
 abline(h=0.4*B0,lwd=2)
 abline(h=0.25*B0,lwd=2)

}

#  ==================================================================================================

PlotA <- function(UUU,offset,title,Ipnt,Npnt)
{
 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])

 Y1 <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),offset+2])
 Y2 <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),offset+3])
 Y3 <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),offset+4])
 Y4 <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),offset+5])
 Y5 <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),offset+6])

 ymax <- max(Y5)*1.1
 plot(Xvals,Y5,xlab="Year",ylab=title,type='n',yaxs="i",ylim=c(0,ymax))
 XX <- c(Xvals,rev(Xvals))
 polygon(XX,c(Y1,rev(Y5)),col="lightgray")
 polygon(XX,c(Y2,rev(Y4)),col="gray")
 lines(Xvals,Y3,lty=1,lwd=4)

}

#  ==================================================================================================

FirstFive<-function(UUU,Title)
{
 par(mfrow=c(2,2))

 Ipnt <- which(UUU=="# Individual")+2
 Npnt <- as.double(UUU[Ipnt-1,1])

 Ipnt <- which(UUU=="# First Five")+1
 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 ymax <- 0
 for (II in 1:5)
  {
   Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1+II])
   if (max(Yvals)  > ymax) ymax <- max(Yvals)
  }
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2])
 plot(Xvals,Yvals,xlab="Year",ylab="Spawning Output \\ Target",type='n',yaxs="i",ylim=c(0,1.05*ymax))
 for (II in 1:5)
  {
   Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1+II])
   lines(Xvals,Yvals,lty=II)
  }
 title(Title)

}

#  ==================================================================================================

FinalRecovery<-function(UUU,Title)
{
 par(mfrow=c(2,2))

 Ipnt <- which(UUU=="# Final Recovery")+2
 Npnt <- as.double(UUU[Ipnt-1,1])
 Npnt <- Npnt - 1

 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),3])
 ymax <- max(Yvals)
 plot(Xvals,Yvals,xlab="Year",ylab="Proportion of Simulations",type='n',yaxs="i",ylim=c(0,1.4*ymax))

 Inc <- (Xvals[2]-Xvals[1])/2
 for (II in 1:Npnt)
  {
   xx <- c(Xvals[II]-Inc,Xvals[II]-Inc,Xvals[II]+Inc,Xvals[II]+Inc)
   yy <- c(0,Yvals[II],Yvals[II],0)
   polygon(xx,yy,col="gray")
  }
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),6])*ymax*1.2
 lines(Xvals,Yvals,lty=1,lwd=5,col="red")
 title(Title)


}
#  ==================================================================================================

 for (Ifile in 1:length(fileN))
  {

   FileName <- paste(dirn,fileN[Ifile],sep="\\")
   print(FileName)
   UUU <- read.table(file=FileName,col.names=1:ncols,fill=T,colClass="character",comment.char="$",sep=",")

   # Extract key parameters
   Nsim <- as.double(UUU[3,1])
   Amin <- as.double(UUU[4,1])
   Amax <- as.double(UUU[5,1])
   Nsex <- as.double(UUU[6,1])
   RecType <- as.double(UUU[7,1])
   Ncatch <- as.double(UUU[8,1])
   Ioutput <- as.double(UUU[9,1])

   # Plot Age versus fecudity
   if (1 %in% Plots[[Ifile]]) Net_Spawn_Graph(UUU,Amin,Amax,Titles[Ifile])

   # Recruitment and Recruits/Spawners
   if (2 %in% Plots[[Ifile]]) RecruitmentPlots(UUU,Titles[Ifile])

   # Histogram of B0
   if (3 %in% Plots[[Ifile]]) B0Dist(UUU,Titles[Ifile])

   # Histogram of recovery times
   if (4 %in% Plots[[Ifile]]) RecHist(UUU,Titles[Ifile])

   # Results across strategies
   if (5 %in% Plots[[Ifile]]) AltStrategies(UUU,Options,Titles[Ifile])

   #Individual plots
   if (6 %in% Plots[[Ifile]]) IndividualPlots(UUU,Titles[Ifile])

   # First five trajectories of SSB/target
   if (7 %in% Plots[[Ifile]]) FirstFive(UUU,Titles[Ifile])

   # Plot of when recovery occurs
   if (8 %in% Plots[[Ifile]]) FinalRecovery(UUU,Titles[Ifile])

  }
}

# ================================================================================================================


## # Plots - set to get specific plots
## # Options - set to get specific strategies in the trajectory plots
##
## Example use:
##
## Titles <- c("Res1","Res2","Res3")
## Plots <- list(c(1:8),c(6:7))
## DoProjectPlots(fileN=c("res1.csv","res2.csv"),Titles=Titles,Plots=Plots,Options=c(1,2,3,4,5,6,7,9),LegLoc="bottomleft")
