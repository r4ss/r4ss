DoProjectPlots<-function(dirn="C:/NWFSC/NWFSC25D/",fileN="res.csv",ncols=200,Plots=1:25,Options=1:9,LegLoc="bottomright")
{
 windows(record=T)

 FileName <- paste(dirn,fileN,sep="/")
 print(FileName)
 UUU <- read.table(file=FileName,col.names=1:ncols,fill=T,colClass="character",comment.char="$",sep=",")
 

#  ==================================================================================================

Net_Spawn_Graph<-function(UUU,Amin,Amax)
{
 Ipnt <- which(UUU=="# Age Fecu")+1

 Xvals <- as.double(UUU[Ipnt:(Ipnt+Amax*5-Amin),1])
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Amax*5-Amin),2])
 par(mfrow=c(2,2))
 plot(Xvals,Yvals,xlab="Age (years)",ylab="Net Spawning Output",lty=1,type='l',lwd=2,xaxs="i",yaxs="i",ylim=c(0,1.05*max(Yvals)))

}

#  ==================================================================================================

RecruitmentPlots<-function(UUU)
{
 par(mfrow=c(2,2))

 Ipnt <- which(UUU=="# Recruitments")+2
 Npnt <- as.double(UUU[Ipnt-1,1])
 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2])
 plot(Xvals,Yvals,xlab="Year",ylab="Recruitment",lty=1,type='l',lwd=2,yaxs="i",ylim=c(0,1.05*max(Yvals)))

 Ipnt <- which(UUU=="# Recruits-per-spawner")+2
 Npnt <- as.double(UUU[Ipnt-1,1])
 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2])
 plot(Xvals,Yvals,xlab="Year",ylab="Recruits \\ Spawning Output",lty=1,type='l',lwd=2,yaxs="i",ylim=c(0,1.05*max(Yvals)))

}
#  ==================================================================================================

B0Dist<-function(UUU)
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

}

#  ==================================================================================================

RecHist<-function(UUU)
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

AltStrategies<-function(UUU,Options)
{
 par(mfrow=c(2,2))

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
 legend(LegLoc,legend=titles[Options],lty=Options,cex=0.75) 

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
 
}
# =============================================================================================================

IndividualPlots<-function(UUU)
{
 par(mfrow=c(2,3))

 Ipnt <- which(UUU=="# Individual")+2
 Npnt <- as.double(UUU[Ipnt-1,1])

 PlotA(UUU,0,"Spawning Output \\ Target",Ipnt,Npnt)
 PlotA(UUU,6,"Catch (t)",Ipnt,Npnt)
 PlotA(UUU,12,"Recruitment",Ipnt,Npnt)
 PlotA(UUU,18,expression(paste("Fishing Mortality ", (yr^-1))),Ipnt,Npnt)
 PlotA(UUU,24,"Exploitable Biomass",Ipnt,Npnt)
 PlotA(UUU,30,"Cumulative (discounted) Catch (t)",Ipnt,Npnt)
 
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

FirstFive<-function(UUU)
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

}

#  ==================================================================================================

FinalRecovery<-function(UUU)
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
  

}
#  ==================================================================================================
 # Extract key parameters
 Nsim <- as.double(UUU[3,1])
 Amin <- as.double(UUU[4,1])
 Amax <- as.double(UUU[5,1])
 Nsex <- as.double(UUU[6,1])
 RecType <- as.double(UUU[7,1])
 Ncatch <- as.double(UUU[8,1])
 Ioutput <- as.double(UUU[9,1])
 
 # Plot Age versus fecudity
 if (1 %in% Plots) Net_Spawn_Graph(UUU,Amin,Amax)
 
 # Recruitment and Recruits/Spawners
 if (2 %in% Plots) RecruitmentPlots(UUU)
 
 # Histogram of B0
 if (3 %in% Plots) B0Dist(UUU)
 
 # Histogram of recovery times
 if (4 %in% Plots) RecHist(UUU)
 
 # Results across strategies
 if (5 %in% Plots) AltStrategies(UUU,Options)
 
 #Individual plots
 if (6 %in% Plots) IndividualPlots(UUU)

 # First five trajectories of SSB/target
 if (7 %in% Plots) FirstFive(UUU)
 
 # Plot of when recovery occurs
 if (8 %in% Plots) FinalRecovery(UUU)
 
}
