DoProjectPlots<-function(dirn="C:/NWFSC/NWFSC25D/",fileN=c("res.csv"),Titles="",ncols=200,Plots=list(1:25),Options=list(c(1:9)),LegLoc="bottomright",yearmax= -1)
{
 if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
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

AltStrategies<-function(FileN,UUUs,Options,Title,yearmax,Titles,cols=c("red","blue","green","orange","black","pink"))
{
 par(mfrow=c(2,2))

 Ipnt <- which(UUUs[[1]]=="# Recovery Histogram")
 Tmax <- as.double(UUUs[[1]][Ipnt-1,1])
 Yinit <-as.double(UUUs[[1]][Ipnt-7,1])
 MinRev <-as.double(UUUs[[1]][Ipnt-10,1])
 Tmin <- MinRev+Yinit

 if (length(FileN) > 0)
  for (Ifile in 2:length(FileN))
   {
    Ipnt <- which(UUUs[[1]]=="# Recovery Histogram")
    Tmax2 <- as.double(UUUs[[1]][Ipnt-1,1])
    Yinit2 <-as.double(UUUs[[1]][Ipnt-7,1])
    MinRev2 <-as.double(UUUs[[1]][Ipnt-10,1])
    Tmin2 <- MinRev2+Yinit2
    if (Tmin != Tmin2)
     {
      stop("Tmin values in the different files don't match")
     }
   }

 NOpts <- 0
 for (Ifile in 1:length(FileN)) NOpts <- NOpts+ length(Options[[Ifile]])

 xmin <- 1.0e20
 xmax <- 0
 for (Ifile in 1:length(FileN))
  {
   Ipnt <- which(UUUs[[Ifile]]=="# Summary 1")+3
   Npnt <- as.double(UUUs[[Ifile]][Ipnt-2,1])
   Xvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),1])
   if (yearmax > 0)
    Use <- Xvals <= yearmax
   else
    Use <- rep(T,length=length(Xvals))
   Xvals <- Xvals[Use]
   if (min(Xvals) < xmin) xmin <- min(Xvals)
   if (max(Xvals) > xmax) xmax <- max(Xvals)
  }

 plot(xmin,0,xlab="Year",ylab="Probability Above Target",type='n',yaxs="i",ylim=c(0,105),xlim=c(xmin,xmax))
 IlineType <- 0
 for (Ifile in 1:length(FileN))
  for (II in Options[[Ifile]])
    {
     IlineType <- IlineType + 1
     Ipnt <- which(UUUs[[Ifile]]=="# Summary 1")+3
     Npnt <- as.double(UUUs[[Ifile]][Ipnt-2,1])
     Xvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),1])
     Yvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),2+II])*100
     lines(Xvals,Yvals,lty=IlineType,col=cols[IlineType],lwd=2)
    }
 abline(h=0.5,lwd=3)
 abline(v=Tmin,lwd=1,lty=2)
 abline(v=Tmax,lwd=1,lty=2)
 title(Title)

 ymax <- 0
 for (Ifile in 1:length(FileN))
  for (II in Options[[Ifile]])
    {
     Ipnt <- which(UUUs[[Ifile]]=="# Summary 1")+3
     Npnt <- as.double(UUUs[[Ifile]][Ipnt-2,1])
     Yvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),2+10+II])*100
     if (max(Yvals)  > ymax) ymax <- max(Yvals)
    }
 plot(xmin,0,xlab="Year",ylab="Catch (mt)",type='n',yaxs="i",ylim=c(0,1.05*ymax),xlim=c(xmin,xmax))
 IlineType <- 0
 for (Ifile in 1:length(FileN))
  for (II in Options[[Ifile]])
   {
     IlineType <- IlineType + 1
     Ipnt <- which(UUUs[[Ifile]]=="# Summary 1")+3
     Npnt <- as.double(UUUs[[Ifile]][Ipnt-2,1])
     Xvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),1])
     Yvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),2+10+II])*100
     lines(Xvals,Yvals,lty=IlineType,col=cols[IlineType],lwd=2)
    }

 ymax <- 0
 for (Ifile in 1:length(FileN))
  for (II in Options[[Ifile]])
    {
     Ipnt <- which(UUUs[[Ifile]]=="# Summary 1")+3
     Npnt <- as.double(UUUs[[Ifile]][Ipnt-2,1])
     Yvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),2+20+II])*100
     if (max(Yvals)  > ymax) ymax <- max(Yvals)
    }
 plot(xmin,0,xlab="Year",ylab="Spawning Output \\ Target",type='n',yaxs="i",ylim=c(0,1.05*ymax),xlim=c(xmin,xmax))
 IlineType <- 0
 for (Ifile in 1:length(FileN))
  for (II in Options[[Ifile]])
    {
     IlineType <- IlineType + 1
     Ipnt <- which(UUUs[[Ifile]]=="# Summary 1")+3
     Npnt <- as.double(UUUs[[Ifile]][Ipnt-2,1])
     Xvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),1])
     Yvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),2+20+II])*100
     lines(Xvals,Yvals,lty=IlineType,col=cols[IlineType],lwd=2)
    }

 for (Ifile in 1:length(FileN))
  for (II in Options[[Ifile]])
    {
     Ipnt <- which(UUUs[[Ifile]]=="# Summary 1")+3
     Npnt <- as.double(UUUs[[Ifile]][Ipnt-2,1])
     Yvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),2+30+II])
     if (max(Yvals)  > ymax) ymax <- max(Yvals)
    }
 plot(xmin,0,xlab="Year",ylab="Spawning Output",type='n',yaxs="i",ylim=c(0,1.05*ymax),xlim=c(xmin,xmax))
 IlineType <- 0
 for (Ifile in 1:length(FileN))
  for (II in Options[[Ifile]])
    {
     IlineType <- IlineType + 1
     Ipnt <- which(UUUs[[Ifile]]=="# Summary 1")+3
     Npnt <- as.double(UUUs[[Ifile]][Ipnt-2,1])
     Xvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),1])
     Yvals <- as.double(UUUs[[Ifile]][Ipnt:(Ipnt+Npnt-1),2+30+II])
     lines(Xvals,Yvals,lty=IlineType,col=cols[IlineType],lwd=2)
    }
 Jpnt <- which(UUUs[[1]]=="# Recruitments")-8
 B0 <- as.double(UUUs[[1]][Jpnt,1])
 abline(h=0.4*B0,lwd=2)
 abline(h=0.25*B0,lwd=2)


 plot(0,0,xlab="",ylab="",axes=F,type="n",cex=0)

 Ltys <- NULL
 legs <- NULL
 IlineType <- 0
 col2 <- NULL
 for (Ifile in 1:length(FileN))
  for (II in Options[[Ifile]])
    {
     Ipnt <- which(UUUs[[Ifile]]=="# Summary 1")+3
     titles <- UUUs[[Ifile]][Ipnt-1,3:11]

     IlineType <- IlineType + 1
     titls <- titles[II]
     if (length(FileN) > 0) titls <- paste(Titles[Ifile],": ",titls,sep="")
     legs <- c(legs, titls)
     Ltys <- c(Ltys,IlineType)
     col2 <- c(col2,cols[IlineType])
    }

 legend(LegLoc,legend=legs,lty=Ltys,cex=1,col=col2,lwd=2)

}
# =============================================================================================================

IndividualPlots<-function(UUU,Title,yearmax)
{
 par(mfrow=c(2,2))

 Ipnt <- which(UUU=="# Individual")+2
 Npnt <- as.double(UUU[Ipnt-1,1])

 PlotA(UUU,0,"Spawning Output \\ Target",Ipnt,Npnt,yearmax)
 title(Title)

 PlotA(UUU,6,"Catch (mt)",Ipnt,Npnt,yearmax)

 PlotA(UUU,12,"Recruitment",Ipnt,Npnt,yearmax)

 PlotA(UUU,18,expression(paste("Fishing Mortality ", (yr^-1))),Ipnt,Npnt,yearmax)

 PlotA(UUU,24,"Exploitable Biomass",Ipnt,Npnt,yearmax)

 PlotA(UUU,30,"Cumulative (discounted) Catch (mt)",Ipnt,Npnt,yearmax)

 PlotA(UUU,36,"Spawning Biomass",Ipnt,Npnt,yearmax)
 Jpnt <- which(UUU=="# Recruitments")-8
 B0 <- as.double(UUU[Jpnt,1])
 abline(h=0.4*B0,lwd=2)
 abline(h=0.25*B0,lwd=2)

}

#  ==================================================================================================

PlotA <- function(UUU,offset,title,Ipnt,Npnt,yearmax)
{
 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 if (yearmax > 0)
  Use <- Xvals <= yearmax
 else
  Use <- rep(T,length=length(Xvals))
 Xvals <- Xvals[Use]

 Y1 <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),offset+2])
 Y2 <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),offset+3])
 Y3 <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),offset+4])
 Y4 <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),offset+5])
 Y5 <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),offset+6])

 ymax <- max(Y5[Use])*1.1
 plot(Xvals,Y5[Use],xlab="Year",ylab=title,type='n',yaxs="i",ylim=c(0,ymax))
 XX <- c(Xvals,rev(Xvals))
 polygon(XX,c(Y1[Use],rev(Y5[Use])),col="lightgray")
 polygon(XX,c(Y2[Use],rev(Y4[Use])),col="gray")
 lines(Xvals,Y3[Use],lty=1,lwd=4)

}

#  ==================================================================================================

FirstFive<-function(UUU,Title,yearmax)
{
 par(mfrow=c(2,2))

 Ipnt <- which(UUU=="# Individual")+2
 Npnt <- as.double(UUU[Ipnt-1,1])

 Ipnt <- which(UUU=="# First Five")+1
 Xvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1])
 if (yearmax > 0)
  Use <- Xvals <= yearmax
 else
  Use <- rep(T,length=length(Xvals))
 Xvals <- Xvals[Use]

 ymax <- 0
 for (II in 1:5)
  {
   Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1+II])
   if (max(Yvals[Use])  > ymax) ymax <- max(Yvals[Use])
  }
 Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),2])
 plot(Xvals,Yvals[Use],xlab="Year",ylab="Spawning Output \\ Target",type='n',yaxs="i",ylim=c(0,1.05*ymax))
 for (II in 1:5)
  {
   Yvals <- as.double(UUU[Ipnt:(Ipnt+Npnt-1),1+II])
   lines(Xvals,Yvals[Use],lty=II)
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

 UUUs <- vector("list",5)
 for (Ifile in 1:length(fileN))
  {

   FileName <- paste(dirn,fileN[Ifile],sep="\\")
   print(FileName)
   UUU <- read.table(file=FileName,col.names=1:ncols,fill=T,colClass="character",comment.char="$",sep=",")
   UUUs[[Ifile]] <- UUU

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

   #Individual plots
   if (6 %in% Plots[[Ifile]]) IndividualPlots(UUU,Titles[Ifile],yearmax)

   # First five trajectories of SSB/target
   if (7 %in% Plots[[Ifile]]) FirstFive(UUU,Titles[Ifile],yearmax)

   # Plot of when recovery occurs
   if (8 %in% Plots[[Ifile]]) FinalRecovery(UUU,Titles[Ifile])

  }

 # Results across strategies
 DoStrategies <- F
 for (Ifile in 1:length(fileN))
  if (5 %in% Plots[[Ifile]]) DoStrategies <- T
 if (DoStrategies==T) AltStrategies(fileN,UUUs,Options,"",yearmax,Titles)
}

# ================================================================================================================
