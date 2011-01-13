SSplotSelex <-
  function(replist, fleets="all", fleetnames="default",
           selexlines=1:5,
           subplot=1:11,
           plot=TRUE, print=FALSE, add=FALSE,
           labels=c("Length (cm)", #1
                    "Age (yr)",    #2
                    "Year",        #3
                    "Selectivity", #4
                    "Retention"),  #5
           col1="red",col2="blue",
           pwidth = 7, pheight = 7, punits = "in",
           res = 300, ptsize = 12,
           cex.main=1, plotdir = "default",
           verbose = TRUE)
{
  nsexes         <- replist$nsexes
  nseasons       <- replist$nseasons
  nfleets        <- replist$nfleets
  lbinspop       <- replist$lbinspop
  nlbinspop      <- replist$nlbinspop
  sizeselex      <- replist$sizeselex
  ageselex       <- replist$ageselex
  accuage        <- replist$accuage
  endyr          <- replist$endyr
  FleetNames     <- replist$FleetNames
  growdat        <- replist$endgrowth
  mainmorphs     <- replist$mainmorphs
  nareas         <- replist$nareas
  ngpatterns     <- replist$ngpatterns
  derived_quants <- replist$derived_quants
  
  # subsetting for season 1 only. This could be replaced
  #   by info on the growth within the season when each fleet operates.
  growdat <- growdat[growdat$Seas==1,]
  if(nseasons>1) cat("Warning: plots showing growth curve with selectivity are using season 1 growth,\nwhich may not match the timing of the fishery.\n")
  
  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
  if(plotdir=="default") plotdir <- replist$inputs$dir

  ians_blues <- c("white","grey","lightblue","skyblue","steelblue1","slateblue",topo.colors(6),"blue","blue2","blue3","blue4","black")

  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  }else{
    if(length(intersect(fleets,1:nfleets))!=length(fleets)){
      return("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
    }
  }
  if(fleetnames[1]=="default") fleetnames <- FleetNames

  # selex and retention
  for(i in fleets)
  {
    for(m in 1:nsexes)
    {
      if(m==1 & nsexes==1) sextitle1 <- "Time-"
      if(m==1 & nsexes==2) sextitle1 <- "Female time-"
      if(m==2) sextitle1 <- "Male time-"
      if(m==1 & nsexes==1) sextitle2 <- "Ending"
      if(m==1 & nsexes==2) sextitle2 <- "Female ending"
      if(m==2) sextitle2 <- "Male ending"
      intret <- sizeselex[sizeselex$Factor=="Ret" & sizeselex$gender==m,]
      intmort <- sizeselex[sizeselex$Factor=="Mort" & sizeselex$gender==m,]
      intkeep <- sizeselex[sizeselex$Factor=="Keep" & sizeselex$gender==m,]
      intdead <- sizeselex[sizeselex$Factor=="Dead" & sizeselex$gender==m,]
      intselex <- sizeselex[sizeselex$Factor=="Lsel" & sizeselex$gender==m,]
      plotselex <- intselex[intselex$Fleet==i,]
      plotret <- intret[intret$Fleet==i,]

      # test for time-varying length selectivity
      time <- any(apply(plotselex[-nrow(plotselex),-(1:5)], 2, function(x){any(x!=x[1])}))      
      if(time)
      {
        x <- lbinspop
        y <- plotselex$year
        z <- plotselex[,-(1:5)]
        z <- matrix(as.numeric(as.matrix(z)),ncol=ncol(z))
        z <- t(z)
        main <- paste(sextitle1,"varying selectivity for ", FleetNames[i],sep="")
        if(plot)
        {
          if(1 %in% subplot) persp(x,y,z,col="white",xlab=labels[1],ylab=labels[3],zlab=labels[4],expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
          if(2 %in% subplot) contour(x,y,z,nlevels=5,xlab=labels[1],ylab=labels[3],main=main,cex.main=cex.main,col=ians_blues,lwd=2)
        }
        if(print)
        {
          if(1 %in% subplot){
            pngfun(file=paste(plotdir,"sel1_len_timevary_surf_flt",i,"sex",m,".png",sep=""))
            persp(x,y,z,col="white",xlab=labels[1],ylab=labels[3],zlab=labels[4],expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
            dev.off()
          }
          if(2 %in% subplot){
            pngfun(file=paste(plotdir,"sel2_len_timevary_contour_flt",i,"sex",m,".png",sep=""))
            contour(x,y,z,nlevels=5,xlab=labels[1],ylab=labels[3],main=main,cex.main=cex.main,col=ians_blues,lwd=2)
            dev.off()
          }
        }
      }
      # test for time-varying length retention
      time2 <- any(apply(plotret[-nrow(plotret),-(1:5)],2,function(x){any(x!=x[1])}))
      if(time2)
      {
        x <- lbinspop
        y <- intret$year[intret$Fleet==i]
        z <- intret[intret$Fleet==i,-(1:5)]
        z <- matrix(as.numeric(as.matrix(z)),ncol=ncol(z))
        z <- t(z)
        main <- paste(sextitle1,"varying retention for ", FleetNames[i],sep="")
        if(plot)
        {
          if(3 %in% subplot) persp(x,y,z,col="white",xlab=labels[1],ylab=labels[3],zlab=labels[5],expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
          if(4 %in% subplot) contour(x,y,z,nlevels=5,xlab=labels[1],ylab=labels[3],main=main,cex.main=cex.main,col=ians_blues,lwd=2)
        }
        if(print)
        {
          if(3 %in% subplot){
            pngfun(file=paste(plotdir,"sel3_timevary_ret_surf_flt",i,"sex",m,".png",sep=""))
            persp(x,y,z,col="white",xlab=labels[1],ylab=labels[3],zlab=labels[5],expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
            dev.off()
          }
          if(4 %in% subplot){
            pngfun(file=paste(plotdir,"sel4_timevary_ret_contour_flt",i,"sex",m,".png",sep=""))
            contour(x,y,z,nlevels=5,xlab=labels[1],ylab=labels[3],main=main,cex.main=cex.main,col=ians_blues,lwd=2)
            dev.off()
          }
        }
      }
      plotselex <- plotselex[plotselex$year==endyr,-(1:5)]

      plotret <- plotret[nrow(plotret),-(1:5)] # final year only
      ylab <- labels[4]
      bins <- as.numeric(names(plotselex))
      vals <- as.numeric(paste(plotselex))
      retvals <- as.numeric(plotret)
      main <- paste(sextitle2," year selectivity for ", FleetNames[i],sep="")
      selfunc <- function()
      {
        # determine whether retention was used
        intret2 <- intret[intret$Fleet==i,]
        retchecktemp <- as.vector(unlist(intret2[1,]))
        retcheck <- as.numeric(retchecktemp[6:length(retchecktemp)])
        if(is.na(sum(retcheck))) retcheckuse <- 0
        if(!is.na(sum(retcheck))) retcheckuse <- max(retcheck)-min(retcheck)
        # make plot
        plot(bins,vals,xlab=labels[1],ylim=c(0,1),main=main,cex.main=cex.main,ylab="",type="n")
        abline(h=0,col="grey")
        abline(h=1,col="grey")
        if(1%in%selexlines) lines(bins,vals,type="o",col=col2,cex=1.1)
        if(retcheckuse > 0){
          # if retention, then add additional lines & legend
          useret <- intret[intret$Fleet==i,]
          usekeep <- intkeep[intkeep$Fleet==i,]
          usemort <- intmort[intmort$Fleet==i,]
          usedead <- intdead[intdead$Fleet==i,]
          plotret <- useret[useret$year==max(as.numeric(useret$year)),]
          plotkeep <- usekeep[usekeep$year==max(as.numeric(usekeep$year)),]
          plotmort <- usemort[usemort$year==max(as.numeric(usemort$year)),]
          plotdead <- usedead[usedead$year==max(as.numeric(usedead$year)),]
          if(2%in%selexlines){
            lines((as.numeric(as.vector(names(plotret)[-(1:5)]))),(as.numeric(as.character(plotret[1,-(1:5)]))),col="red",type="o",pch=3,cex=.9)
            ylab <- paste(ylab,", Retention",sep="")
          }
          if(3%in%selexlines){
            lines((as.numeric(as.vector(names(plotmort)[-(1:5)]))),(as.numeric(as.character(plotmort[1,-(1:5)]))),col="orange",type="o",pch=4,cex=.9)
            ylab <- paste(ylab,", Mortality",sep="")
          }
          if(4%in%selexlines) lines((as.numeric(as.vector(names(plotkeep)[-(1:5)]))),(as.numeric(as.character(plotkeep[1,-(1:5)]))),col="purple",type="o",pch=2,cex=.9)
          if(5%in%selexlines) lines((as.numeric(as.vector(names(plotdead)[-(1:5)]))),(as.numeric(as.character(plotdead[1,-(1:5)]))),col="green3",type="o",pch=5,cex=.9)
          legend("bottomright",inset=c(0.05,0.05),bty="n",
      	   c(labels[4],labels[5],"Discard mortality","Keep = Sel*Ret","Dead = Sel*(Ret+(1-Ret)*Mort)")[selexlines],
      	   lty=1,col=c("blue","red","orange","purple","green3")[selexlines],
      	   pch=c(1,3,4,2,5)[selexlines], pt.cex=c(1.1,.9,.9,.9,.9)[selexlines])
        }
        mtext(ylab,side=2,line=3)
      }
      # make plot if selectivity is not constant
      if((min(vals)<1 & max(vals)>0) | (!is.na(diff(range(retvals))) && diff(range(retvals))!=0)) # only make plot of selectivity is not constant
      {
        if(5 %in% subplot){
          if(plot) selfunc()
          if(print){
            pngfun(file=paste(plotdir,"sel5_len_flt",i,"sex",m,".png",sep=""))
            selfunc()
            dev.off()
          }
        }
      }
    } # sexes
  } # fleets

  # Age based selex
  ylab <- labels[4]
  for(i in fleets)
  {
    for(m in 1:nsexes)
    {
      if(m==1 & nsexes==1) sextitle1 <- "Time-"
      if(m==1 & nsexes==2) sextitle1 <- "Female time-"
      if(m==2) sextitle1 <- "Male time-"
      if(m==1 & nsexes==1) sextitle2 <- "Ending"
      if(m==1 & nsexes==2) sextitle2 <- "Female ending"
      if(m==2) sextitle2 <- "Male ending"
      ageselexcols <- (1:ncol(ageselex))[names(ageselex) %in% as.character(0:accuage)]
      plotageselex <- ageselex[ageselex$factor=="Asel" & ageselex$fleet==i & ageselex$gender==m,]
      # test for time-varying age selectivity
      time <- any(apply(plotageselex[-nrow(plotageselex),ageselexcols],2,function(x){any(x!=x[1])}))      
      if(time)
      {
        if((min(as.numeric(as.vector(t(plotageselex[,-(1:7)])))) < 1))
        {
          x <- seq(0,accuage,by=1)
          y <- as.numeric(plotageselex$year)
          z <- plotageselex[,-(1:7)]
          z <- matrix(as.numeric(as.matrix(z)),ncol=ncol(z))
          z <- t(z)
          main <- paste(sextitle1,"varying selectivity for ", FleetNames[i],sep="")
          if(plot){
            if(6 %in% subplot) persp(x,y,z,col="white",xlab=labels[2],ylab=labels[3],zlab=ylab,expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
            if(7 %in% subplot) contour(x,y,z,nlevels=5,xlab=labels[2],main=main,cex.main=cex.main,col=ians_blues,lwd=2)}
          if(print){
            if(6 %in% subplot){
              pngfun(file=paste(plotdir,"sel6_timevary_surf_flt",i,"sex",m,".png",sep=""))
              persp(x,y,z,col="white",xlab=labels[2],ylab=labels[3],zlab=ylab,expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
              dev.off()
            }
            if(7 %in% subplot){
              pngfun(file=paste(plotdir,"sel7_timevary_contour_flt",i,"sex",m,".png",sep=""))
              contour(x,y,z,nlevels=5,xlab=labels[2],main=main,cex.main=cex.main,col=ians_blues,lwd=2)
              dev.off()
            }
          }
          plotageselex2 <- plotageselex[plotageselex$year %in% c(max(as.numeric(plotageselex$year))),]
          plotageselex2 <- plotageselex2[,-(1:7)]
          main <- paste(sextitle2," year selectivity for ", FleetNames[i],sep="")
          endselfunc <- function()
          {
            plot((as.numeric(names(plotageselex2))),(as.numeric(paste(c(plotageselex2)))),xlab=labels[2],ylim=c(0,1),main=main,cex.main=cex.main,ylab=ylab,type="o",col=col2,cex=1.1)
            abline(h=0,col="grey")
          }
          if(8 %in% subplot){
            if(plot) endselfunc()
            if(print)
            {
              pngfun(file=paste(plotdir,"sel8_age_flt",i,"sex",m,".png",sep=""))
              endselfunc()
              dev.off()
            }
          }
        }
      }
      if(!time)
      {
        plotageselex <- plotageselex[plotageselex$year==endyr,]
        plotageselex <- plotageselex[,-(1:7)]
        vals <- as.numeric(paste(c(plotageselex)))
        if(diff(range(vals))!=0)
        {
          main <- paste(sextitle2," year selectivity for ", FleetNames[i],sep="")
          endselfunc2 <- function(){
            plot((as.numeric(names(plotageselex))),vals,xlab=labels[2],ylim=c(0,1),main=main,cex.main=cex.main,ylab=ylab,type="o",col=col2,cex=1.1)
            abline(h=0,col="grey")
          }
          if(9 %in% subplot){
            if(plot) endselfunc2()
            if(print)
            {
              pngfun(file=paste(plotdir,"sel9_age_flt",i,"sex",m,".png",sep=""))
              endselfunc2()
              dev.off()
            }
          }
        } # end if
      } # end if not time varying
    } # sexes
  } # fleets
  flush.console()

  # Age-length combined with growth curve
  if(10 %in% subplot & ngpatterns==1){ # need to connect growth patterns to fleets in future

    # Mid year mean length at age with 95% range of lengths (by sex if applicable)
    growdatF <- growdat[growdat$Gender==1 & growdat$Morph==mainmorphs[1],]
    growdatF$Sd_Size <- growdatF$SD_Mid
    growdatF$high <- growdatF$Len_Mid + 1.96*growdatF$Sd_Size
    growdatF$low <- growdatF$Len_Mid - 1.96*growdatF$Sd_Size
    if(nsexes > 1){
      growdatM <- growdat[growdat$Gender==2 & growdat$Morph==mainmorphs[2],]
      xm <- growdatM$Age
      growdatM$Sd_Size <- growdatM$SD_Mid
      growdatM$high <- growdatM$Len_Mid + 1.96*growdatM$Sd_Size
      growdatM$low <- growdatM$Len_Mid - 1.96*growdatM$Sd_Size
    }

    xlab <- labels[2]
    ylab <- labels[1]
    zlab <- labels[4]
    for(i in fleets)
    {
      for(m in 1:nsexes)
      {
        if(m==1 & nsexes==1) sextitle2 <- "Ending"
        if(m==1 & nsexes==2) sextitle2 <- "Female ending"
        if(m==2) sextitle2 <- "Male ending"
        plotlenselex <- as.numeric(sizeselex[sizeselex$Factor=="Lsel" & sizeselex$year==endyr & sizeselex$Fleet==i & sizeselex$gender==m,-(1:5)])
        # test if there is any length-based selectivity (otherwise plot is uninformative)
        if(any(plotlenselex!=1)){ 
          plotageselex <- as.numeric(ageselex[ageselex$factor=="Asel" & ageselex$year==endyr & ageselex$fleet==i & ageselex$gender==m,-(1:7)])
          x <- seq(0,accuage,by=1)
          y <- lbinspop
          z <- plotageselex %o% plotlenselex # outer product of age- and length-selectivity
          
          main <- paste(sextitle2," year selectivity and growth for ", FleetNames[i],sep="")
    
          agelenselcontour <- function(){
            contour(x,y,z,nlevels=5,xlab=xlab,ylab=ylab,
                    main=main,cex.main=cex.main,col=ians_blues,lwd=2)
            if(m==1){
              lines(x,growdatF$Len_Mid,col='white',lwd=5)
              lines(x,growdatF$Len_Mid,col=col1,lwd=3)
              lines(x,growdatF$high,col='white',lwd=1,lty=1)
              lines(x,growdatF$high,col=col1,lwd=1,lty="dashed")
              lines(x,growdatF$low,col='white',lwd=1,lty=1)
              lines(x,growdatF$low,col=col1,lwd=1,lty="dashed")
            }
            if(m==2){
              lines(xm,growdatM$Len_Mid,col='white',lwd=5)
              lines(xm,growdatM$Len_Mid,col=col2,lwd=3)
              lines(xm,growdatM$high,col='white',lwd=1,lty=1)
              lines(xm,growdatM$high,col=col2,lwd=1,lty="dashed")
              lines(xm,growdatM$low,col='white',lwd=1,lty=1)
              lines(xm,growdatM$low,col=col2,lwd=1,lty="dashed")
            }
          }
          if(plot){
            if(10 %in% subplot) agelenselcontour()
          }
          if(print){
            if(10 %in% subplot){
              pngfun(file=paste(plotdir,"sel10_agelen_contour_flt",i,"sex",m,".png",sep=""))
              agelenselcontour()
              dev.off()
            }
          }
        } # if there is any length-based selectivity
      } # sexes
    } # fleets
  } # if 10 in subplot

  # Extra SD reporting plot
  if(11 %in% subplot){
    # get values from Extra SD reporting if created by request at bottom of control file
    rows <- grep("Selex_std",derived_quants$LABEL)
    if(length(rows)>0){
      plot_extra_selex_SD <- function(){
        sel <- derived_quants[rows,]
        names <- sel$LABEL
        splitnames <- strsplit(names,"_")
        namesDF <- as.data.frame(matrix(unlist(strsplit(names,"_")),ncol=6,byrow=T))
        sel$fleet   <- as.numeric(as.character(namesDF$V3))
        sel$sex     <- as.character(namesDF$V4)
        sel$agelen  <- as.character(namesDF$V5)
        sel$bin     <- as.numeric(as.character(namesDF$V6))
        sel$lower   <- qnorm(0.025, mean=sel$Value, sd=sel$StdDev)
        sel$upper   <- pmin(qnorm(0.975, mean=sel$Value, sd=sel$StdDev),1)
        i <- sel$fleet[1]
        m <- sel$sex[1]
        agelen <- sel$agelen[1]
        xlab <- labels[1:2][1 + (sel$agelen[1]=="A")] # decide label between length and age
        if(m=="Fem" & nsexes==1) sextitle3 <- ""
        if(m=="Fem" & nsexes==2) sextitle3 <- "females"
        if(m=="Mal") sextitle3 <- "males"
        main <- paste("Uncertainty in selectivity for",FleetNames[i],sextitle3)
        no0 <- sel$StdDev>0

        if(agelen=="L") plotselex <- sizeselex[sizeselex$Factor=="Lsel" & ageselex$fleet==i & sizeselex$gender==m,]
        if(agelen=="A") plotselex <- ageselex[ageselex$factor=="Asel" & ageselex$fleet==i & ageselex$gender==m,]
        #Ian T.: finish this part to add full selectivity line, including bins for which
        #        no uncertainty was requested
        plot(sel$bin,sel$Value,xlab=xlab,ylim=c(0,1),main=main,cex.main=cex.main,
             ylab=labels[1],type="o",col=col2,cex=1.1,xlim=c(0,max(sel$bin)))
        arrows(x0=sel$bin[no0], y0=sel$lower[no0], x1=sel$bin[no0], y1=sel$upper[no0],
               length=0.01, angle=90, code=3, col=col2)
        abline(h=0,col="grey")
        return(sel)
      }
    }
    if(plot) plot_extra_selex_SD()
    if(print){
      pngfun(file=paste(plotdir,"sel11_uncertainty","sex",m,".png",sep=""))
      plot_extra_selex_SD()
      dev.off()
    }
  }
  
  flush.console()
} 
