SSplotNumbers <-
  function(replist,subplots=1:5,
           plot=TRUE,print=FALSE,
           areas="all",
           areanames="default",
           areacols="default",
           cex.main=1,
           pntscalar=2.6,
           add=FALSE,
           labels=c("Year",              #1
             "Age",                      #2
             "True age (yr)",            #3
             "SD of observed age (yr)"), #4
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,
           plotdir="default",
           verbose=TRUE)
{
  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  natage    <- replist$natage
  if(plotdir=="default") plotdir <- replist$inputs$dir

  if(is.null(natage)){
    print("Skipped plot 14 because NUMBERS_AT_AGE unavailable in report file",quote=FALSE)
    print("	change starter file setting for 'detailed age-structured reports'",quote=FALSE)
  }else{
    # get more stuff from replist
    nsexes          <- replist$nsexes
    nareas          <- replist$nareas
    nseasons        <- replist$nseasons
    ngpatterns      <- replist$ngpatterns
    morphlist       <- replist$morphlist
    morph_indexing  <- replist$morph_indexing
    accuage         <- replist$accuage
    endyr           <- replist$endyr
    AAK             <- replist$AAK

    mainmorphs <- morph_indexing$Index[morph_indexing$Bseas==1 & morph_indexing$Sub_Morph_Dist==max(morph_indexing$Sub_Morph_Dist)]

    if(areas[1]=="all"){
      areas <- 1:nareas
    }else{ if(length(intersect(areas,1:nareas))!=length(areas)){
        return("Input 'areas' should be 'all' or a vector of values between 1 and nareas.")
    }}
    if(areanames[1]=="default") areanames <- paste("area",1:nareas)

    if(areacols[1]=="default"){
      areacols  <- rich.colors.short(nareas)
      if(nareas > 2) areacols <- rich.colors.short(nareas+1)[-1]
    }

    bseas <- unique(natage$BirthSeas)
    if(length(bseas)>1) print("Numbers at age plots are for only the first birth season",quote=FALSE)
    if(ngpatterns>1) print("Numbers at age plots are for growth pattern 1 only",quote=FALSE)
    if(nseasons>1) print("Numbers at age plots are for season 1 only",quote=FALSE)
    for(iarea in areas){
      for(m in 1:nsexes){
        # warning! implementation of birthseasons may not be correct in this section
        # data frame to combine values across factors
        natagetemp_all <- natage[natage$Area==iarea &
      			   natage$Gender==m &
      			   natage$Seas==1 &
      			   natage$Era!="VIRG" &
      			   natage$Yr <= (endyr+1) &
      			   natage$BirthSeas==min(bseas) &
      			   natage$Bio_Pattern==1,]
        # create data frame with 0 values to fill across submorphs
        morphlist <- unique(natagetemp_all$SubMorph)
        natagetemp0 <- natagetemp_all[natagetemp_all$SubMorph==morphlist[1],]
        for(iage in 0:accuage) natagetemp0[,11 + iage] <- 0

        # combining across submorphs
        for(imorph in 1:length(morphlist)){
          natagetemp_morph <- natagetemp_all[natagetemp_all$SubMorph==morphlist[imorph],]
          natagetemp0[,11+0:accuage] <- natagetemp0[,11+0:accuage] + natagetemp_morph[,11+0:accuage]
        } # end morph loop

        nyrsplot <- nrow(natagetemp0)
        resx <- rep(natagetemp0$Yr, accuage+1)
        resy <- NULL
        for(i in 0:accuage) resy <- c(resy,rep(i,nyrsplot))
        resz <- NULL
        for(i in 11+0:accuage) resz <- c(resz,natagetemp0[,i])

        # assign unique name to data frame for area, sex
        assign(paste("natagetemp0area",iarea,"sex",m,sep=""),natagetemp0)

        if(m==1 & nsexes==1) sextitle <- ""
        if(m==1 & nsexes==2) sextitle <- " of females"
        if(m==2) sextitle=" of males"
        if(nareas>1) sextitle <- paste(sextitle," in ",areanames[iarea],sep="")
        plottitle <- paste("Expected numbers at age",sextitle," in thousands (max=",max(resz),")",sep="")

        # calculations related to mean age
        natagetemp1 <- as.matrix(natagetemp0[,-(1:10)])
        ages <- 0:accuage
        natagetemp2 <- as.data.frame(natagetemp1)
        natagetemp2$sum <- as.vector(apply(natagetemp1,1,sum))

        # remove rows with 0 fish (i.e. no growth pattern in this area)
        natagetemp0 <- natagetemp0[natagetemp2$sum > 0, ]
        natagetemp1 <- natagetemp1[natagetemp2$sum > 0, ]
        natagetemp2 <- natagetemp2[natagetemp2$sum > 0, ]
        prodmat <- t(natagetemp1)*ages
        prodsum <- as.vector(apply(prodmat,2,sum))
        natagetemp2$sumprod <- prodsum
        natagetemp2$meanage <- natagetemp2$sumprod/natagetemp2$sum - (natagetemp0$BirthSeas-1)/nseasons
        natageyrs <- sort(unique(natagetemp0$Yr))
        meanage <- 0*natageyrs
        for(i in 1:length(natageyrs)){ # averaging over values within a year (depending on birth season)
          meanage[i] <- sum(natagetemp2$meanage[natagetemp0$Yr==natageyrs[i]]*natagetemp2$sum[natagetemp0$Yr==natageyrs[i]])/sum(natagetemp2$sum[natagetemp0$Yr==natageyrs[i]])}
        if(m==1 & nsexes==2) meanagef <- meanage # save value for females in 2 sex models

        ylim <- c(0,max(meanage))

        ylab <- "Mean age (yr)"
        plottitle1 <- "Mean age in the population"
        tempfun <- function(){
          # bubble plot with line
          bubble3(x=resx, y=resy, z=resz,
      	    xlab=labels[1],ylab=labels[2],col=c("black","black"),main=plottitle,maxsize=(pntscalar+1.0),
      	    las=1,cex.main=cex.main,allopen=1)
          lines(natageyrs,meanage,col="red",lwd=3)
        }
        tempfun2 <- function(){
          # mean length for males and femails
          plot(natageyrs,meanage,col="blue",lty=1,pch=4,xlab=labels[1],ylim=ylim,type="o",ylab=ylab,main=plottitle1,cex.main=cex.main)
          points(natageyrs,meanagef,col="red",lty=2,pch=1,type="o")
          legend("bottomleft",bty="n", c("Females","Males"), lty=c(2,1), pch=c(1,4), col = c("red","blue"))
        }
        if(plot){
          if(1 %in% subplots) tempfun()
          if(2 %in% subplots & m==2 & nsexes==2) tempfun2()
        }
        if(print){
          filepartsex <- paste("_sex",m,sep="")
          filepartarea <- ""
          if(nareas > 1) filepartarea <- paste("_",areanames[iarea],sep="")
          if(1 %in% subplots){
            pngfun(file=paste(plotdir,"14_natage",filepartarea,filepartsex,".png",sep=""))
            tempfun()
            dev.off()
          }
          # make 2-sex plot after looping over both sexes
          if(2 %in% subplots & m==2 & nsexes==2){
            pngfun(file=paste(plotdir,"14_meanage",filepartarea,".png",sep=""))
            tempfun2()
            dev.off()
          }
        } # end printing of plot 14
      } # end gender loop
    } # end area loop
    if(nsexes>1){
      for(iarea in areas){
        plottitle2 <- paste("Sex ratio of numbers at age (males/females)",sep="")
        if(nareas > 1) plottitle2 <- paste(plottitle2," for ",areanames[iarea],sep="")

        natagef <- get(paste("natagetemp0area",iarea,"sex",1,sep=""))
        natagem <- get(paste("natagetemp0area",iarea,"sex",2,sep=""))
        natageratio <- as.matrix(natagem[,-(1:10)]/natagef[,-(1:10)])
        if(diff(range(natageratio,finite=TRUE))!=0){
          tempfun <- function(...){
            contour(natageyrs,0:accuage,natageratio,xaxs="i",yaxs="i",xlab=labels[1],ylab=labels[2],
      	      main=plottitle2,cex.main=cex.main,...)
          }
          if(plot & 3 %in% subplots){
            tempfun(labcex=1)
          }
          if(print & 3 %in% subplots){
            filepart <- ""
            if(nareas > 1) filepart <- paste("_",areanames[iarea],filepart,sep="")
            pngfun(file=paste(plotdir,"14_natageratio",filepart,".png",sep=""))
            tempfun(labcex=0.4)
            dev.off()}
        }else{
          print("skipped sex ratio contour plot because ratio=1 for all ages and years",quote=FALSE)
        }
      } # end area loop
    } # end if nsexes>1

    # plot of equilibrium age composition by gender and area
    tempfun <- function(){
      equilage <- natage[natage$Era=="VIRG",]
      plot(0,type='n',xlim=c(0,accuage),ylim=c(0,1.05*max(equilage[,-(1:10)])),xaxs='i',yaxs='i',
           xlab='Age',ylab='Numbers at age at equilibrium')

      # now fill in legend
      legendlty <- NULL
      legendcol <- NULL
      legendlegend <- NULL
      for(iarea in areas){
        for(m in 1:nsexes){
          equilagetemp <- equilage[equilage$Area==iarea & equilage$Gender==m & equilage$Morph==mainmorphs[m],]
          if(nrow(equilagetemp)>1){
            print('in plot of equilibrium age composition by gender and area',quote=FALSE)
            print('multiple morphs or seasons not supporting, using first row from choices below',quote=FALSE)
            print(equilagetemp[,1:10])
          }
          equilagetemp <- equilagetemp[1,-(1:10)]
          lines(0:accuage,equilagetemp,lty=m,lwd=3,col=areacols[iarea])
          legendlty <- c(legendlty,m)
          legendcol <- c(legendcol,areacols[iarea])

          if(m==1 & nsexes==1) sextitle <- ""
          if(m==1 & nsexes==2) sextitle <- "Females"
          if(m==2) sextitle="Males"
          if(nareas>1) sextitle <- paste(sextitle," in ",areanames[iarea],sep="")
          legendlegend <- c(legendlegend,sextitle)
        }
      }
      if(length(legendlegend)>1) legend('topright',legend=legendlegend,col=legendcol,lty=legendlty,lwd=3)
    }

    if(plot & 4 %in% subplots){
      tempfun()
    } # end if 14 in plot
    if(print & 4 %in% subplots){
      pngfun(file=paste(plotdir,"14_equilagecomp.png",sep=""))
      tempfun()
      dev.off()
    } # close if 14 in print

    # plot the ageing imprecision for all age methods
    if(!is.null(AAK)){
      sd_vectors <- as.data.frame(AAK[,1,])
      n_age_error_keys <- 1
      if(!is.null(nrow(AAK[,1,]))){n_age_error_keys <- nrow(AAK[,1,])}
      if(is.null(nrow(AAK[,1,]))){xvals <- seq(0.5,length(sd_vectors[,1])-0.5,by=1)}
      if(!is.null(nrow(AAK[,1,]))){xvals <- seq(0.5,length(sd_vectors[1,]-0.5),by=1)}
      ylim <- c(0,max(sd_vectors))
      if(n_age_error_keys==1){ploty <- sd_vectors[,1]}
      if(n_age_error_keys>1){ploty <- sd_vectors[1,]}
      tempfun <- function(){
        plot(xvals,ploty,ylim=ylim,type="o",col="black",xlab=labels[3],ylab=labels[4])
        if(n_age_error_keys > 1){
          for(i in 2:n_age_error_keys){
            lines(xvals,sd_vectors[i,],type="o",col=rich.colors.short(n_age_error_keys)[i])
          } # close for n keys loop
        } # close if more than one key statement
        abline(h=0,col="grey") # grey line at 0
      }
      if(plot & 5 %in% subplots){
        tempfun()
      } # end if 14 in plot
      if(print & 5 %in% subplots){
        pngfun(file=paste(plotdir,"14_ageerrorkeys.png",sep=""))
        tempfun()
        dev.off()
      } # close if 14 in print
    } # end if AAK

    if(verbose) print("Finished plot 14: Numbers at age",quote=FALSE)
    flush.console()
  } # end if data available
} # end function
