#' Plot numbers-at-age related data and fits.
#' 
#' Plot numbers-at-age related data and fits from Stock Synthesis output.
#' Plots include bubble plots, mean age, equilibrium age composition,
#' sex-ratio, and ageing imprecision patterns.
#' 
#' 
#' @param replist list created by \code{SSoutput}
#' @param subplots vector controlling which subplots to create
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param areas optional subset of areas to plot for spatial models
#' @param areanames names for areas. Default is to use Area1, Area2,...
#' @param areacols vector of colors by area
#' @param pntscalar maximum bubble size for bubble plots; each plot scaled
#' independently based on this maximum size and the values plotted. Often some
#' plots look better with one value and others with a larger or smaller value.
#' Default=2.6
#' @param bublegend Add legend with example bubble sizes?
#' @param period indicator of whether to make plots using numbers at age just
#' from the beginning ("B") or middle of the year ("M") (new option starting
#' with SSv3.11)
#' @param add add to existing plot? (not yet implemented)
#' @param labels vector of labels for plots (titles and axis labels)
#' @param pwidth width of plot written to PNG file
#' @param pheight height of plot written to PNG file
#' @param punits units for PNG file
#' @param res resolution for PNG file
#' @param ptsize ptsize for PNG file
#' @param cex.main character expansion for plot titles
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param verbose report progress to R GUI?
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso \code{\link{SS_output}}, \code{\link{SS_plots}}
#' @keywords hplot
SSplotNumbers <-
  function(replist,subplots=1:9,
           plot=TRUE,print=FALSE,
           areas="all",
           areanames="default",
           areacols="default",
           pntscalar=2.6,
           bublegend=TRUE,
           period=c("B","M"),
           add=FALSE,
           labels=c("Year",                   #1
             "Age",                           #2
             "True age (yr)",                 #3
             "SD of observed age (yr)",       #4
             "Mean observed age (yr)",        #5
             "Mean age (yr)",                 #6
             "mean age in the population",    #7
             "Ageing imprecision",            #8
             "Numbers at age at equilibrium", #9
             "Equilibrium age distribution",  #10
             "Sex ratio of numbers at age (males/females)", #11
             "Length",                        #12
             "Mean length (cm)",              #13
             "mean length (cm) in the population", #14
             "expected numbers at age",       #15
             "Beginning of year",             #16
             "Middle of year",                #17
             "expected numbers at length",   #18
             "Sex ratio of numbers at length (males/females)", #19 (out of order, but I don't feel like runumbering)
             "Sex ratio of numbers at length (females/males)"), #20 (out of order, but I don't feel like runumbering)
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,
           cex.main=1,
           plotdir="default",
           verbose=TRUE)
{
  # plot various things related to numbers-at-age for Stock Synthesis

  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  natage    <- replist$natage
  natlen    <- replist$natlen
  if(plotdir=="default") plotdir <- replist$inputs$dir
  if(is.null(natage)){
    cat("Skipped some plots because NUMBERS_AT_AGE unavailable in report file\n",
        "     change starter file setting for 'detailed age-structured reports'\n")
  }else{
    # get more stuff from replist
    nsexes          <- replist$nsexes
    nareas          <- replist$nareas
    nseasons        <- replist$nseasons
    spawnseas       <- replist$spawnseas
    ngpatterns      <- replist$ngpatterns
    morphlist       <- replist$morphlist
    morph_indexing  <- replist$morph_indexing
    accuage         <- replist$accuage
    agebins         <- replist$agebins
    endyr           <- replist$endyr
    N_ageerror_defs <- replist$N_ageerror_defs
    AAK             <- replist$AAK
    age_error_mean  <- replist$age_error_mean
    age_error_sd    <- replist$age_error_sd
    lbinspop        <- replist$lbinspop
    nlbinspop       <- replist$nlbinspop
    recruitment_dist <- replist$recruitment_dist
    # this logic is now out of date, not sure why it wasn't coming from replist anyway
    #mainmorphs <- morph_indexing$Index[morph_indexing$Bseas==1 & morph_indexing$Sub_Morph_Dist==max(morph_indexing$Sub_Morph_Dist)]
    mainmorphs <- replist$mainmorphs

    if(!"BirthSeas" %in% names(natage)){
      cat("Numbers at age plots haven't been updated to work with SS version 3.30\n")
      return()
    }


    SS_versionshort <- toupper(substr(replist$SS_version,1,8))

    if(areas[1]=="all"){
      areas <- 1:nareas
    }else{ if(length(intersect(areas,1:nareas))!=length(areas)){
        stop("Input 'areas' should be 'all' or a vector of values between 1 and nareas.")
    }}
    if(areanames[1]=="default") areanames <- paste("area",1:nareas)

    if(areacols[1]=="default"){
      areacols  <- rich.colors.short(nareas)
      if(nareas > 2) areacols <- rich.colors.short(nareas+1)[-1]
    }

    if(SS_versionshort==c("SS-V3.10")) stop("numbers at age plots no longer supported for SS-V3.10 and earlier")

    ###########
    # Numbers at age plots

    # combining across submorphs and growth patterns
    column1 <- 12
    remove <- -(1:(column1-1)) # removes first group of columns

    bseas <- unique(natage$BirthSeas)
    if(length(bseas)>1) cat("Numbers at age plots are for only the first birth season\n")
    if(ngpatterns>1) cat("Numbers at age plots may not deal correctly with growth patterns: no guarantees!\n")
    if(nseasons>1) cat("Numbers at age plots are for season 1 only\n")
    for(iarea in areas){
      for(iperiod in 1:length(period)){
        for(m in 1:nsexes){
          # warning! implementation of birthseasons may not be correct in this section
          # data frame to combine values across factors
          natagetemp_all <- natage[natage$Area==iarea &
                                   natage$Gender==m &
                                   natage$Seas==1 &
                                   natage$Era!="VIRG" &
                                   !is.na(natage$"0") &
                                   natage$Yr < (endyr+2) &
                                   natage$BirthSeas==min(bseas),]
                                   # natage$Bio_Pattern==1,] # formerly filtered
          natagetemp_all <- natagetemp_all[natagetemp_all$"Beg/Mid"==period[iperiod],]
          # create data frame with 0 values to fill across submorphs
          morphlist <- unique(natagetemp_all$SubMorph)
          natagetemp0 <- natagetemp_all[natagetemp_all$SubMorph==morphlist[1] & natagetemp_all$Bio_Pattern==1,]
          for(iage in 0:accuage) natagetemp0[,column1 + iage] <- 0 # matrix of zeros for upcoming calculations

          for(imorph in 1:length(morphlist)){
            for(igp in 1:ngpatterns){
              natagetemp_imorph_igp <- natagetemp_all[natagetemp_all$SubMorph==morphlist[imorph] &
                                                      natagetemp_all$Bio_Pattern==igp,]
              natagetemp0[,column1+0:accuage] <- natagetemp0[,column1+0:accuage] + natagetemp_imorph_igp[,column1+0:accuage]
            } # end growth pattern loop
          } # end morph loop
          if(ngpatterns>0) natagetemp0$Bio_Pattern==999

          nyrsplot <- nrow(natagetemp0)
          resx <- rep(natagetemp0$Yr, accuage+1)
          resy <- NULL
          for(i in 0:accuage) resy <- c(resy,rep(i,nyrsplot))
          resz <- NULL
          for(i in column1+0:accuage) resz <- c(resz,natagetemp0[,i])

          # assign unique name to data frame for area, sex (beginning of year only)
          if(iperiod==1) assign(paste("natagetemp0area",iarea,"sex",m,sep=""),natagetemp0)

          if(m==1 & nsexes==1) sextitle <- ""
          if(m==1 & nsexes==2) sextitle <- " of females"
          if(m==2) sextitle=" of males"
          if(nareas>1) sextitle <- paste(sextitle," in ",areanames[iarea],sep="")
          if(period[iperiod]=="B") periodtitle <- labels[16] else
            if(period[iperiod]=="M") periodtitle <- labels[17] else
              stop("'period' input to SSplotNumbers should include only 'B' or 'M'")
          plottitle1 <- paste(periodtitle, " ", labels[15], sextitle," in thousands (max=",max(resz),")",sep="")

          # calculations related to mean age
          natagetemp1 <- as.matrix(natagetemp0[,remove]) # removing the first columns to get just numbers
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
          if(iperiod==1) natageyrsB <- natageyrs # unique name for beginning of year
          meanage <- 0*natageyrs

          for(i in 1:length(natageyrs)){ # averaging over values within a year (depending on birth season)
            meanage[i] <- sum(natagetemp2$meanage[natagetemp0$Yr==natageyrs[i]]*
                              natagetemp2$sum[natagetemp0$Yr==natageyrs[i]])/
                                sum(natagetemp2$sum[natagetemp0$Yr==natageyrs[i]])
          }

          if(m==1 & nsexes==2) meanagef <- meanage # save value for females in 2 sex models

          ylab <- labels[6]
          plottitle2 <- paste(periodtitle,labels[7])
          if(nareas>1) plottitle2 <- paste(plottitle2,"in",areanames[iarea])

          tempfun <- function(){
            # bubble plot with line
            bubble3(x=resx, y=resy, z=resz,
                    xlab=labels[1],ylab=labels[2],
                    legend=bublegend,
                    main=plottitle1,maxsize=(pntscalar+1.0),
                    las=1,cex.main=cex.main,allopen=1)
            lines(natageyrs,meanage,col="red",lwd=3)
          }
          tempfun2 <- function(){
            # mean age for males and femails
            ylim <- c(0, max(meanage, meanagef, na.rm=TRUE))
            plot(natageyrs,meanage,col="blue",lty=1,pch=4,xlab=labels[1],ylim=ylim,type="o",ylab=ylab,main=plottitle2,cex.main=cex.main)
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
              file <- paste(plotdir,"/numbers1",filepartarea,filepartsex,".png",sep="")
              caption <- plottitle1
              plotinfo <- pngfun(file=file, caption=caption)
              tempfun()
              dev.off()
            }
            # make 2-sex plot after looping over both sexes
            if(2 %in% subplots & m==2 & nsexes==2){
              file <- paste(plotdir,"/numbers2_meanage",filepartarea,".png",sep="")
              caption <- plottitle2
              plotinfo <- pngfun(file=file, caption=caption)
              tempfun2()
              dev.off()
            }
          } # end printing to PNG file
        } # end gender loop
      } # end period loop
    } # end area loop
    if(nsexes>1){
      for(iarea in areas){
        plottitle3 <- paste(labels[11],sep="")
        if(nareas > 1) plottitle3 <- paste(plottitle3," for ",areanames[iarea],sep="")

        natagef <- get(paste("natagetemp0area",iarea,"sex",1,sep=""))
        natagem <- get(paste("natagetemp0area",iarea,"sex",2,sep=""))
        natagefyrs <- natagef$Yr
        natageratio <- as.matrix(natagem[,remove]/natagef[,remove])
        natageratio[is.nan(natageratio)] <- NA
        if(diff(range(natageratio,finite=TRUE))!=0){
          tempfun3 <- function(...){
            contour(natagefyrs,0:accuage,natageratio,xaxs="i",yaxs="i",xlab=labels[1],ylab=labels[2],
              main=plottitle3,cex.main=cex.main,...)
          }
          if(plot & 3 %in% subplots){
            tempfun3(labcex=1)
          }
          if(print & 3 %in% subplots){
            filepart <- ""
            if(nareas > 1) filepart <- paste("_",areanames[iarea],filepart,sep="")
            file <- paste(plotdir,"/numbers3_ratio_age",filepart,".png",sep="")
            caption <- plottitle3
            plotinfo <- pngfun(file=file, caption=caption)
            tempfun3(labcex=0.4)
            dev.off()}
        }else{
          cat("skipped sex ratio contour plot because ratio=1 for all ages and years\n")
        }
      } # end area loop
    } # end if nsexes>1


    ##########
    # repeat code above for numbers at length
    if(length(intersect(6:7, subplots))>1) # do these numbers at length plots
    {
      column1 <- column1 - 1 # because index of lengths starts at 1, not 0 as in ages

      for(iarea in areas){
        for(iperiod in 1:length(period)){
          for(m in 1:nsexes){
            # warning! implementation of birthseasons may not be correct in this section
            # data frame to combine values across factors
            natlentemp_all <- natlen[natlen$Area==iarea &
                                     natlen$Gender==m &
                                     natlen$Seas==1 &
                                     natlen$Era!="VIRG" &
                                     natlen$Yr < (endyr+2) &
                                     natlen$BirthSeas==min(bseas),]
                                     # natlen$Bio_Pattern==1,] # formerly filtered
            natlentemp_all <- natlentemp_all[natlentemp_all$"Beg/Mid"==period[iperiod],]

            # create data frame with 0 values to fill across submorphs
            morphlist <- unique(natlentemp_all$SubMorph)
            natlentemp0 <- natlentemp_all[natlentemp_all$SubMorph==morphlist[1] & natlentemp_all$Bio_Pattern==1,]
            for(ilen in 1:nlbinspop) natlentemp0[,column1 + ilen] <- 0 # matrix of zeros for upcoming calculations

            for(imorph in 1:length(morphlist)){
              for(igp in 1:ngpatterns){
                natlentemp_imorph_igp <- natlentemp_all[natlentemp_all$SubMorph==morphlist[imorph] &
                                                        natlentemp_all$Bio_Pattern==igp,]
                natlentemp0[,column1+1:nlbinspop] <- natlentemp0[,column1+1:nlbinspop] + natlentemp_imorph_igp[,column1+1:nlbinspop]
              } # end growth pattern loop
            } # end morph loop
            if(ngpatterns>0) natlentemp0$Bio_Pattern==999

            nyrsplot <- nrow(natlentemp0)
            resx <- rep(natlentemp0$Yr, nlbinspop)
            resy <- NULL
            for(ilen in 1:nlbinspop) resy <- c(resy,rep(lbinspop[ilen],nyrsplot))
            resz <- NULL
            for(ilen in column1+1:nlbinspop) resz <- c(resz,natlentemp0[,ilen])

            # assign unique name to data frame for area, sex
            assign(paste("natlentemp0area",iarea,"sex",m,sep=""),natlentemp0)

            if(m==1 & nsexes==1) sextitle <- ""
            if(m==1 & nsexes==2) sextitle <- " of females"
            if(m==2) sextitle=" of males"
            if(nareas>1) sextitle <- paste(sextitle," in ",areanames[iarea],sep="")
            if(period[iperiod]=="B") periodtitle <- labels[16] else
            if(period[iperiod]=="M") periodtitle <- labels[17] else
            stop("'period' input to SSplotNumbers should include only 'B' or 'M'")
            plottitle1 <- paste(periodtitle," ",labels[18], sextitle," in thousands (max=",max(resz),")",sep="")

            # calculations related to mean len
            natlentemp1 <- as.matrix(natlentemp0[,remove]) # removing the first columns to get just numbers
            natlentemp2 <- as.data.frame(natlentemp1)
            natlentemp2$sum <- as.vector(apply(natlentemp1,1,sum))

            # remove rows with 0 fish (i.e. no growth pattern in this area)
            natlentemp0 <- natlentemp0[natlentemp2$sum > 0, ]
            natlentemp1 <- natlentemp1[natlentemp2$sum > 0, ]
            natlentemp2 <- natlentemp2[natlentemp2$sum > 0, ]
            prodmat <- t(natlentemp1)*lbinspop

            prodsum <- as.vector(apply(prodmat,2,sum))
            natlentemp2$sumprod <- prodsum
            natlentemp2$meanlen <- natlentemp2$sumprod/natlentemp2$sum - (natlentemp0$BirthSeas-1)/nseasons
            natlenyrs <- sort(unique(natlentemp0$Yr))
            if(iperiod==1) natlenyrsB <- natlenyrs # unique name for beginning of year

            meanlen <- 0*natlenyrs
            for(i in 1:length(natlenyrs)){ # averaging over values within a year (depending on birth season)
              meanlen[i] <- sum(natlentemp2$meanlen[natlentemp0$Yr==natlenyrs[i]]*natlentemp2$sum[natlentemp0$Yr==natlenyrs[i]])/sum(natlentemp2$sum[natlentemp0$Yr==natlenyrs[i]])}

            if(m==1 & nsexes==2) meanlenf <- meanlenf <- meanlen # save value for females in 2 sex models

            ylab <- labels[13]
            plottitle2 <- paste(periodtitle,labels[14])
            if(nareas>1) plottitle2 <- paste(plottitle2,"in",areanames[iarea])

            tempfun4 <- function(){
              # bubble plot with line
              bubble3(x=resx, y=resy, z=resz,
                      xlab=labels[1],ylab=labels[12],
                      legend=bublegend,
                      main=plottitle1,maxsize=(pntscalar+1.0),
                      las=1,cex.main=cex.main,allopen=1)
              lines(natlenyrs,meanlen,col="red",lwd=3)
            }
            tempfun5 <- function(){
              # mean length for males and females
              ylim <- c(0, max(meanlen, meanlenf))
              plot(natlenyrs,meanlen,col="blue",lty=1,pch=4,xlab=labels[1],ylim=ylim,
                   type="o",ylab=ylab,main=plottitle2,cex.main=cex.main)
              points(natlenyrs,meanlenf,col="red",lty=2,pch=1,type="o")
              legend("bottomleft",bty="n", c("Females","Males"), lty=c(2,1), pch=c(1,4), col = c("red","blue"))
            }
            if(plot){
              if(6 %in% subplots) tempfun4()
              if(7 %in% subplots & m==2 & nsexes==2) tempfun5()
            }
            if(print){
              filepartsex <- paste("_sex",m,sep="")
              filepartarea <- ""
              if(nareas > 1) filepartarea <- paste("_",areanames[iarea],sep="")
              if(6 %in% subplots){
                file <- paste(plotdir,"/numbers6_len",filepartarea,filepartsex,".png",sep="")
                caption <- plottitle1
                plotinfo <- pngfun(file=file, caption=caption)
                tempfun4()
                dev.off()
              }
              # make 2-sex plot after looping over both sexes
              if(7 %in% subplots & m==2 & nsexes==2){
                file <- paste(plotdir,"/numbers7_meanlen",filepartarea,".png",sep="")
                caption <- plottitle2
                plotinfo <- pngfun(file=file, caption=caption)
                tempfun5()
                dev.off()
              }
            } # end printing of plot 14
          } # end gender loop
        } # end period loop
      } # end area loop

      if(nsexes>1){
        for(iarea in areas){

          natlenf <- get(paste("natlentemp0area",iarea,"sex",1,sep=""))
          natlenm <- get(paste("natlentemp0area",iarea,"sex",2,sep=""))
          natlenratio <- as.matrix(natlenm[,remove]/natlenf[,remove])
          if(diff(range(natlenratio,finite=TRUE))!=0){
            tempfun6 <- function(males.to.females=TRUE,...){
              if(males.to.females){
                main <- labels[19]
                z <- natlenratio
              }else{
                main <- labels[20]
                z <- 1/natlenratio
              }
              if(nareas > 1) main <- paste(main," for ",areanames[iarea],sep="")
              contour(natlenyrsB,lbinspop,z,
                      xaxs="i",yaxs="i",xlab=labels[1],ylab=labels[12],
                      main=main,cex.main=cex.main,...)
            }
            if(plot & 8 %in% subplots){
              tempfun6(males.to.females=TRUE,labcex=1)
            }
            if(plot & 9 %in% subplots){
              tempfun6(males.to.females=FALSE,labcex=1)
            }
            if(print & 8 %in% subplots){
              filepart <- ""
              if(nareas > 1) filepart <- paste("_",areanames[iarea],filepart,sep="")
              file <- paste(plotdir,"/numbers8_ratio_len1",filepart,".png",sep="")
              caption <- labels[19]
              plotinfo <- pngfun(file=file, caption=caption)
              tempfun6(labcex=0.4)
              dev.off()
            }
            if(print & 9 %in% subplots){
              filepart <- ""
              if(nareas > 1) filepart <- paste("_",areanames[iarea],filepart,sep="")
              file <- paste(plotdir,"/numbers8_ratio_len2",filepart,".png",sep="")
              caption <- labels[20]
              plotinfo <- pngfun(file=file, caption=caption)
              tempfun6(labcex=0.4)
              dev.off()
            }
          }else{
            cat("skipped sex ratio contour plot because ratio=1 for all lengths and years\n")
          }
        } # end area loop
      } # end if nsexes>1

    } # end numbers at length plots

    ##########
    # plot of equilibrium age composition by gender and area
    equilibfun <- function(){
      equilage <- natage[natage$Era=="VIRG",]
      equilage <- equilage[as.vector(apply(equilage[,remove],1,sum))>0,]


      BirthSeas <- spawnseas
      if(!(spawnseas %in% bseas)) BirthSeas <- min(bseas)
      if(length(bseas)>1) cat("showing equilibrium age for first birth season",BirthSeas,"\n")

      plot(0,type='n',xlim=c(0,accuage),
           ylim=c(0,1.05*max(equilage[equilage$BirthSeas==BirthSeas
             & equilage$Seas==BirthSeas,remove])),
           xaxs='i',yaxs='i',xlab='Age',ylab=labels[9],main=labels[10],cex.main=cex.main)

      # now fill in legend
      legendlty <- NULL
      legendcol <- NULL
      legendlegend <- NULL
      for(iarea in areas){
        for(m in 1:nsexes){
          equilagetemp <- equilage[equilage$Area==iarea & equilage$Gender==m
                                   & equilage$BirthSeas==BirthSeas
                                   & equilage$Seas==BirthSeas,]
          if(nrow(equilagetemp)>1){
            cat("in plot of equilibrium age composition by gender and area\n",
                "multiple morphs are not supported, using first row from choices below\n")
            print(equilagetemp[,1:10])
          }
          equilagetemp <- equilagetemp[1,remove]
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
      if(length(legendlegend)>1) legend("topright",legend=legendlegend,col=legendcol,lty=legendlty,lwd=3)
    }

    if(plot & 4 %in% subplots){
      equilibfun()
    }
    if(print & 4 %in% subplots){
      file=paste(plotdir,"/numbers4_equilagecomp.png",sep="")
      caption <- labels[10]
      plotinfo <- pngfun(file=file, caption=caption)
      equilibfun()
      dev.off()
    } # end print to PNG

    # plot the ageing imprecision for all age methods
    if(N_ageerror_defs > 0){
      xvals <- age_error_sd$age + 0.5
      yvals <- age_error_sd[,-1]
      ylim <- c(0,max(yvals))
      if(N_ageerror_defs == 1) colvec <- "black" else colvec <- rich.colors.short(N_ageerror_defs)

      ageingfun <- function(){
        matplot(xvals,yvals,ylim=ylim,type="o",pch=1,lty=1,col=colvec,
                xlab=labels[3],ylab=labels[4],main=labels[8],cex.main=cex.main)
        abline(h=0,col="grey") # grey line at 0
        legend('topleft',bty='n',pch=1,lty=1,col=colvec,
               ncol=ifelse(N_ageerror_defs<20,1,2), # more columns for crazy models like hake
               legend=paste("Ageing method",1:N_ageerror_defs))
      }

      # check for bias in ageing error pattern
      ageingbias <- age_error_mean[,-1] - (age_error_mean$age+0.5)

      if(mean(ageingbias==0)!=1){
        ageingfun2 <- function(){
          yvals <- age_error_mean[,-1]
          ylim <- c(0,max(yvals))
          matplot(xvals,yvals,ylim=ylim,type="o",pch=1,lty=1,col=colvec,
                  xlab=labels[3],ylab=labels[5],main=labels[8])
          abline(h=0,col="grey") # grey line at 0
          abline(0,1,col="grey") # grey line with slope = 1
          legend('topleft',bty='n',pch=1,lty=1,col=colvec,
                 ncol=ifelse(N_ageerror_defs<20,1,2), # more columns for crazy models like hake
                 legend=paste("Ageing method",1:N_ageerror_defs))
        }
      }

      ageing_matrix_fun <- function(i_ageerror_def){
        ## function to make shaded image illustrating true vs. obs. age
        
        # change label from "Mean observered age" to "Observed age"
        # this could be additional input instead
        ylab <- gsub(pattern="Mean o", replacement="O", x=labels[5])
        # take subset of age-age-key that for particular method
        # but only the rows for females because males should always be identical
        agebins.tmp <- sort(unique(as.numeric(dimnames(AAK)$ObsAgeBin)))
        z <- t(AAK[i_ageerror_def, rev(1:length(agebins.tmp)), ])
        # make image
        image(x=as.numeric(rownames(z)),
              y=as.numeric(colnames(z)),
              z=z,
              xlab=labels[3],
              ylab=ylab,
              main=paste(labels[8], ": matrix for method ", i_ageerror_def, sep=""),
              axes=FALSE)
        if(accuage<=40){
          axis(1, at=0:accuage)
          axis(2, at=agebins.tmp, las=2)
        }          
        if(accuage>40){
          axis(1, at=0:accuage, labels=rep("",accuage+1))
          axis(1, at=seq(0,accuage,5))
          axis(2, at=agebins.tmp, labels=rep("",length(agebins.tmp)))
          axis(2, at=agebins.tmp[agebins.tmp %in% seq(0,accuage,5)], las=2)
        }
        box()
      }

      # run functions to make requested plots
      
      if(plot & 5 %in% subplots){
        # make plots of age error standard deviations
        ageingfun()
        # make plots of age error means
        if(mean(ageingbias==0)!=1) ageingfun2()
        # make plots of age error matrices
        for(i_ageerror_def in 1:N_ageerror_defs){
          ageing_matrix_fun(i_ageerror_def)
        }
      }
      if(print & 5 %in% subplots){
        # make files with plots of age error standard deviations
        file <- paste(plotdir,"/numbers5_ageerrorSD.png",sep="")
        caption <- paste(labels[8], ": ", labels[4], sep="")
        plotinfo <- pngfun(file=file, caption=caption)
        ageingfun()
        dev.off()

        # make files with plots of age error means
        if(mean(ageingbias==0)!=1){
          file <- paste(plotdir,"/numbers5_ageerrorMeans.png",sep="")
          caption <- paste(labels[8], ": ", labels[5], sep="")
          plotinfo <- pngfun(file=file, caption=caption)
          ageingfun2()
          dev.off()
        }

        # make files with plots of age error matrices
        for(i_ageerror_def in 1:N_ageerror_defs){
          file <- paste(plotdir,"/numbers5_ageerror_matrix_",
                        i_ageerror_def,".png",sep="")
          caption <- paste(labels[8], ": matrix for method ", i_ageerror_def, sep="")
          plotinfo <- pngfun(file=file, caption=caption)
          ageingfun()
          ageing_matrix_fun(i_ageerror_def)
          dev.off()
        } # end loop over ageing error methods
        
      } # end print to PNG
    } # end if AAK
  } # end if data available
  if(!is.null(plotinfo)) plotinfo$category <- "Numbers"
  return(invisible(plotinfo))
} # end function
