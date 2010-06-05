SSplotComps <-
  function(replist=replist,
           kind="LEN", aalyear=-1, aalbin=-1, GUI=T, png=F, plotdir=NA, fleets="all",
           datonly=F, Natageplot=T, samplesizeplots=T, compresidplots=T, bub=F, showsampsize=T, showeffN=T,
           minnbubble=8, pntscalar=2.6, pwidth=7, pheight=7, punits="in", ptsize=12, res=300, cex.main=1,
           linepos=1, fitbar=F,maxsize=3,do.sqrt=TRUE,smooth=TRUE,cohortlines=c(),
           labels = c("Length (cm)",           #1
                      "Age (yr)",              #2
                      "Year",                  #3
                      "Observed sample size",  #4
                      "Effective sample size", #5
                      "Proportion",            #6
                      "cm",                    #7
                      "Frequency",             #8
                      "Weight (lbs)",          #9
                      "(mt)",                  #10
                      "(numbers x1000)"),      #11
           printmkt=T,printsex=T,
           maxrows=6,maxcols=6,maxrows2=2,maxcols2=4,
           fixdims=T,maxneff=5000,returntitles=T,verbose=T,
           aalmaxbinrange=0,...)
{
  ################################################################################
  # SSplotComps June 4, 2010
  ################################################################################

  nfleets   <- replist$nfleets
  nseasons   <- replist$nseasons
  compdbase <- replist$composition_database
  FleetNames <- replist$FleeNames

  # configure seasons
  if(nseasons>1) compdbase$YrSeasName <- paste(floor(compdbase$Yr),"s",compdbase$Seas,sep="") else compdbase$YrSeasName <- compdbase$Yr

  # deal with Lbins
  compdbase$Lbin_range <- compdbase$Lbin_hi - compdbase$Lbin_lo
  compdbase$Lbin_mid <- 0.5*(compdbase$Lbin_lo + compdbase$Lbin_hi)

  # divide into objects by kind
  lendbase         <- compdbase[compdbase$Kind=="LEN" & compdbase$N > 0,]
  sizedbase        <- compdbase[compdbase$Kind=="SIZE" & compdbase$N > 0,]
  agedbase         <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0
                        & !is.na(compdbase$Lbin_range) & compdbase$Lbin_range > aalmaxbinrange,]
  condbase         <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0
                        & !is.na(compdbase$Lbin_range) & compdbase$Lbin_range <= aalmaxbinrange,]
  ghostagedbase  <- compdbase[compdbase$Kind=="AGE" & compdbase$N < 0
                        & !is.na(compdbase$Lbin_range) & compdbase$Lbin_range > aalmaxbinrange,]
  compdbase$Kind[compdbase$Kind=="L@A" & compdbase$Ageerr < 0] <- "W@A"
  ladbase          <- compdbase[compdbase$Kind=="L@A" & !is.na(compdbase$N),]
  wadbase          <- compdbase[compdbase$Kind=="W@A" & !is.na(compdbase$N),]
  tagdbase1        <- compdbase[compdbase$Kind=="TAG1",]
  tagdbase2        <- compdbase[compdbase$Kind=="TAG2",]
  # consider range of bins for conditional age at length data

  ### temporarily turning off this stuff until it gets put in SSv3_output

  ## print(paste("CompReport file separated by this code as follows (rows = num. comps * num. bins):"),quote=FALSE)
  ## print(paste("  ",nrow(lendbase),"rows of length comp data,"),quote=FALSE)
  ## print(paste("  ",nrow(sizedbase),"rows of generalized size comp data,"),quote=FALSE)
  ## print(paste("  ",nrow(agedbase),"rows of age comp data,"),quote=FALSE)
  ## print(paste("  ",nrow(condbase),"rows of conditional age-at-length data, and"),quote=FALSE)
  ## print(paste("  ",nrow(ghostagedbase),"rows of ghost fleet age comp data"),quote=FALSE)
  ## print(paste("  ",nrow(ladbase),"rows of mean length at age data"),quote=FALSE)
  ## print(paste("  ",nrow(wadbase),"rows of mean weight at age data"),quote=FALSE)
  ## print(paste("  ",nrow(tagdbase1),"rows of 'TAG1' comp data"),quote=FALSE)
  ## print(paste("  ",nrow(tagdbase2),"rows of 'TAG2' comp data"),quote=FALSE)
  Lbin_ranges <- as.data.frame(table(agedbase$Lbin_range))
  names(Lbin_ranges)[1] <- "Lbin_hi-Lbin_lo"
  if(length(unique(agedbase$Lbin_range)) > 1){
    print("Warning!: different ranges of Lbin_lo to Lbin_hi found in age comps.",quote=FALSE)
    print(Lbin_ranges)
    print("  consider increasing 'aalmaxbinrange' to designate",quote=FALSE)
    print("  some of these data as conditional age-at-length",quote=FALSE)
  }
  # convert bin indices to true lengths

  ## # don't remember why these are not converted to numeric in SSv3_output, nor why they aren't done in 1 step to compdbase
  ## lendbase$effN  <- as.numeric(lendbase$effN)
  ## sizedbase$effN <- as.numeric(sizedbase$effN)
  ## agedbase$effN  <- as.numeric(agedbase$effN)
  ## condbase$effN  <- as.numeric(condbase$effN)


  if(!exists("make_multifig"))
  print("you are missing the function 'make_mulitifig'")

  titles <- NULL
  if(png) if(is.na(plotdir)) return("plotdir must be specified to write png files.")

  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  }else{
    if(length(intersect(fleets,1:nfleets))!=length(fleets)){
      return("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
    }
  }

  # a few quantities related to data type and plot number
  if(kind=="LEN"){
    dbase_kind <- lendbase
    kindlab=labels[1]
    if(datonly){
      filenamestart <- "15_lendat_"
      titledata <- "length comp data, "
    }else{
      filenamestart <- "18_lenfit_"
      titledata <- "length comps, "
    }
  }
  if(kind=="SIZE"){
    dbase_kind <- sizedbase
    kindlab=labels[9]
    if(datonly){
      filenamestart <- "15_sizedat_"
      titledata <- "size comp data, "
    }else{
      filenamestart <- "18_sizefit_"
      titledata <- "size comps, "
    }
  }
  if(kind=="AGE"){
    dbase_kind <- agedbase
    kindlab=labels[2]
    if(datonly){
      filenamestart <- "16_agedat_"
      titledata <- "age comp data, "
    }else{
      filenamestart <- "19_agefit_"
      titledata <- "age comps, "
    }
  }
  if(kind=="cond"){
    dbase_kind <- condbase
    kindlab=labels[2]
    if(datonly){
      filenamestart <- "17_condAALdat_"
      titledata <- "conditional age at length data, "
    }else{
      filenamestart <- "20_condAALfit_"
      titledata <- "conditional age at length, "
    }
  }

  if(kind=="GSTAGE"){
    dbase_kind <- ghostagedbase
    kindlab=labels[2]
    if(datonly){
      filenamestart <- "16_gstagedat_"
      titledata <- "gst age comp data, "
    }else{
      filenamestart <- "19_gstagefit_"
      titledata <- "gst age comps, "
    }
  }
  if(kind=="L@A"){
    dbase_kind <- ladbase
    kindlab=labels[2]
    filenamestart <- "21_lenatagefit_"
    titledata <- "mean length at age, "
  }
  if(kind=="W@A"){
    dbase_kind <- wadbase
    kindlab=labels[2]
    filenamestart <- "21_wtatagefit_"
    titledata <- "mean weight at age, "
  }
  if(!(kind%in%c("LEN","SIZE","AGE","cond","GSTAGE","L@A","W@A"))) return("Input 'kind' to SSv3_plot_comps is not right.")
  # loop over fleets
  for(f in fleets)
  {
    # check for the presence of data
    if(length(dbase_kind$Obs[dbase_kind$Fleet==f])>0)
    {
      dbasef <- dbase_kind[dbase_kind$Fleet==f,]
      testor    <- length(dbasef$Gender[dbasef$Gender==1 & dbasef$Pick_gender==0 ])>0
      testor[2] <- length(dbasef$Gender[dbasef$Gender==1 & dbasef$Pick_gender %in% c(1,3)])>0
      testor[3] <- length(dbasef$Gender[dbasef$Gender==2])>0

      # loop over genders combinations
      for(k in (1:3)[testor])
      {
        if(k==1){dbase_k <- dbasef[dbasef$Gender==1 & dbasef$Pick_gender==0,]}
        if(k==2){dbase_k <- dbasef[dbasef$Gender==1 & dbasef$Pick_gender %in% c(1,3),]}
        if(k==3){dbase_k <- dbasef[dbasef$Gender==2,]}
        sex <- ifelse(k==3, 2, 1)

        # loop over partitions (discard, retain, total)
        for(j in unique(dbase_k$Part))
        {
          # dbase is the final data.frame used in the individual plots
          # it is subset based on the kind (age, len, age-at-len), fleet, gender, and partition
          dbase <- dbase_k[dbase_k$Part==j,]

          ## assemble pieces of plot title
          # sex
          if(k==1) titlesex <- "sexes combined, "
          if(k==2) titlesex <- "female, "
          if(k==3) titlesex <- "male, "
          titlesex <- ifelse(printsex,titlesex,"")

          # market category
          if(j==0) titlemkt <- "whole catch, "
          if(j==1) titlemkt <- "discard, "
          if(j==2) titlemkt <- "retained, "
          titlemkt <- ifelse(printmkt,titlemkt,"")

          # plot bars for data only or if input 'fitbar=T'
          if(datonly | fitbar) bars <- T else bars <- F

          # aggregating identifiers for plot titles and filenames
          title_sexmkt <- paste(titlesex,titlemkt,sep="")
          filename_fltsexmkt <- paste("flt",f,"sex",sex,"mkt",j,sep="")

          ### subplot 1: multi-panel composition plot
          if(kind!="cond") # for age or length comps, but not conditional AAL
          {
          ptitle <- paste(titledata,title_sexmkt, FleetNames[f],sep="") # total title
          titles <- c(ptitle,titles) # compiling list of all plot titles
          tempfun <- function(ipage,...){
            if(!(kind %in% c("GSTAGE","L@A","W@A"))){
              make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
                            sampsize=dbase$N,effN=dbase$effN,showsampsize=showsampsize,showeffN=showeffN,
                            bars=bars,linepos=(1-datonly)*linepos,
                            nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
                            main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                            maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,ipage=ipage,...)
            }
            if(kind=="GSTAGE"){
                make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
                              sampsize=dbase$N,effN=dbase$effN,showsampsize=F,showeffN=F,
                              bars=bars,linepos=(1-datonly)*linepos,
                              nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                              maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,ipage=ipage,...)
            }
            if(kind %in% c("L@A","W@A")){
                make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
                              sampsize=dbase$N,effN=0,showsampsize=F,showeffN=F,
                              nlegends=1,legtext=list(dbase$YrSeasName),
                              bars=bars,linepos=(1-datonly)*linepos,
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=ifelse(kind=="W@A",labels[9],labels[1]),
                              maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,ipage=ipage,...)
            }

          }
          if(GUI) tempfun(ipage=0,...)
          if(png){ # set up plotting to png file if required
            npages <- ceiling(length(unique(dbase$Yr))/maxrows/maxcols)
            for(ipage in 1:npages)
            {
                if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                filename <- paste(plotdir,filenamestart,filename_fltsexmkt,pagetext,".png",sep="")
                png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                tempfun(ipage=ipage,...)
                dev.off()
            }
          }
          }

          # some things related to the next two bubble plots (single or multi-panel)
          if(datonly){
            z <- dbase$Obs
            col <- rep("black",2)
            titletype <- titledata
            filetype <- "bub"
            allopen <- TRUE
          }else{
            z <- dbase$Pearson
            col <- rep("blue",2)
            titletype <- "Pearson residuals, "
            filetype <- "resids"
            allopen <- FALSE
          }

          ### subplot 2: single panel bubble plot for numbers at length or age
          if(bub & kind!="cond")
          {
            ptitle <- paste(titletype, title_sexmkt, FleetNames[f],sep="")
            ptitle <- paste(ptitle," (max=",round(max(z),digits=2),")",sep="")
            titles <- c(ptitle,titles) # compiling list of all plot titles

            tempfun <- function(){
              bubble3(x=dbase$Yr, y=dbase$Bin, z=z, xlab=labels[3],ylab=kindlab,col=col,
                    las=1,main=ptitle,cex.main=cex.main,maxsize=pntscalar,allopen=allopen,minnbubble=minnbubble)
              # add lines for growth of individual cohorts if requested
              if(length(cohortlines)>0){
                  for(icohort in 1:length(cohortlines)){
                    print(paste("Adding line for",cohortlines[icohort],"cohort"),quote=F)
                    if(k %in% c(1,2)) lines(growdatF$Age+cohortlines[icohort],growdatF$Len_Mid, col="red")  #females
                    if(k %in% c(1,3)) lines(growdatM$Age+cohortlines[icohort],growdatM$Len_Mid, col="blue") #males
                  }
              }
            }

            if(GUI) tempfun()
            if(png){ # set up plotting to png file if required
              filename <- paste(plotdir,filenamestart,filetype,filename_fltsexmkt,".png",sep="")
              png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              tempfun()
              dev.off() # close device if png
            }
          } # end bubble plot

          ### subplot 3: multi-panel bubble plots for conditional age at length
          if(kind=="cond")
          {
            ptitle <- paste(titletype, title_sexmkt, FleetNames[f],sep="")
            ptitle <- paste(ptitle," (max=",round(max(z),digits=2),")",sep="")
            titles <- c(ptitle,titles) # compiling list of all plot titles
            tempfun <- function(ipage,...){
              make_multifig(ptsx=dbase$Bin,ptsy=dbase$Lbin_mid,yr=dbase$Yr,size=z,
                        sampsize=dbase$N,showsampsize=showsampsize,showeffN=F,
                        nlegends=1,legtext=list(dbase$YrSeasName),
                        bars=F,linepos=0,main=ptitle,cex.main=cex.main,
                        xlab=labels[2],ylab=labels[1],ymin0=F,maxrows=maxrows2,maxcols=maxcols2,
                        fixdims=fixdims,allopen=allopen,minnbubble=minnbubble,
                        ptscol=col[1],ptscol2=col[2],ipage=ipage,...)
            }
            if(GUI) tempfun(ipage=0,...)
            if(png){ # set up plotting to png file if required
              npages <- ceiling(length(unique(dbase$Yr))/maxrows2/maxcols2)
              for(ipage in 1:npages)
              {
                  if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                  filename <- paste(plotdir,filenamestart,filetype,filename_fltsexmkt,pagetext,".png",sep="")
                  png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                  tempfun(ipage=ipage,...)
                  dev.off() # close device if png
              }
            }
          } # end conditional bubble plot

          ### subplots 4 and 5: multi-panel plot of point and line fit to conditional age at length
          #                        and Pearson residuals of A-L key for specific years
          if(aalyear[1] > 0 & kind=="cond")
          {
          for(y in 1:length(aalyear))
          {
            aalyr <- aalyear[y]
            if(length(dbase$Obs[dbase$Yr==aalyr])>0)
            {
              ### subplot 4: multi-panel plot of fit to conditional age at length for specific years
              ptitle <- paste(aalyr," age at length bin, ",title_sexmkt,FleetNames[f],sep="")
              titles <- c(ptitle,titles) # compiling list of all plot titles
              ydbase <- dbase[dbase$Yr==aalyr,]
              lenbinlegend <- paste(ydbase$Lbin_lo,labels[7],sep="")
              lenbinlegend[ydbase$Lbin_range>0] <- paste(ydbase$Lbin_lo,"-",ydbase$Lbin_hi,labels[7],sep="")
              tempfun <- function(ipage,...){ # temporary function to aid repeating the big function call
                  make_multifig(ptsx=ydbase$Bin,ptsy=ydbase$Obs,yr=ydbase$Lbin_lo,
                                linesx=ydbase$Bin,linesy=ydbase$Exp,
                                sampsize=ydbase$N,effN=ydbase$effN,showsampsize=showsampsize,showeffN=showeffN,
                                nlegends=3,legtext=list(lenbinlegend,"sampsize","effN"),
                                bars=F,linepos=linepos,main=ptitle,cex.main=cex.main,
                                xlab=labels[2],ylab=labels[6],maxrows=maxrows,maxcols=maxcols,
                                fixdims=fixdims,ipage=ipage,...)
              }
              if(GUI) tempfun(ipage=0,...)
              if(png){
                  npages <- ceiling(length(unique(ydbase$Yr))/maxrows/maxcols)
                  for(ipage in 1:npages)
                  {
                      if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                      filename <- paste(plotdir,filenamestart,filename_fltsexmkt,"_",aalyr,"_",pagetext,".png",sep="")
                      png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                      tempfun(ipage=ipage,...)
                      dev.off() # close device if png
                  }
              }

              ### subplot 5: Pearson residuals for A-L key
              z <- ydbase$Pearson
              ptitle <- paste(aalyr," Pearson residuals for A-L key, ",title_sexmkt,FleetNames[f],sep="")
              ptitle <- paste(ptitle," (max=",round(abs(max(z)),digits=2),")",sep="")
              titles <- c(ptitle,titles) # compiling list of all plot titles
              tempfun <- function(){
                  bubble3(x=ydbase$Bin,y=ydbase$Lbin_lo,z=z,xlab=labels[2],ylab=labels[1],col=rep("blue",2),
                          las=1,main=ptitle,cex.main=cex.main,maxsize=pntscalar,allopen=F,minnbubble=minnbubble)
              }
              if(GUI) tempfun()
              if(png)
              {
                  filename <- paste(plotdir,filenamestart,"yearresids_",filename_fltsexmkt,"_",aalyr,".png",sep="")
                  png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                  tempfun()
                  dev.off() # close device if png
              }
            }
          }
          }

          ### subplot 6: multi-panel plot of point and line fit to conditional age at length
          #                   for specific length bins
          if(aalbin[1] > 0)
          {
            badbins <- setdiff(aalbin, dbase$Lbin_hi)
            goodbins <- intersect(aalbin, dbase$Lbin_hi)
            if(length(badbins)>0){
              print(paste("Error! the following inputs for 'aalbin' do not match the Lbin_hi values for the conditional age at length data:",badbins),quote=F)
              print(paste("            the following inputs for 'aalbin' are fine:",goodbins),quote=F)
            }
            if(length(goodbins)>0)
            {
              for(ibin in 1:length(goodbins)) # loop over good bins
              {
              ilenbin <- goodbins[ibin]
              abindbase <- dbase[dbase$Lbin_hi==ilenbin,]
              if(nrow(abindbase)>0) # check for data associated with this bin
              {
                ptitle <- paste("Age at length ",ilenbin,labels[7],", ",title_sexmkt,FleetNames[f],sep="")
                titles <- c(ptitle,titles) # compiling list of all plot titles
                tempfun <- function(ipage,...){ # temporary function to aid repeating the big function call
                    make_multifig(ptsx=abindbase$Bin,ptsy=abindbase$Obs,yr=abindbase$Yr,linesx=abindbase$Bin,linesy=abindbase$Exp,
                              sampsize=abindbase$N,effN=abindbase$effN,showsampsize=showsampsize,showeffN=showeffN,
                              nlegends=3,legtext=list(abindbase$YrSeasName,"sampsize","effN"),
                              bars=bars,linepos=(1-datonly)*linepos,
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],maxrows=maxrows,maxcols=maxcols,
                              fixdims=fixdims,ipage=ipage,...)
                }
                if(GUI) tempfun(ipage=0,...)
                if(png){
                  npages <- ceiling(length(unique(abindbase$Yr))/maxrows/maxcols)
                  for(ipage in 1:npages)
                  {
                    if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                    filename <- paste(plotdir,filenamestart,filename_fltsexmkt,"_length",ilenbin,labels[7],pagetext,".png",sep="")
                    png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                    tempfun(ipage=ipage,...)
                    dev.off() # close device if png
                  }
                } # end png
              } # end if data
            } # end loop over length bins
          } # end if length(goodbins)>0
          } # end if plot requested

          ### subplot 7: sample size plot
          if(samplesizeplots & !datonly & !(kind %in% c("GSTAGE","L@A","W@A")))
          {
            ptitle <- paste("N-EffN comparison, ",titledata,title_sexmkt,FleetNames[f], sep="")
            titles <- c(ptitle,titles) # compiling list of all plot titles
            lfitfunc <- function()
            {
              if(kind=="cond"){
                  # trap nonrobust effective n's
                  # should this only be for conditional age at length or all plots?
                  dbasegood <- dbase[dbase$Obs>=0.0001 & dbase$Exp<0.99 & !is.na(dbase$effN) & dbase$effN<maxneff,]
              }else{
                  dbasegood <- dbase
              }
              if(nrow(dbasegood)>0)
              {
                  plot(dbasegood$N,dbasegood$effN,xlab=labels[4],main=ptitle,cex.main=cex.main,
                     ylim=c(0,1.05*max(dbasegood$effN)),xlim=c(0,1.05*max(dbasegood$N)),
                     col="blue",pch=19,ylab=labels[5],xaxs="i",yaxs="i")
                  abline(h=0,col="grey")
                  abline(0,1,col="black")
                  # add loess smoother if there's at least 6 points with a range greater than 2
                  if(smooth & length(unique(dbasegood$N)) > 6 & diff(range(dbasegood$N))>2)
                  {
                    psmooth <- loess(dbasegood$effN~dbasegood$N,degree=1)
                    lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")
                  }
              }
            }
            if(GUI) lfitfunc()
            if(png){ # set up plotting to png file if required
              filename <- paste(plotdir,filenamestart,"sampsize_",filename_fltsexmkt,".png",sep="")
              png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              lfitfunc()
              dev.off()
            }
          } # end subplot 7
        } # end loop over partitions
      } # end loop over combined/not-combined genders
    } # end if data
  } # end loop over fleets
  if(returntitles) return(titles)
} # end embedded SSv3_plot_comps function
###########################
