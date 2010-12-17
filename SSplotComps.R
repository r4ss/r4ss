SSplotComps <-
  function(replist, subplots=1:9,
           kind="LEN", aalyear=-1, aalbin=-1, plot=TRUE, print=FALSE, fleets="all",
           fleetnames="default",
           datonly=FALSE, samplesizeplots=TRUE, compresidplots=TRUE, bub=FALSE,
           showsampsize=TRUE, showeffN=TRUE, minnbubble=8, pntscalar=2.6,
           pwidth=7, pheight=7, punits="in", ptsize=12, res=300,
           plotdir="default", cex.main=1, linepos=1, fitbar=FALSE, maxsize=3,
           do.sqrt=TRUE, smooth=TRUE, cohortlines=c(),
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
                      "(numbers x1000)",       #11
                      "Stdev (Age) (yr)",      #12
                      "Andre's conditional AAL plot, "), #13
           printmkt=TRUE,printsex=TRUE,
           maxrows=6,maxcols=6,maxrows2=2,maxcols2=4,rows=1,cols=1,
           fixdims=TRUE,maxneff=5000,verbose=TRUE,...)
{
  ################################################################################
  # SSplotComps October 21, 2010
  ################################################################################
 
  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  lendbase      <- replist$lendbase
  sizedbase     <- replist$sizedbase
  agedbase      <- replist$agedbase
  condbase      <- replist$condbase
  ghostagedbase <- replist$ghostagedbase
  ladbase       <- replist$ladbase
  wadbase       <- replist$wadbase
  tagdbase1     <- replist$tagdbase1
  tagdbase2     <- replist$tagdbase2

  nfleets       <- replist$nfleets
  nseasons      <- replist$nseasons
  FleetNames    <- replist$FleetNames
  nsexes        <- replist$nsexes

  if(!exists("make_multifig"))
  print("you are missing the function 'make_mulitifig'")

  titles <- NULL
  if(plotdir=="default") plotdir <- replist$inputs$dir

  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  }else{
    if(length(intersect(fleets,1:nfleets))!=length(fleets)){
      return("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
    }
  }
  if(fleetnames[1]=="default") fleetnames <- FleetNames

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
    dbase_kind <- ladbase[ladbase$N!=0,] # remove values with 0 sample size
    kindlab=labels[2]
    filenamestart <- "21_lenatagefit_"
    titledata <- "mean length at age, "
  }
  if(kind=="W@A"){
    dbase_kind <- wadbase[wadbase$N!=0,] # remove values with 0 sample size
    kindlab=labels[2]
    filenamestart <- "21_wtatagefit_"
    titledata <- "mean weight at age, "
  }
  if(!(kind%in%c("LEN","SIZE","AGE","cond","GSTAGE","L@A","W@A"))) stop("Input 'kind' to SSplotComps is not right.")
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

          # plot bars for data only or if input 'fitbar=TRUE'
          if(datonly | fitbar) bars <- TRUE else bars <- FALSE

          # aggregating identifiers for plot titles and filenames
          title_sexmkt <- paste(titlesex,titlemkt,sep="")
          filename_fltsexmkt <- paste("flt",f,"sex",sex,"mkt",j,sep="")

          ### subplot 1: multi-panel composition plot
          if(1 %in% subplots & kind!="cond") # for age or length comps, but not conditional AAL
          {
            ptitle <- paste(titledata,title_sexmkt, fleetnames[f],sep="") # total title
            titles <- c(ptitle,titles) # compiling list of all plot titles
            tempfun <- function(ipage,...){
              if(!(kind %in% c("GSTAGE","L@A","W@A"))){
                make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
                              sampsize=dbase$N,effN=dbase$effN,showsampsize=showsampsize,showeffN=showeffN,
                              bars=bars,linepos=(1-datonly)*linepos,
                              nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                              maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                              fixdims=fixdims,ipage=ipage,...)
              }
              if(kind=="GSTAGE"){
                  make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
                                sampsize=dbase$N,effN=dbase$effN,showsampsize=FALSE,showeffN=FALSE,
                                bars=bars,linepos=(1-datonly)*linepos,
                                nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
                                main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                                maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                                fixdims=fixdims,ipage=ipage,...)
              }
              if(kind %in% c("L@A","W@A")){
                  make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
                                sampsize=dbase$N,effN=0,showsampsize=FALSE,showeffN=FALSE,
                                nlegends=1,legtext=list(dbase$YrSeasName),
                                bars=bars,linepos=(1-datonly)*linepos,
                                main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=ifelse(kind=="W@A",labels[9],labels[1]),
                                maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                                fixdims=fixdims,ipage=ipage,...)
              }

            }
            if(plot) tempfun(ipage=0,...)
            if(print){ # set up plotting to png file if required
              npages <- ceiling(length(unique(dbase$Yr))/maxrows/maxcols)
              for(ipage in 1:npages)
              {
                  if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                  filename <- paste(plotdir,filenamestart,filename_fltsexmkt,pagetext,".png",sep="")
                  pngfun(file=filename)
                  tempfun(ipage=ipage,...)
                  dev.off()
              }
            }
          } # end subplot 1

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
          if(2 %in% subplots & bub & kind!="cond")
          {
            # get growth curves if requested
            if(length(cohortlines)>0){
              growdat <- replist$endgrowth
              growdatF <- growdat[growdat$Gender==1 & growdat$Morph==min(growdat$Morph[growdat$Gender==1]),]
              if(nsexes > 1){
                growdatM <- growdat[growdat$Gender==2 & growdat$Morph==min(growdat$Morph[growdat$Gender==2]),]
              }
            }
            ptitle <- paste(titletype, title_sexmkt, fleetnames[f],sep="")
            ptitle <- paste(ptitle," (max=",round(max(z),digits=2),")",sep="")
            titles <- c(ptitle,titles) # compiling list of all plot titles

            tempfun <- function(){
              bubble3(x=dbase$Yr, y=dbase$Bin, z=z, xlab=labels[3],ylab=kindlab,col=col,
                    las=1,main=ptitle,cex.main=cex.main,maxsize=pntscalar,allopen=allopen,minnbubble=minnbubble)
              # add lines for growth of individual cohorts if requested
              if(length(cohortlines)>0){
                for(icohort in 1:length(cohortlines)){
                  print(paste("Adding line for",cohortlines[icohort],"cohort"),quote=FALSE)
                  if(k %in% c(1,2)) lines(growdatF$Age+cohortlines[icohort],growdatF$Len_Mid, col="red")  #females
                  if(k %in% c(1,3)) lines(growdatM$Age+cohortlines[icohort],growdatM$Len_Mid, col="blue") #males
                }
              }
            }

            if(plot) tempfun()
            if(print){ # set up plotting to png file if required
              filename <- paste(plotdir,filenamestart,filetype,filename_fltsexmkt,".png",sep="")
              pngfun(file=filename)
              tempfun()
              dev.off() # close device if png
            }
          } # end bubble plot

          ### subplot 3: multi-panel bubble plots for conditional age at length
          if(3 %in% subplots & kind=="cond")
          {
            ptitle <- paste(titletype, title_sexmkt, fleetnames[f],sep="")
            ptitle <- paste(ptitle," (max=",round(max(z),digits=2),")",sep="")
            titles <- c(ptitle,titles) # compiling list of all plot titles
            tempfun <- function(ipage,...){
              make_multifig(ptsx=dbase$Bin,ptsy=dbase$Lbin_mid,yr=dbase$Yr,size=z,
                        sampsize=dbase$N,showsampsize=showsampsize,showeffN=FALSE,
                        nlegends=1,legtext=list(dbase$YrSeasName),
                        bars=FALSE,linepos=0,main=ptitle,cex.main=cex.main,
                        xlab=labels[2],ylab=labels[1],ymin0=FALSE,maxrows=maxrows2,maxcols=maxcols2,
                        fixdims=fixdims,allopen=allopen,minnbubble=minnbubble,
                        ptscol=col[1],ptscol2=col[2],ipage=ipage,...)
            }
            if(plot) tempfun(ipage=0,...)
            if(print){ # set up plotting to png file if required
              npages <- ceiling(length(unique(dbase$Yr))/maxrows2/maxcols2)
              for(ipage in 1:npages)
              {
                  if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                  filename <- paste(plotdir,filenamestart,filetype,filename_fltsexmkt,pagetext,".png",sep="")
                  pngfun(file=filename)
                  tempfun(ipage=ipage,...)
                  dev.off() # close device if png
              }
            }
          } # end conditional bubble plot
          ### subplots 4 and 5: multi-panel plot of point and line fit to conditional age at length
          #                        and Pearson residuals of A-L key for specific years
          if((4 %in% subplots | 5 %in% subplots) & aalyear[1] > 0 & kind=="cond")
          {
            for(y in 1:length(aalyear))
            {
              aalyr <- aalyear[y]
              if(length(dbase$Obs[dbase$Yr==aalyr])>0)
              {
                if(4 %in% subplots){
                  ### subplot 4: multi-panel plot of fit to conditional age at length for specific years
                  ptitle <- paste(aalyr," age at length bin, ",title_sexmkt,fleetnames[f],sep="")
                  titles <- c(ptitle,titles) # compiling list of all plot titles
                  ydbase <- dbase[dbase$Yr==aalyr,]
                  lenbinlegend <- paste(ydbase$Lbin_lo,labels[7],sep="")
                  lenbinlegend[ydbase$Lbin_range>0] <- paste(ydbase$Lbin_lo,"-",ydbase$Lbin_hi,labels[7],sep="")
                  tempfun <- function(ipage,...){ # temporary function to aid repeating the big function call
                    make_multifig(ptsx=ydbase$Bin,ptsy=ydbase$Obs,yr=ydbase$Lbin_lo,
                                  linesx=ydbase$Bin,linesy=ydbase$Exp,
                                  sampsize=ydbase$N,effN=ydbase$effN,showsampsize=showsampsize,showeffN=showeffN,
                                  nlegends=3,legtext=list(lenbinlegend,"sampsize","effN"),
                                  bars=FALSE,linepos=linepos,main=ptitle,cex.main=cex.main,
                                  xlab=labels[2],ylab=labels[6],maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,

                                  fixdims=fixdims,ipage=ipage,...)
                  }
                  if(plot) tempfun(ipage=0,...)
                  if(print){
                    npages <- ceiling(length(unique(ydbase$Yr))/maxrows/maxcols)
                    for(ipage in 1:npages)
                    {
                      if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                      filename <- paste(plotdir,filenamestart,filename_fltsexmkt,"_",aalyr,"_",pagetext,".png",sep="")
                      pngfun(file=filename)
                      tempfun(ipage=ipage,...)
                      dev.off() # close device if print
                    }
                  }
                } # end if 4 in subplots
                if(5 %in% subplots){
                  ### subplot 5: Pearson residuals for A-L key
                  z <- ydbase$Pearson
                  ptitle <- paste(aalyr," Pearson residuals for A-L key, ",title_sexmkt,fleetnames[f],sep="")
                  ptitle <- paste(ptitle," (max=",round(abs(max(z)),digits=2),")",sep="")
                  titles <- c(ptitle,titles) # compiling list of all plot titles
                  tempfun <- function(){
                      bubble3(x=ydbase$Bin,y=ydbase$Lbin_lo,z=z,xlab=labels[2],ylab=labels[1],col=rep("blue",2),
                              las=1,main=ptitle,cex.main=cex.main,maxsize=pntscalar,allopen=FALSE,minnbubble=minnbubble)
                  }
                  if(plot) tempfun()
                  if(print)
                  {
                      filename <- paste(plotdir,filenamestart,"yearresids_",filename_fltsexmkt,"_",aalyr,".png",sep="")
                      pngfun(file=filename)
                      tempfun()
                      dev.off() # close device if print
                  }
                } # end if 5 in subplots
              }
            }
          }

          ### subplot 6: multi-panel plot of point and line fit to conditional age at length
          #                   for specific length bins
          if(6 %in% subplots & aalbin[1] > 0)
          {
            badbins <- setdiff(aalbin, dbase$Lbin_hi)
            goodbins <- intersect(aalbin, dbase$Lbin_hi)
            if(length(goodbins)>0)
            {
              if(length(badbins)>0){
                print(paste("Error! the following inputs for 'aalbin' do not match the Lbin_hi values for the conditional age at length data:",badbins),quote=FALSE)
                print(paste("            the following inputs for 'aalbin' are fine:",goodbins),quote=FALSE)
              }
              for(ibin in 1:length(goodbins)) # loop over good bins
              {
              ilenbin <- goodbins[ibin]
              abindbase <- dbase[dbase$Lbin_hi==ilenbin,]
              if(nrow(abindbase)>0) # check for data associated with this bin
              {
                ptitle <- paste("Age at length ",ilenbin,labels[7],", ",title_sexmkt,fleetnames[f],sep="")
                titles <- c(ptitle,titles) # compiling list of all plot titles
                tempfun <- function(ipage,...){ # temporary function to aid repeating the big function call
                    make_multifig(ptsx=abindbase$Bin,ptsy=abindbase$Obs,yr=abindbase$Yr,linesx=abindbase$Bin,linesy=abindbase$Exp,
                              sampsize=abindbase$N,effN=abindbase$effN,showsampsize=showsampsize,showeffN=showeffN,
                              nlegends=3,legtext=list(abindbase$YrSeasName,"sampsize","effN"),
                              bars=bars,linepos=(1-datonly)*linepos,
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                              fixdims=fixdims,ipage=ipage,...)
                }
                if(plot) tempfun(ipage=0,...)
                if(print){
                  npages <- ceiling(length(unique(abindbase$Yr))/maxrows/maxcols)
                  for(ipage in 1:npages)
                  {
                    if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                    filename <- paste(plotdir,filenamestart,filename_fltsexmkt,"_length",ilenbin,labels[7],pagetext,".png",sep="")
                    pngfun(file=filename)
                    tempfun(ipage=ipage,...)
                    dev.off() # close device if print
                  }
                } # end print
              } # end if data
            } # end loop over length bins
          } # end if length(goodbins)>0
          } # end if plot requested

          ### subplot 7: sample size plot
          if(7 %in% subplots & samplesizeplots & !datonly & !(kind %in% c("GSTAGE","L@A","W@A")))
          {
            ptitle <- paste("N-EffN comparison, ",titledata,title_sexmkt,fleetnames[f], sep="")
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
            if(plot) lfitfunc()
            if(print){ # set up plotting to png file if required
              filename <- paste(plotdir,filenamestart,"sampsize_",filename_fltsexmkt,".png",sep="")
              pngfun(file=filename)
              lfitfunc()
              dev.off()
            }
          } # end subplot 7

          ### subplot 8: Andre's mean age and std. dev. in conditional AAL
          if(8 %in% subplots & kind=="cond")
          {
            Lens <-sort(unique(dbase$Lbin_lo))
            Yrs <- sort(unique(dbase$Yr))
            par(mfrow=c(3,2),mar=c(2,4,1,1),oma=c(3,0,3,0))
            for (Yr in Yrs){
              y <- dbase[dbase$Yr==Yr,]
              Size <- NULL; Size2 <- NULL
              Obs <- NULL; Obs2 <- NULL
              Pred <- NULL;  Pred2 <- NULL
              Upp <- NULL; Low <- NULL; Upp2 <- NULL; Low2 <- NULL
              for (Ilen in Lens){
                z <- y[y$Lbin_lo == Ilen,]
                if (length(z[,1]) > 0){
                  weightsPred <- z$Exp/sum(z$Exp)
                  weightsObs <- z$Obs/sum(z$Obs)
                  ObsV <- sum(z$Bin*weightsObs)
                  ObsV2 <- sum(z$Bin*z$Bin*weightsObs)
                  PredV <- sum(z$Bin*weightsPred)
                  PredV2 <- sum(z$Bin*z$Bin*weightsPred)
                  # Overdispersion on N
                  # NN <- z$N[1]*0.01 # Andre did this for reasons unknown
                  NN <- z$N[1]
                  if (max(z$Obs) > 1.0e-4){
                    Size <- c(Size,Ilen)
                    Obs <- c(Obs,ObsV)
                    Pred <- c(Pred,PredV)
                    varn <-sqrt(PredV2-PredV*PredV)/sqrt(NN)
                    Pred2 <- c(Pred2,varn)
                    varn <-sqrt(ObsV2-ObsV*ObsV)/sqrt(NN)
                    Obs2 <- c(Obs2,varn)
                    Low <- c(Low,ObsV-1.64*varn)
                    Upp <- c(Upp,ObsV+1.64*varn)
                    if (NN > 1){
                      Size2 <- c(Size2,Ilen)
                      Low2 <- c(Low2,varn*sqrt((NN-1)/qchisq(0.95,NN)))
                      Upp2 <- c(Upp2,varn*sqrt((NN-1)/qchisq(0.05,NN)))
                    }
                  }
                }
              }
              if (length(Obs) > 0){
                ymax <- max(Pred,Obs,Upp)*1.1
                plot(Size,Obs,xlab="",ylab="Age",pch=16,xlim=c(min(Lens),max(Lens)),ylim=c(0,ymax),yaxs="i")
                text(x=par("usr")[1],y=.9*ymax,labels=Yr,adj=c(-.5,0),font=2,cex=1.2)
                lines(Size,Pred)
                lines(Size,Low,lty=3)
                lines(Size,Upp,lty=3)
                #title(paste("Year = ",Yr,"; Gender = ",Gender))

                ptitle <- paste(labels[13], title_sexmkt, fleetnames[f],sep="")
                titles <- c(ptitle,titles) # compiling list of all plot titles
                if(par("mfg")[1] & par("mfg")[2]==1){ # first plot on any new page
                  title(main=ptitle,xlab=labels[1],outer=TRUE,line=1)
                }
                ymax <- max(Obs2,Pred2)*1.1
                plot(Size,Obs2,xlab=labels[1],ylab=labels[12],pch=16,xlim=c(min(Lens),max(Lens)),ylim=c(0,ymax),yaxs="i")
                lines(Size,Pred2)
                lines(Size2,Low2,lty=3)
                lines(Size2,Upp2,lty=3)
              } # end if data exist
            } # end loop over years
            flush.console()

          } # end subplot 8
        } # end loop over partitions
      } # end loop over combined/not-combined genders
    } # end if data
  } # end loop over fleets

  ### subplot 9: by fleet aggregating across years
  if(9 %in% subplots & kind!="cond") # for age or length comps, but not conditional AAL
  {
    dbasef <- dbase_kind[dbase_kind$Fleet %in% fleets,]
    # check for the presence of data
    if(nrow(dbasef)>0)
    {
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
          if(nrow(dbase)>0){
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
  
            # plot bars for data only or if input 'fitbar=TRUE'
            if(datonly | fitbar) bars <- TRUE else bars <- FALSE
  
            # aggregating identifiers for plot titles and filenames
            title_sexmkt <- paste(titlesex,titlemkt,sep="")
            filename_fltsexmkt <- paste("flt",f,"sex",sex,"mkt",j,sep="")
  
            ptitle <- paste(titledata,title_sexmkt, "aggregated across time by fleet",sep="") # total title
            titles <- c(ptitle,titles) # compiling list of all plot titles
  
            # group remaining calculations as a function
            tempfun <- function(ipage,...){
              Bins <- sort(unique(dbase$Bin))
              nbins <- length(Bins)
              df <- data.frame(N=dbase$N,
                               effN=dbase$effN,
                               obs=dbase$Obs*dbase$N,
                               exp=dbase$Exp*dbase$N)
              agg <- aggregate(x=df, by=list(bin=dbase$Bin,f=dbase$Fleet), FUN=sum)
              agg <- agg[agg$f %in% fleets,]
              agg$obs <- agg$obs/agg$N
              agg$exp <- agg$exp/agg$N
              # note: sample sizes will be different for each bin if tail compression is used
              #       printed sample sizes in plot will be maximum, which may or may not
              #       represent sum of sample sizes over all years/ages
              for(f in unique(agg$f)){
                infleet <- agg$f==f
                agg$N[infleet] <- max(agg$N[infleet])
                agg$effN[infleet] <- max(agg$effN[infleet])
              }
  
              namesvec <- fleetnames[agg$f]
              if(!(kind %in% c("GSTAGE","L@A","W@A"))){
                make_multifig(ptsx=agg$bin,ptsy=agg$obs,yr=agg$f,
                              linesx=agg$bin,linesy=agg$exp,
                              sampsize=agg$N,effN=agg$effN,
                              showsampsize=showsampsize,showeffN=showeffN,
                              bars=bars,linepos=(1-datonly)*linepos,
                              nlegends=3,
                              legtext=list(namesvec,"sampsize","effN"),
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                              maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                              fixdims=F,ipage=ipage,lwd=2,...)
              }
  
         # haven't configured this aggregated plot for other types
              ## if(kind=="GSTAGE"){
              ##   make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
              ##                 sampsize=dbase$N,effN=dbase$effN,showsampsize=FALSE,showeffN=FALSE,
              ##                 bars=bars,linepos=(1-datonly)*linepos,
              ##                 nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
              ##                 main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
              ##                 maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
              ##                 fixdims=fixdims,ipage=ipage,...)
              ## }
              ## if(kind %in% c("L@A","W@A")){
              ##   make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
              ##                 sampsize=dbase$N,effN=0,showsampsize=FALSE,showeffN=FALSE,
              ##                 nlegends=1,legtext=list(dbase$YrSeasName),
              ##                 bars=bars,linepos=(1-datonly)*linepos,
              ##                 main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=ifelse(kind=="W@A",labels[9],labels[1]),
              ##                 maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
              ##                 fixdims=fixdims,ipage=ipage,...)
              ## }
  
            }
            if(plot) tempfun(ipage=0,...) 
            if(print){ # set up plotting to png file if required
              npages <- ceiling(length(unique(dbase$Yr))/maxrows/maxcols)
              for(ipage in 1:npages)
              {
                if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                filename <- paste(plotdir,filenamestart,filename_fltsexmkt,pagetext,".png",sep="")
                pngfun(file=filename)
                tempfun(ipage=ipage,...)
                dev.off()
              }
            } # end print function
          } # end test for presence of observations in this partition
        } # end loop over partitions
      } # end loop over combined/not-combined genders
    } # end if data
  } # end subplot 9
  
  return(invisible(titles))
} # end embedded SSplotComps function
###########################
