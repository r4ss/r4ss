SSv3_plot_comps <- function(
    replist="ReportObject", kind="LEN", aalyear=-1, aalbin=-1, GUI=T, png=F, plotdir=NA, fleets="all",
    datonly=F, Natageplot=T, samplesizeplots=T, compresidplots=T, bub=F, showsampsize=T,
    minnbubble=8, pntscalar=2.6, pwidth=7, pheight=7, punits="in", ptsize=12, res=300, cex.main=1,
    linepos=1, fitbar=F,maxsize=3,do.sqrt=TRUE,smooth=TRUE,
    agelab="Age (years)", lenlab="Length (cm)",proplab="Proportion",yearlab="Year", lenunits="cm",
    osslab="Observed sample size",esslab="Effective sample size",printmkt=T,printsex=T,
    maxrows=6,maxcols=6,maxrows2=2,maxcols2=4,fixrows=F,fixcols=F,maxneff=5000,returntitles=T,verbose=T,...)
{
################################################################################
#
# SSv3_plot_comps BETA March 2, 2009
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: test subset of SSv3_plots to show compositional data with or without fits
# Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
#          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
# Returns: Plots with plot history in R GUI and/or .png files.
# General: Updated for Stock Synthesis version 3.02f March, 2009; R version 2.8.1
# Notes:   See users guide for documentation: http://code.google.com/p/r4ss/wiki/Documentation
# Required SS3v_output function
#
################################################################################


###### temporary notes #######
#
# this function to be called with commands like the following:
#
#   SSv3_plot_comps(replist=replist,datonly=T,kind="LEN")
#   SSv3_plot_comps(replist=replist,datonly=F,kind="LEN")
#   SSv3_plot_comps(replist=replist,datonly=F,kind="LEN",bub=T)
#   SSv3_plot_comps(replist=replist,datonly=F,kind="AGE")
#   SSv3_plot_comps(replist=replist,datonly=F,kind="AGE",bub=T)
#   SSv3_plot_comps(replist=replist,datonly=F,kind="cond",bub=T,maxrows=2,maxcols=4)
#
################################

  titles <- NULL
  if(png) if(is.na(plotdir)) return("plotdir must be specified to write png files.")

  nfleets    <- replist$nfleets
  FleetNames <- replist$FleetNames
  nseasons   <- replist$nseasons
  compdbase  <- replist$composition_database
  if(nseasons>1) compdbase$YrSeasName <- paste(floor(compdbase$Yr),"s",compdbase$Seas,sep="") else compdbase$YrSeasName <- compdbase$Yr

  lendbase   <- compdbase[compdbase$Kind=="LEN" & compdbase$N > 0,]
  agedbase   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0 & compdbase$Lbin_lo != compdbase$Lbin_hi,]
  condbase   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0 & compdbase$Lbin_lo == compdbase$Lbin_hi,]

  # not sure why these are not converted to numeric in SSv3_output, nor why they aren't done in 1 step a few lines above
  lendbase$effN <- as.numeric(lendbase$effN)
  agedbase$effN <- as.numeric(agedbase$effN)
  condbase$effN <- as.numeric(condbase$effN)

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
    kindlab=lenlab
    if(datonly){
      filenamestart <- "15_lendat_"
      titledata <- "length comp data, "
    }else{
      filenamestart <- "18_lenfit_"
      titledata <- "length comps, "
    }
  }
  if(kind=="AGE"){
    dbase_kind <- agedbase
    kindlab=agelab
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
    kindlab=agelab
    if(datonly){
      filenamestart <- "17_condAALdat_"
      titledata <- "conditional length at age data, "
    }else{
      filenamestart <- "20_condAALfit_"
      titledata <- "conditional length at age, "
    }
  }

  if(!(kind%in%c("LEN","AGE","cond"))) return("Input 'kind' to SSv3_plot_comps should be 'LEN' or 'AGE'.")
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
                make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
                              sampsize=showsampsize*dbase$N,bars=bars,linepos=(1-datonly)*linepos,
                              nlegends=2,legtext=list(dbase$YrSeasName,"sampsize"),
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=proplab,maxrows=maxrows,maxcols=maxcols,
                              fixrows=fixrows,fixcols=fixcols,ipage=ipage,...)
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
            if(GUI) bubble3(x=dbase$Yr, y=dbase$Bin, z=z, xlab=yearlab,ylab=kindlab,col=col,
                    las=1,main=ptitle,cex.main=cex.main,maxsize=pntscalar,allopen=allopen,minnbubble=minnbubble)
            if(png){ # set up plotting to png file if required
              filename <- paste(plotdir,filenamestart,filetype,filename_fltsexmkt,".png",sep="")
              png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              bubble3(x=dbase$Yr, y=dbase$Bin, z=z, xlab=yearlab,ylab=kindlab,col=col,
                      las=1,main=ptitle,cex.main=cex.main,maxsize=pntscalar,allopen=allopen,minnbubble=minnbubble)
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
                make_multifig(ptsx=dbase$Bin,ptsy=dbase$Lbin_lo,yr=dbase$Yr,size=z,
                              sampsize=showsampsize*dbase$N,
                              nlegends=1,legtext=list(dbase$YrSeasName),
                              bars=F,linepos=0,main=ptitle,cex.main=cex.main,
                              xlab=agelab,ylab=lenlab,ymin0=F,maxrows=maxrows2,maxcols=maxcols2,
                              fixrows=fixrows,fixcols=fixcols,allopen=allopen,minnbubble=minnbubble,
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
          #                     and Pearson residuals of A-L key for specific years
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
                      lenbinlegend <- paste(ydbase$Lbin_lo,lenunits,sep="")
                      tempfun <- function(ipage,...){ # temporary function to aid repeating the big function call
                          make_multifig(ptsx=ydbase$Bin,ptsy=ydbase$Obs,yr=ydbase$Lbin_lo,
                                        linesx=ydbase$Bin,linesy=ydbase$Exp,
                                        sampsize=showsampsize*ydbase$N,
                                        nlegends=2,legtext=list(lenbinlegend,"sampsize"),
                                        bars=F,linepos=linepos,main=ptitle,cex.main=cex.main,
                                        xlab=agelab,ylab=proplab,maxrows=maxrows,maxcols=maxcols,
                                        fixrows=fixrows,fixcols=fixcols,ipage=ipage,...)
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
                          bubble3(x=ydbase$Bin,y=ydbase$Lbin_lo,z=z,xlab=agelab,ylab=lenlab,col=rep("blue",2),
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
          #              for specific length bins
          if(aalbin[1] > 0)
          {
            badbins <- setdiff(aalbin, dbase$Lbin_hi)
            goodbins <- intersect(aalbin, dbase$Lbin_hi)
            if(length(badbins)>0){
                print(paste("Error! the following inputs for 'aalbin' do not match the Lbin_hi values for the conditional age at length data:",badbins),quote=F)
                print(paste("       the following inputs for 'aalbin' are fine:",goodbins),quote=F)
            }
            if(length(goodbins)>0)
            {
              for(ibin in 1:length(goodbins)) # loop over good bins
              {
                ilenbin <- goodbins[ibin]
                abindbase <- dbase[dbase$Lbin_hi==ilenbin,]
                if(nrow(abindbase)>0) # check for data associated with this bin
                {
                  ptitle <- paste("Age at length ",ilenbin,lenunits,", ",title_sexmkt,FleetNames[f],sep="")
                  titles <- c(ptitle,titles) # compiling list of all plot titles
                  tempfun <- function(ipage,...){ # temporary function to aid repeating the big function call
                      make_multifig(ptsx=abindbase$Bin,ptsy=abindbase$Obs,yr=abindbase$Yr,linesx=abindbase$Bin,linesy=abindbase$Exp,
                                    nlegends=2,legtext=list(abindbase$YrSeasName,"sampsize"),
                                    sampsize=showsampsize*abindbase$N,bars=bars,linepos=(1-datonly)*linepos,
                                    main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=proplab,maxrows=maxrows,maxcols=maxcols,
                                    fixrows=fixrows,fixcols=fixcols,ipage=ipage,...)
                  }
                  if(GUI) tempfun(ipage=0,...)
                  if(png){
                      npages <- ceiling(length(unique(abindbase$Yr))/maxrows/maxcols)
                      for(ipage in 1:npages)
                      {
                          if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                          filename <- paste(plotdir,filenamestart,filename_fltsexmkt,"_length",ilenbin,lenunits,pagetext,".png",sep="")
                          png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                          tempfun(ipage=ipage,...)
                          dev.off() # close device if png
                      }
                  }
                } # end if data
              } # end loop over length bins
            } # end if length(goodbins)>0
          } # end if plot requested

          ### subplot 7: sample size plot
          if(samplesizeplots & !datonly)
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
                plot(dbasegood$N,dbasegood$effN,xlab=osslab,main=ptitle,cex.main=cex.main,
                     ylim=c(0,1.05*max(dbasegood$effN)),xlim=c(0,1.05*max(dbasegood$N)),
                     col="blue",pch=19,ylab=esslab,xaxs='i',yaxs='i')
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
          }
        } # end loop over partitions
      } # end loop over combined/not-combined genders
    }# end if data
  } # end loop over fleet
  if(returntitles) return(titles)
} # end SSv3_plot_comps function

######################################################################################################################
######################################################################################################################
######################################################################################################################

make_multifig <- function(ptsx, ptsy, yr, linesx=0, linesy=0, sampsize=0, minsampsize=0,sampsizeround=1,
  maxrows=6, maxcols=6, fixrows=F, fixcols=F, main="",cex.main=1,xlab="",ylab="",
  size=1,maxsize=3,do.sqrt=TRUE,minnbubble=8,allopen=TRUE,
  horiz_lab="default",xbuffer=c(.1,.1),ybuffer=c(0,0.1),ymin0=T,axis1="default",axis2="default",linepos=1,
  bars=F,barwidth="default",ptscol=1,ptscol2=1,linescol=2,lty=1,lwd=1,pch=1,
  nlegends=2,legtext=list("yr","sampsize"),legx="default",legy="default",
  legadjx="default",legadjy="default",legsize=c(1.2,1.0),legfont=c(2,1),
  ipage=0)
{

  ################################################################################
  #
  # make_multifig March 19, 2009
  #
  # Purpose: To plot a multifigure environment similar to lattice but simpler
  #          and with easier controls over some things
  # Written: Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
  # Returns: a plot
  # General:
  # Notes:
  # Required packages: none
  #
  ################################################################################

  # notes on inputs for make_multifig
  # ptsx                    = vector of x values for points or bars
  # ptsy                    = vector of y values for points or bars  of same length as ptsx
  # yr                       = vector of category values (years) of same length as ptsx
  # linesx=0                = optional vector of x values for lines
  # linesy=0                = optional vector of y values for lines
  # sampsize=0              = optional sample size vector of same length as ptsx
  # minsampsize=0           = optional lower limit on sample sizes to be printed on the plot
  # sampsizeround=1         = number of decimal places to include in the sample size text
  # maxrows=6               = the maximum number of rows of plots
  # maxcols=6               = the maximum number of columns of plots
  # fixrows=F               = should the number of rows be fixed to the maximum (T/F)
  # fixcols=F               = should the number of columns be fixed to the maximum (T/F)
  # main=""                 = the main title for the plot
  # cex.main=1               = the font size multiplier for the main title
  # xlab=""                 = the x-axis label
  # ylab=""                 = the y-axis label
  # horiz_lab="default"     = axis labels set horizontal all the time (T), never (F) or
  #                           only when relatively short ("default")
  # xbuffer=c(.1,.1)        = extra space around points on the left and right as fraction of total width of plot
  # axis1="default"         = vector of values for x-axis, "default" chooses in typical R fashion
  # axis2="default"         = vector of values for y-axis, "default" chooses in typical R fashion
  # linepos=1               = position of lines relative to point/bars (0=no lines, 1=behind, 2=in front)
  # bars=F                  = should the ptsx/ptsy values be bars instead of points (T/F)
  # barwidth="default"      = width of bars in barplot, default method chooses based on quick and dirty formula
  #                           also, current method of plot(...type='h') could be replaced with better approach
  # ptscol=1                = color for points/bars
  # linescol=2              = color for lines
  # lty=1                   = line type
  # lwd=1                   = line width
  # pch=1                   = point character
  # nlegends=2              = number of legends
  # legtext=list("yr","sampsize") = text in legend, a list of length=nlegends values may be
  #                           1. "yr" to make the legend for each plot equal to the yr input for the values within
  #                           2. "sampsize" to make the legend be "n=999" where 999 is the sample size for the values
  #                           3. a vector of length = ptsx
  # legx="default"          = vector of length=nlegends of x-values of legends (default is first one on left,
  #                           all after on right)
  # legy="default"          = vector of length=nlegends of y-values of legends (default is top for all plots)
  # legadjx="default"       = left/right adjustment of legends around legx
  # legadjy="default"       = left/right adjustment of legends around legy
  # legsize=c(1.2,1.0)      = font size for legends
  # legfont=c(2,1)          = font type for legends, same as "font" under ?par

  # define dimensions
  yrvec <- sort(unique(yr))
  npanels <- length(yrvec)
  nvals <- length(yr)

  nrows <- min(ceiling(sqrt(npanels)), maxrows)
  ncols <- min(ceiling(npanels/nrows), maxcols)
  if(fixrows) nrows <- maxrows
  if(fixcols) ncols <- maxcols

  npages <- ceiling(npanels/nrows/ncols) # how many pages of plots

  # if no input on lines, then turn linepos to 0
  if(length(linesx)==1 | length(linesy)==1){
    linepos <- 0
    linesx <- ptsx
    linesy <- ptsy
  }

  # quick and dirty formula to get width of bars (if used) based on
  #   number of columns and maximum number of bars within a in panel
  if(bars & barwidth=="default") barwidth <- 400/max(table(yr)+2)/ncols

  # make size vector have full length
  if(length(size)==1) size <- rep(size,length(yr))

  # get axis limits
  xrange <- range(c(ptsx,linesx,ptsx,linesx))
  if(ymin0) yrange <- c(0,max(ptsy,linesy)) else yrange <- range(c(ptsy,linesy,ptsy,linesy))
  xrange_big <- xrange+c(-1,1)*xbuffer*diff(xrange)
  yrange_big <- yrange+c(-1,1)*ybuffer*diff(yrange)

  # get axis labels
  yaxs_lab <- pretty(yrange)
  maxchar <- max(nchar(yaxs_lab))
  if(horiz_lab=="default") horiz_lab <- maxchar<6 # should y-axis label be horizontal?

  if(axis1=="default") axis1=pretty(xrange)
  if(axis2=="default") axis2=pretty(yrange)

  if(length(sampsize)==1) sampsize <- 0

  # create multifigure layout and set inner margins all to 0 and add outer margins
  par(mfcol=c(nrows,ncols),mar=rep(0,4),oma=c(5,5,4,2)+.1)

  panelrange <- 1:npanels
  if(npages > 1 & ipage!=0) panelrange <- intersect(panelrange, 1:(nrows*ncols) + nrows*ncols*(ipage-1))

  for(ipanel in panelrange)
  {
    # subset values
    yr_i <- yrvec[ipanel]
    ptsx_i <- ptsx[yr==yr_i]
    ptsy_i <- ptsy[yr==yr_i]

    linesx_i <- linesx[yr==yr_i]
    linesy_i <- linesy[yr==yr_i]

    # sort values in lines
    linesy_i <- linesy_i[order(linesx_i)]
    linesx_i <- sort(linesx_i)

    z_i <- size[yr==yr_i]

    # make plot
    plot(0,type='l',axes=F,xlab="",ylab="",xlim=xrange_big,ylim=yrange_big,
      xaxs="i",yaxs=ifelse(bars,"i","r"))
    abline(h=0,col="grey") # grey line at 0
    if(linepos==1) lines(linesx_i,linesy_i,col=linescol,lwd=lwd,lty=lty) # lines first
    if(diff(range(size))!=0){ # if size input is provided then use bubble function
      bubble3(x=ptsx_i,y=ptsy_i,z=z_i,col=c(ptscol,ptscol2),
              maxsize=maxsize,minnbubble=minnbubble,allopen=allopen,add=T) # bubble plot
    }else{
      if(!bars) points(ptsx_i,ptsy_i,pch=pch,col=ptscol)  # points
      if( bars) points(ptsx_i,ptsy_i,type='h',lwd=barwidth,col=ptscol,lend=2)  # histogram-style bars
    }
    if(linepos==2) lines(linesx_i,linesy_i,col=linescol,lwd=lwd,lty=lty)

    # add legends
    usr <- par("usr")
    for(i in 1:nlegends)
    {
      text_i <- ""
      legtext_i <- legtext[[i]] # grab element of list
      # elements of list can be "default" to make equal to yr
      # or vector of length 1, npanels, or the full length of the input vectors
      if(length(legtext_i)==1){      text_i <- legtext_i           # one value repeated
        if(legtext_i=="yr")          text_i <- yr_i                # values in "yr" input
        if(legtext_i=="sampsize"){                                 # sample sizes
          if(max(sampsize)>=minsampsize){text_i <- paste("n=",round(sampsize[yr==yr_i],sampsizeround),sep="")
        }else{                           text_i <- ""}}
      }
      if(length(legtext_i)==npanels) text_i <- legtext_i[ipanel]   # one input value per panel
      if(length(legtext_i)==nvals)   text_i <- legtext_i[yr==yr_i][1] # one input value per element

      if(legx[1]=="default"){
        # default is left side for first plot, right thereafter
        textx <- ifelse(i==1, usr[1], usr[2])
      }else{ textx <- legx[i] }
      if(legy[1]=="default"){
        texty <- usr[4]         # default is top for all plots
      }else{ texty <- legy[i] }
      if(legadjx[1]=="default"){
        adjx <- ifelse(i>1, 1.2, -0.2) # default is left side for first legend, right thereafter
      }else{ adjx <- legadjx[i] }
      if(legadjy[1]=="default"){
        adjy <- ifelse(i<3, 1.2, 1.2 + 1.2*(i-2))  # default is top for first 2 legends, below thereafter
      }else{ adjy <- legadjy[i] }

      # add legend text
      text(x=textx,y=texty,labels=text_i,adj=c(adjx,adjy),cex=legsize[i],font=legfont[i])
    }

    # add axes in left and lower outer margins
    mfg <- par("mfg")
    if(mfg[1]==mfg[3] | ipanel==npanels) axis(side=1,at=axis1) # axis on bottom panels and final panel
    if(mfg[2]==1) axis(side=2,at=axis2,las=horiz_lab)        # axis on left side panels
    box()

    if(ipanel %% (nrows*ncols) == 1) # if this is the first panel of a given page
    {
      # add title after plotting first panel on each page of panels
      fixcex = 1 # fixcex compensates for automatic adjustment caused by par(mfcol)
      if(max(nrows,ncols)==2) fixcex = 1/0.83
      if(max(nrows,ncols)>2) fixcex = 1/0.66

      title(main=main, line=c(2,0,3,3), outer=T, cex.main=cex.main*fixcex)
      title(xlab=xlab, outer=T, cex.lab=fixcex)
      title(ylab=ylab, line=ifelse(horiz_lab,max(3,2+.4*maxchar),3.5), outer=T, cex.lab=fixcex)
    }
  }
  # restore default single panel settings
  par(mfcol=c(1,1),mar=c(5,5,4,2)+.1,oma=rep(0,4))

  # return information on what was plotted
  return(list(npages=npages, npanels=npanels, ipage=ipage))
}

bubble3 <- function (x,y,z,col=c(1,1),maxsize=3,do.sqrt=TRUE,
                     main="",cex.main=1,xlab="",ylab="",minnbubble=8,
                     xlimextra=1,add=F,las=1,allopen=TRUE)
{
    # vaguely based on bubble() from gstat
    az <- abs(z)
    if (do.sqrt) az <- sqrt(az)
    cex <- maxsize * az/max(az)
    z.col <- ifelse(z < 0, col[1], col[2])
    xlim <- range(x)
    if(length(unique(x))<minnbubble) xlim=xlim+c(-1,1)*xlimextra
    pch <- z
    pch[pch==0] <- NA
    pch[pch>0] <- 16
    pch[pch<0] <- 1
    if(allopen) pch[!is.na(pch)] <- 1
    if(!add){
        plot(x,y,type='n',xlim=xlim,main=main,xlab=xlab,ylab=ylab,axes=F,cex.main=cex.main)
        axis(1,at=unique(x))
        axis(2,las=las)
        box()
    }
    points(x,y,pch=pch,cex=cex,col=z.col)
}
