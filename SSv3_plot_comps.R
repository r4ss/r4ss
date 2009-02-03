SSv3_plot_comps <- function(
    replist="ReportObject", kind="LEN", png=F, printfolder="", dir="default", fleets="all", 
    datonly=F, Natageplot=T, samplesizeON=T, compresidsON=T, pwidth=700, pheight=700, linepos=1, fitbar=F,
    xlab="default", ylab="Proportion",maxrows=6,maxcols=6,fixrows=F,fixcols=F, ...)
{
################################################################################
#
# SSv3_plot_comps BETA January 30, 2009
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: test subset of SSv3_plots to show compositional data with or without fits
# Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
#          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
# Returns: Plots with plot history in R GUI and/or .png files.
# General: Updated for Stock Synthesis version 3.02b January, 2009; R version 2.8.1
# Notes:   See users guide for documentation: http://code.google.com/p/r4ss/wiki/Documentation
# Required SS3v_output function
#
################################################################################

  print("This function, SSv3_plot_comps, is still in testing phase.",quote=F)  

  nfleets    <- replist$nfleets
  FleetNames <- replist$FleetNames
  nseasons   <- replist$nseasons
  compdbase  <- replist$composition_database
  lendbase   <- compdbase[compdbase$Kind=="LEN" & compdbase$N > 0,]
  agedbase   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0 & compdbase$Lbin_lo != compdbase$Lbin_hi,]
  lendbase$effN <- as.numeric(lendbase$effN)
  agedbase$effN <- as.numeric(agedbase$effN)

  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  }else{ 
    if(length(intersect(fleets,1:nfleets))!=length(fleets)){
      return("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
    }
  } 
  
  # a few quantities related to whether it's length of age data
  if(kind=="LEN"){ 
    dbase_k <- lendbase 
    if(xlab=="default") xlab="Length bin (cm)"
    if(datonly){
      filenamestart <- "15lendat"
      titlefit <- "lengths"
    }else{
      filenamestart <- "17lendatfit"
      titlefit <- "length fits"
    }
  }
  if(kind=="AGE"){
    dbase_k <- agedbase
    if(xlab=="default") xlab="Age bin (years)"
    if(datonly){
      filenamestart <- "16agedat"
      titlefit <- "ages"
    }else{
      filenamestart <- "17agedatfit"
      titlefit <- "ages fits"
    }
  }
  if(!(kind%in%c("LEN","AGE"))) return("Input 'kind' to SSv3_plot_comps should be 'LEN' or 'AGE'.")
  # loop over fleets
  for(f in fleets) 
  {
    # check for the presence of data
    if(length(dbase_k$Obs[dbase_k$Fleet==f])>0) 
    {
      dbasef <- dbase_k[dbase_k$Fleet==f,]
      testor    <- length(dbasef$Gender[dbasef$Gender==1 & dbasef$Pick_gender==0 ])>0
      testor[2] <- length(dbasef$Gender[dbasef$Gender==1 & dbasef$Pick_gender %in% c(1,3)])>0
      testor[3] <- length(dbasef$Gender[dbasef$Gender==2])>0
      
      # loop over genders combinations
      for(k in (1:3)[testor]) 
      {
        if(k==1){dbase <- dbasef[dbasef$Gender==1 & dbasef$Pick_gender==0,]}
        if(k==2){dbase <- dbasef[dbasef$Gender==1 & dbasef$Pick_gender %in% c(1,3),]}
        if(k==3){dbase <- dbasef[dbasef$Gender==2,]}
        sex <- ifelse(k==3, 2, 1)
        
        # loop over partitions (discard, retain, total)
        for(j in unique(dbase$Part)) 
        {
          dbase2 <- dbase[dbase$Part==j,]
          if(nseasons>1) dbase2$Yr <- dbase2$Yr + (dbase2$Seas - 1)*(1/nseasons) + (1/nseasons)/2
          
          # assemble pieces of plot title
          if(k==1) titlesex <- "Sexes combined"
          if(k==2) titlesex <- "Female"
          if(k==3) titlesex <- "Male"
            
          if(j==0) titlepart <- "discard"
          if(j==1) titlepart <- "retained"
          if(j==2) titlepart <- "whole catch"
          
          ptitle <- paste(titlesex, titlepart, titlefit, "for", FleetNames[f]) # total title

          # plot bars for data only and if input 'fitbar=T'
          if(datonly | fitbar) bars <- T else bars <- F 
          
          if(png){ # set up plotting to png file if required
            filename <- paste(plotdir,filenamestart,"_flt",f,"sex",sex,"mkt",j,".png",sep="")
            png(file=filename,width=pwidth,height=pheight)
          }
          # make plot
          make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,z=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
            sampsize=samplesizeON*dbase$N,bars=bars,linepos=(1-datonly)*linepos,main=ptitle,
            xlab=xlab,ylab=ylab,maxrows=maxrows,maxcols=maxcols,fixrows=fixrows,fixcols=fixcols,
            ...)
          if(png) dev.off() # close device if png
        } # end loop over partitions
      } # end loop over combined/not-combined genders 
    }# end if data
  } # end loop over fleet
} # end SSv3_plot_comps function

######################################################################################################################
######################################################################################################################
######################################################################################################################

make_multifig <- function(ptsx,ptsy,z,linesx=0,linesy=0,sampsize=0,maxrows=6,maxcols=6,
  fixrows=F, fixcols=F, main="",xlab="",ylab="",
  horiz_lab="default",xbuffer=c(.1,.1),axis1="default",axis2="default",linepos=1,
  bars=F,barwidth="default",ptscol=1,linescol=2,lty=1,lwd=1,pch=1,
  nlegends=2,legtext=list("z","sampsize"),legx="default",legy="default",
  legadjx="default",legadjy="default",legsize=c(1.2,1.0),legfont=c(2,1))
{  
  ################################################################################
  #
  # make_multifig January 29, 2009
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
  # z                       = vector of category values (years) of same length as ptsx
  # linesx=0                = optional vector of x values for lines
  # linesy=0                = optional vector of y values for lines 
  # sampsize=0              = optional sample size vector of same length as ptsx
  # maxrows=6               = the maximum number of rows of plots
  # maxcols=6               = the maximum number of columns of plots
  # fixrows=F               = should the number of rows be fixed to the maximum (T/F)
  # fixcols=F               = should the number of columns be fixed to the maximum (T/F)
  # main=""                 = the main title for the plot
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
  # legtext=list("z","sampsize") = text in legend, a list of length=nlegends values may be
  #                           1. "z" to make the legend for each plot equal to the z input for the values within
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
  zvec <- sort(unique(z))
  npanels <- length(zvec)
  nvals <- length(z)

  nrows <- min(ceiling(sqrt(npanels)), maxrows)
  ncols <- min(ceiling(npanels/nrows), maxcols)
  if(fixrows) nrows <- maxrows
  if(fixcols) ncols <- maxcols

  pages <- ceiling(npanels/nrows/ncols) # how many pages full of plots
  
  # if no input on lines, then turn linepos to 0
  if(length(linesx)==1 | length(linesy)==1){
    linepos <- 0
    linesx <- ptsx
    linesy <- ptsy
  }
  
  # quick and dirty formula to get width of bars (if used) based on 
  #   number of columns and maximum number of bars within a in panel
  if(bars & barwidth=="default") barwidth <- 400/max(table(z)+2)/ncols 

  # get axis limits
  yrange <- c(0,max(ptsy,linesy))
  xrange <- c(min(ptsx,linesx),max(ptsx,linesx))
  yrange_big <- 1.2*yrange
  xrange_big <- xrange+c(-1,1)*xbuffer*diff(xrange)

  # get axis labels
  yaxs_lab <- pretty(yrange)
  maxchar <- max(nchar(yaxs_lab))
  if(horiz_lab=="default") horiz_lab <- maxchar<6 # should y-axis label be horizontal?

  if(axis1=="default") axis1=pretty(xrange)
  if(axis2=="default") axis2=pretty(yrange)

  if(length(sampsize)==1) sampsize <- 0

  # create multifigure layout and set inner margins all to 0 and add outer margins
  par(mfcol=c(nrows,ncols),mar=rep(0,4),oma=c(5,5,4,2)+.1)
  
  for(ipanel in 1:npanels)
  {
    z_i <- zvec[ipanel]
    ptsx_i <- ptsx[z==z_i]
    ptsy_i <- ptsy[z==z_i]
    ptsy_i <- ptsy_i[order(ptsx_i)]
    ptsx_i <- sort(ptsx_i)

    linesx_i <- linesx[z==z_i]
    linesy_i <- linesy[z==z_i]
    linesy_i <- linesy_i[order(linesx_i)]
    linesx_i <- sort(linesx_i)
       
    # make plot
    plot(0,type='l',axes=F,xlab="",ylab="",xlim=xrange_big,ylim=yrange_big,
      xaxs="i",yaxs=ifelse(bars,"i","r"))
    abline(h=0,col="grey") # grey line at 0 
    if(linepos==1) lines(linesx_i,linesy_i,col=linescol,lwd=lwd,lty=lty) # lines first
    if(!bars) points(ptsx_i,ptsy_i,pch=pch,col=ptscol)  # points
    if(bars)  points(ptsx_i,ptsy_i,type='h',lwd=barwidth,col=ptscol,lend=2)  # histogram-style bars
    if(linepos==2) lines(linesx_i,linesy_i,col=linescol,lwd=lwd,lty=lty)
    
    # add legends
    usr <- par("usr")
    for(i in 1:nlegends)
    {
      text_i <- ""
      legtext_i <- legtext[[i]] # grab element of list
      # elements of list can be "default" to make equal to z
      # or vector of length 1, npanels, or the full length of the input vectors 
      if(length(legtext_i)==1)           text_i <- legtext_i  # one value repeated
      if(legtext_i=="z")                 text_i <- z_i        # values in "z" input
      if(legtext_i=="sampsize"){
        if(max(sampsize)>1){ text_i <- paste("n=",sampsize[z==z_i],sep="") # sample sizes
        }else{ text_i <- ""}}
      if(length(legtext_i)==length(zvec)) text_i <- legtext_i[ipanel]  # one value per panel
      if(length(legtext_i)==nvals)    text_i <- legtext_i[z==z_i] # one value per element
      
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
      
      # pad legend size and font vectors with 1's if they aren't as long as input vectors
      legsize <- c(legsize,rep(1,nlegends-length(legsize)))
      legfont <- c(legfont,rep(1,nlegends-length(legfont)))

      # add legend text
      text(x=textx,y=texty,labels=text_i,adj=c(adjx,adjy),cex=legsize[i],font=legfont[i])
    }
      
    # add axes in left and lower outer margins
    mfg <- par("mfg")                   
    if(mfg[1]==mfg[3] | ipanel==npanels) axis(side=1,at=axis1) # axis on bottom panels and final panel
    if(mfg[2]==1) axis(side=2,at=axis2,las=horiz_lab)        # axis on left side panels
    box()
    
    # add title after plotting first panel
    fixcex = 1
    if(max(nrows,ncols)==2) fixcex = 1/0.83
    if(max(nrows,ncols)>2) fixcex = 1/0.66

    if(ipanel %% (nrows*ncols) == 1){
      title(main=main, line=c(2,0,3,3), outer=T, cex.main=1.2*fixcex)
      title(xlab=xlab, outer=T, cex.lab=fixcex)
      title(ylab=ylab, line=ifelse(horiz_lab,4,3.5), outer=T, cex.lab=fixcex)
    }
  }
  par(mfcol=c(1,1))
}


