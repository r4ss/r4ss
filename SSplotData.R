SSplotData <- function(replist,
                       plot=TRUE,print=FALSE,
                       plotdir="default",
                       fleetcol="default",
                       datatypes="all",fleets="all",fleetnames="default",ghost=FALSE,
                       pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
                       margins=c(5.1,2.1,4.1,8.1),
                       verbose=TRUE)
{
  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  ### get info from replist
  # dimensions
  startyr       <- replist$startyr
  endyr         <- replist$endyr
  nfleets       <- replist$nfleets
  nfishfleets   <- replist$nfishfleets
  if(fleetnames[1]=="default") fleetnames  <- replist$FleetNames
  if(plotdir=="default") plotdir <- replist$inputs$dir
  
  # catch
  catch <- SSplotCatch(replist,plot=F,print=F,verbose=FALSE)
  catch <- catch$totobscatchmat
  ## if(is.null(catch$totcatchmat2)) catch <- catch$totcatchmat else
  ##                                 catch <- catch$totcatchmat

  # index
  cpue          <- replist$cpue
  
  # composition data
  lendbase      <- replist$lendbase
  sizedbase     <- replist$sizedbase
  agedbase      <- replist$agedbase
  condbase      <- replist$condbase
  ghostagedbase <- replist$ghostagedbase
  ghostcondbase <- replist$ghostcondbase
  ghostlendbase <- replist$ghostlendbase
  ladbase       <- replist$ladbase
  wadbase       <- replist$wadbase
  tagdbase1     <- replist$tagdbase1
  tagdbase2     <- replist$tagdbase2

  # mean body weight
  mnwgt         <- replist$mnwgt

  # discards
  discard       <- replist$discard

  typetable <- matrix(c(
      "catch",         "Catch",                                         #1  
      "cpue",          "Abundance indices",                             #2 
      "lendbase",      "Length compositions",                           #3 
      "sizedbase",     "Size compositions",                             #4 
      "agedbase",      "Age compositions",                              #5 
      "condbase",      "Conditional age-at-length compositions",        #6 
      "ghostagedbase", "Ghost age compositions",                        #7 
      "ghostcondbase", "Ghost conditional age-at-length compositions",  #8 
      "ghostlendbase", "Ghost length compositions",                     #9 
      "ladbase",       "Mean length-at-age",                            #10 
      "wadbase",       "Mean weight-at-age",                            #11
      "mnwgt",         "Mean body weight",                              #12
      "discard",       "Discards",                                      #13
      "tagdbase1",     "Tagging data",                                  #14 
      "tagdbase2",     "Tagging data"),ncol=2,byrow=TRUE)               #15
  if(!ghost) typetable <- typetable[-grep("ghost",typetable[,1]),]
  typenames <- typetable[,1]
  typelabels <- typetable[,2]
  
  # loop over types to make a database of years with comp data
  ntypes <- 0
  # replace typetable object with empty table
  typetable <- NULL
  # now loop over typenames looking for presence of this data type
  for(itype in 1:length(typenames)){
    dat <- get(typenames[itype])
    typename <- typenames[itype]
    if(!is.null(dat) && !is.na(dat) && nrow(dat)>0){
      ntypes <- ntypes+1
      for(ifleet in 1:nfleets){
        allyrs <- NULL
        # identify years from different data types
        if(typename=="catch" & ifleet<=nfishfleets) allyrs <- dat$Yr[dat[,ifleet]>0]
        if(typename %in% c("cpue","mnwgt","discard")) allyrs <- dat$Yr[dat$FleetNum==ifleet]
        if(length(grep("dbase",typename))>0) allyrs <- dat$Yr[dat$Fleet==ifleet]
        # expand table of years with data
        if(!is.null(allyrs) & length(allyrs)>0){
          yrs <- sort(unique(floor(allyrs)))
          typetable <- rbind(typetable,
                             data.frame(yr=yrs,fleet=ifleet,
                                        itype=ntypes,typename=typename,
                                        stringsAsFactors=FALSE))
        }
      }
    }
  }
  # typetable is full data frame of all fleets and data types
  # typetable2 has been subset according to requested choices
  
  # subset by fleets and data types if requested
  if(fleets[1]=="all") fleets <- 1:nfleets
  if(datatypes[1]=="all") datatypes <- typenames
  typetable2 <- typetable[typetable$fleet %in% fleets &
                          typetable$typename %in% datatypes,]
  # define dimensions of plot
  ntypes <- max(typetable2$itype)
  fleets <- sort(unique(typetable2$fleet))
  nfleets2 <- length(fleets)

  # define colors
  if(fleetcol[1]=="default"){
    if(nfleets2>3) fleetcol <- rich.colors.short(nfleets2+1)[-1]
    if(nfleets2==1) fleetcol <- "grey40"
    if(nfleets2==2) fleetcol <- rich.colors.short(nfleets2)
    if(nfleets2==3) fleetcol <- c("blue","red","green3")
  }else{
    if(length(fleetcol) < nfleets2) fleetcol=rep(fleetcol,nfleets2)
  }

  # function containing plotting commands
  plotdata <- function(){
    par(mar=margins) # multi-panel plot
    xlim <- c(-1,1)+range(typetable2$yr,na.rm=TRUE)
    yval <- 0
    # count number of unique combinations of fleet and data type
    ymax <- sum(as.data.frame(table(typetable2$fleet,typetable2$itype))$Freq>0)
    plot(0,xlim=xlim,ylim=c(0,ymax+ntypes+.5),axes=FALSE,xaxs='i',yaxs='i',
         type="n",xlab="Year",ylab="",main="Data by type and year",cex.main=cex.main)
    xticks <- 5*round(xlim[1]:xlim[2]/5)
    abline(v=xticks,col='grey',lty=3)
    axistable <- data.frame(fleet=rep(NA,ymax),yval=NA)
    itick <- 1
    for(itype in rev(unique(typetable2$itype))){
      typename <- unique(typetable2$typename[typetable2$itype==itype])
      #fleets <- sort(unique(typetable2$fleet[typetable2$itype==itype]))
      for(ifleet in rev(fleets)){
        yrs <- typetable2$yr[typetable2$fleet==ifleet & typetable2$itype==itype]
        if(length(yrs)>0){
          yval <- yval+1
          x <- min(yrs):max(yrs)
          n <- length(x)
          y <- rep(yval,n)
          y[!x%in%yrs] <- NA
          # identify solo points (no data from adjacent years)
          solo <- rep(FALSE,n)
          if(n==1) solo <- 1
          if(n==2 & yrs[2]!=yrs[1]+1) solo <- rep(TRUE,2)
          if(n>=3){
            for(i in 2:(n-1)) if(is.na(y[i-1]) & is.na(y[i+1])) solo[i] <- TRUE
            if(is.na(y[2])) solo[1] <- TRUE
            if(is.na(y[n-1])) solo[n] <- TRUE
          }
          # add points and lines
          points(x[solo], y[solo], pch=16, cex=2,col=fleetcol[fleets==ifleet])
          lines(x, y, lwd=12, col=fleetcol[fleets==ifleet])
          axistable[itick,] <- c(ifleet,yval)
          itick <- itick+1
        }
      }
      
      yval <- yval+1
      if(itype!=1) abline(h=yval,col='grey',lty=3)
      text(mean(xlim),yval-.3,typelabels[typenames==typename],font=2)
    }
    axis(4,at=axistable$yval,labels=fleetnames[axistable$fleet],las=1)
    box()
    axis(1,at=xticks)
  }

  if(plot) plotdata()
  if(print) {
    file <- paste(plotdir,"data_plot.png",sep="")
    caption <- "Data presence by year for each fleet"
    plotinfo <- pngfun(file=file, caption=caption)
    plotdata()
    dev.off()
  }

  returnlist <- list(typetable2=typetable2)
  
  if(!is.null(plotinfo)){
    plotinfo$category <- "Data"
    returnlist$plotinfo <- plotinfo
  }
  return(invisible(returnlist))
}
