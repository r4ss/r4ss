#' Timeline of presence/absence of data by type, year, and fleet.
#'
#' Plot shows graphical display of what data is being used in the model.  Some
#' data types may not yet be included. Note, this is based on output from the
#' model, not the input data file.
#'
#'
#' @param replist list created by \code{\link{SS_output}}
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param subplot vector controlling which subplots to create
#' Currently there are only 2 subplots:
#' \itemize{
#'   \item 1 equal size points showing presence/absence of data type by year/fleet
#'   \item 2 points scaled to indicate quantity or precision of data
#' }
#' @param fleetcol Either the string "default", or a vector of colors to use
#' for each fleet. If tagging data is included, an additional color needs to be
#' added for the tag releases which are not assigned to a fleet.
#' @param datatypes Either the string "all", or a vector including some subset
#' of the following: "catch", "cpue", "lendbase", "sizedbase", "agedbase",
#' "condbase", "ghostagedbase", "ghostcondbase", "ghostlendbase", "ladbase",
#' "wadbase", "mnwgt", "discard", "tagrelease", and "tagdbase1".
#' @param fleets Either the string "all", or a vector of numerical values, like
#' c(1,3), listing fleets or surveys to be included in the plot.
#' @param fleetnames A vector of alternative names to use in the plot. By
#' default the parameter names in the data file are used.
#' @param ghost TRUE/FALSE indicator for whether to show presence of
#' composition data from ghost fleets (data for which the fit is shown, but is
#' not included in the likelihood calculations).
#' @param pwidth width of plot
#' @param pheight height of plot
#' @param punits units for PNG file
#' @param res resolution for PNG file
#' @param ptsize point size for PNG file
#' @param cex.main character expansion for plot titles
#' @param margins margins of plot (passed to par() function), which may need to
#' be increased if fleet names run off right-hand margin
#' @param cex Character expansion for points showing isolated years of data
#' @param lwd Line width for lines showing ranges of years of data
#' @param verbose report progress to R GUI?
#' @param maxsize The size (cex) of the largest bubble in the datasize
#' plot. Default is 1.
#' @param alphasize The transparency of the bubbles in the datasize
#' plot. Defaults to 1 (no transparency). Useful for models with lots of
#' overlapping points.
#' @param mainTitle TRUE/FALSE switch to turn on/off the title on the plot.
#' @author Ian Taylor, Chantel Wetzel, Cole Monnahan
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}},
#' \code{\link{SS_readdat}}
SSplotData <- function(replist,
                       plot=TRUE,print=FALSE,
                       plotdir="default",
                       subplot=1:2,
                       fleetcol="default",
                       datatypes="all",fleets="all",fleetnames="default",ghost=FALSE,
                       pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1,
                       margins=c(5.1,2.1,2.1,8.1),
                       cex=2,lwd=12,
                       maxsize=1.0,
                       alphasize=1,
                       mainTitle=FALSE,
                       verbose=TRUE)
{
  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  ## ### override datasize variable in seasonal models
  ## if(replist$nseasons > 1){
  ##   cat("  Setting datasize to FALSE because not yet implemented for seasonal models.\n")
  ##   datasize <- FALSE
  ## }

  ### get info from replist
  # dimensions
  startyr       <- replist$startyr
  endyr         <- replist$endyr
  nfleets       <- replist$nfleets
  
  if(fleetnames[1]=="default"){
    fleetnames  <- replist$FleetNames
  }
  if(plotdir=="default"){
    plotdir <- replist$inputs$dir
  }

  # catch
  catch         <- replist$catch
  
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

  # mean body weight
  mnwgt         <- replist$mnwgt

  # discards
  discard       <- replist$discard

  # tag data
  tagrelease      <- replist$tagrelease

  # make table of data types
  typetable <- matrix(c(
      "catch",         "Catches",                                       #1
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
      "tagrelease",    "Tag releases",                                  #14 
      "tagdbase1",     "Tag recaptures"),ncol=2,byrow=TRUE)             #15
  # note: tagdbase2 excluded since it is not fleet specific and the years
  #       should always match those in tagdbase1

  # exclude ghost fleet observations if requested
  if(!ghost){
    typetable <- typetable[-grep("ghost",typetable[,1]),]
  }
  typenames <- typetable[,1]
  typelabels <- typetable[,2]

  # loop over types to make a database of years with comp data
  ntypes <- 0
  # replace typetable object with empty table
  typetable <- NULL
  ## now loop over typenames looking for presence of this data type
  ### --- 11/2015 Cole added a "size" column to this data so that relative
  ### uncertainties can be used for cex values in a new plot below.
  for(itype in 1:length(typenames)){
    dat <- get(typenames[itype])
    typename <- typenames[itype]
    # confirm that there is non-NA data of this type
    if(!is.null(dat) && !all(is.na(dat)) && nrow(dat)>0){
      ntypes <- ntypes+1
      for(ifleet in 1:nfleets){
        allyrs <- NULL
        size <- NULL
        # subset for this fleet
        dat.f <- dat[dat$Fleet == ifleet,]
        # check for observations from this fleet
        if(nrow(dat.f) > 0){

          # identify years from different data types
          if(typename == "catch"){
            # aggregate catch by year
            dat.agg <- aggregate(dat.f$Obs, by=list(dat.f$Yr), FUN=sum)
            allyrs <- dat.agg$Group.1[dat.agg$x > 0]
            size <- dat.agg$x[dat.agg$x > 0]

            #### turning off this feature because a data plot should probably
            #### only show data, rather than estimated discard mortality
            ## # use updated table in newer versions of SS (probably 3.30+)
            ## # presumably this will include discards whereas the previous approach
            ## # may have only been landed catch
            ## if("kill_bio" %in% names(dat.f)){
            ##   size <- dat.f$kill_bio[dat.f$Obs>0]
            ## }
          }
          if(typename == "cpue"){
            # filter out rows that aren't used
            dat.f <- dat.f[dat.f$Use > 0,]
            # aggregate by year, taking average SE (on a log scale)
            dat.agg <- aggregate(dat.f$SE, by=list(dat.f$Yr), FUN=mean)
            allyrs <- dat.agg$Group.1
            size <- 1/dat.agg$x # inverse of mean SE
          }
          if(typename == "mnwgt"){
            # filter out rows that aren't used
            dat.f <- dat.f[dat.f$Use > 0,]
            # get mean CV across partitions
            dat.agg <- aggregate(dat.f$CV, by=list(dat.f$Yr), FUN=mean)
            allyrs <- dat.agg$Group.1
            size <- 1/dat.agg$x # inverse of mean CV
          }
          if(typename == "discard"){
            # filter out rows that aren't used
            dat.f <- dat.f[dat.f$Use > 0,]
            # get mean standard deviation across partitions
            dat.agg <- aggregate(dat.f$Std_in, by=list(dat.f$Yr), FUN=mean)
            allyrs <- dat.agg$Group.1
            size <- 1/dat.agg$x # inverse of mean CV
          }
          if(typename %in% c("lendbase", "sizedbase", "agedbase")){
            # aggregate sample sizes by year
            dat.agg <- aggregate(dat.f$N, by=list(dat.f$Yr), FUN=sum)
            allyrs <- dat.agg$Group.1
            size <- dat.agg$x
          }
          if(typename %in% c("ghostagedbase", "ghostcondbase", "ghostlendbase")){
            allyrs <- unique(dat.f$Yr)
            # sample sizes not currently (as of 3.30.13) reported
            # for ghost observations
            size <- rep(1, length(allyrs))
          }
          if(typename %in% c("condbase", "ghostcondbase")){
            # subset by smallest bin (sample size is repeated
            # for all bins within each vector of observations)
            dat.sub <- dat.f[dat.f$Bin == min(dat.f$Bin),]
            # check for observations within this fleet
            if(nrow(dat.sub) > 0){
              # aggregate sample sizes by year
              dat.agg <- aggregate(dat.sub$N, by=list(dat.sub$Yr), FUN=sum)
              allyrs <- dat.agg$Group.1
              size <- dat.agg$x
            }
          }
          if(typename=="tagrelease" & ifleet==1){
            # aggregate sample sizes by year
            dat.agg <- aggregate(dat.f$Nrelease, by=list(dat.f$Yr), FUN=sum)
            allyrs <- dat.agg$Group.1
            size <- dat.agg$x
          }
          if(typename=="tagdbase1"){
            # filter out rows that aren't used
            dat.f <- dat.f[dat.f$Used == "yes",]
            # aggregate sample sizes by year
            dat.agg <- aggregate(dat.f$Obs, by=list(dat.f$Yr), FUN=sum)
            allyrs <- dat.agg$Group.1[dat.agg$x > 0]
            size <- dat.agg$x[dat.agg$x > 0]
          }
          # length- and weight-at-age have different sample sizes for each age
          # within a year, use sum of sample sizes
          # (results will be same as if average was used due to rescaling) 
          if(typename %in% c("ladbase","wadbase")){
            # filter out rows that aren't used
            dat.f <- dat.f[dat.f$Used == "yes",]
            # aggregate sample sizes by year
            dat.agg <- aggregate(dat.f$N, by=list(dat.f$Yr), FUN=sum)
            allyrs <- dat.agg$Group.1
            size <- dat.agg$x
          }
          
          # expand table of years with data
          if(!is.null(allyrs) & length(allyrs)>0){
            ## subset to unique values and be careful about keeping the
            ## order of size
            ## this will cause repeat observations for different partitions
            ## to be averaged
            unique.index <- which(!duplicated(allyrs))
            yrs <- floor(allyrs[unique.index])
            size.sorted <- size[unique.index][order(yrs)]
            yrs.sorted <- yrs[order(yrs)]

            # add to big typetable
            typetable <- rbind(typetable,
                               data.frame(yr=yrs.sorted,
                                          fleet=ifelse(typename=="tagrelease",
                                              nfleets+1, ifleet),
                                          itype=ntypes,typename=typename,
                                          size=size.sorted,
                                          stringsAsFactors=FALSE))
          } # end example table
        } # check for values within this fleet
      } # end loop over fleets
    } # end check for presence of data of this type
  } # end loop over typenames

  # subset by fleets and data types if requested
  if(fleets[1]=="all"){
    fleets <- 1:(nfleets+1)
  }
  if(datatypes[1]=="all"){
    datatypes <- typenames
  }

  # typetable2 has been subset according to requested choices of type
  typetable2 <- typetable[typetable$fleet %in% fleets &
                            typetable$typename %in% datatypes,]

  # define dimensions of plot
  ntypes <- max(typetable2$itype)
  # fleets2 is a subset of fleets that have data of the requested types
  fleets2 <- sort(unique(typetable2$fleet))
  fleets2 <- fleets2[fleets2 %in% c(0,fleets)]
  nfleets2 <- length(fleets2)
  # add name for tag releases which are not assigned to a fleet
  if(nfleets + 1 %in% fleets2){
    fleetnames <- c(fleetnames, "unassigned")
  }
  
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
  plotdata <- function(datasize){
    par(mar=margins) # multi-panel plot
    xlim <- c(-1,1)+range(typetable2$yr,na.rm=TRUE)
    yval <- 0
    # count number of unique combinations of fleet and data type
    ymax <- sum(as.data.frame(table(typetable2$fleet,typetable2$itype))$Freq>0)
    main.temp <- ""
    if(mainTitle){
      main.temp <- if(datasize) {
        "Data by type and year, circle area is relative to precision within data type"
      } else {
        "Data by type and year"
      }
    }
    plot(0,xlim=xlim,ylim=c(0,ymax+2*ntypes+.5),axes=FALSE,xaxs='i',yaxs='i',
         type="n",xlab="Year",ylab="",main=main.temp, cex.main=cex.main)
    xticks <- 5*round(xlim[1]:xlim[2]/5)
    abline(v=xticks,col='grey',lty=3)
    axistable <- data.frame(fleet=rep(NA,ymax),yval=NA)
    itick <- 1
    # loop over data types
    for(itype in rev(unique(typetable2$itype))){
      ## Calculate relative size for each data type separately
      typetable2$size[typetable2$itype==itype] <-
        typetable2$size[typetable2$itype==itype] /
          max(typetable2$size[typetable2$itype==itype], na.rm=TRUE)
      # name for this data type
      typename <- unique(typetable2$typename[typetable2$itype==itype])
      # subset of fleets for this data type
      type.fleets <- sort(unique(typetable2$fleet[typetable2$itype==itype]))
      for(ifleet in rev(type.fleets)){
        yrs <- typetable2$yr[typetable2$fleet==ifleet & typetable2$itype==itype]
        if(length(yrs)>0){
          size.cex <- typetable2$size[typetable2$fleet==ifleet & typetable2$itype==itype]
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
          if(!datasize){
            ## The original plot is to add points and lines
            points(x[solo], y[solo], pch=16, cex=cex, col=fleetcol[fleets==ifleet])
            lines(x, y, lwd=lwd, col=fleetcol[fleets==ifleet])
          } else {
            ## make circle sizes propotional to the uncertainty,
            ## contained in size, NA's don't work for symbols so remove them
            x <- x[!is.na(y)]
            y <- y[!is.na(y)]
            symbols(x=x, y=y, circles=sqrt(size.cex)*maxsize,
                    bg=adjustcolor(fleetcol[fleets==ifleet], alpha.f=alphasize),
                    add=TRUE, inches=FALSE)
          }
          axistable[itick,] <- c(ifleet,yval)
          itick <- itick+1
        }
      }
      yval <- yval+2
      if(itype!=1) abline(h=yval+.3,col='grey',lty=3)
      text(mean(xlim),yval-.3,typelabels[typenames==typename],font=2)
      #text(mean(xlim),yval,typelabels[typenames==typename],font=2)
    }
    axis(4,at=axistable$yval,labels=fleetnames[axistable$fleet],las=1)
    box()
    axis(1,at=xticks)
  }

  ## Make the plot with no scaling on circles
  if(1 %in% subplot){
    if(plot){
      plotdata(datasize=FALSE)
    }
    if(print) {
      caption <- "Data presence by year for each fleet and data type"
      plotinfo <- pngfun(file="data_plot.png", caption=caption)
      plotdata(datasize=FALSE)
      dev.off()
    }
  }

  ## Make second data plot
  if(2 %in% subplot){
    if(plot){
      plotdata(datasize=TRUE)
    }
    if(print) {
      caption <- paste(
          "Data presence by year for each fleet, where circle area is <br> ",
          "relative within a data type. Circles are proportional to <br> ",
          "total catch for catches; to precision for indices, discards, and <br> ",
          "mean body weight observations; and to total sample size for <br>",
          "compositions and mean weight- or length-at-age observations. <br>",
          "'Ghost' observations (not included in the likelihood) have <br>",
          "equal size for all years. <br>",
          "Note that since the circles are are scaled relative <br> ",
          "to maximum within each type, the scaling within separate plots <br> ",
          "should not be compared.")
      if(replist$nseasons > 1){
        caption <- paste(
            caption,
            "<br>This is a seasonal model, so scaling is based on either <br> ",
            "the sum of samples within each year (for things like comps) <br> ",
            "or the average among observations within a year (for  <br> ",
            "things like index uncertainty).")
      }
      plotinfo <- pngfun(file="data_plot2.png", caption=caption)
      plotdata(datasize=TRUE)
      dev.off()
    }
  }

  returnlist <- list(typetable2=typetable2)

  if(!is.null(plotinfo)){
    plotinfo$category <- "Data"
    returnlist$plotinfo <- plotinfo
  }
  return(invisible(returnlist))
}
