#' Plot tagging data and fits
#' 
#' Plot observed and expected tag recaptures in aggregate and by tag group.
#' 
#' 
#' @param replist list created by \code{SS_output}
#' @param subplots vector controlling which subplots to create
#' @param latency period of tag mixing to exclude from plots (in future could
#' be included in SS output)
#' @param taggroups which tag groups to include in the plots. Default=NULL
#' causes all groups to be included.
#' @param rows number or rows of panels for regular plots
#' @param cols number or columns of panels for regular plots
#' @param tagrows number or rows of panels for multi-panel plots
#' @param tagcols number or columns of panels for multi-panel plots
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param pntscalar maximum bubble size for balloon plots; each plot scaled
#' independently based on this maximum size and the values plotted. Often some
#' plots look better with one value and others with a larger or smaller value.
#' Default=2.6
#' @param minnbubble minimum number of years below which blank years will be
#' added to bubble plots to avoid cropping
#' @param pwidth default width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param pheight default height width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param punits units for \code{pwidth} and \code{pheight}. Can be "px"
#' (pixels), "in" (inches), "cm" or "mm". Default="in".
#' @param ptsize point size for plotted text in plots printed to files (see
#' help("png") in R for details). Default=12.
#' @param res resolution of plots printed to files. Default=300
#' @param cex.main character expansion parameter for plot titles
#' @param col1 color for bubbles
#' @param col2 color for lines with expected values
#' @param col3 shading color for observations within latency period
#' @param col4 shading color for observations after latency period
#' @param labels vector of labels for plots (titles and axis labels)
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param verbose return updates of function progress to the R GUI?
#' @author Andre Punt, Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}}
#' @keywords hplot
SSplotTags <-
  function(replist=replist, subplots=1:8, latency=NULL, taggroups=NULL,
           rows=1, cols=1,
           tagrows=3, tagcols=3,
           plot=TRUE, print=FALSE,
           pntscalar=2.6,minnbubble=8,
           pwidth=6.5, pheight=5.0, punits="in", ptsize=10, res=300, cex.main=1,
           col1=rgb(0,0,1,.7),col2="red",col3="grey95",col4="grey70",
           labels = c("Year",                                   #1
           "Frequency",                                         #2
           "Tag Group",                                         #3
           "Fit to tag recaptures by tag group",                #4
           "Post-latency tag recaptures aggregated across tag groups", #5
           "Observed tag recaptures by year and tag group",     #6
           "Residuals for post-latency tag recaptures: (obs-exp)/sqrt(exp)", #7
           "Observed and expected post-latency tag recaptures by year and tag group"), #8
           plotdir="default",
           verbose=TRUE)
{
  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  if(plotdir=="default") plotdir <- replist$inputs$dir

  tagdbase2 <- replist$tagdbase2
  if(is.null(tagdbase2) || nrow(tagdbase2)==0){
    if(verbose) cat("skipping tag plots because there's no tagging data\n")
  }else{
    # filter tag groups if requested
    if(!is.null(taggroups)){
      tagdbase2 <- tagdbase2[tagdbase2$Rep %in% taggroups,]
      cat("Filtered tag groups for plotting based on input vector taggroups\n",
          "Plots will show", length(unique(tagdbase2$Rep)),
          "out of", length(unique(replist$tagdbase2$Rep)), "total included in the model.\n")
    }
    
    # calculations needed for printing to multiple PNG files
    grouprange     <- unique(tagdbase2$Rep)
    ngroups        <- length(unique(tagdbase2$Rep))
    npages         <- ceiling(ngroups/(tagrows*tagcols))
    nseasons       <- replist$nseasons
    width          <- 0.5/nseasons
    tagreportrates <- replist$tagreportrates
    tagrecap       <- replist$tagrecap
    tagsalive      <- replist$tagsalive
    tagtotrecap    <- replist$tagtotrecap
    if(is.null(latency)){
      latency <- replist$tagfirstperiod
    }
    
    tagfun1 <- function(ipage=0){
      if(verbose) cat("Note: lighter colored bars in tag plot indicate latency period excluded from likelihood\n")
      # obs & exp recaps by tag group
      par(mfcol=c(tagrows,tagcols),mar=c(2.5,2.5,2,1),cex.main=cex.main,oma=c(2,2,2,0))
      if(npages > 1 & ipage!=0) grouprange <- intersect(grouprange, 1:(tagrows*tagcols) + tagrows*tagcols*(ipage-1))
      for(igroup in grouprange){
        tagtemp <- tagdbase2[tagdbase2$Rep==igroup,]
        ylim=c(0,max(5,cbind(tagtemp$Obs,tagtemp$Exp)*1.05))
        plot(0,type="n",xlab="",ylab="",ylim=ylim,main=paste("TG ",igroup,sep=""),
             xaxs="i",yaxs="i",xlim=c(min(tagtemp$Yr.S)-0.5,max(tagtemp$Yr.S)+0.5))
        for (iy in 1:length(tagtemp$Yr.S)){
          xx <- c(tagtemp$Yr.S[iy]-width,tagtemp$Yr.S[iy]-width,
                  tagtemp$Yr.S[iy]+width,tagtemp$Yr.S[iy]+width)
          yy <- c(0,tagtemp$Obs[iy],tagtemp$Obs[iy],0)
          polygon(xx,yy,col=ifelse(iy<=latency,col3,col4))
        }
        points(tagtemp$Yr.S,tagtemp$Exp,type="o",lty=1,pch=16)
        if(latency>0){
          points(tagtemp$Yr.S[1:latency], tagtemp$Exp[1:latency],
                 type="o", lty=1, pch=21, bg="white")
          if(all(par()$mfg[1:2]==1)){
            legend('topright', fill=c(col3,col4),
                   c("Latency period","Post-latency"), bty='n')
          }
        }
        box()
        
        # add labels in left and lower outer margins once per page
        mfg <- par("mfg")
        if(mfg[1]==1 & mfg[2]==1){
          mtext(labels[1],side=1,line=0,outer=TRUE)
          mtext(labels[2],side=2,line=0,outer=TRUE)
          mtext(labels[4],side=3,line=0,outer=TRUE,cex=cex.main,font=2)
        }
      }

      # restore default single panel settings
      par(mfcol=c(rows,cols),mar=c(5,5,4,2)+.1,oma=rep(0,4))
    }

    cat("Calculated tagging related quantities...\n")
    # reconfiguring tagdbase2
    ## # old system from Andre which exclude exactly 1 year for each group as the latency period
    ## XRep <- -1
    ## x <- NULL
    ## for (irow in 1:length(tagdbase2[,1])){
    ##   if (tagdbase2$Rep[irow] != XRep){
    ##     XRep <- tagdbase2$Rep[irow]
    ##   }else{
    ##     x <- rbind(x,tagdbase2[irow,])
    ##   }
    ## }
    ## # alternatively, don't reconfigure by using:
    ## #x <- tagdbase

    # new system which takes latency value as input
    tgroups <- sort(unique(tagdbase2$Rep))
    x <- NULL
    for(igroup in tgroups){
      temp <- tagdbase2[tagdbase2$Rep==igroup,] # subset results for only 1 tag group
      temp <- temp[-(1:latency),] # remove the first rows corresponding to the latency period
      x <- rbind(x, temp)
    }
    
    #obs vs exp tag recaptures by year aggregated across group
    tagobs <- aggregate(x$Obs,by=list(x$Yr.S,x$Rep),FUN=sum,na.rm=TRUE)
    tagexp <- aggregate(x$Exp,by=list(x$Yr.S,x$Rep),FUN=sum,na.rm=TRUE)
    Recaps <- data.frame(Yr.S=tagobs[,1],Group=tagobs[,2],Obs=tagobs[,3],Exp=tagexp[,3])
print(head(Recaps))
    xlim <- range(Recaps$Yr.S)
    xx2 <- aggregate(Recaps$Obs,by=list(Recaps$Yr.S),FUN=sum,na.rm=TRUE)
    xx3 <- aggregate(Recaps$Exp,by=list(Recaps$Yr.S),FUN=sum,na.rm=TRUE)
    RecAg <- data.frame(Yr.S=xx2[,1],Obs=xx2[,2],Exp=xx3[,2])

    tagfun2 <- function(){
      #obs vs exp tag recaptures by year aggregated across group
      plot(0,xlim=xlim+c(-0.5,0.5),ylim=c(0,max(RecAg$Obs,RecAg$Exp)*1.05),type="n",xaxs="i",yaxs="i",
           xlab=labels[1],ylab=labels[2],main=labels[5],cex.main=cex.main)
      for (iy in 1:nrow(RecAg)){
        xx <- c(RecAg$Yr.S[iy]-width, RecAg$Yr.S[iy]-width,
                RecAg$Yr.S[iy]+width, RecAg$Yr.S[iy]+width)
        yy <- c(0,RecAg$Obs[iy],RecAg$Obs[iy],0)
        polygon(xx,yy,col=col4)
      }
      lines(RecAg$Yr.S,RecAg$Exp,type="o",pch=16,lty=1,lwd=2)
    }

    Recaps$Pearson <- (Recaps$Obs-Recaps$Exp)/sqrt(Recaps$Exp)
    Recaps$Pearson[Recaps$Exp==0] <- NA
print(head(Recaps))

    tagfun3 <- function(){
      # bubble plot of observed recapture data
      plottitle <- labels[6]
      bubble3(x=Recaps$Yr.S,y=Recaps$Group,z=Recaps$Obs,xlab=labels[1],ylab=labels[3],col=rep(col1,2),
              las=1,main=plottitle,cex.main=cex.main,maxsize=pntscalar,allopen=FALSE,minnbubble=minnbubble)
    }
    tagfun4 <- function(){
      # bubble plot of residuals
      plottitle <- labels[7]
      bubble3(x=Recaps$Yr.S,y=Recaps$Group,z=Recaps$Pearson,xlab=labels[1],ylab=labels[3],col=rep(col1,2),
              las=1,main=plottitle,cex.main=cex.main,maxsize=pntscalar,allopen=FALSE,minnbubble=minnbubble)
    }
    tagfun5 <- function(){
      # line plot by year and group
      plottitle <- labels[8]
      plot(0,type="n",xlim=range(Recaps$Yr.S),ylim=range(Recaps$Group)+c(0,1),xlab=labels[1],ylab=labels[3],
           main=plottitle,cex.main=cex.main)
      rescale <- .9*min(ngroups-1,5)/max(Recaps$Obs,Recaps$Exp)
      for(igroup in sort(unique(Recaps$Group))){
        lines(Recaps$Yr.S[Recaps$Group==igroup],igroup+0*Recaps$Obs[Recaps$Group==igroup],col="grey",lty=3)
        points(Recaps$Yr.S[Recaps$Group==igroup],igroup+rescale*Recaps$Obs[Recaps$Group==igroup],type="o",pch=16,cex=.5)
        lines(Recaps$Yr.S[Recaps$Group==igroup],igroup+rescale*Recaps$Exp[Recaps$Group==igroup],col=col2,lty="42",lwd=2)
      }
      legend('topleft',bty='n',lty=c('91','42'),pch=c(16,NA),pt.cex=c(.5,NA),
             col=c(1,2),lwd=c(1,2),legend=c('Observed','Expected'))
    }
    tagfun6 <- function(){
      # a function to plot tag parameters after transformation
      # into reporting rate and tag loss quantities
      
      par(mfrow=c(2,2))
      # first plot is reporting rate parameters
      barplot(height=tagreportrates$Init_Reporting,
              names.arg=tagreportrates$Fleet,ylim=c(0,1),yaxs='i',
              ylab="Reporting rate",xlab="Fleet number",
              main="Initial reporting rate")
      box()

      # second plot shows any decay in reporting rate over time
      matplot(0:5, exp((0:5) %*% t(tagreportrates$Report_Decay)),
              type='l',lwd=3,lty=1,col=rich.colors.short(nrow(tagreportrates)),
              ylim=c(0,1.05),yaxs='i',
              ylab="Reporting rate",xlab="Time at liberty (years)",
              main="Reporting rate decay")

      # third plot shows initial tag loss
      barplot(height=tagrecap$Init_Loss,
              names.arg=tagrecap$Fleet,ylim=c(0,1),yaxs='i',
              ylab="Initial tag loss",xlab="Tag group",
              main="Initial tag loss\n(fraction of tags lost at time of tagging)")
      box()

      # fourth plot shows chronic tag loss
      barplot(height=tagrecap$Chron_Loss,
              names.arg=tagrecap$Fleet,ylim=c(0,1),yaxs='i',
              ylab="Chronic tag loss",xlab="Tag group",
              main="Chronic tag loss\n(fraction of tags lost per year)")
      box()

      # restore default single panel settings
      par(mfcol=c(rows,cols),mar=c(5,5,4,2)+.1,oma=rep(0,4))
    }

    tagfun7 <- function(){
      # a function to plot the "tags alive" matrix
      xvals <- as.numeric(substring(names(tagsalive)[-1],7))
      matplot(xvals,t(tagsalive[,-1]),type='l',lwd=3,
              col=rich.colors.short(nrow(tagsalive)),
              xlab="Period at liberty",
              ylab="Estimated number of alive tagged fish",
              main="'Tags alive' by tag group")
      abline(h=0,col='grey')
    }
    tagfun8 <- function(){
      # a function to plot the "total recaptures" matrix
      xvals <- as.numeric(substring(names(tagtotrecap)[-1],7))
      matplot(xvals,t(tagtotrecap[,-1]),type='l',lwd=3,
              col=rich.colors.short(nrow(tagtotrecap)),
              xlab="Period at liberty",
              ylab="Estimated number of recaptures",
              main="'Total recaptures' by tag group")
      abline(h=0,col='grey')
    }
    
    # make plots
    if(plot){
      if(1 %in% subplots) tagfun1()
      if(2 %in% subplots) tagfun2()
      if(3 %in% subplots) tagfun3()
      if(4 %in% subplots) tagfun4()
      if(5 %in% subplots) tagfun5()
      if(6 %in% subplots) tagfun6()
      if(7 %in% subplots) tagfun7()
      if(8 %in% subplots) tagfun8()
    }
    # send to files if requested
    if(print){
      filenamestart <- "tags_by_group"
      if(1 %in% subplots){
        for(ipage in 1:npages){
          if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
          file <- paste(plotdir,filenamestart,pagetext,".png",sep="")
          caption <- paste(labels[4],"(lighter colored bars indicate latency period excluded from likelihood)")
          if(npages>1) caption <- paste(caption, ", (plot ",ipage,"of ",npages,")",sep="")
          plotinfo <- pngfun(file=file, caption=caption)
          tagfun1(ipage=ipage)
          dev.off() # close device if png
        }
      }
      if(2 %in% subplots){
        file <- paste(plotdir,"tags_aggregated.png",sep="")
        caption <- labels[5]
        plotinfo <- pngfun(file=file, caption=caption)
        tagfun2()
        dev.off()
      }
      if(3 %in% subplots){
        file <- paste(plotdir,"tags_data_bubbleplot.png",sep="")
        caption <- labels[6]
        plotinfo <- pngfun(file=file, caption=caption)
        tagfun3()
        dev.off()
      }
      if(4 %in% subplots){
        file <- paste(plotdir,"tags_residuals.png",sep="")
        caption <- labels[7]
        plotinfo <- pngfun(file=file, caption=caption)
        tagfun4()
        dev.off()
      }
      if(5 %in% subplots){
        file <-paste(plotdir,"tags_lines.png",sep="")
        caption <- labels[8]
        plotinfo <- pngfun(file=file, caption=caption)
        tagfun5()
        dev.off()
      }
      if(6 %in% subplots){
        file <-paste(plotdir,"tags_parameters.png",sep="")
        caption <- "Tag-related parameters"
        plotinfo <- pngfun(file=file, caption=caption)
        tagfun6()
        dev.off()
      }
      if(7 %in% subplots){
        file <-paste(plotdir,"tags_alive.png",sep="")
        caption <- "'Tags alive' by tag group"
        plotinfo <- pngfun(file=file, caption=caption)
        tagfun7()
        dev.off()
      }
      if(8 %in% subplots){
        file <-paste(plotdir,"tags_total_recaptures.png",sep="")
        caption <- "Total tag recaptures"
        plotinfo <- pngfun(file=file, caption=caption)
        tagfun8()
        dev.off()
      }
    }
    flush.console()
    
  } # end if data
  if(!is.null(plotinfo)) plotinfo$category <- "Tag"
  return(invisible(plotinfo))
} # end SSplotTags
