make_multifig <- function(ptsx, ptsy, yr, linesx=0, linesy=0, ptsSD=0,
                          sampsize=0, effN=0, showsampsize=TRUE, showeffN=TRUE, sampsizeround=1,
                          maxrows=6, maxcols=6, rows=1, cols=1, fixdims=TRUE, main="",cex.main=1,
                          xlab="",ylab="",size=1,cexZ1=1.5,bublegend=TRUE,
                          maxsize=NULL,do.sqrt=TRUE,minnbubble=8,allopen=TRUE,
                          horiz_lab="default",xbuffer=c(.1,.1),ybuffer=c(0,0.15),ymin0=TRUE,
                          axis1="default",axis2="default",linepos=1,type="o",
                          bars=FALSE,barwidth="default",ptscex=1,ptscol=1,ptscol2=1,linescol=2,lty=1,lwd=1,pch=1,
                          nlegends=3,legtext=list("yr","sampsize","effN"),legx="default",legy="default",
                          legadjx="default",legadjy="default",legsize=c(1.2,1.0),legfont=c(2,1),
                          sampsizeline=FALSE,effNline=FALSE,sampsizemean=NULL,effNmean=NULL,
                          ipage=0,scalebins=FALSE){
  ################################################################################
  #
  # make_multifig
  #
  # Purpose: To plot a multifigure environment similar to lattice but simpler
  #		 and with easier controls over some things
  # Written: Ian Taylor
  #
  ################################################################################

  # define dimensions
  yrvec <- sort(unique(yr))
  npanels <- length(yrvec)
  nvals <- length(yr)

  nrows <- min(ceiling(sqrt(npanels)), maxrows)
  ncols <- min(ceiling(npanels/nrows), maxcols)

  if(fixdims){
    nrows <- maxrows
    ncols <- maxcols
  }

  npages <- ceiling(npanels/nrows/ncols) # how many pages of plots
  doSD <- length(ptsSD)==length(ptsx) & max(ptsSD) > 0 # T/F on whether to add error bars on points
  
  # if no input on lines, then turn linepos to 0
  if(length(linesx)==1 | length(linesy)==1){
    linepos <- 0
    linesx <- ptsx
    linesy <- ptsy
  }
  anyscaled <- FALSE
  
  # quick and dirty formula to get width of bars (if used) based on
  #	  number of columns and maximum number of bars within a in panel
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
  if(length(effN)==1) effN <- 0

  # create multifigure layout and set inner margins all to 0 and add outer margins
  # new settings
  par(mfcol=c(nrows,ncols),mar=rep(0,4),oma=c(5,5,5,2)+.1)

  panelrange <- 1:npanels
  if(npages > 1 & ipage!=0) panelrange <- intersect(panelrange, 1:(nrows*ncols) + nrows*ncols*(ipage-1))
  for(ipanel in panelrange){
    # subset values
    yr_i <- yrvec[ipanel]
    ptsx_i <- ptsx[yr==yr_i]
    ptsy_i <- ptsy[yr==yr_i]
    ptsy_i[ptsy_i < 0] <- NA
    if(doSD) ptsSD_i <- ptsSD[yr==yr_i]
      
    linesx_i <- linesx[yr==yr_i]
    linesy_i <- linesy[yr==yr_i]

    # sort values in lines
    linesy_i <- linesy_i[order(linesx_i)]
    linesx_i <- sort(linesx_i)

    z_i <- size[yr==yr_i]

    # optional rescaling of bins for line plots
    scaled <- FALSE
    if(scalebins){
      bins <- sort(unique(ptsx_i))
      binwidths <- diff(bins)
      if(diff(range(binwidths))>0){
        binwidths <- c(binwidths,tail(binwidths,1))
        allbinwidths <- apply(as.matrix(ptsx_i),1,function(x) (binwidths)[bins==x])
        ptsy_i <- ptsy_i/allbinwidths
        linesy_i <- linesy_i/allbinwidths
        scaled <- TRUE
      }
      if(scaled){
        anyscaled <- TRUE
        if(ylab=="Proportion") ylab <- "Proportion / bin width"
      }
    }

    # make plot
    plot(0,type="n",axes=FALSE,xlab="",ylab="",xlim=xrange_big,ylim=yrange_big,
         xaxs="i",yaxs=ifelse(bars,"i","r"))
    abline(h=0,col="grey") # grey line at 0
    if(linepos==2) lines(linesx_i,linesy_i,col=linescol,lwd=lwd,lty=lty) # lines first
    if(diff(range(size))!=0){ # if size input is provided then use bubble function
      bubble3(x=ptsx_i,y=ptsy_i,z=z_i,col=ptscol,cexZ1=cexZ1,legend.yadj=1.5,
              legend=bublegend,legendloc='topright',
              maxsize=maxsize,minnbubble=minnbubble,allopen=allopen,add=TRUE) # bubble plot
      # add optional lines showing (adjusted) input sample size
      if(linepos==0) effNline <- 0
      if(effNline>0 && length(effN)>0){
        effN_i         <- effN[yr==yr_i]
        effN_i_vec     <- unlist(lapply(split(effN_i,ptsy_i),unique))
        ptsy_i_vec     <- sort(unique(ptsy_i))
        lines(effNline*effN_i_vec,ptsy_i_vec,col='green3')
        if(!is.null(effNmean))
          lines(rep(effNline*effNmean,length(ptsy_i_vec)),ptsy_i_vec,col='green3',lty=2)
      }
      # add optional lines showing effective sample size 
      if(sampsizeline>0 && length(sampsize)>0){
        sampsize_i     <- sampsize[yr==yr_i]
        sampsize_i_vec <- unlist(lapply(split(sampsize_i,ptsy_i),unique))
        ptsy_i_vec     <- sort(unique(ptsy_i))

        lines(sampsizeline*sampsize_i_vec,ptsy_i_vec,col=2)
        if(!is.null(sampsizemean))
          lines(rep(sampsizeline*sampsizemean,length(ptsy_i_vec)),ptsy_i_vec,col=2,lty=3)
      }
    }else{
      if(FALSE){
        # turning off old way
        if(!bars) points(ptsx_i,ptsy_i,type=type,pch=pch,col=ptscol,cex=ptscex)	# points
        if( bars) points(ptsx_i,ptsy_i,type="o",lwd=barwidth,col=ptscol,lend=1)  # histogram-style bars
      }
      # new way
      if(!doSD) polygon(c(ptsx_i[1],ptsx_i,tail(ptsx_i,1)),c(0,ptsy_i,0),col='grey80')  # polygon
      points(ptsx_i,ptsy_i,type=type,lwd=1,pch=16,cex=0.7,col=ptscol)  # lines with solid points on top

      # adding uncertainty for mean length or weight at age plots
      if(doSD){
        old_warn <- options()$warn      # previous setting
        options(warn=-1)                # turn off "zero-length arrow" warning
        arrows(x0=ptsx_i,y0=qnorm(p=0.05,mean=ptsy_i,sd=ptsSD_i),
               x1=ptsx_i,y1=qnorm(p=0.95,mean=ptsy_i,sd=ptsSD_i),
               length=0.01, angle=90, code=3, col=ptscol)
        options(warn=old_warn)  #returning to old value
      }
    }
    if(linepos==1) lines(linesx_i,linesy_i,col=linescol,lwd=lwd,lty=lty)

    # add legends
    usr <- par("usr")
    for(i in 1:nlegends){
      text_i <- ""
      legtext_i <- legtext[[i]] # grab element of list
      # elements of list can be "default" to make equal to yr
      # or vector of length 1, npanels, or the full length of the input vectors
      if(length(legtext_i)==1){
        if(legtext_i=="yr"){ text_i <- yr_i }	 # values in "yr" input
        if(legtext_i=="sampsize" & showsampsize){	      # sample sizes
          vals <- unique(sampsize[yr==yr_i])
          if(length(vals)>1){
            print(paste("Warning: sampsize values are not all equal--choosing the first value:",vals[1]),quote=FALSE)
            print(paste("  yr=",yr_i,", and all sampsize values:",paste(vals,collapse=","),sep=""),quote=FALSE)
            vals <- vals[1]
          }
          text_i <- paste("N=",round(vals,sampsizeround),sep="")
        }
        if(legtext_i=="effN" & showeffN){				      # effective sample sizes
          vals <- unique(effN[yr==yr_i])
          if(length(vals)>1){
            print(paste("Warning: effN values are not all equal--choosing the first value:",vals[1]),quote=FALSE)
            print(paste("  all effN values:",paste(vals,collapse=",")),quote=FALSE)
            vals <- vals[1]
          }
          text_i <- paste("effN=",round(vals,sampsizeround),sep="")
        }
      }
      #if(length(legtext_i)==npanels) text_i <- legtext_i[ipanel]      # one input value per panel
      if(length(legtext_i)==nvals)   text_i <- legtext_i[yr==yr_i][1] # one input value per element
      if(length(legtext_i)==1)	     text_i <- text_i		      # yr, sampsize, or effN

      if(legx[1]=="default"){
        # default is left side for first plot, right thereafter
        textx <- ifelse(i==1, usr[1], usr[2])
      }else{ textx <- legx[i] }
      if(legy[1]=="default"){
        texty <- usr[4]		# default is top for all plots
      }else{ texty <- legy[i] }
      if(legadjx[1]=="default"){
        adjx <- ifelse(i==1, -.1, 1.0) # default is left side for first legend, right thereafter
      }else{ adjx <- legadjx[i] }
      if(legadjy[1]=="default"){
        adjy <- ifelse(i<3, 1.3, 1.3 + 1.3*(i-2))  # default is top for first 2 legends, below thereafter
      }else{ adjy <- legadjy[i] }

      # add legend text
      text(x=textx,y=texty,labels=text_i,adj=c(adjx,adjy),cex=legsize[i],font=legfont[i])
    }

    # add axes in left and lower outer margins
    mfg <- par("mfg")
    if(mfg[1]==mfg[3] | ipanel==npanels) axis(side=1,at=axis1) # axis on bottom panels and final panel
    if(mfg[2]==1) axis(side=2,at=axis2,las=horiz_lab)	   # axis on left side panels
    box()
    
    if(npanels==1 | ipanel %% (nrows*ncols) == 1){ # if this is the first panel of a given page
      # add title after plotting first panel on each page of panels
      fixcex = 1 # fixcex compensates for automatic adjustment caused by par(mfcol)
      if(max(nrows,ncols)==2) fixcex = 1/0.83
      if(max(nrows,ncols)>2) fixcex = 1/0.66
      if(npanels>1){
        title(main=main, line=c(2,0,3,3), outer=TRUE, cex.main=cex.main*fixcex)
        title(xlab=xlab, outer=TRUE, cex.lab=fixcex)
        title(ylab=ylab, line=ifelse(horiz_lab,max(3,2+.4*maxchar),3.5), outer=TRUE, cex.lab=fixcex)
      }else{
        title(main=main, xlab=xlab, ylab=ylab, outer=TRUE,cex.main=cex.main)
      }
    }
  }
  # restore default single panel settings
  par(mfcol=c(rows,cols),mar=c(5,4,4,2)+.1,oma=rep(0,4))

  if(anyscaled) cat("Note: compositions have been rescaled by dividing by binwidth\n")
  # return information on what was plotted
  return(list(npages=npages, npanels=npanels, ipage=ipage))
} # end embedded function: make_multifig
