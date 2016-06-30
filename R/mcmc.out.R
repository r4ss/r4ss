#' Summarize, analyze and plot key MCMC output.
#'
#' Makes four panel plot showing trace plots, moving average, autocorrelations,
#' and densities for chosen parameters from MCMC output.
#'
#'
#' @param directory Directory where all results are located, one level above
#' directory for particular run.
#' @param run Directory with files from a particular run.
#' @param file File containing posterior samples for key parameters. This could
#' be written by the function \code{\link{SSgetMCMC}}.
#' @param namefile The (optional) file name of the dimension and names of
#' posteriors.
#' @param names Read in names file (T) or use generic naming (F).
#' @param headernames Use the names in the header of \code{file}?
#' @param numparams The number of parameters to analyze.
#' @param closeall By default close all open devices.
#' @param burn Optional burn-in value to apply on top of the option in the
#' starter file and \code{\link{SSgetMCMC}}.
#' @param thin Optional thinning value to apply on top of the option in the
#' starter file, in the \code{-mcsave} runtime command, and in
#' \code{\link{SSgetMCMC}}.
#' @param scatter Can add a scatter-plot of all params at end, default is none.
#' @param surface Add a surface plot of 2-way correlations.
#' @param surf1 The first parameter for the surface plot.
#' @param surf2 The second parameter for the surface plot.
#' @param stats Print stats if desired.
#' @param plots Show plots or not.
#' @param header Data file with header?
#' @param sep Separator for data file passed to the \code{read.table} function.
#' @param print Send to screen unless asked to print.
#' @param new Logical whether or not to open a new plot window before plotting
#' @param colNames Specific names of the \code{file} to extract and work with. \code{NULL} keeps all columns
#' @author Ian Stewart, Allan Hicks (modifications)
#' @export
#' @seealso \code{\link{mcmc.nuisance}}, \code{\link{SSgetMCMC}}
#' @examples
#'
#'   \dontrun{
#'       mcmc.df <- SSgetMCMC(dir="mcmcRun", writecsv=T,
#'                   keystrings = c("NatM", "R0", "steep", "Q_extraSD"),
#'                   nuisancestrings = c("Objective_function", "SPB_", "InitAge", "RecrDev"))
#'       mcmc.out("mcmcRun",run="",numparams=4,closeall=F)
#'
#'   #Or for more control
#'       par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
#'       mcmc.out("mcmcRun",run="",numparams=1,closeall=F,new=F,colNames=c("NatM_p_1_Fem_GP_1"))
#'       mtext("M (natural mortality)",side=2,outer=T,line=1.5,cex=1.1)
#'   }
mcmc.out <- function (
          directory="c:/mydirectory/",
          run="mymodel/",			# folder with ADMB run files
          file="keyposteriors.csv",		# the file name of the posteriors
          namefile="postplotnames.sso",		# the (optional) file name of the dimension and names of posteriors
          names=FALSE, 				# read in names file (T) or use generic naming (F)
          headernames=TRUE,                     # use the names in the header of 'file'
          numparams=1, 				# the number of parameters to analyze
          closeall=TRUE, 			# by default close all open devices
          burn=0, 				# can specify a burn in to remove
          thin=1,  				# can specify further thinning, default is none
          scatter=FALSE,			# can add a scatter-plot of all params at end, default is none
          surface=FALSE, 			# add a surface plot of 2-way correlations
          surf1=1,				# the first parameter for the surface plot
          surf2=2, 				# the second parameter for the surface plot
          stats=FALSE, 				# print stats if desired
          plots=TRUE, 				# show plots or not
          header=TRUE,				# data file with header?
          sep=",",				# sep for data file
          print=FALSE, 				# send to screen unless asked to print
          new=T,              # open a new window
          colNames=NULL       #specific column names to subset on
         )

  # sample call: mcmc.out(run="english_8.4\\",names=T,burn=10,thin=1,scatter=F,stats=T,plots=T)

##############################################################################################################
	# Purpose: To summarize,analyze and plot key MCMC output
	# Written: Ian Stewart, July 2003
     	# Arguments: See above
     	# Returns: Graphical devices containing plots and summaries,
     	#          parameter plots are stacked in the history, page up and down scrolls through them
     	# Notes: columns with fixed values will cause the diagnostic tests to crash
##############################################################################################################
{
  #### the following commands no longer needed since packages are required by r4ss
  ## require(coda) || stop("package coda is required")
  ## geterrmessage()
  ## require(gtools) || stop("package gtools is required")
  ## geterrmessage()

  # add section to set up for printing or display to screen (default)
  if(print==TRUE){}# not implemented

  if(closeall==TRUE) {						# see if the user asked to retain open graphics devices
		# useful to compare multiple runs
    ### Note: the following line has been commented out because it was identified
    ###       by Brian Ripley as "against CRAN policies".
    #rm(.SavedPlots,pos=1) 					# remove any plotting history
  }

  filename  <- file.path(directory,run,file)			# put directory,run and file names together for use
  # warning if file does not exist
  if(!file.exists(filename)){
    stop("file doesn't exist:\n",filename)   
  }
  
  mcmcdata <- read.table(filename, 				# make data table of whole file
                         header = header, 			# choice of header or not
                         sep = sep, 				# space delimited
                         fill = TRUE)				# fill empty cells to make a symmetrical array

  #### Naming section ####
  if(names == TRUE) {
    nameout  <- file.path(directory,run,namefile)		# put directory,run and file names together for use
    namedata <- read.table(nameout, 				# make data table of whole file
                           header = FALSE, 			# no headers
                           sep = "", 				# space delimited
                           colClasses="character", 		# don't convert to factors
                           fill = TRUE)				# fill empty cells to make a symmetrical array

    numparams <- as.numeric(namedata[1,1]) 			# get the dimension of the output

    # add names to the data table, only used in the scatterplot of all parameters
    for (j in 1:numparams) {					# loop over the moveparam columns
      names(mcmcdata)[j] <- namedata[(j+1),1]			# name each column
    }
  }

  if(!is.null(colNames)) {
    if(length(colNames)!=numparams) cat("numparams argument overidden by length of colNames argument\n")
    numparams <- length(colNames)
    mcmcdata <- mcmcdata[,colNames]
    if(length(colNames)==1) {
      mcmcdata <- data.frame(mcmcdata)
      names(mcmcdata) <- colNames
    }
  }

  ##### change to mcmc object for coda #####
  mcmcfirst <- mcmc(mcmcdata)					# make the mcmc object from the data table
  mcmctemp <- window(mcmcfirst,thin=thin,start=(1+burn))       # thin the chain  and remove burn in
  mcthinned  <- as.matrix(mcmctemp)        			# get rid of iteration labels
  mcmcobject <- mcmc(mcthinned)				# send back to mcmc object

  draws <- length(mcmcobject[,1]) 			# define the post thinning and burn in length of the chain

  ##### plotting section #####
  if(plots==TRUE) {					# have plots been activated by user
    if(new) dev.new(record=TRUE) 				# keep the window open for each parameter
    if(numparams==5||numparams==9||numparams==13||numparams==17) {	# plots a blank plot if 5,9,13, or 17 plots created
   		# this avoids the loss of plot n-1 in history (is this an R bug??)
      plot(0,0,
         xlab="",
         ylab="",
         frame.plot=FALSE,
         yaxt="n", 					# turn off this axis
         xaxt="n",
         type="n") 					# plot nothing
    }

    for(i in 1:numparams) { 				# loop over the number of parameters
      par(new=FALSE,					# make sure to use a new graphical window
          mfrow=c(2,2), 				# set up "cells" to graph into
          ann=TRUE) 					# annotate plots

      ##### Trace plot section #####
      traceplot(mcmcobject[,i], 			# trace plot of parameters
                smooth = TRUE) 				# add a smoothing line
      mtext("Value", 					# label for y-axis
            side=2,					# place it on left of the graph
            line=3, 					# set the distance above the graph
            font=1, 					# make the font regular
            cex=0.8) 					# scale the text size

      if(names | headernames) {
        mtext(names(mcmcdata)[i], 			# label for whole plotting page
               side=3,					# place it on left of the graph
               adj=0,					# left adjust the text
               line=2, 					# set the distance above the graph
               font=2, 					# make the font regular
               cex=1) 					# scale the text size
      }else{
         mtext(paste("param",i),	# label for whole plotting page
               side=3,					# place it on left of the graph
               adj=0,					# left adjust the text
               line=2, 					# set the distance above the graph
               font=2, 					# make the font regular
               cex=1) 					# scale the text size
      }

      #### Cumulative plot section ####
      lowest <- min(mcmcobject[,i]) 			# minimum value in chain
      highest <- max(mcmcobject[,i]) 			# maximum value in chain, for graphing
      plot(c(seq(1,draws,by=1)), 			# the x values
           c(lowest,rep(c(highest),(draws-1))),		# the y values
           xlab="Iterations",
           ylab="",
           yaxt="n", 					# turn off this axis
           type="n") 					# plot nothing

      if(!exists("running")){ # temporary turning off if function "running" is not present
        cat("skipping running average section because function 'running' is needed\n")
      }else{
        lines(running(mcmcobject[,i],
                      fun=median, 				# plot the mean
                      allow.fewer=TRUE,  			# begin calculating from the first point
                      width=draws))			# Averages the last - observations (all in this case)

        fun <- function(x,prob) quantile(x,probs=prob,names=FALSE) 	# the quantile to use in next 2 lines

        lines(running(mcmcobject[,i],
                      fun=fun, 				# function to use
                      prob=0.05,
                      allow.fewer=TRUE,  			# begin calculating from the first point
                      width=draws),
              col="GREY")
        lines(running(mcmcobject[,i],
                      fun=fun, 				# function to use
                      prob=0.95,
                      allow.fewer=TRUE,  			# begin calculating from the first point
                      width=draws),
              col="GREY")
      } # end temporary turning off
      #### Autocorrelation plot section ####
      par(ann=FALSE) 					# turn off default annotation
      autocorr.plot(mcmcobject[,i],
                    auto.layout=FALSE, 			# do not make a new graph sheet
                    lag.max=20,				# set the maximum lag
                    ask=FALSE)				# do not require prompt to move through plots
      mtext("Autocorrelation", 				# label for y-axis
            side=2,					# place it on left of the graph
            line=3, 					# set the distance above the graph
            font=1, 					# make the font regular
            cex=0.8) 					# scale the text size
      mtext("Lag", 					# label for y-axis
            side=1,					# place it on left of the graph
            line=3, 					# set the distance above the graph
            font=1, 					# make the font regular
            cex=0.8) 					# scale the text size
      lines(seq(1,20,by=1),rep(0.1,20),col="GREY") 	# plot the 0.1 line
      lines(seq(1,20,by=1),rep(-0.1,20),col="GREY")     # plot the -0.1 line

      ##### Density plot section #####
      densplot(mcmcobject[,i],
              show.obs=TRUE) 				# show the x axis
      mtext("Density", 					# label for y-axis
           side=2,					# place it on left of the graph
           line=3, 					# set the distance above the graph
           font=1, 					# make the font regular
           cex=0.8) 					# scale the text size
      mtext("Value", 					# label for y-axis
           side=1,					# place it on left of the graph
           line=3, 					# set the distance above the graph
           font=1, 					# make the font regular
           cex=0.8) 					# scale the text size
    } 						# end parameter loop
  } 					# end plotting loop

   #### Statistics section #####
    if(stats == TRUE)
     {
      dev.new()						# opens a new graphics device
      par(mar=c(0,0,3,0)) 				# makes the margins large
      plot(0,  						# plot a graph of a single point
           ylab="",					# label the y-axis for whole screen
           xlab="", 					# label the x-axis for whole screen
           type='n', 					# plot nothing in the graph
           xlim=c(0,25),
           ylim=c(0,25),
           main="Summary statistics for key parameters",# label the output
           axes = FALSE) 				# do not plot the axis

    # set up the titles
      text(0.001,						# the x-axis location
           25, 						# the y-axis location
           "Parameter", 				# what to write
             font=2, 					# bold font
             cex=0.9, 					# font size
             adj=0)
      text(4,						# the x-axis location
           25, 						# the y-axis location
           "Median (0.05-0.95)", 					# what to write
             font=2, 					# bold font
             cex=0.9, 					# font size
             adj=0)
      text(13,						# the x-axis location
           25, 						# the y-axis location
           "AC Lag 1", 					# what to write
             font=2, 					# bold font
             cex=0.9, 					# font size
             adj=0)
      text(16.5,						# the x-axis location
           25, 						# the y-axis location
           "Eff. N", 					# what to write
             font=2, 					# bold font
             cex=0.9, 					# font size
             adj=0)
      text(19,						# the x-axis location
           25, 						# the y-axis location
           "Geweke-Z", 					# what to write
             font=2, 					# bold font
             cex=0.9, 					# font size
             adj=0)
      text(22.5,					        # the x-axis location
           25, 						# the y-axis location
           "Heidel-W", 					# what to write
             font=2, 					# bold font
             cex=0.9, 					# font size
             adj=0)

   # loop over parameters and fill in the values
      for(i in 1:numparams)
       {
        text(0,						# the x-axis location
             (25-i), 					# the y-axis location
             paste("param",i), 				# what to write
             font=1, 					# normal font
             cex=0.9, 					# font size
             adj=0) 					# make the text left-aligned

       med <- quantile(mcmcobject[,i],probs=0.5,names=FALSE)
       range <- quantile(mcmcobject[,i],probs=c(0.05,0.95),names=FALSE)
       text(3.2,25-i,
            paste(signif(round(med,6),6),
            "(",
            paste(signif(round(range[1],6),6),
                  "-",
                  signif(round(range[2],6),6)),
            ")"),
            font=1,cex=0.9,adj=0)

       l1.ac <- acf(mcmcobject[,i],lag.max=1,type="correlation",plot=F)
       acoruse <- round(l1.ac$acf[2],6)
       text(13,25-i,acoruse,font=1,cex=0.9,adj=0)

       effsize<- effectiveSize(mcmcobject[,i])
       text(16.5,25-i,round(min(effsize,draws),0),font=1,cex=0.9,adj=0)

       if(acoruse > 0.4)
        {gewuse <- "None"}
       if(acoruse <= 0.4)
        {
         geweke <- geweke.diag(mcmcobject[,i],frac1=0.1,frac2=0.5)
         gewuse <- round(geweke$z,3)
        }
       text(19,25-i,gewuse,font=1,cex=0.9,adj=0)

       if(acoruse > 0.4)
        {send <- "None"}
       if(acoruse <= 0.4)
        {
         hw <- as.list(heidel.diag(mcmcobject[,i], pvalue=0.05))
         if(hw[1]==0){send <- "Failed"}
         if(hw[1]==1){send <- "Passed"}
        }
       text(22.5,25-i,send,font=1,cex=0.9,adj=0)
       } 						# close the parameter loop
     }			 				# end stats statement

   ##### Scatter plot section #####
    if(scatter == TRUE)
     {
      dev.new()
      par(xaxt="n",yaxt="n") 				# suppress the axis labels
      pairs(mcmcdata[1:numparams], 			# make the scatterplot
            cex=0.1,
            gap=0)
     }							# end the scatter loop

   ##### Surface plot section #####
    if(surface == TRUE)
     {
      dev.new()
      par(new=FALSE) 					# use a new window
      hist2d(mcmcobject[,surf1],			# x data as a vector
             mcmcobject[,surf2], 			# y data as a vector
             nbins=100,
             na.rm=TRUE,
             xlab=paste("parameter",surf1),
             ylab=paste("parameter",surf2),
             show=TRUE, 				# make figure or not
             col=c("GREY", topo.colors(20)))
     } 							# close surface plot loop

 }							# end function
