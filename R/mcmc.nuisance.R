#' Summarize nuisance MCMC output
#' 
#' Summarize nuisance MCMC output (used in combination with
#' \code{\link{mcmc.out}} for key parameters).
#' 
#' 
#' @param directory Directory where all results are located, one level above
#' directory for particular run.
#' @param run Directory with files from a particular run.
#' @param file File containing posterior samples for nuisance parameters. This
#' could be posteriors.sso or something written by the function
#' \code{\link{SSgetMCMC}}.
#' @param file2 Optional second file containing posterior samples for nuisance
#' parameters. This could be derived_posteriors.sso.
#' @param bothfiles TRUE/FALSE indicator on whether to read \code{file2} in
#' addition to \code{file1}.
#' @param printstats Return all the statistics for a closer look.
#' @param burn Optional burn-in value to apply on top of the option in the
#' starter file and \code{\link{SSgetMCMC}}.
#' @param header Data file with header?
#' @param thin Optional thinning value to apply on top of the option in the
#' starter file, in the \code{mcsave} runtime command, and in
#' \code{\link{SSgetMCMC}}.
#' @param trace Plot trace for param # (to help sort out problem parameters).
#' @param labelstrings Vector of strings that partially match the labels of the
#' parameters you want to consider.
#' @param columnnumbers Vector of column numbers indicating the columns you
#' want to consider.
#' @param sep Separator for data file passed to the \code{read.table} function.
#' @author Ian Stewart
#' @export
#' @seealso \code{\link{mcmc.out}}, \code{\link{SSgetMCMC}}
#' @keywords data
mcmc.nuisance <- function (
          directory="c:/mydirectory/",    # directory to use
          run="mymodel/",		    # folder with ADMB run files
          file="posteriors.sso",	    # the file name of the posteriors
          file2="derived_posteriors.sso",   # the file name of the posteriors
          bothfiles=FALSE,                  # read and combine both file and file2                 
          printstats=FALSE, 		    # return all the statistics for a closer look
          burn=0, 			    # can specify a burn in to remove
          header=TRUE,			    # header on data file?
          thin=1,  			    # can specify further thinning, default is none
          trace=0,			    # plot trace for param # (to help sort out problem parameters)
          labelstrings="all",               # vector of strings that partially match the columns you want to consider
          columnnumbers="all",              # vector of column numbers indicating the columns you want to consider
          sep=""		            # sep for data file
         )
  # sample call:  mcmc.nuisance(run="flatfish_tagging\\",burn=0,thin=1,printstats=F,trace=0)

##############################################################################################################
    # Purpose: To summarize nuisance MCMC output (used in combination with mcmc.out() for key parameters)
    # Written: Ian Stewart, August 2003
    # Arguments: See above
    # Returns: Graphical devices containing summaries and plots
##############################################################################################################
{
  #### the following commands no longer needed since packages are required by r4ss
  ## require(coda) || stop("package coda is required")
  ## geterrmessage()

  filename   <- file.path(directory,run,file)			# put directory,run and file names together for use
  filename2  <- file.path(directory,run,file2)			# put directory,run and file names together for use

  # warning if file does not exist
  if(!file.exists(filename)){
    stop("file doesn't exist:\n",filename)   
  }

  mcmcdata <- read.table(filename, 				# make data table of whole file
                         header = header, 			# no headers 
                         sep = sep, 				# space delimited
                         fill = TRUE)				# fill empty cells to make a symmetrical array
  if(bothfiles){
    mcmcdata2 <- read.table(filename2, 				# make data table of whole file
                         header = header, 			# no headers 
                         sep = sep, 				# space delimited
                         fill = TRUE)				# fill empty cells to make a symmetrical array

    mcmcdata <- cbind(mcmcdata,mcmcdata2)
  }

  if(header & labelstrings[1]!="all"){
    labels <- NULL
    for(istring in 1:length(labelstrings)){
      labels <- c(labels,names(mcmcdata)[grep(labelstrings[istring],names(mcmcdata))])
    }
    cat("All labels matching the input 'labelstrings':\n")
    print(labels)
    mcmcdata <- mcmcdata[,names(mcmcdata)%in%labels]
  }
#print(head(mcmcdata))  
  ##### change to mcmc object for coda #####
   mcmcfirst <- mcmc(mcmcdata)					# make the mcmc object from the data table
   mcmctemp <- window(mcmcfirst,thin=thin,start=(1+burn))       # thin the chain  and remove burn in
   mcthinned  <- as.matrix(mcmctemp)        			# get rid of iteration labels
   mcmcobject <- mcmc(mcthinned)				# send back to mcmc object

  draws <- niter(mcmcobject) 					# define the post thinning and burn in length of the chain
  parameters <- nvar(mcmcobject)  
  # add robustification for fixed parameters
  vec <- seq(1,parameters,by=1)
  for(i in 1:parameters)
   {
    if(min(mcmcobject[,i]) == max(mcmcobject[,i]))
     {vec <- vec[vec != i]}
   }
  mcmcobject <- mcmcobject[,vec]
  parameters <- length(vec) 

  stats <- data.frame(cbind(rep(0,parameters),			# initialize the data.frame for results
                            rep(0,parameters),
                            rep(0,parameters),
                            rep(0,parameters)))	 		# add columns for each statistic	 
  names(stats)[1] <- "autocor" 					# name the first column
  names(stats)[2] <- "geweke" 					# name the second column
  names(stats)[3] <- "effn"
  names(stats)[4] <- "heidelwelsch"
  stats$Label <- names(mcmcdata) # add the header values if they exist
  
  hwsums <- as.vector(c(0,0,0)) 				# for use in plotting later on 

  for(i in 1:parameters)
   {
    ##### Autocorrelation #####
     acftemp <- acf(mcmcobject[,i],lag.max=1,type="correlation",plot=F) # calculate the AC at lag 1
     acoruse <- round(acftemp$acf[2],3) 		# extract the value and round it
     stats[i,1] <- acoruse 				# store it in the stats data.frame

    ##### Geweke statistic #####
     if(acoruse > 0.4) 
        {gewuse <- 3}
     if(acoruse <= 0.4)
        {
         geweke <- geweke.diag(mcmcobject[,i],frac1=0.1,frac2=0.5)
         gewuse <- round(geweke$z,3)
        }
     if(gewuse > 3)
        {gewuse <- 3}
     if(gewuse < -3)
        {gewuse <- -2.9}
     stats[i,2] <- gewuse

    ##### Effective sample size #####
     effsize <- effectiveSize(mcmcobject[,i])
     effnuse <- round(effsize,0)
     stats[i,3] <- min(effnuse,draws)

    ##### Heidelberger and Welch statistic #####
     if(acoruse > 0.4)
      {
       hwuse <- "No test" # no statistic
       hwsums[1] <- hwsums[1] + 1
      }	
     if(acoruse <= 0.4)
      {
       hw <- as.list(heidel.diag(mcmcobject[,i], pvalue=0.05))
       if(hw[1]==0)
        {
         hwuse <- "Failed" # failed
         hwsums[2] <- hwsums[2] + 1
        }
       if(hw[1]==1)
        {
         hwuse <- "Passed"# passed
         hwsums[3] <- hwsums[3] + 1
        } 
      }
     stats[i,4] <- hwuse
   } 								# end parameter loop

  ##### plotting section #####
  par(new=FALSE,						# Use same graphical window
      mfrow=c(2,2)) 						# set up "cells" to graph into

  hist(stats$autocor,
       main="",col = "GREY",
       breaks=c(seq(-1,1,by=0.1)),
       xlim=c(-1,1),
       xlab="Autocorrelation")
  mtext("Summary of nuisance parameters", 			# label for whole plotting page
               side=3,						# place it on left of the graph
               adj=0,						# left adjust the text
               line=2, 						# set the distance above the graph
               font=2, 						# make the font bold
               cex=1.5) 					# scale the text size

  hist(stats$effn,main="",ylab="",xlab="Effective sample size",
       breaks=c(seq(0,draws,by=(draws/10))),
       xlim=c(0,draws),
       col = "GREY",)
  hist(stats$geweke,main="",xlab="Geweke statistic",
       breaks=c(seq(-5,5,by=0.25)),
       xlim=c(-3,3),
       right=TRUE,
       col = "GREY",)

  barplot(hwsums,
          space = 0,
          ylab="",
          col = "GREY",
          xlab="Heidelberger and Welch statistic",
          names.arg = c("No test","Failed","Passed"))

  ##### Trace section #####
  if(trace > 0)
   {
    dev.new()
    par(new=FALSE)
    traceplot(mcmcobject[,trace],                        	 # trace plot of parameters
              smooth = TRUE)                         		 # add a smoothing line
    mtext("Value",                                    # label for y-axis
          side=2,                                     # place it on left of the graph
          line=3,                                     # set the distance above the graph
          font=1,                                     # make the font regular
          cex=0.8)                                    # scale the text size
    
    mtext(paste("param",trace,labels[trace]),      # label for whole plotting page
          side=3,                                  # place it on left of the graph
          adj=0,                                   # left adjust the text
          line=2,                                  # set the distance above the graph
          font=2,                                  # make the font regular
          cex=1)                                   # scale the text size
   }

  ##### Statistics section #####
  if (printstats == TRUE)
   {
    return(stats)
   }
 } 									# end function
