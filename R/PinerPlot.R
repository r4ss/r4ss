##' Make plot of likelihood contributions by fleet
##'
##' This style of plot was officially named a "Piner Plot" at the
##' CAPAM Selectivity Workshop, La Jolla March 2013. This is in
##' honor of Kevin Piner's contributions to interpreting likelihood
##' profiles. He's surely not the first person to make such a plot
##' but the name seems to have stuck.
##' @param summaryoutput List created by the function
##' \code{\link{SSsummarize}}.
##' @param plot Plot to active plot device?
##' @param print Print to PNG files?
##' @param component Which likelihood component to plot. Default is "Length_like".
##' @param main Title for plot. Should match component.
##' @param models Optional subset of the models described in
##' \code{summaryoutput}.  Either "all" or a vector of numbers indicating
##' columns in summary tables.
##' @param fleets Optional vector of fleet numbers to include.
##' @param fleetnames Optional character vector of names for each fleet.
##' @param profile.string Character string used to find parameter over which the
##' profile was conducted. Needs to match substring of one of the SS parameter
##' labels found in the Report.sso file. For instance, the default input 'steep'
##' matches the parameter 'SR_BH_steep'.
##' @param profile.label Label for x-axis describing the parameter over which
##' the profile was conducted.
##' @param ylab Label for y-axis. Default is "Change in -log-likelihood".
##' @param col Optional vector of colors for each line.
##' @param pch Optional vector of plot characters for the points.
##' @param lty Line total for the liklihood components.
##' @param lty.total Line type for the total likelihood.
##' @param lwd Line width for the liklihood components.
##' @param lwd.total Line width for the total likelihood.
##' @param cex Character expansion for the points representing the likelihood
##' components.
##' @param cex.total Character expansion for the points representing the total
##' likelihood.
##' @param xlim Range for x-axis. Change in likelihood is calculated relative to
##' values within this range.
##' @param ymax Maximum y-value. Default is 10\% greater than largest value
##' plotted.
##' @param xaxs The style of axis interval calculation to be used for the x-axis
##' (see ?par for more info)
##' @param yaxs The style of axis interval calculation to be used for the y-axis
##' (see ?par for more info).
##' @param type Line type (see ?plot for more info).
##' @param legend Include legend?
##' @param legendloc Location of legend (see ?legend for more info).
##' @param pwidth Width of plot
##' @param pheight Height of plot
##' @param punits Units for PNG file
##' @param res Resolution for PNG file
##' @param ptsize Point size for PNG file
##' @param cex.main Character expansion for plot titles
##' @param plotdir Directory where PNG files will be written. by default it will
##' be the directory where the model was run.
##' @param add_cutoff Add dashed line at ~1.92 to indicate 95% confidence interval
##' based on common cutoff of half of chi-squared of p=.95 with 1 degree of
##' freedom: \code{0.5*qchisq(p=cutoff_prob, df=1)}. The probability value
##' can be adjusted using the \code{cutoff_prob} below.
##' @param cutoff_prob Probability associated with \code{add_cutoff} above.
##' @param verbose Return updates of function progress to the R GUI? (Doesn't do
##' anything yet.)
##' @param fleetgroups Optional character vector, with length equal to
##' the number of declared fleets, where fleets with the same value are
##' aggregated
##' @param likelihood_type choice of "raw" or "raw_times_lambda" (the default)
##' determines whether or not likelihoods plotted are adjusted by lambdas
##' (likelihood weights)
##' @param minfraction Minimum change in likelihood (over range considered) as a
##' fraction of change in total likelihood for a component to be included in the
##' figure.
##' @references Kevin Piner says that he's not the originator of this idea so
##' Athol Whitten is going to add a reference here.
##' @author Ian Taylor, Kevin Piner, Jim Thorson
##' @export
PinerPlot <-
  function(summaryoutput,
           plot=TRUE,print=FALSE,
           component="Length_like",
           main="Changes in length-composition likelihoods by fleet",
           models="all",
           fleets="all",
           fleetnames="default",
           profile.string="R0",
           profile.label=expression(log(italic(R)[0])),
           ylab="Change in -log-likelihood",
           col="default",
           pch="default",
           lty=1, lty.total=1,
           lwd=2, lwd.total=3,
           cex=1, cex.total=1.5,
           xlim="default",
           ymax="default",
           xaxs="r", yaxs="r",
           type="o",
           legend=TRUE, legendloc="topright",
           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1,
           plotdir=NULL,
           add_cutoff=FALSE,
           cutoff_prob = 0.95,
           verbose=TRUE,
           fleetgroups=NULL,
           likelihood_type="raw_times_lambda",
           minfraction=0.01)
{
  # this function is very similar to SSplotProfile, but shows fleet-specific likelihoods
  # for a single components rather than multiple components aggregated across fleets

  # subfunction to write png files
  pngfun <- function(file){
    png(filename=file.path(plotdir,file),width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
  }

  if(print & is.null(plotdir)) stop("to print PNG files, you must supply a directory as 'plotdir'")

  # get stuff from summary output into shorter variable names
  n    <- summaryoutput$n
  lbf  <- summaryoutput$likelihoods_by_fleet
  lbtg <- summaryoutput$likelihoods_by_tag_group

  if (is.null(lbf)) {
    stop("Input 'summaryoutput' needs to be a list output from SSsummarize\n",
         "and have an element named 'likelihoods_by_fleet'.")
  }
  # count of fleets
  nfleets <- ncol(lbf)-3
  pars <- summaryoutput$pars
  # names of fleets
  FleetNames     <- summaryoutput$FleetNames[[1]]
  # stop if lengths don't match
  if(length(FleetNames)!=nfleets){
    stop("problem with FleetNames: length!= ",nfleets,"\n",
         paste(FleetNames,collapse="\n"))
  }
  # stop if component input isn't found in table
  component_options <- c(unique(lbf$Label[-grep("_lambda", lbf$Label)]),
                         unique(lbtg$Label[-grep("_lambda", lbtg$Label)]))
  if(!component %in% component_options){
    stop("input 'component' needs to be one of the following\n",
         paste("    ", component_options, "\n"))
  }


  if(fleetnames[1]=="default") fleetnames <- FleetNames # note lower-case value is the one used below (either equal to vector from replist, or input by user)

  # check number of models to be plotted
  if(models[1]=="all"){
    models <- 1:n
  }else{
    if(!all(models %in% 1:n))
      stop("Input 'models' should be a vector of values from 1 to n=",n," (for your inputs).\n")
  }
  # check number of fleets to be plotted
  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  }else{
    if(!all(fleets %in% 1:nfleets))
      stop("Input 'fleets' should be a vector of values from 1 to nfleets=",nfleets," (for your inputs).\n")
  }

  # find the parameter that the profile was over
  parnumber <- grep(profile.string,pars$Label)
  if(length(parnumber)<=0) stop("No parameters matching profile.string='",profile.string,"'",sep="")
  parlabel <- pars$Label[parnumber]
  if(length(parlabel) > 1){
    stop("Multiple parameters matching profile.string='",profile.string,"': ",paste(parlabel,collapse=", "),sep="")
  }
  parvec <- as.numeric(pars[pars$Label==parlabel,models])
  cat("Parameter matching profile.string='",profile.string,"': '",parlabel,"'\n",sep="")
  cat("Parameter values (after subsetting based on input 'models'):\n")
  print(parvec)
  if(xlim[1]=="default") xlim <- range(parvec)

  # rearange likelihoods to be in columns by type
  if(likelihood_type=="raw") prof.table <- lbf[which(lbf$model %in% models & lbf$Label==component), ]
  if(likelihood_type=="raw_times_lambda"){
    prof.table <- lbf[which(lbf$model %in% models & lbf$Label==component), ]
    prof.table[,-c(1:3)] <- prof.table[,-c(1:3)] * lbf[which(lbf$model %in% models & lbf$Label==component)-1, ][,-c(1:3)]
  }

  # Aggregate by input fleetgroups (a character vector, where two fleets with the same value are aggregated)
  if( !is.null(fleetgroups) ){
    if( length(fleetgroups)!=nfleets ) stop("fleetgroups, if specified, must have length equal to the number of declared fleets")
    FleetNames <- unique(fleetgroups)
    prof.table_new <- data.frame( matrix(nrow=nrow(prof.table),
                                         ncol=3+length(unique(fleetgroups)),
                                         dimnames=list(rownames(prof.table),
                                             c(colnames(prof.table)[1:3],
                                               unique(fleetgroups)))) )
    prof.table_new[,1:3] <- prof.table[,1:3]
    for(rowI in 1:nrow(prof.table)){
      prof.table_new[rowI,-c(1:3)] <- tapply( as.numeric(prof.table[rowI,-c(1:3)]),
                                             FUN=sum,
                                             INDEX=as.numeric(factor(fleetgroups,
                                                 levels=unique(fleetgroups))))
    }
    prof.table <- prof.table_new
    nfleets <- ncol(prof.table)-3
  }
  # subtract minimum value from each likelihood component (over requested parameter range)
  subset <- parvec >= xlim[1] & parvec <= xlim[2]
  for(icol in 3:ncol(prof.table)){
    prof.table[,icol] <- prof.table[,icol] -
      min(prof.table[subset,icol], na.rm=TRUE)
  }

  # remove columns that have change less than minfraction change relative to total
  column.max <- apply(prof.table[,-c(1:3)],2,max, na.rm=TRUE)
  change.fraction <- column.max / max(prof.table[,3], na.rm=TRUE)
  include <- change.fraction >= minfraction
  cat("\nFleets-specific likelihoods showing max change as fraction of total change.\n",
     "To change which components are included, change input 'minfraction'.\n\n",sep="")
  print(data.frame(frac_change=round(change.fraction,4),include=include))

  # subset values and reorder values
  # Note: first 3 columns are "model", "Label", and "ALL", and
  # are excluded from subsetting process
  # a future option to exclude the "ALL" column is possible if requested
  prof.table <- prof.table[order(parvec),]
  prof.table <- prof.table[,c(1:3,3+intersect((1:nfleets)[fleets],
                                              (1:nfleets)[include]))]
  nfleets <- ncol(prof.table)-3
  # replace column names with fleetnames unless "fleetgroup" is used
  if(is.null(fleetgroups)){
    for(icol in 4:ncol(prof.table)){
      if(names(prof.table)[icol] %in% FleetNames){
        names(prof.table)[icol] <- fleetnames[which(FleetNames==names(prof.table)[icol])]
      }
      if(names(prof.table)[icol] %in% paste("X",FleetNames,sep="")){
        names(prof.table)[icol] <- fleetnames[which(paste("X",FleetNames,sep="")==names(prof.table)[icol])]
      }
    }
  }

  # set default y-limits
  if(ymax=="default") ymax <- 1.1*max(prof.table[subset,-(1:2)],na.rm=TRUE)
  ylim <- c(0,ymax)

  parvec <- parvec[order(parvec)]

  # default colors and plot characters
  nlines <- ncol(prof.table)-2
  if(col[1]=="default") col <- rich.colors.short(nlines)
  if(pch[1]=="default") pch <- 1:nlines
  lwd <- c(lwd.total,rep(lwd,nlines-1))
  cex <- c(cex.total,rep(cex,nlines-1))
  lty <- c(lty.total,rep(lty,nlines-1))
  #return(prof.table)

  # make plot
  plotprofile <- function(){
    plot(0,type='n',xlim=xlim,ylim=ylim,xlab=profile.label, ylab=ylab,
         yaxs=yaxs,xaxs=xaxs,main=main)
    abline(h=0,col='grey')
    # optionally add horizontal line at ~1.92 (or other value depending
    # on chosen probability)
    if(add_cutoff){
      abline(h=0.5*qchisq(p=cutoff_prob, df=1), lty=2)
    }
    matplot(parvec, prof.table[,-(1:2)], type=type,
            pch=pch, col=col,
            cex=cex, lty=lty, lwd=lwd, add=TRUE)
    if(legend)
      legend(legendloc,bty='n',legend=names(prof.table)[-(1:2)],
             lwd=lwd, pt.cex=cex, lty=lty, pch=pch, col=col)
    box()
  }

  if(plot){
    plotprofile()
  }
  if(print){
    pngfun("profile_plot_likelihood.png")
    plotprofile()
    dev.off()
  }
  out <- data.frame(parvec=parvec,prof.table)
  names(out)[1] <- parlabel
  return(invisible(out))
}
