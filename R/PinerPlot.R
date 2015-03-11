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
##' @param verbose Return updates of function progress to the R GUI? (Doesn't do
##' anything yet.)
##' @param fleetgroups Optional character vector, with length equal to
##' the number of declared fleets, where fleets with the same value are
##' aggregated
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
           verbose=TRUE,
           fleetgroups=NULL)
{
  # this function is very similar to SSplotProfile, but shows fleet-specific likelihoods
  # for a single components rather than multiple components aggregated across fleets
  
  # subfunction to write png files
  pngfun <- function(file){
    png(filename=paste(plotdir,file,sep="/"),width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
  }
  
  if(print & is.null(plotdir)) stop("to print PNG files, you must supply a directory as 'plotdir'")

  # get stuff from summary output
  n    <- summaryoutput$n
  lbf  <- summaryoutput$likelihoods_by_fleet
  if (is.null(lbf)) {
    stop("Input 'summaryoutput' needs to be a list output from SSsummarize\n",
         "and have an element named 'likelihoods_by_fleet'.")
  }
  nfleets <- ncol(lbf)-3
  pars <- summaryoutput$pars
  FleetNames     <- summaryoutput$FleetNames[[1]]

  # Aggregate by input fleetgroups (a character vector, where two fleets with the same value are aggregated)
  if( !is.null(fleetgroups) ){
    if( length(fleetgroups)!=nfleets ) stop("fleetgroups, if specified, must have length equal to the number of declared fleets")
    FleetNames <- unique(fleetgroups)
    lbf_new <- data.frame( matrix(nrow=nrow(lbf),ncol=3+length(unique(fleetgroups)),dimnames=list(rownames(lbf),c(colnames(lbf)[1:3],unique(fleetgroups)))) )
    lbf_new[,1:3] <- lbf[,1:3]
    for(rowI in 1:nrow(lbf)) lbf_new[rowI,-c(1:3)] <- tapply( as.numeric(lbf[rowI,-c(1:3)]), FUN=sum, INDEX=as.numeric(factor(fleetgroups,levels=unique(fleetgroups))))
    lbf <- lbf_new
    nfleets <- ncol(lbf)-3
  }

  if(!component %in% lbf$Label) stop("input 'component' needs to be one of the following\n",
                                     paste("    ",unique(lbf$Label),"\n"))

  
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
  if(length(parlabel) > 1)
    stop("Multiple parameters matching profile.string='",profile.string,"': ",paste(parlabel,collapse=", "),sep="")

  parvec <- as.numeric(pars[pars$Label==parlabel,models])
  cat("Parameter matching profile.string='",profile.string,"': '",parlabel,"'\n",sep="")
  cat("Parameter values (after subsetting based on input 'models'):\n")
  print(parvec)
  if(xlim[1]=="default") xlim <- range(parvec)

  # rearange likelihoods to be in columns by type
  prof.table <- lbf[lbf$model %in% models & lbf$Label==component, ]

  # subtract minimum value from each likelihood component (over requested parameter range)
  subset <- parvec >= xlim[1] & parvec <= xlim[2]

  for(icol in 3:ncol(prof.table)){
    prof.table[,icol] <- prof.table[,icol] - min(prof.table[subset,icol])
  }
  
  # subset values and reorder values
  prof.table <- prof.table[order(parvec),]
  nfleets <- ncol(prof.table)-3
  prof.table <- prof.table[,c(1:3,3+(1:nfleets)[fleets])]
  for(icol in 3:ncol(prof.table)){
    if(names(prof.table)[icol] %in% FleetNames){
      names(prof.table)[icol] <- fleetnames[which(FleetNames==names(prof.table)[icol])]
    }
    if(names(prof.table)[icol] %in% paste("X",FleetNames,sep="")){
      names(prof.table)[icol] <- fleetnames[which(paste("X",FleetNames,sep="")==names(prof.table)[icol])]
    }
  }
  
  if(ymax=="default") ymax <- 1.1*max(prof.table[subset,-(1:2)],na.rm=TRUE)
  ylim <- c(0,ymax)
  
  parvec <- parvec[order(parvec)]
    
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
    matplot(parvec, prof.table[,-(1:2)], type=type,
            pch=pch, col=col,
            cex=cex, lty=lty, lwd=lwd, add=T)
    if(legend)
      legend(legendloc,bty='n',legend=names(prof.table)[-(1:2)],
             lwd=lwd, pt.cex=cex, lty=lty, pch=pch, col=col)
    box()
  }

  if(plot) plotprofile()
  if(print){
    pngfun("profile_plot_likelihood.png")
    plotprofile()
    dev.off()
  }
  out <- data.frame(parvec=parvec,prof.table)
  names(out)[1] <- parlabel
  return(invisible(out))
}
