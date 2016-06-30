#' Plot likelihood profile results
#' 
#' Makes a plot of change in negative-log-likelihood for each likelihood
#' component that contributes more than some minimum fraction of change in
#' total.
#' 
#' 
#' @param summaryoutput List created by the function \code{\link{SSsummarize}}.
#' @param plot Plot to active plot device?
#' @param print Print to PNG files?
#' @param models Optional subset of the models described in
#' \code{summaryoutput}.  Either "all" or a vector of numbers indicating
#' columns in summary tables.
#' @param profile.string Character string used to find parameter over which the
#' profile was conducted. Needs to match substring of one of the SS parameter
#' labels found in the Report.sso file. For instance, the default input 'steep'
#' matches the parameter 'SR_BH_steep'.
#' @param profile.label Label for x-axis describing the parameter over which
#' the profile was conducted.
#' @param ylab Label for y-axis. Default is "Change in -log-likelihood".
#' @param components Vector of likelihood components that may be included in
#' plot. List is further refined by any components that are not present in
#' model or have little change over range of profile (based on limit
#' \code{minfraction}). Hopefully this doesn't need to be changed.
#' @param component.labels Vector of labels for use in the legend that matches
#' the vector in \code{components}.
#' @param minfraction Minimum change in likelihood (over range considered) as a
#' fraction of change in total likelihood for a component to be included in the
#' figure.
#' @param sort.by.max.change Switch giving option to sort components in legend
#' in order of maximum amount of change in likelihood (over range considered).
#' Default=TRUE.
#' @param col Optional vector of colors for each line.
#' @param pch Optional vector of plot characters for the points.
#' @param lty Line total for the liklihood components.
#' @param lty.total Line type for the total likelihood.
#' @param lwd Line width for the liklihood components.
#' @param lwd.total Line width for the total likelihood.
#' @param cex Character expansion for the points representing the likelihood
#' components.
#' @param cex.total Character expansion for the points representing the total
#' likelihood.
#' @param xlim Range for x-axis. Change in likelihood is calculated relative to
#' values within this range.
#' @param ymax Maximum y-value. Default is 10\% greater than largest value
#' plotted.
#' @param xaxs The style of axis interval calculation to be used for the x-axis
#' (see ?par for more info)
#' @param yaxs The style of axis interval calculation to be used for the y-axis
#' (see ?par for more info).
#' @param type Line type (see ?plot for more info).
#' @param legend Include legend?
#' @param legendloc Location of legend (see ?legend for more info).
#' @param pwidth Width of plot
#' @param pheight Height of plot
#' @param punits Units for PNG file
#' @param res Resolution for PNG file
#' @param ptsize Point size for PNG file
#' @param cex.main Character expansion for plot titles
#' @param plotdir Directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param verbose Return updates of function progress to the R GUI? (Doesn't do
#' anything yet.)
#' @param \dots Additional arguments passed to the \code{plot} command.
#' @note Someday the function \code{\link{SS_profile}} will be improved and
#' made to work directly with this plotting function, but they don't yet work
#' well together. Thus, even if \code{\link{SS_profile}} is used, the output
#' should be read using \code{\link{SSgetoutput}} or by multiple calls to
#' \code{\link{SS_output}}.
#' @author Ian Taylor, Ian Stewart
#' @export
#' @seealso \code{\link{SSsummarize}}, \code{\link{SS_profile}},
#' \code{\link{SS_output}}, \code{\link{SSgetoutput}}
SSplotProfile <-
  function(summaryoutput,
           plot=TRUE,print=FALSE,
           models="all",
           profile.string="steep",
           profile.label="Spawner-recruit steepness (h)",
           ylab="Change in -log-likelihood",
           components=
           c("TOTAL",
             "Catch",
             "Equil_catch",
             "Survey",
             "Discard",
             "Mean_body_wt",
             "Length_comp",
             "Age_comp",
             "Size_at_age",
             "SizeFreq",
             "Morphcomp",
             "Tag_comp",
             "Tag_negbin",
             "Recruitment",
             "Forecast_Recruitment",
             "Parm_priors",
             "Parm_softbounds",
             "Parm_devs",
             "F_Ballpark",
             "Crash_Pen"),
           component.labels=
           c("Total",
             "Catch",
             "Equilibrium catch",
             "Index data",
             "Discard",
             "Mean body weight",
             "Length data",
             "Age data",
             "Size-at-age data",
             "Generalized size data",
             "Morph composition data",
             "Tag recapture distribution",
             "Tag recapture total",
             "Recruitment",
             "Forecast recruitment",
             "Priors",
             "Soft bounds",
             "Parameter deviations",
             "F Ballpark",
             "Crash penalty"),
           minfraction=0.01,
           sort.by.max.change=TRUE,
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
           verbose=TRUE,...)
{
  # subfunction to write png files
  pngfun <- function(file){
    png(filename=paste(plotdir,file,sep="/"),width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
  }
  
  if(print & is.null(plotdir)) stop("to print PNG files, you must supply a directory as 'plotdir'")

  if(length(components) != length(component.labels))
    stop("Inputs 'components' and 'component.labels' should have equal length")

  # get stuff from summary output
  n             <- summaryoutput$n
  likelihoods   <- summaryoutput$likelihoods
  if (is.null(likelihoods)) {
    stop("Input 'summaryoutput' needs to be a list output from SSsummarize\n",
         "and have an element named 'likelihoods'.")
  }
  pars          <- summaryoutput$pars

  # check number of models to be plotted
  if(models[1]=="all"){
    models <- 1:n
  }else{
    if(!all(models %in% 1:n))
      stop("Input 'models' should be a vector of values from 1 to n=",n," (for your inputs).\n")
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
  # Fixed bug that crashes plot when only a subset of components are listed (Steve Teo)
  prof.table <- data.frame(t(likelihoods[likelihoods$Label %in% components,models])) 
  names(prof.table) <- likelihoods[likelihoods$Label %in% components,ncol(likelihoods)]
  component.labels.good <- rep("",ncol(prof.table))
  for(icol in 1:ncol(prof.table)){
    ilabel <- which(components==names(prof.table)[icol])
    #print(names(prof.table)[icol])
    #print(ilabel)
    #print(component.labels[ilabel])
    component.labels.good[icol] <- component.labels[ilabel]
  }
  
  # subtract minimum value from each likelihood component (over requested parameter range)
  subset <- parvec >= xlim[1] & parvec <= xlim[2]
  for(icol in 1:ncol(prof.table)){
    prof.table[,icol] <- prof.table[,icol] - min(prof.table[subset,icol])
  }
  if(ymax=="default") ymax <- 1.1*max(prof.table[subset,])
  ylim <- c(0,ymax)

  # remove columns that have change less than minfraction change relative to total
  column.max <- apply(prof.table[subset,],2,max)
  change.fraction <- column.max / column.max[1]
  include <- change.fraction >= minfraction
  
  cat("\nLikelihood components showing max change as fraction of total change.\n",
      "To change which components are included, change input 'minfraction'.\n\n",sep="")
  print(data.frame(frac_change=round(change.fraction,4),include=include,label=component.labels.good))
  component.labels.used <- component.labels.good[include]

  # reorder values
  prof.table <- prof.table[order(parvec),include]
  parvec <- parvec[order(parvec)]

  # reorder columns by largest change (if requested)
  change.fraction <- change.fraction[include]
  if(sort.by.max.change){
    neworder <- c(1,1+order(change.fraction[-1],decreasing=TRUE))
    prof.table <- prof.table[,neworder]
    component.labels.used <- component.labels.used[neworder]
  }
    
  nlines <- ncol(prof.table)
  if(col[1]=="default") col <- rich.colors.short(nlines)
  if(pch[1]=="default") pch <- 1:nlines
  lwd <- c(lwd.total,rep(lwd,nlines-1))
  cex <- c(cex.total,rep(cex,nlines-1))
  lty <- c(lty.total,rep(lty,nlines-1))
  #return(prof.table)
  
  # make plot
  plotprofile <- function(){
    plot(0,type='n',xlim=xlim,ylim=ylim,xlab=profile.label, ylab=ylab,
         yaxs=yaxs,xaxs=xaxs,...)
    abline(h=0,col='grey')
    matplot(parvec, prof.table, type=type,
            pch=pch, col=col,
            cex=cex, lty=lty, lwd=lwd, add=T)
    if(legend)
      legend(legendloc,bty='n',legend=component.labels.used,
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
