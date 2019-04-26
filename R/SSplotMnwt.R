#' Plot mean weight data and fits.
#'
#' Plot mean weight data and fits from Stock Synthesis output. Intervals are
#' based on T-distributions as specified in model.
#'
#'
#' @param replist list created by \code{SS_output}
#' @param ymax Optional input to override default ymax value.
#' @param subplots Vector of which plots to make (1 = data only, 2 = with fit).
#' If \code{plotdat = FALSE} then subplot 1 is not created, regardless of
#' choice of \code{subplots}.
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param fleets optional vector to subset fleets for which plots will be made
#' @param fleetnames optional replacement for fleenames used in data file
#' @param datplot Make data-only plot of discards? This can override the choice
#' of \code{subplots}.
#' @param labels vector of labels for plots (titles and axis labels)
#' @param col1 first color to use in plot (for expected values)
#' @param col2 second color to use in plot (for observations and intervals)
#' @param pwidth width of plot
#' @param pheight height of plot
#' @param punits units for PNG file
#' @param res resolution for PNG file
#' @param ptsize point size for PNG file
#' @param cex.main character expansion for plot titles
#' @param verbose report progress to R GUI?
#' @author Ian Taylor, Ian Stewart
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}}
SSplotMnwt <-
  function(replist, subplots=1:2, ymax=NULL,
           plot=TRUE, print=FALSE,
           fleets="all",
           fleetnames="default",
           datplot=FALSE,
           labels=c("Year",  #1
           "discard",        #2
           "retained catch", #3
           "whole catch",    #4
           "Mean individual body weight (kg)", #5
           "Mean weight in", #6
           "for"),     #7
           col1="blue", col2="black",
           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,
           cex.main=1,
           plotdir="default", verbose=TRUE)
{
  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  # get stuff from replist
  mnwgt         <- replist$mnwgt
  FleetNames    <- replist$FleetNames
  DF_mnwgt      <- replist$DF_mnwgt
  nfleets       <- replist$nfleets
  SS_versionshort <- replist$SS_versionshort


  if(fleets[1]=="all") fleets <- 1:nfleets
  if(fleetnames[1]=="default") fleetnames <- FleetNames
  if(plotdir=="default") plotdir <- replist$inputs$dir

  # mean body weight observations ###
  if(!is.na(mnwgt)[1]){
    for(ifleet in intersect(fleets,unique(mnwgt$Fleet))){
      # usemnwgt is subset of mnwgt for the particular fleet
      usemnwgt <- mnwgt[mnwgt$Fleet==ifleet & mnwgt$Obs>0,]
      if(SS_versionshort == "3.30"){
        usemnwgt$Part <- usemnwgt$Part
      } else {
        usemnwgt$Part <- usemnwgt$Mkt
      }
      FleetName <- fleetnames[ifleet]
      for(j in unique(usemnwgt$Part)){
        yr <- usemnwgt$Yr[usemnwgt$Part==j]
        ob <- usemnwgt$Obs[usemnwgt$Part==j]
        cv <- usemnwgt$CV[usemnwgt$Part==j]
        ex <- usemnwgt$Exp[usemnwgt$Part==j]
        xmin <- min(yr)-3
        xmax <- max(yr)+3
        liw <- -ob*cv*qt(0.025,DF_mnwgt) # quantile of t-distribution
        uiw <- ob*cv*qt(0.975,DF_mnwgt) # quantile of t-distribution
        liw[(ob-liw)<0] <- ob[(ob-liw)<0] # no negative limits
        if(is.null(ymax)){
          ymax <- max(ob + uiw)
          ymax <- max(ymax,ex)
        }
        titlepart <- labels[2]
        if(j==2) titlepart <- labels[3]
        if(j==0) titlepart <- labels[4]
        ptitle <- paste(labels[6],titlepart,labels[7],FleetName,sep=" ")
        ylab <- labels[5]

        # wrap up plot command in function
        bdywtfunc <- function(addfit){
          plotCI(x=yr,y=ob,uiw=uiw,liw=liw,xlab=labels[1],main=ptitle,
                 ylo=0,col=col2,sfrac=0.005,ylab=ylab,lty=1,pch=21,bg="white",
                 xlim=c(xmin,xmax),cex.main=cex.main,ymax=ymax)
          abline(h=0,col="grey")
          if(addfit) points(yr,ex,col=col1,cex=2,pch="-")
        }

        # make plots
        if(!datplot){
          subplots <- setdiff(subplots,1) # don't do subplot 1 if datplot=FALSE
        }
        for(isubplot in subplots){ # loop over subplots (data only or with fit)
          if(isubplot==1){
            addfit <- FALSE
          }else{
            addfit <- TRUE
          }
          if(plot){
            bdywtfunc(addfit=addfit)
          }
          if(print){
            if(!addfit){
              file <- paste0("bodywt_data_flt",FleetName,".png")
            }else{
              file <- paste0("bodywt_fit_flt",FleetName,".png")
            }
            caption <- ptitle
            plotinfo <- pngfun(file=file, caption=caption)
            bdywtfunc(addfit=addfit)
            dev.off()
          }
        } # end loop over subplots
      } # end loop over market categories
    } # end loop over fleets
  ##   if(verbose) cat("Finished mean body weight plot\n")
  ## }else{ # if mean weight data exists
  ##   if(verbose) cat("No mean body weight data to plot\n")
  }
  if(!is.null(plotinfo)) plotinfo$category <- "Mnwt"
  return(invisible(plotinfo))
}
