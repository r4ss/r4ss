#' Plot spawner-recruit curve.
#' 
#' Plot spawner-recruit curve based on output from Stock Synthesis model.
#' 
#' 
#' @param replist list created by \code{SS_output}
#' @param subplot vector of which subplots to show.  1=plot without labels,
#' 2=plot with year labels.
#' @param add add to existing plot?
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param xlim optional control of x range
#' @param ylim optional control of y range
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param bioscale multiplier on spawning biomass, set to 0.5 for single-sex
#' models
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param pwidth width of plot
#' @param pheight height of plot
#' @param punits units for PNG file
#' @param res resolution for PNG file
#' @param ptsize point size for PNG file
#' @param cex.main character expansion for plot titles
#' @param verbose report progress to R GUI?
#' @param colvec vector of length 4 with colors for 3 lines and 1 set of points
#' @param legend add a legend to the figure?
#' @param legendloc location of legend. See ?legend for more info
#' @param minyr minimum year of recruitment deviation to show in plot
#' @param textmindev minimum recruitment deviation for label to be added so
#' only extreme devs are labeled (labels are added to first and last years as
#' well).  Default=0.7.
#' @param virg add point for equilibrium conditions (x=B0,y=R0)
#' @param init add point for initial conditions (x=B1,y=R1), only appears
#' if this point differs from virgin values
#' @param forecast include forecast years in the curve?
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}}
SSplotSpawnrecruit <-
  function(replist,subplot=1:2,add=FALSE,plot=TRUE,print=FALSE,
           xlim=NULL,ylim=NULL,
           xlab="Spawning biomass (mt)",
           ylab="Recruitment (1,000s)",
           bioscale="default",
           plotdir="default",
           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1,
           verbose=TRUE,colvec=c("blue","green3","black","red"),
           legend=TRUE, legendloc="topleft",
           #line1="blue",line2="green3",line3="black",ptcol="red",
           minyr="default",textmindev=0.5,
           virg=TRUE,init=TRUE,forecast=FALSE)
{
  # plot of spawner recruit curve

  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  recruit <- replist$recruit
  nsexes <- replist$nsexes
  
  #scaling factor for single sex models
  if(bioscale=="default"){
    if(nsexes==1) bioscale <- 0.5 else bioscale <- 1
  }
  

  if(plotdir=="default") plotdir <- replist$inputs$dir
  if(minyr=="default") minyr <- min(recruit$Yr)

  recruit <- recruit[recruit$era %in% c("Early","Main","Fixed","Late",
                                        ifelse(forecast,"Forecast",NA)) &
                     recruit$Yr>=minyr,]
  
  timeseries <- replist$timeseries
  recruit$spawn_bio <- bioscale*recruit$SpawnBio
  timeseries$SpawnBio <- bioscale*timeseries$SpawnBio
  
  if(is.null(ylim)) ylim=c(0, max(recruit$pred_recr,
                        recruit$exp_recr, recruit$bias_adjusted))
  x <- recruit$spawn_bio
  if(is.null(xlim)) xlim=c(0, max(x))

  # only add lines for environmentally dependent recruitment if it differs
  # from expected recruitment without environmental link
  show_env <- any(recruit$with_env!=recruit$exp_recr)
  # store virgin and initial values
  B0 <- sum(timeseries$SpawnBio[timeseries$Era=="VIRG"], na.rm=TRUE)
  B1 <- sum(timeseries$SpawnBio[timeseries$Era=="INIT"], na.rm=TRUE)
  R0 <- sum(timeseries$Recruit_0[timeseries$Era=="VIRG"], na.rm=TRUE)
  R1 <- sum(timeseries$Recruit_0[timeseries$Era=="INIT"], na.rm=TRUE)
  if(B0==B1 & R0==R1){
    init <- FALSE
  }

  # prepare for legend
  if(legend){
    legend_entries <- c(TRUE,     # expected
                        show_env, # with environmental link
                        TRUE,     # bias adjusted
                        TRUE,     # estimated points
                        virg,     # virgin
                        init)     # initial equilibrium
    legend_col <- colvec[c(3,1,2,4,3,3)][legend_entries]
    legend_lwd <- c(2,  1,  1,  NA, NA,  NA)[legend_entries]
    legend_pch <- c(NA, NA, NA, 1,  3,   4)[legend_entries]
    legend_cex <- c(1,  1,  1,  1,  1.5, 1.5)[legend_entries]
    legend_lab <- c("Exp. recruitment",
                    "Exp. recruitment with env. link",
                    "Exp. recruitment after bias adj.",
                    "Estimated recruitments",
                    "Unfished equilibrium",
                    "Initial equilibrium")[legend_entries]
  }
  
  StockRecruitCurve.fn <- function(text=FALSE){
    ### a function to make the plots
    if(!add){
      # make empty plot (if not adding to existing plot)
      plot(0,type='n',ylim=ylim,xlim=xlim,xlab=xlab,ylab=ylab)
    }
    # add lines at 0 in both dimensions
    abline(h=0,col="grey")
    abline(v=0,col="grey")
    if(show_env){
      # add line for expected recruitment with environmental variability
      lines(x[order(x)],recruit$with_env[order(x)],lwd=1, col=colvec[1])
    }
    # add line for expected recruitment
    lines(x[order(x)],  recruit$exp_recr[order(x)],lwd=2, col=colvec[3])
    # add line for adjusted recruitment
    lines(x,            recruit$bias_adjusted,          lwd=1, col=colvec[2])
    # add points for individual estimates
    points(x,recruit$pred_recr,col=colvec[4])
    if(text){
      # add text, but only label values with larger devs (in abs value)
      show <- abs(recruit$dev) > textmindev
      show[1] <- show[length(show)] <- TRUE  # also include first & last years
      text(x[show],recruit$pred_recr[show],labels=recruit$Yr[show], pos=2, cex=.7)
    }
    # add point for virgin biomass/recruitment (if requested)
    if(virg){
      points(B0, R0, pch=3, cex=1.5)
    }
    # add point for initial biomass/recruitment (if requested)
    if(init){
      points(B1, R1, pch=4, cex=1.5)
    }
    # add legend
    if(legend){
      legend(legendloc, legend=legend_lab, col=legend_col, lwd=legend_lwd,
             pch=legend_pch, bg=rgb(1,1,1,.9))
    }
  }
  if(plot){
    if(1 %in% subplot){
      StockRecruitCurve.fn()
    }
    if(2 %in% subplot){
      StockRecruitCurve.fn(text=TRUE)
    }
  }    
  if(print){
    if(1 %in% subplot){
      file <- "SR_curve.png"
      caption <- "Spawner-recruit curve"
      plotinfo <- pngfun(file=file, caption=caption)
      StockRecruitCurve.fn()
      dev.off()
    }
    if(2 %in% subplot){
      file <- "SR_curve2.png"
      caption <- paste("Spawner-recruit curve with labels on first, last, and years with (log) deviations >",textmindev)
      plotinfo <- pngfun(file=file, caption=caption)
      StockRecruitCurve.fn(text=TRUE)
      dev.off()
    }
  }
  if(!is.null(plotinfo)) plotinfo$category <- "S-R"
  return(invisible(plotinfo))
}
