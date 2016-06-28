#' Plot sex-ratio data and fits for two sex models
#'
#' Plot sex-ratio data and fits from Stock Synthesis output.  Multi-figure
#' plots depend on \code{make_multifig}.
#'
#'
#' @param replist list created by \code{SSoutput}
#' @param kind indicator of type of plot can be "LEN", "SIZE", "AGE", "cond",
#' "GSTAGE", "L[at]A", or "W[at]A".
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param fleets optional vector to subset fleets for which plots will be made
#' @param fleetnames optional vector of fleet names to put in the labels
#' @param yupper upper limit on ymax for polygon/histogram composition plots
#' @param linescol Color for lines on top of polygons
#' @param lwd line width
#' @param axis1 position of bottom axis values
#' @param axis2 position of left size axis values
#' @param pwidth default width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param pheight default height width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param punits units for \code{pwidth} and \code{pheight}. Can be "px"
#' (pixels), "in" (inches), "cm" or "mm". Default="in".
#' @param ptsize point size for plotted text in plots printed to files (see
#' help("png") in R for details). Default=12.
#' @param res resolution of plots printed to files. Default=300
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param cex.main character expansion parameter for plot titles
#' @param labels vector of labels for plots (titles and axis labels)
#' @param maxrows maximum (or fixed) number or rows of panels in the plot
#' @param maxcols maximum (or fixed) number or columns of panels in the plot
#' plots
#' @param rows number or rows to return to as default for next plots to come or
#' for single plots
#' @param cols number or cols to return to as default for next plots to come or
#' for single plots
#' @param fixdims fix the dimensions at maxrows by maxcols or resize based on
#' number of years of data
#' @param verbose return updates of function progress to the R GUI?
#' @param \dots additional arguments that will be passed to the plotting.
#' @author Cole Monnahan
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{make_multifig_sexratio}}
#' @keywords hplot
SSplotSexRatio <-
  function(replist, kind="AGE", plot=TRUE, print=FALSE, fleets="all",
           fleetnames="default",  yupper=4, linescol=1, lwd=1,
           axis1=NULL, axis2=NULL,  pwidth=6.5, pheight=5.0, punits="in",
           ptsize=10, res=300, plotdir="default", cex.main=1, 
           labels = c("Length (cm)", "Age (yr)"), maxrows=6, maxcols=6,
           rows=1, cols=1, fixdims=TRUE, verbose=TRUE, ...)
{
################################################################################
    ## SSplotSexRatio
################################################################################


  if(!exists("make_multifig_sexratio")) stop("you are missing the function 'make_muliti_sexratio'")

  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  SS_versionNumeric <- replist$SS_versionNumeric

  lendbase      <- replist$lendbase
  agedbase      <- replist$agedbase
  ghostagedbase <- replist$ghostagedbase
  ghostlendbase <- replist$ghostlendbase
  nfleets       <- replist$nfleets
  nseasons      <- replist$nseasons
  seasfracs     <- replist$seasfracs
  FleetNames    <- replist$FleetNames
  nsexes        <- replist$nsexes

  ## define a variety of titles and labels
  titles <- NULL
  titlemkt <- ""
  if(plotdir=="default"){
    plotdir <- replist$inputs$dir
  }

  ## sort out which fleets will be included
  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  } else if(length(intersect(fleets,1:nfleets))!=length(fleets)) {
    stop("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
  }

  if(fleetnames[1]=="default") fleetnames <- FleetNames

  ## a few quantities related to data type and plot number
  if(kind=="LEN"){
    dbase_kind <- lendbase
    kindlab=labels[1]
    filenamestart <- "sexratio_len_"
    titledata <- "sex ratios for length comps, "
  } else if(kind=="AGE"){
    dbase_kind <- agedbase
    kindlab=labels[2]
    filenamestart <- "sexratio_age_"
    titledata <- "sex ratios for age comps, "
  } else {
    stop("Only kind of LEN and AGE are currently supported")
  }
  ## else if(kind=="GSTLEN"){
  ##     dbase_kind <- ghostlendbase
  ##     kindlab=labels[1]
  ##     filenamestart <- "sexratio_gstlen_"
  ##     titledata <- "sex ratios for ghost length comps, "
  ##   } else if(kind=="GSTAGE"){
  ##     dbase_kind <- ghostagedbase
  ##     kindlab=labels[2]
  ##     filenamestart <- "sexratio_gstage_"
  ##     titledata <- "sex ratios for ghost age comps, "
  ##   }

  ## Add asterix to indicate super periods and then remove rows labeled "skip".
  ## It would be better to somehow show the range of years, but that seems difficult.
  if(any(dbase_kind$SuprPer=="Sup" & dbase_kind$Used=="skip")){
    cat("Note: removing super-period composition values labeled 'skip'\n",
        "     and designating super-period values with a '*'\n")
    dbase_kind <- dbase_kind[dbase_kind$SuprPer=="No" | dbase_kind$Used!="skip",]
    dbase_kind$YrSeasName <- paste(dbase_kind$YrSeasName,ifelse(dbase_kind$SuprPer=="Sup","*",""),sep="")
  }
  ageerr_warning <- TRUE

  ## # subset data based on requested range of sexes
  ## dbase_kind <- dbase_kind[dbase_kind$sex %in% sexes,]
  ## loop over fleets
  for(f in fleets){
    dbasef <- dbase_kind[dbase_kind$Fleet==f,]
    ## For a 2-sex model some fleets may have only females so skip those
    if(length(unique(dbasef$Gender)) > 1 & nrow(dbasef) > 0 ){
      ## loop over partitions (discard, retain, total)
      for(j in unique(dbasef$Part)){
        dbase <- dbasef[dbasef$Part==j,]
        ## assemble pieces of plot title
        ## market category
        if(j==0) titlemkt <- "whole catch, "
        if(j==1) titlemkt <- "discard, "
        if(j==2) titlemkt <- "retained, "

        ## aggregating identifiers for plot titles and filenames
        title_mkt <- paste(titlemkt, sep="")
        filename_fltmkt <- paste("flt",f, "mkt",j,sep="")
        ptitle <- paste(titledata,title_mkt, fleetnames[f],sep="") ## total title
        titles <- c(ptitle,titles) ## compiling list of all plot titles
        tempfun <- function(ipage,...){
          ## a function to combine a bunch of repeated commands
          make_multifig_sexratio(dbase=dbase,
                         nlegends=3, legtext=list("yr","N","effN"), lwd=lwd,
                         main=ptitle, cex.main=cex.main, xlab=kindlab, ylab=labels[6],
                         maxrows=maxrows, maxcols=maxcols, rows=rows, cols=cols,
                         fixdims=fixdims, ipage=ipage, scalebins=FALSE,
                         linescol=linescol, axis1=axis1, axis2=axis2,
                          yupper=yupper)
        } # end tempfun

        ## Do the plotting and saving
        if(plot) tempfun(ipage=0,...)
        if(print){ # set up plotting to png file if required
          npages <- ceiling(length(unique(dbase$Yr.S))/maxrows/maxcols)
          for(ipage in 1:npages){
            caption <- ptitle
            pagetext <- ""
            if(npages>1){
              pagetext <- paste("_page",ipage,sep="")
              caption <- paste(caption, " (plot ",ipage," of ",npages,")",sep="")
            }
            file <- paste(plotdir,"/",filenamestart,
                          filename_fltmkt,pagetext,".png",sep="")
            plotinfo <- pngfun(file=file, caption=caption)
            tempfun(ipage=ipage,...)
            dev.off()
          }
        }
      } # end loop over partitions (index j)
      ##        } # end test for whether gender in vector of requested sexes
      ##      } # end loop over combined/not-combined genders
    } # end if data
  } # end loop over fleets

  if(!is.null(plotinfo)) plotinfo$category <- "Comp"
  return(invisible(plotinfo))
} # end embedded SSplotComps function
###########################
