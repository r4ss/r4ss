#' Plot matrix of either length or observed age at true age
#'
#' Distribution of length at age or observed age at true age is represented
#' as a histogram. Values are from the AGE_LENGTH_KEY and AGE_AGE_KEY sections
#' of Report.sso ($ALK and $AAK in the list created by SS_output)
#'
#' @param replist List created by \code{SS_output}
#' @param option Switch set to either 1 for length at true age or
#' 2 for obs. age at true age
#' @param scale Multiplier for bars showing distribution. Species with many ages
#' benefit from expanded bars. Future default could be function of number of ages
#' or variability among bins within each age. Current default is 3.
#' @param plot Plot to active plot device?
#' @param print Print to PNG files?
#' @param labels Vector of labels for plots (titles and axis labels)
#' @param pwidth Width of plot
#' @param pheight Height of plot
#' @param punits Units for PNG file
#' @param res Resolution for PNG file
#' @param ptsize Point size for PNG file
#' @param cex.main Character expansion for plot titles
#' @param addmain switch which allows the plot title to be left off
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @author Ian G. Taylor
#' @export
#' @seealso \code{\link{SSplotNumbers}}


SSplotAgeMatrix <- function(replist, option=1, scale=3, plot=TRUE, print=FALSE,
                            labels=c("Age",          #1
                                "Length",            #2
                                "True age",          #3
                                "Observed age",      #4
                                "for ageing error type", #5
                                "Distribution of",   #6
                                "at"),               #7
                            pwidth=6.5, pheight=5.0, punits="in", res=300, ptsize=10,
                            cex.main=1, addmain=TRUE, plotdir="default"){
  # in-development function to plot matrix of length at age

  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL
  if(plotdir=="default"){
    plotdir <- replist$inputs$dir
  }

  # get stuff form replist created by SS_output
  # matrix of length at age (not really an age-length-key as the name implies, as
  # that would be a matrix used to convert length to age rather than age to length)

  if(option==1){
    # option 1 is plotting distribution of length at age
    array <- replist$ALK
    # vertical dimension is plotting length bins
    ybins <- replist$lbinspop
    # number of slices should be the number of sex/growth-pattern/morph combos
    nslices <- dim(array)[3]
    # axis labels
    xlab <- labels[1]
    ylab <- labels[2]
    # first part of title (can have addition info appended)
    titleStart <- paste(labels[6], tolower(paste(labels[2], labels[7], labels[1])))
    # first part of PNG file name (only used if print=TRUE)
    filenameStart <- "bio1B_len_at_age_matrix_"
  }
  if(option==2){
    # option 2 is plotting distribution of observed age at true age
    array <- replist$AAK
    # age bins
    ybins <- replist$agebins
    # number of slices is the number of ageing error matrices
    nslices <- dim(array)[1]
    # axis labels
    xlab <- labels[3]
    ylab <- labels[4]
    # first part of title (can have addition info appended)
    titleStart <- paste(labels[6], tolower(paste(labels[4], labels[7], labels[3])))
    # first part of PNG file name (only used if print=TRUE)
    filenameStart <- "numbers6_ageerror_matrix_"
  }
  nybins <- length(ybins)

  # maximum age
  accuage <- replist$accuage
  # vector of ages
  agevec <- 0:accuage
  # length of vector (always accuage+1, but convenient to name)
  nages <- length(agevec)

  #### rainbow colors (worked better with grey background commented-out further down)
  ## colvec <- rev(rich.colors.short(n=nages,alpha=.8))
  colvec <- rep(grey(0, alpha=.5), nages)

  ymax <- 1.1*(ybins[nybins] + ybins[nybins] - ybins[nybins-1])

  AgeMatrix.fn <- function(slice=1){
    if(option==1){
      # choose which morph/sex/etc of the array
      mat <- array[,,slice]
      # need to figure out which slice corresponds to which
      # morph/sex/etc. for labeling purposes
      title <- titleStart
    }
    if(option==2){
      # choose which ageing error matrix
      mat <- array[slice,,]
      # concatenate title
      title <- paste(titleStart, "\n", labels[5], slice)
    }
    # option to not have plot title (to avoid redundancy with caption)
    if(!addmain){
      title <- ""
    }

    plot(0, type='n', las=1, 
         xlim=c(0,1.1*accuage), xaxs='i',
         ylim=c(0, ymax), yaxs='i',
         xlab=xlab, ylab=ylab, main=title)
    #### grey background
    ## rect(par("usr")[1], par("usr")[3],
    ##      par("usr")[2], par("usr")[4], col = grey(.8))
    abline(h=ybins, v=0:accuage, col='grey90', lwd=0.5)
    for(iage in nages:1){
      #print(iage)
      #print(dim(mat))
      a <- agevec[iage] # actual age
      yvec <- rev(mat[,iage]) # vector of values
      for(iybin in 1:nybins){
        # lower limit of bin is value in vector of bins
        ybin_lo <- ybins[iybin]
        # upper limit is following bin value...
        if(iybin < length(ybins)){
          ybin_hi <- ybins[iybin+1]
        }else{
          # unless it's the final bin in which case
          # it's depicted as equal in width to the previous bin even
          # though it's actually a plus group
          ybin_hi <- ybins[iybin] +
            (ybins[iybin] - ybins[iybin-1])
        }
        # add a filled rectangle for this combination of age and ybin
        rect(xleft = a,
             ybottom = ybin_lo,
             xright = a + scale*yvec[iybin],
             ytop = ybin_hi,
             col=colvec[iage], border=NA)
      } # end loop over bins (length or observed age)
      #lines(a+yvec*scale, ybins)
    } # end loop over true ages
    box()
  } # end of internal AgeMatrix.fn

  # loop over slices (ageing matrices or growth patterns/sexes)
  for(islice in 1:nslices){
    if(plot){
      # plot to GUI or PDF
      AgeMatrix.fn(slice=islice)
    }
    if(print) {
      # print to PNG file
      file <- paste0(filenameStart,islice,".png")
      # caption creation is redundant with title creation inside AgeMatrix.fn
      # but is required in advance to pass to pngfun
      # not sure how this issue was dealt with in other functions
      if(option==1){
        caption <- paste(titleStart)
      }
      if(option==2){
        caption <- paste(titleStart, "\n", labels[5], islice)
      }
      plotinfo <- pngfun(file=file, caption=caption)
      AgeMatrix.fn(slice=islice)
      dev.off()
    }
  } # end loop over slices (ageing matrices or growth patterns/sexes)

  # add category and return plotinfo
  if(!is.null(plotinfo)){
    if(option==1){
      plotinfo$category <- "Bio"
    }
    if(option==1){
      plotinfo$category <- "Numbers"
    }
  }
  return(invisible(plotinfo))
} # end of function
