#' Plot matrix of either length or observed age at true age
#' 
#' Distribution of length at age or observed age at true age is represented
#' as a histogram. Values are from the AGE_LENGTH_KEY and AGE_AGE_KEY sections
#' of Report.sso ($ALK and $AAK in the list created by SS_output)
#'
#' @param replist list created by \code{SS_output}
#' @param option switch set to either 1 for length at true age or
#' 2 for obs. age at true age
#' @param labels vector of labels for plots (titles and axis labels)
#' @param cex.main character expansion for plot titles
#' @param addmain switch which allows the plot title to be left off

SSplotAgeMatrix <- function(replist, option=1, scale=3,
                            labels=c("Age",          #1
                                "Length",            #2
                                "True age",          #3
                                "Observed age",      #4
                                "ageing error type", #5
                                "Distribution of",   #6
                                "at"),               #7
                            cex.main=1, addmain=TRUE){
  # in-development function to plot matrix of length at age

  # get stuff form replist created by SS_output
  # matrix of length at age (not really an age-length-key as the name implies, as
  # that would be a matrix used to convert length to age rather than age to length)

  if(option==1){
    # option 1 is plotting distribution of length at age
    mat <- replist$ALK
    # temporarily choosing the first morph/sex/etc of the array for simplicity
    mat <- mat[,,1]
    # vertical dimension is plotting length bins
    ybins <- replist$lbinspop
    xlab <- "Age"
    ylab <- "Length"
    main <- paste(labels[5], labels[2], labels[6], labels[1])
  }
  if(option==2){
    # option 2 is plotting distribution of observed age at true age
    mat <- replist$AAK
    # temporarily choosing the first ageing error matrix for simplicity
    mat <- mat[1,,]
    # age bins
    ybins <- replist$agebins
    xlab <- "True age"
    ylab <- "Observed age"
    main <- paste(labels[5], labels[4], labels[6], labels[3])
  }
  nybins <- length(ybins)
  
  # maximum age
  accuage <- replist$accuage
  # vector of ages
  agevec <- 0:accuage
  # length of vector (always accuage+1, but convenient to name)
  nages <- length(agevec)

  colvec <- rev(rich.colors.short(n=nages,alpha=.8))
  ## # alternate colors to improve contrast
  ## colvec <- rich.colors.short(n=2*ceiling(nages/2),alpha=.8)
  ## colvec1 <- colvec[1:ceiling(nages/2)]
  ## colvec2 <- colvec[-(1:ceiling(nages/2))]
  ## colvec <- as.vector(matrix(c(colvec1,colvec2), 2,
  ##                            ceiling(nages/2), byrow=TRUE))[1:nages]

  ymax <- 1.1*(ybins[nybins] + ybins[nybins] - ybins[nybins-1])
  plot(0, type='n', las=1,
       xlim=c(0,1.1*accuage), xaxs='i',
       ylim=c(0, ymax), yaxs='i',
       xlab=xlab, ylab=ylab, main=main)
  abline(h=ybins, v=0:accuage, col='grey90')
  for(iage in nages:1){
    #print(iage)
    #print(dim(mat))
    a <- agevec[iage] # actual age
    yvec <- rev(mat[,iage])
    for(iybin in 1:nybins){
      # lower limit of bin is value in vector of bins
      ybin_lo <- ybins[iybin]
      # upper limit is following bin...
      if(iybin < length(ybins)){
        ybin_hi <- ybins[iybin+1]
      }else{
        # unless it's the final bin in which case
        # it's depicted as equal in width to the previous bin even
        # though it's actually a plus group
        ybin_hi <- ybins[iybin] +
          (ybins[iybin] - ybins[iybin-1])
      }
      rect(xleft = a, 
           ybottom = ybin_lo,
           xright = a + scale*yvec[iybin],
           ytop = ybin_hi,
           col=colvec[iage])
    }
    #lines(a+yvec*scale, ybins)
  }
  
  box()
}

# directory obviously specific to my computer
#s2 <- SS_output('C:/ss/SSv3.30beta_May17/simple_3.30_ALK_tol')
#SSplotALK(s2)
