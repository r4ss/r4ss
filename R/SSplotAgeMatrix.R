SSplotALK <- function(replist, scale=3){
  # in-development function to plot matrix of length at age

  # get stuff form replist created by SS_output
  # matrix of length at age (not really an age-length-key as the name implies, as
  # that would be a matrix used to convert length to age rather than age to length)
  ALK <- replist$ALK
  # maximum age
  accuage <- replist$accuage
  # vector of ages
  agevec <- 0:accuage
  # length of vector (always accuage+1, but convenient to name)
  nages <- length(agevec)
  # length bins
  lbinspop <- replist$lbinspop
  # temporarily choosing the first morph/sex/etc of the array for simplicity
  ALK <- ALK[,,1]

  colvec <- rev(rich.colors.short(n=nages,alpha=.8))
  ## # alternate colors to improve contrast
  ## colvec <- rich.colors.short(n=2*ceiling(nages/2),alpha=.8)
  ## colvec1 <- colvec[1:ceiling(nages/2)]
  ## colvec2 <- colvec[-(1:ceiling(nages/2))]
  ## colvec <- as.vector(matrix(c(colvec1,colvec2), 2,
  ##                            ceiling(nages/2), byrow=TRUE))[1:nages]
  
  plot(0, type='n', las=1,
       xlim=c(0,1.05*accuage), xaxs='i',
       ylim=c(0,1.05*max(lbinspop)), yaxs='i',
       xlab="Age", ylab="Length")
  abline(h=lbinspop, v=0:accuage, col='grey')
  for(iage in nages:1){
    a <- agevec[iage] # actual age
    lenvec <- rev(ALK[,iage])
    #    print(lenvec)
    #    print(length(lenvec))
    for(ilbin in 1:length(lbinspop)){
      # lower limit of bin is value in vector of bins
      lbin_lo <- lbinspop[ilbin]
      # upper limit is following bin...
      if(ilbin < length(lbinspop)){
        lbin_hi <- lbinspop[ilbin+1]
      }else{
        # unless it's the final bin in which case
        # an arbitrary upper limit is chosen
        # to suite the visual depiction of a plus group
        lbin_hi <- lbinspop[ilbin] +
          3*(lbinspop[ilbin] - lbinspop[ilbin-1])
      }
      rect(xleft = a, 
           ybottom = lbin_lo,
           xright = a + scale*lenvec[ilbin],
           ytop = lbin_hi,
           col=colvec[iage])
    }
    #lines(a+lenvec*scale, lbinspop)
  }
  
  box()
}

# directory obviously specific to my computer
#s2 <- SS_output('C:/ss/SSv3.30beta_May17/simple_3.30_ALK_tol')
#SSplotALK(s2)
