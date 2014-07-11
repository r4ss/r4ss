#' Make shaded polygons with a mountain-like appearance
#' 
#' Designed to replicate like the cool-looking Figure 7 in Butterworth et al.
#' (2003).
#' 
#' 
#' @param zmat A matrix where the rows represent the heights of each mountain
#' range
#' @param xvec Optional input for the x variable
#' @param yvec Optional input for the y variable
#' @param zscale Controls the height of the mountains relative to the y-axis
#' and max(zmat)
#' @param rev Reverse the order of the display of yvec values.
#' @param nshades Number of levels of shading
#' @param axes Add axes to the plot?
#' @param xaxs X-axis as internal or regular (see ?par for details)
#' @param yaxs Y-axis as internal or regular (see ?par for details)
#' @param xlab Optional label for x-axis
#' @param ylab Optional label for y-axis
#' @param las Xxis label style (see ?par for details). Default = 1 = horizontal
#' axis labels.
#' @param addbox Puts a box around the whole plot
#' @param ...  Extra inputs passed to the plot command
#' @author Ian Taylor
#' @export
#' @references Butterworth D.S., Ianelli J.N., Hilborn R. (2003) A statistical
#' model for stock assessment of southern bluefin tuna with temporal changes in
#' selectivity. South African Journal of Marine Science 25:331-362.
#' @keywords hplot
mountains <- function(zmat, xvec=NULL, yvec=NULL, zscale=3, rev=TRUE,
                      nshades=100,axes=TRUE, xaxs='i', yaxs='i',
                      xlab="", ylab="", las=1, addbox=FALSE, ...){
  
  ## DESCRIPTION:
  # a function by Ian Taylor designed to look like the cool-looking Figure 7 in
  # Butterworth D.S., Ianelli J.N., Hilborn R. (2003) A statistical model for
  # stock assessment of southern bluefin tuna with temporal changes in selectivity.
  # South African Journal of Marine Science 25:331-362.

  errors <- FALSE
  for(icol in 1:ncol(zmat)){
    if(!is.numeric(zmat[,icol])){
      errors <- TRUE
      print(paste("error: column",icol,"of zmat is not numeric"))
    }
  }
  if(errors) return(invisible())

  if(rev) yvec <- -yvec
  
  # fill in vectors if not provided
  nrowz <- nrow(zmat)
  ncolz <- ncol(zmat)
  if(is.null(yvec)) yvec <- 1:nrowz
  if(is.null(xvec)) xvec <- 1:ncolz

  # define some limits
  xmin <- min(xvec)
  xmax <- max(xvec)
  zmax <- zscale*max(zmat)

  ny <- length(yvec)
  if(ny!=nrowz){
    print("length(yvec) must equal nrow(zmat)",quote=FALSE)
    return()
  }
  if(length(xvec)!=ncolz){
    print("length(xvec) must equal ncol(zmat)",quote=FALSE)
    return()
  }

  zseq <- seq(0, zmax, length=nshades)
  xvec2 <- c(xmin, xvec, xmax) # adding extra points for bottom corners of polygon

  # plot(0, type='n', xlim=c(xmin, xmax), ylim=c(0, 1.1*(ymax+ny)), xaxs='i', yaxs='i', ...)
  plot(0, type='n', xlim=c(xmin, xmax), ylim=c(min(yvec), (max(yvec) + 1.1*zmax)),
       xaxs=xaxs, yaxs=yaxs, xlab=xlab, ylab=ylab,
       axes=FALSE, ...)

  if(rev) order <- 1:ny else order <- ny:1
  for(iy in order){
    zvec <- as.numeric(zmat[iy, ])
    zvec2 <- c(0, zscale*zvec, 0) # row from z matrix
    if(any(zvec2>0)){
      
      # calculate set of all intersections between polygon and the horizontal lines
      x3list <- list()
      for(iz in 1:nshades){
        z <- zseq[iz]
        x3 <- numeric()
        for(ix in 2:length(xvec2)){
          z1 <- zvec2[ix-1]
          z2 <- zvec2[ix]
          x1 <- xvec2[ix-1]
          x2 <- xvec2[ix]
          if(z >= min(z1, z2) & z < max(z1, z2)){
            x3 <- c(x3, (z-z1)*(x2-x1)/(z2-z1)+x1)
          }
        }
        x3list[[iz]] <- x3
      }
      # draw little polygons between each pair of horizontal lines
      for(iz in 2:length(x3list)){

        z2 <- zseq[iz]
        z1 <- zseq[iz-1]

        x3hi <- x3list[[iz]] # x-values of intersections along upper line
        x3lo <- x3list[[iz-1]]   # x-values of intersections along lower line

        npoly <- length(x3lo)/2

        for(ipoly in 1:npoly){
          xlo <- x3lo[ipoly*2 + -1:0] # lower line intersections for individual polygon
          xhi <- x3hi[x3hi>=xlo[1] & x3hi<=xlo[2]] # upper line intersections
          extra <- (zvec2 >= z1 & zvec2 <= z2 & xvec2>=xlo[1] & xvec2<=xlo[2]) # identifying extra points to add
          xhi2 <- c(xhi,xvec2[extra]) # adding extra points to vector of upper x-values
          zhi <- c(rep(z2,length(xhi)), zvec2[extra]) # add corresponding z-values
          zhi2 <- zhi[order(xhi2)] # put the z-values in order based on order of x-values
          xhi2 <- sort(xhi2) # now order the x-values

          # make polygon
          polygon(x = c(xlo[2:1],xhi2),
                  y = yvec[iy]+ c(z1,z1,zhi2),
                  col = grey(1-.9*z1/zmax),border=grey(1-.9*z1/zmax))
        }
      }
      # black polygon around the outside
      polygon(xvec2, yvec[iy]+zvec2)
    }
  }
  # add axes
  if(axes){
    axis(1,at=xvec)
    axis(2,at=yvec,labels=abs(yvec),las=las)
    axis(4,at=yvec,labels=FALSE) # extra ticks on right hand side
  }
  if(addbox) box() # add box if desired
}

