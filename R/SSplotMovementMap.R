#' Show movement rates on a map.
#' 
#' Make a map with colored spatial cells and add arrows representing movement
#' rates between cells.
#' 
#' 
#' @param replist optional list created by \code{\link{SS_output}}
#' @param xlim range of longitude values in the map
#' @param ylim range of latitude values in the map
#' @param polygonlist a list of data frames, each with two columns representing
#' the longitude and latitude values of the colored polygons. The order of
#' elements in the list should match the numbering of areas in the SS model.
#' @param colvec vector of colors for each polygon (if \code{replist} is
#' provided)
#' @param land color of landmasses in the map
#' @param xytable data frame of latitude and longitude values which will be
#' connected by the arrows representing movement rates. The order should match
#' the order of areas in \code{polygonlist} and in the SS model. Not necessary
#' if no arrows are shown on the map.
#' @param moveage age for which movemement rates will be represented
#' @param moveseas season for which movement rates will be represented
#' @param lwdscale scaling factor for arrows in the plot. The largest rate of
#' movement shown will be scaled to have a line width equal to this value.
#' @param legend add a legend to show the movement rate associated with the
#' widest arrows
#' @param title optional title to be added above map
#' @param areanames optional vector of names to be shown on map at coordinates
#' matching xytable values
#' @param cex character expansion to apply to text shown by areanames (if used)
#' @note Inspired by plots of MULTIFAN-CL movement patterns presented by Adam
#' Langley
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_output}}, \code{\link{SSplotMovementRates}},
#' \code{\link{IOTCmove}}
#' @keywords hplot
SSplotMovementMap <-
  function(replist=NULL, xlim, ylim,
           polygonlist, colvec, land="grey", xytable=NULL,
           moveage=5,moveseas=1,lwdscale=5,legend=TRUE,title=NULL,
           areanames=NULL,cex=1)
{
  # plot movement rates on map to help visualize patterns
 
  par(mar=c(3,3,3,3))
  map(xlim=xlim,ylim=ylim,xaxs='i',yaxs='i')
  for(i in 1:length(polygonlist)){
    polygon(polygonlist[[i]],col=colvec[i],lwd=2)
  }
  map(xlim=xlim,ylim=ylim,xaxs='i',yaxs='i',
      add=T,fill=T,col="grey")
#  map.axes()

  if(!is.null(title)) mtext(side=3,line=1,font=2,title,cex=1.2)
  box()

  # add arrows
  if(!is.null(xytable) & !is.null(replist)){
    move <- replist$movement
    move <- move[move$Source_area!=move$Dest_area
                 & move$Seas==moveseas,]

    lwdvec <- NULL
    ratevec <- NULL
    for(i in 1:nrow(move)){
      area1 <- move$Source_area[i]
      area2 <- move$Dest_area[i]

      x1b <- xytable[area1,1]
      y1b <- xytable[area1,2]
      x2b <- xytable[area2,1]
      y2b <- xytable[area2,2]

      x1 <- x1b + .35*(x2b-x1b)
      y1 <- y1b + .35*(y2b-y1b)
      x2 <- x2b + .2*(x1b-x2b)
      y2 <- y2b + .2*(y1b-y2b)

      slope1 <- (y2-y1)/(x2-x1+0.001)
      slope2 <- -1/slope1
      length1 <- sqrt((y2-y1)^2 + (x2-x1)^2)
      length2 <- 2
      angle1 <- atan(slope1)
      angle2 <- atan(slope2)
    
      shift1 <- .1*length1*c(cos(angle1),sin(angle1))
      shift2 <- length2*c(cos(angle2),sin(angle2))

      if(area1 < area2){
        x1 <- x1 + shift2[1]
        y1 <- y1 + shift2[2]
        x2 <- x2 + shift2[1]
        y2 <- y2 + shift2[2]
      }
      lwd <- lwdscale*move[i,7+moveage]/max(move[,7+moveage])
      lwdvec <- c(lwdvec,lwd)
      ratevec <- c(ratevec,move[i,7+moveage])
      arrows(x1,y1,x2,y2,
             length=.15,
             lwd=lwd,
             lend=1)
    }
    if(legend){
      legend('topright',lwd=max(lwdvec),bg="white",
             legend=paste("rate = ",
               round(100*max(ratevec),3),"%",sep=""))
    }
  }
  if(!is.null(areanames) & !is.null(areanames)) text(xytable,areanames,cex=cex)
}

  
