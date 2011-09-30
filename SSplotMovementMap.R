SSplotMovementMap <-
  function(replist=NULL, xlim, ylim,
           polygonlist, colvec, land="grey", xytable=NULL,
           moveage=5,moveseas=1,lwdscale=5,legend=TRUE,title=NULL,
           areanames=NULL,cex=1)
{
  # plot movement rates on map to help visualize patterns
  # note: requires R package "maps"
  # which can be installed using command
  # > install.packages("maps")
  
  ## library(maps)
  if(!exists("map")){
    cat("The function 'SSplotMovementMap' depends on the 'maps' package.
  In order to allow the package to be loaded without depending on the
  maps package, it is left to the user to install and load the maps library
  themselves. This can be done with
    > install.packages('maps')
    > library(maps)\n")
  }
  
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

  
