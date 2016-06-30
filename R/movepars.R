#' Explore movement parameterizations in a GUI
#'
#' A function to visualize parameterization of movement in Stock Synthesis. It
#' creates a GUI interface for movement exploration. Based on selectivity GUI
#' by Tommy Garrison
#'
#' @param nareas Number of areas
#' @param accuage Accumulator age
#' @param season.duration Length of season (annual rates are scaled by
#' this value in SS).
#' @param min.move.age Minimum age of movement.
#' @author Ian Taylor
#' @export
#'
movepars <-
function(nareas=4,accuage=40,season.duration=1,min.move.age=0.5)
{
  if(!nareas %in% 2:4) stop("'nareas' input must be 2, 3, or 4")
  geterrmessage()
  if(season.duration > 1) stop("'season.duration' should <= 1 (a fraction of the year).")
  geterrmessage()

  movecalc <- function(firstage, accuage, minage, maxage, valueA, valueB,
                       destination=1) {
    # subfunction to calculate movement rates
    # can be used as a stand-alone function
    # by uncommenting the plot command near the bottom
    veclengths <- unique(c(length(minage),length(maxage),length(valueA),length(valueB)))
    if(length(veclengths)!=1){
      stop("Input vectors  minage, maxage, valueA, valueB need to all have the same length")
    }else{
      npars <- veclengths
    }

    agevec <- 0:accuage
    nages <- length(agevec)

    movemat1 <- matrix(NA,npars,nages) # raw values
    movemat2 <- matrix(NA,npars,nages) # normalized to sum to 1

    temp <- 1/(maxage-minage)
    temp1 <- temp*(valueB-valueA)

    for(iage in 1:nages){
      for(ipar in 1:npars){
        if(agevec[iage] <= minage[ipar]){
          movemat1[ipar,iage] <- valueA[ipar]
        }
        if(agevec[iage] >= maxage[ipar]){
          movemat1[ipar,iage] <- valueB[ipar]
        }
        if(agevec[iage] > minage[ipar] & agevec[iage] < maxage[ipar]){
          movemat1[ipar,iage] <- valueA[ipar] + (agevec[iage]-minage[ipar])*temp1[ipar]
        }
      }
    }
    # exponentiate
    movemat1 <- exp(movemat1)
    # scale by season duration
    scale <- matrix(season.duration, npars, nages)
    scale[1,] <- 1
    movemat1 <- scale*movemat1
    # rescale
    movemat2 <- movemat1/matrix(apply(movemat1,2,sum),npars,nages,byrow=TRUE)
    # stop youngest fish from moving if requested
    movemat2[-1 , agevec < min.move.age] <- 0

    lty <- c('91','42','22','4222')
    lwd <- rep(3,npars)
    col <- c('blue','red','green3','purple')

    namevec <- paste("area 1 to area",1:npars)
    # plot(0,type='n',xlim=range(agevec),ylim=c(0,1),xaxs='i',yaxs='i',
    #       xlab='Age',ylab='Movement rate')
    matplot(x=agevec,y=t(movemat2),type='l',lty=lty,lwd=lwd,col=col,add=T)
    legend('topright',legend=namevec,lty=lty,lwd=lwd,col=col,bty='n')
    return(movemat2)

  } # end movecalc subfunction

  ## don't know how to print to command line while GUI is open
  # print(paste("running movement parameter GUI for Stock Synthesis with nareas=",nareas," and accumulator age=",accuage,sep=""),quote=F)

  done <- tclVar(0)
  movepars <- new.env()

  # initial values
  minage1 <- tclVar(0)
  maxage1 <- tclVar(0)
  valueA1 <- tclVar(0)
  valueB1 <- tclVar(0)

  minage2 <- tclVar( 3)
  maxage2 <- tclVar(15)
  valueA2 <- tclVar(-2)
  valueB2 <- tclVar(-1)

  if(nareas>=3){
    minage3 <- tclVar( 3)
    maxage3 <- tclVar(15)
    valueA3 <- tclVar(-3)
    valueB3 <- tclVar(-2)
  }
  if(nareas==4){
    minage4 <- tclVar( 3)
    maxage4 <- tclVar(15)
    valueA4 <- tclVar(-4)
    valueB4 <- tclVar(-3)
  }

  replot <- function(...) {
    # subfunction to remake the plot
    minage1 <- as.numeric(tclObj(minage1))
    maxage1 <- as.numeric(tclObj(maxage1))
    valueA1 <- as.numeric(tclObj(valueA1))
    valueB1 <- as.numeric(tclObj(valueB1))

    minage2 <- as.numeric(tclObj(minage2))
    maxage2 <- as.numeric(tclObj(maxage2))
    valueA2 <- as.numeric(tclObj(valueA2))
    valueB2 <- as.numeric(tclObj(valueB2))

    if(nareas>=3){
      minage3 <- as.numeric(tclObj(minage3))
      maxage3 <- as.numeric(tclObj(maxage3))
      valueA3 <- as.numeric(tclObj(valueA3))
      valueB3 <- as.numeric(tclObj(valueB3))
    }else{ # dummies needed for later
      minage3 <- NA
      maxage3 <- NA
      valueA3 <- NA
      valueB3 <- NA
    }
    if(nareas==4){
      minage4 <- as.numeric(tclObj(minage4))
      maxage4 <- as.numeric(tclObj(maxage4))
      valueA4 <- as.numeric(tclObj(valueA4))
      valueB4 <- as.numeric(tclObj(valueB4))
    }else{ # dummies needed for later
      minage4 <- NA
      maxage4 <- NA
      valueA4 <- NA
      valueB4 <- NA
    }
    plot(0,type='n',xlim=c(0,accuage),ylim=c(0,1),xaxs='i',yaxs='i',
         xlab='Age',ylab='Movement rate')

    rates <- movecalc(firstage=0, accuage=accuage,
             minage=c(minage1,minage2,minage3,minage4)[1:nareas],
             maxage=c(maxage1,maxage2,maxage3,maxage4)[1:nareas],
             valueA=c(valueA1,valueA2,valueA3,valueA4)[1:nareas],
             valueB=c(valueB1,valueB2,valueB3,valueB4)[1:nareas])

    dat <- list(pars = data.frame(
      minage=c(minage1,minage2,minage3,minage4)[1:nareas],
      maxage=c(maxage1,maxage2,maxage3,maxage4)[1:nareas],
      valueA=c(valueA1,valueA2,valueA3,valueA4)[1:nareas],
      valueB=c(valueB1,valueB2,valueB3,valueB4)[1:nareas],
      label=paste("area_1_to_area_",1:nareas,sep="")),
                rates = rates)

    assign('dat',dat,envir=movepars)
  } # end replot

  base <- tktoplevel()
  tkwm.title(base, "Examine Movement Patterns")
  spec.frm <- tkframe(base, borderwidth = 2)
  left.frm <- tkframe(spec.frm)
  right.frm <- tkframe(spec.frm)

  # frame 1:
  frame1 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
  tkpack(tklabel(frame1, text = "Parameters for movement from area 1 to area 1 (usually not estimated)",font="variable 12 bold"), fill = "both", side = "top")

  #minage
  entry.minage1 <- tkentry(frame1, textvariable = minage1, width="8")
  tkpack(ts1 <- tkscale(frame1, label = "beginning of ramp", command = replot,
                        from = 0, to = accuage, showvalue = 1, variable = minage1,
                        resolution = 1, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

  #maxage
  entry.maxage1 <- tkentry(frame1, textvariable = maxage1, width="8")
  tkpack(ts1 <- tkscale(frame1, label = "end of ramp", command = replot,
                        from = 0, to = accuage, showvalue = 1, variable = maxage1,
                        resolution = 1, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

  #valueA
  entry.valueA1 <- tkentry(frame1, textvariable = valueA1, width="8")
  tkpack(ts1 <- tkscale(frame1, label = "value A :", command = replot,
                        from = -5, to = 5, showvalue = 1, variable = valueA1,
                        resolution = 0.01, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

  #valueB
  entry.valueB1 <- tkentry(frame1, textvariable = valueB1, width="8")
  tkpack(ts1 <- tkscale(frame1, label = "value B :", command = replot,
                        from = -5, to = 5, showvalue = 1, variable = valueB1,
                        resolution = 0.01, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

  # entry boxes
  tkpack(entry.valueB1, side = "right")
  tkpack(entry.valueA1, side = "right")
  tkpack(entry.maxage1,  side = "right")
  tkpack(entry.minage1,  side = "right")

  # frame2:
  frame2 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
  tkpack(tklabel(frame2, text = "Parameters for movement from area 1 to area 2",font="variable 12 bold"), fill = "both", side = "top")

  #minage
  entry.minage2 <- tkentry(frame2, textvariable = minage2, width="8")
  tkpack(ts1 <- tkscale(frame2, label = "beginning of ramp", command = replot,
                        from = 0, to = accuage, showvalue = 1, variable = minage2,
                        resolution = 1, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

  #maxage
  entry.maxage2 <- tkentry(frame2, textvariable = maxage2, width="8")
  tkpack(ts1 <- tkscale(frame2, label = "end of ramp", command = replot,
                        from = 0, to = accuage, showvalue = 1, variable = maxage2,
                        resolution = 1, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

  #valueA
  entry.valueA2 <- tkentry(frame2, textvariable = valueA2, width="8")
  tkpack(ts1 <- tkscale(frame2, label = "value A :", command = replot,
                        from = -5, to = 5, showvalue = 1, variable = valueA2,
                        resolution = 0.01, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

  #valueB
  entry.valueB2 <- tkentry(frame2, textvariable = valueB2, width="8")
  tkpack(ts1 <- tkscale(frame2, label = "value B :", command = replot,
                        from = -5, to = 5, showvalue = 1, variable = valueB2,
                        resolution = 0.01, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

  # entry boxes
  tkpack(entry.valueB2, side = "right")
  tkpack(entry.valueA2, side = "right")
  tkpack(entry.maxage2, side = "right")
  tkpack(entry.minage2, side = "right")

  ### frame3:
  if(nareas >= 3){
    frame3 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    tkpack(tklabel(frame3, text = "Parameters for movement from area 1 to area 3",font="variable 12 bold"), fill = "both", side = "top")

    #minage
    entry.minage3 <- tkentry(frame3, textvariable = minage3, width="8")
    tkpack(ts1 <- tkscale(frame3, label = "beginning of ramp", command = replot,
                          from = 0, to = accuage, showvalue = 1, variable = minage3,
                          resolution = 1, orient = "horiz", relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

    #maxage
    entry.maxage3 <- tkentry(frame3, textvariable = maxage3, width="8")
    tkpack(ts1 <- tkscale(frame3, label = "end of ramp", command = replot,
                          from = 0, to = accuage, showvalue = 1, variable = maxage3,
                          resolution = 1, orient = "horiz", relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

    #valueA
    entry.valueA3 <- tkentry(frame3, textvariable = valueA3, width="8")
    tkpack(ts1 <- tkscale(frame3, label = "value A :", command = replot,
                          from = -5, to = 5, showvalue = 1, variable = valueA3,
                          resolution = 0.01, orient = "horiz", relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

    #valueB
    entry.valueB3 <- tkentry(frame3, textvariable = valueB3, width="8")
    tkpack(ts1 <- tkscale(frame3, label = "value B :", command = replot,
                          from = -5, to = 5, showvalue = 1, variable = valueB3,
                          resolution = 0.01, orient = "horiz", relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

    # entry boxes
    tkpack(entry.valueB3, side = "right")
    tkpack(entry.valueA3, side = "right")
    tkpack(entry.maxage3, side = "right")
    tkpack(entry.minage3, side = "right")
  }

  ### frame4:
  if(nareas == 4){
    frame4 <- tkframe(left.frm, relief = "groove", borderwidth = 4)
    tkpack(tklabel(frame4, text = "Parameters for movement from area 1 to area 4",font="variable 12 bold"), fill = "both", side = "top")

    #minage
    entry.minage4 <- tkentry(frame4, textvariable = minage4, width="8")
    tkpack(ts1 <- tkscale(frame4, label = "beginning of ramp", command = replot,
                          from = 0, to = accuage, showvalue = 1, variable = minage4,
                          resolution = 1, orient = "horiz", relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

    #maxage
    entry.maxage4 <- tkentry(frame4, textvariable = maxage4, width="8")
    tkpack(ts1 <- tkscale(frame4, label = "end of ramp", command = replot,
                          from = 0, to = accuage, showvalue = 1, variable = maxage4,
                          resolution = 1, orient = "horiz", relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

    #valueA
    entry.valueA4 <- tkentry(frame4, textvariable = valueA4, width="8")
    tkpack(ts1 <- tkscale(frame4, label = "value A :", command = replot,
                          from = -5, to = 5, showvalue = 1, variable = valueA4,
                          resolution = 0.01, orient = "horiz", relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

    #valueB
    entry.valueB4 <- tkentry(frame4, textvariable = valueB4, width="8")
    tkpack(ts1 <- tkscale(frame4, label = "value B :", command = replot,
                          from = -5, to = 5, showvalue = 1, variable = valueB4,
                          resolution = 0.01, orient = "horiz", relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")

    # entry boxes
    tkpack(entry.valueB4, side = "right")
    tkpack(entry.valueA4, side = "right")
    tkpack(entry.maxage4, side = "right")
    tkpack(entry.minage4, side = "right")
  }

  OnOK <- function() {
    replot()
  }
  OnQuit <- function() {
    tclvalue(done) <- 2
  }
  OnUpdate <- function() {
    replot()
  }

  if(nareas==2) tkpack(frame1, frame2, fill = "x")
  if(nareas==3) tkpack(frame1, frame2, frame3, fill = "x")
  if(nareas==4) tkpack(frame1, frame2, frame3, frame4, fill = "x")
  tkpack(left.frm, right.frm, side = "left", anchor = "n")

  q.but <- tkbutton(base, text = "Quit", command = OnQuit)
  update.but <- tkbutton(base, text = "Update", command = OnUpdate)
  tkpack(spec.frm)
  tkpack(q.but, side = "right")
  tkpack(update.but, side = "right")
  replot()

  tkbind(base, "<Destroy>", function() tclvalue(done) <- 2)
  tkwait.variable(done)
  tkdestroy(base)

  # compile info about the results
  dat <- get('dat',envir=movepars)
  dat$season.duration <- season.duration
  dat$min.move.age <- min.move.age
  # return stuff
  return(dat)
} # end movepars function

