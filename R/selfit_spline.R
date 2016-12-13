#' visualize parameterization of cubic spline selectivity in SS
#' 
#' A GUI interface for exploring spline selectivity.
#' 
#' 
#' @param n Number of knots.
#' @param minBin Minimum length or age to show.
#' @param maxBin Maximum length or age to show.
#' @param knots Vector giving location of each knot.
#' @param slopevec Optional initial values parameters controlling slope at
#' first and last knot.
#' @param params Optional initial values for the parameters controlling
#' selectivity at each knot.
#' @param dir Directory in which the spline_selex executable is located
#' (default = working directory).
#' @param silent TRUE/FALSE switch to return fit at the end.
#' @author Ian Taylor, Yukio Takeuchi
#' @export
#' @seealso \code{\link{selfit}}
#' @examples
#' 
#' \dontrun{
#' selfit_spline()
#' }
#' 
selfit_spline <- function (n=4, minBin=10, maxBin=65,
                           knots=NULL, slopevec=c(0.01,-0.01), params=NULL,
                           dir=getwd(),
                           silent=FALSE){

  # test for availability of tcltk package
  if (!requireNamespace("tcltk", quietly = TRUE)) { 
    stop("tcltk is required. please install it", 
         call. = FALSE) 
  }

  if(n<3 | n>7 | as.integer(n)!=n) stop("Number of knots must be an integer from 3 to 7")
  if(.Platform$OS.type=="windows"){
    if(!("spline_selex.exe" %in% dir(dir)))
      stop("File 'spline_selex.exe' needs to be in the directory 'dir'\n",
           "  If you have a 64 bit Windows computer, you can get this file from\n",
           "  https://github.com/r4ss/testing/blob/master/spline_selex.exe\n",
           "  For other operating systems, you will need to compile the executable in ADMB\n",
           "  from the file https://github.com/r4ss/testing/blob/master/spline_selex.tpl\n")
  }else{
    if(.Platform$GUI=="X11")
      if(!("spline_selex" %in% dir(dir)))
        stop("File 'spline_selex' needs to be in the directory 'dir'\n",
             "  For non-Windows operating systems, you will need to compile the executable in ADMB\n",
             "  from the file https://github.com/r4ss/testing/blob/master/spline_selex.tpl\n")
    if(.Platform$GUI=="Aqua")
      stop("Sorry, this function is not yet supported for the Mac\n",
           "      see discussion at https://github.com/r4ss/r4ss/issues/48 for discussion of this issue.")
  }

  
  geterrmessage()
  done <- tclVar(0)
  selfit.env <- new.env()
  assign("selfit.tmp", list(), envir = selfit.env)

  oldwd <- getwd()
  setwd(dir)
  
  minLB <- tclVar(minBin)
  maxLB <- tclVar(maxBin)
  diffL <- maxBin - minBin
  if(is.null(knots)) knots <- round(minBin+(1:n-0.7)*diffL/n)
  if(is.null(params)) params <- c(-3,-2,0,rep(-1,n-3))

  kt1 <- NULL
  sp1 <- NULL

  for(i in 1:n){
    # assign initial values
    assign(paste("kt",i,sep=""), tclVar(knots[i]))
    assign(paste("sp",i,sep=""), tclVar(params[i]))
  }
  slope1 <- tclVar(slopevec[1])
  slope2 <- tclVar(slopevec[2])

  runADMB <- function(x, knots, slopes, pars) {
    file <- "spline_selex.dat"
    dat <- 
      c("# number of knots",
        n,
        "# x and then y values of knots",
        paste(knots,collapse=" "),
        paste(pars,collapse=" "),
        "# derivative at first and last knots",
        paste(slopes,collapse=" "),
        "# N x-values to calculate",
        paste(length(x)),
        "# x-values",
        paste(x))
    writeLines(dat,file)
    if(.Platform$OS.type=="windows"){
      system("spline_selex -maxfn 0 -nohess")
    }else{
      if(.Platform$GUI=="X11")
        system("./spline_selex -maxfn 0 -nohess")
      if(.Platform$GUI=="Aqua")
        return() # add something here once Mac support developed
    }

    Sys.sleep(0.1) # wait a second to let everything finish running
    outfile <- "spline_selex.txt"
    shift <- as.numeric(substring(readLines(outfile,n=1),9))
    output <- read.table(outfile)
    names(output) <- c("x","spl","sel")
    return(list(shift=shift,output=output))
  }
  
  replot <- function(...) {
    minL <- as.numeric(tclObj(minLB))
    maxL <- as.numeric(tclObj(maxLB))
    kvec <- rep(NA,n)
    pvec <- rep(NA,n)
    for(i in 1:n){
      kvec[i] <- as.numeric(tclObj(get(paste("kt",i,sep=""))))
      pvec[i] <- as.numeric(tclObj(get(paste("sp",i,sep=""))))
    }
    slopes <- c(as.numeric(tclObj(slope1)),
                as.numeric(tclObj(slope2)))

    x <- seq(minL, maxL, by=1)
    good <- min(diff(kvec)) > 0
    if(good){
      # only call ADMB if it won't give an error
      out <- runADMB(x=x,knots=kvec, slopes=slopes, pars=pvec)
      shift <- out$shift
      x <- out$output$x
      spl <- out$output$spl
      sel <- out$output$sel
    }else{
      spl <- rep(0,length(x))
      sel <- rep(0,length(x))
    }
    
    # make plot
    par(mfrow=c(2,1),mar=c(0,5,0,0),oma=c(5,0,1,1),las=1)

    # upper plot: spline (proportional to log(selectivity)
    plot(0,xlim=c(0, maxBin), ylim=range(pvec,spl), type='n', xlab="", ylab="Spline value",axes=F)
    if(good){
      lines(x,spl,col=2,lwd=5)
      points(kvec,pvec,pch=16,cex=2.5)
      axis(2)
    }
    box()

    # lower plot: selectivity
    plot(0,xlim=c(0, maxBin), ylim=c(0,1), type='n', xlab="", ylab="Selectivity = exp(spline - max(spline))")
    fit <- get("selfit.tmp", envir = selfit.env)
    #lapply(fit, function(x) sel.line(seq(minL, maxL, by=1), model = x$model, sp = x$sp, min.dist = x$min.dist, max.dist = x$max.dist))

    if(good){
      lines(x,sel,col=4,lwd=5)
      points(kvec,exp(pvec - shift), pch=16,cex=2.5)
    }else{
      # give warning in plot if knots are not strictly increasing
      text(x=maxBin/2, y=0.5, "The knots must be strictly increasing")
    }

    mtext(side=1,line=3,"Length or age")
  }

  redraw <- function(...) {
    tkfocus(entry.sp1)
    replot()
  }

  base <- tktoplevel()
  tkwm.title(base, "Examine Selectivity Patterns")
  spec.frm <- tkframe(base, borderwidth = 2)
  left.frm <- tkframe(spec.frm)
  right.frm <- tkframe(spec.frm)

  ####################################################################################
  ### bounds
  frame0 <- tkframe(left.frm, relief = "groove", borderwidth = 2, width=30)
  tkpack(tklabel(frame0, text = "Length Parameters"), fill = "both", side = "top")

  entry.minLB <- tkentry(frame0, textvariable = minLB, width="8")
  tkpack(ts0 <- tkscale(frame0, label = "Min Bin", command = replot,
                        from = 0, to = round(1.3*minBin), showvalue = 1, variable = minLB,
                        resolution = 1, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")
  tkpack(entry.minLB,  side = "right")

  frame1 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
  entry.maxLB <- tkentry(frame1, textvariable = maxLB, width="8")
  tkpack(ts1 <- tkscale(frame1, label = "Max Bin", command = replot,
                        from = 0, to = round(1.3*maxBin), showvalue = 1, variable = maxLB,
                        resolution = 1, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")
  tkpack(entry.maxLB,  side = "right")

  ####################################################################################
  ### knots

  # first knot
  KnotFrame1 <- tkframe(left.frm, relief = "groove", borderwidth = 2, width=30)
  tkpack(tklabel(KnotFrame1, text = "Knots"), fill = "both", side = "top")

  entry.kt1 <- tkentry(KnotFrame1, textvariable = kt1, width="8")
  tkpack(ts2 <- tkscale(KnotFrame1, label = "Knot 1 :", command = replot,
                        from = 0, to = round(1.3*maxBin), showvalue = 1, variable = kt1,
                        resolution = 1, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")
  tkpack(entry.kt1, side = "right")

  # the rest of the knots
  for(i in 2:n){
    KnotFrame.i <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    assign(paste("KnotFrame",i,sep=""),KnotFrame.i)
    kt.i <- get(paste("kt",i,sep=""))
    entry.kt.i <- tkentry(KnotFrame.i, textvariable = kt.i, width="8")
    assign(paste("entry.kt.",i,sep=""),entry.kt.i)
    tkpack(ts.i <- tkscale(KnotFrame.i, label = paste("Knot ",i," :",sep=""), command = replot,
                          from = 0, to = round(1.3*maxBin), showvalue = 1, variable = kt.i,
                          resolution = 1, orient = "horiz", relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")
    tkpack(entry.kt.i, side = "right")
  }

  ####################################################################################
  ### parameters for slope at first and last knot
  SlopeFrame1 <- tkframe(right.frm, relief = "groove", borderwidth = 2)
  tkpack(tklabel(SlopeFrame1, text = "Slope parameters"), fill = "both", side = "top")

  entry.slope1 <- tkentry(SlopeFrame1, textvariable = slope1, width="8")
  tkpack(ts10 <- tkscale(SlopeFrame1, label = "Slope at first knot :", command = replot,
                         from = -1, to = 1, showvalue = 1, variable = slope1,
                         resolution = 0.01, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")
  tkpack(entry.slope1, side = "right")

  SlopeFrame2 <- tkframe(right.frm, relief = "groove", borderwidth = 2)
  entry.slope2 <- tkentry(SlopeFrame2, textvariable = slope2, width="8")
  tkpack(ts11 <- tkscale(SlopeFrame2, label = "Slope at last knot :", command = replot,
                         from = -5, to = 5, showvalue = 1, variable = slope2,
                         resolution = 0.01, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")
  tkpack(entry.slope2, side = "right")
  
  ####################################################################################
  ### parameters

  # first parameter
  ParFrame1 <- tkframe(right.frm, relief = "groove", borderwidth = 2)
  tkpack(tklabel(ParFrame1, text = "Spline parameters at knots"), fill = "both", side = "top")

  entry.sp1 <- tkentry(ParFrame1, textvariable = sp1, width="8")
  tkpack(ts6 <- tkscale(ParFrame1, label = "Parameter 1 :", command = replot,
                        from = -4, to = 4, showvalue = 1, variable = sp1,
                        resolution = 0.1, orient = "horiz", relief = "groove"),
         fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")
  tkpack(entry.sp1, side = "right")

  # the rest of the parameters
  for(i in 2:n){
    ParFrame.i <- tkframe(right.frm, relief = "groove", borderwidth = 2)
    assign(paste("ParFrame",i,sep=""),ParFrame.i)
    sp.i <- get(paste("sp",i,sep=""))
    entry.sp.i <- tkentry(ParFrame.i, textvariable = sp.i, width="8")
    assign(paste("entry.sp.",i,sep=""),entry.sp.i)
    tkpack(ts.i <- tkscale(ParFrame.i, label = paste("Parameter ",i," :",sep=""), command = replot,
                          from = -4, to = 4, showvalue = 1, variable = sp.i,
                          resolution = 0.1, orient = "horiz", relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")
    tkpack(entry.sp.i, side = "right")
  }


  ####################################################################################
  ### other stuff
  OnOK <- function() {
    replot()
  }
  OnQuit <- function() {
    kvec <- rep(NA,n)
    pvec <- rep(NA,n)
    for(i in 1:n){
      kvec[i] <- as.numeric(tclObj(get(paste("kt",i,sep=""))))
      pvec[i] <- as.numeric(tclObj(get(paste("sp",i,sep=""))))
    }
    slopes <- c(as.numeric(tclObj(slope1)),
                as.numeric(tclObj(slope2)))
    
    aux <- list(knots = kvec, slopes=slopes, params = pvec)
    assign("selfit.tmp", c(get("selfit.tmp", envir = selfit.env), aux),
           envir = selfit.env)

    tclvalue(done) <- 2
  }

  # this is the generalized version of the commented stuff below
  expr <- "tkpack(frame0, frame1, "
  for(i in 1:n) expr <- paste(expr, "KnotFrame",i,", ",sep="")
  expr <- paste(expr, "SlopeFrame1, SlopeFrame2, ")
  for(i in 1:n) expr <- paste(expr, "ParFrame",i,", ",sep="")
  expr <- paste(expr,'fill = "x")')

  eval(parse(text=expr))
  
  ## # Don't know how to generalize the following commands
  ## if(n==3)
  ##   tkpack(frame0, frame1, KnotFrame1, KnotFrame2, KnotFrame3, 
  ##          SlopeFrame1, SlopeFrame2,
  ##          ParFrame1, ParFrame2, ParFrame3, fill = "x")
  ## if(n==4)
  ##   tkpack(frame0, frame1, KnotFrame1, KnotFrame2, KnotFrame3, KnotFrame4,
  ##          SlopeFrame1, SlopeFrame2,
  ##          ParFrame1, ParFrame2, ParFrame3, ParFrame4, fill = "x")
  ## if(n==5)
  ##   tkpack(frame0, frame1, KnotFrame1, KnotFrame2, KnotFrame3, KnotFrame4, KnotFrame5,
  ##          SlopeFrame1, SlopeFrame2,
  ##          ParFrame1, ParFrame2, ParFrame3, ParFrame4, ParFrame5, fill = "x")
  ## if(n==6)
  ##   tkpack(frame0, frame1, KnotFrame1, KnotFrame2, KnotFrame3, KnotFrame4, KnotFrame5, KnotFrame6,
  ##          SlopeFrame1, SlopeFrame2,
  ##          ParFrame1, ParFrame2, ParFrame3, ParFrame4, ParFrame5, ParFrame6, fill = "x")
  ## if(n==7)
  ##   tkpack(frame0, frame1, KnotFrame1, KnotFrame2, KnotFrame3, KnotFrame4, KnotFrame5, KnotFrame6, KnotFrame7,
  ##          SlopeFrame1, SlopeFrame2,
  ##          ParFrame1, ParFrame2, ParFrame3, ParFrame4, ParFrame5, ParFrame6, ParFrame7, fill = "x")
  
  tkpack(left.frm, right.frm, side = "left", anchor = "n")

  q.but <- tkbutton(base, text = "Quit", command = OnQuit)
  tkpack(spec.frm)
  tkpack(q.but, side = "right")
  replot()

  tkwait.variable(done)
  tkdestroy(base)

  setwd(oldwd)
  if (!silent) {
    fit <- get("selfit.tmp", envir = selfit.env)
    return(fit)
  }
  else return(invisible())
} # end selfit_spline function

