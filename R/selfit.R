#' A function to visual parameterization of double normal and double logistic
#' selectivity in Stock Synthesis
#' 
#' A GUI interface for exploring selectivity.
#' 
#' 
#' @param minLength Minimum size to show
#' @param maxLength Maximum size to show
#' @param silent T/F switch to return fit at the end
#' @param init Optional initial values for the parameters
#' @author Tommy Garrison
#' @export
#' @seealso \code{\link{sel.line}}
#' @examples
#' 
#' \dontrun{
#' selfit()
#' }
#' 
selfit <-
function (minLength=10, maxLength=65, silent=FALSE,init=NULL)
{

################################################################################
#
# selfit   March 31, 2009.
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: Provide GUI for the plot function, sel.line
# Written: Tommy Garrison, UW
# Returns: plots double normal or double logistic selectivity
# General: parameterization matched Stock Synthesis v.3
# Required packages: tcltk
#
################################################################################

  #### the following commands no longer needed since packages are required by r4ss
    ## require(tcltk) || stop("package tcltk is required")
    geterrmessage()
    done <- tclVar(0)
    selfit.env <- new.env()
    assign("selfit.tmp", list(), envir = selfit.env)

    kernel <- tclVar("Double_Normal")
    minLB <- tclVar(minLength)
    maxLB <- tclVar(maxLength)
    if(!is.null(init)){
      if(length(init)==6) init <- c(init,0,0)
      if(length(init)==8){
        sp1 <- tclVar(init[1])
        sp2 <- tclVar(init[2])
        sp3 <- tclVar(init[3])
        sp4 <- tclVar(init[4])
        sp5 <- tclVar(init[5])
        sp6 <- tclVar(init[6])
        sp7 <- tclVar(init[7])
        sp8 <- tclVar(init[8])
      }else{
        stop("'init' input should be of length 8 (with dummy values in last 2 spots for double-normal)")
      }
    }else{
      sp1 <- tclVar(40)
      sp2 <- tclVar(0)
      sp3 <- tclVar(4.86)
      sp4 <- tclVar(5.68)
      sp5 <- tclVar(-10)
      sp6 <- tclVar(0.8)
      sp7 <- tclVar(-2.22)
      sp8 <- tclVar(10)
    }
    replot <- function(...) {
        k <- as.character(tclObj(kernel))
        minL <- as.numeric(tclObj(minLB))
        maxL <- as.numeric(tclObj(maxLB))
        p1 <- as.numeric(tclObj(sp1))
        p2 <- as.numeric(tclObj(sp2))
        p3 <- as.numeric(tclObj(sp3))
        p4 <- as.numeric(tclObj(sp4))
        p5 <- as.numeric(tclObj(sp5))
        p6 <- as.numeric(tclObj(sp6))
        p7 <- as.numeric(tclObj(sp7))
        p8 <- as.numeric(tclObj(sp8))

        plot(seq(2, maxLength, l=2), seq(0,1,l=2), type='n',
             xlab="length bin", ylab="selectivity")

        fit <- get("selfit.tmp", envir = selfit.env)

        lapply(fit, function(x) sel.line(seq(minL, maxL, by=2),
                                         model = x$model, sp = x$sp,
                                         min.dist = x$min.dist,
                                         max.dist = x$max.dist))

        if (k == "Double_Logistic") {
            sel.line(x = seq(minL, maxL, by=2), model = k,
                sp = c(p1,p2,p3,p4,p5,p6,p7,p8),
                min.dist = minL, max.dist = maxL)
        }
        if (k == "Double_Normal") {
            sel.line(x = seq(minL, maxL, by=2), model = k,
                sp = c(p1,p2,p3,p4,p5,p6),
                min.dist = minL, max.dist = maxL)
        }
    }

    redraw <- function(...) {
        var <- as.character(tclObj(kernel))

        if (var == "Double_Normal") {
            tkconfigure(ts8, state = "disabled")
            tkconfigure(ts9, state = "disabled")
            tkconfigure(entry.sp7, state = "disabled")
            tkconfigure(entry.sp8, state = "disabled")
        }

        if (var == "Double_Logistic") {
            tkconfigure(entry.sp7, state = "normal")
            tkconfigure(ts8, state = "normal")
            tkfocus(entry.sp7)
            tkconfigure(entry.sp8, state = "normal")
            tkconfigure(ts9, state = "normal")
        }

        replot()
    }

    base <- tktoplevel()
    tkwm.title(base, "Examine Selectivity Patterns")
    spec.frm <- tkframe(base, borderwidth = 2)
    left.frm <- tkframe(spec.frm)
    right.frm <- tkframe(spec.frm)

    frame0 <- tkframe(left.frm, relief = "groove", borderwidth = 2, width=30)
    tkpack(tklabel(frame0, text = "Length Parameters"),
           fill = "both", side = "top")

    entry.minLB <- tkentry(frame0, textvariable = minLB, width="8")
    tkpack(ts0 <- tkscale(frame0, label = "Min Length Bin", command = replot,
                          from = 0, to = minLength, showvalue = 1,
                          variable = minLB, resolution = 1, orient = "horiz",
                          relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2,
           ipady = 2, side = "left")
    tkpack(entry.minLB,  side = "right")

    frame1 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.maxLB <- tkentry(frame1, textvariable = maxLB, width="8")
    tkpack(ts1 <- tkscale(frame1, label = "Max Length Bin", command = replot,
                          from = 0, to = maxLength, showvalue = 1,
                          variable = maxLB, resolution = 1, orient = "horiz",
                          relief = "groove"),
           fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2,
           ipady = 2, side = "left")
    tkpack(entry.maxLB,  side = "right")

    frame3 <- tkframe(left.frm, relief = "groove", borderwidth = 2, width=30)
    tkpack(tklabel(frame3, text = "Function Parameters"), fill = "both",
           side = "top")

    entry.sp1 <- tkentry(frame3, textvariable = sp1, width="8")
    tkpack(ts2 <- tkscale(frame3, label = "Parameter 1 :", command = replot,
        from = 5, to = 200, showvalue = 1, variable = sp1,
        resolution = 0.01, orient = "horiz", relief = "groove"),
        fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2,
           side = "left")
    tkpack(entry.sp1, side = "right")

    frame4 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.sp2 <- tkentry(frame4, textvariable = sp2, width="8")
    tkpack(ts3 <- tkscale(frame4, label = "Parameter 2 :", command = replot,
        from = -5, to = 3, showvalue = 1, variable = sp2,
        resolution = 0.01, orient = "horiz", relief = "groove"),
        fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2,
           side = "left")
    tkpack(entry.sp2, side = "right")

    frame5 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.sp3 <- tkentry(frame5, textvariable = sp3, width="8")
    tkpack(ts4 <- tkscale(frame5, label = "Parameter 3 :", command = replot,
        from = -10, to = 10, showvalue = 1, variable = sp3,
        resolution = 0.01, orient = "horiz", relief = "groove"),
        fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2,
           side = "left")
    tkpack(entry.sp3, side = "right")

    frame6 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.sp4 <- tkentry(frame6, textvariable = sp4, width="8")
    tkpack(ts5 <- tkscale(frame6, label = "Parameter 4 :", command = replot,
        from = -10, to = 10, showvalue = 1, variable = sp4,
        resolution = 0.01, orient = "horiz", relief = "groove"),
        fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2,
           side = "left")
    tkpack(entry.sp4, side = "right")

    frame7 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.sp5 <- tkentry(frame7, textvariable = sp5, width="8")
    tkpack(ts6 <- tkscale(frame7, label = "Parameter 5 :", command = replot,
        from = -999, to = 10, showvalue = 1, variable = sp5,
        resolution = 0.1, orient = "horiz", relief = "groove"),
        fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2,
           side = "left")
    tkpack(entry.sp5, side = "right")

    frame8 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.sp6 <- tkentry(frame8, textvariable = sp6, width="8")
    tkpack(ts7 <- tkscale(frame8, label = "Parameter 6 :", command = replot,
        from = -999, to = 10, showvalue = 1, variable = sp6,
        resolution = 0.1, orient = "horiz", relief = "groove"),
        fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2,
           side = "left")
    tkpack(entry.sp6, side = "right")

    frame9 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.sp7 <- tkentry(frame9, textvariable = sp7, state = "disabled", width="8")
    tkpack(ts8 <- tkscale(frame9, label = "Parameter 7 :", command = replot,
        from = -10, to = 10, showvalue = 1, variable = sp7,
        state = "disabled", resolution = 0.01, orient = "horiz", relief = "groove"),
        fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")
    tkpack(entry.sp7, side = "right")

    frame10 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.sp8 <- tkentry(frame10, textvariable = sp8, state = "disabled", width="8")
    tkpack(ts9 <- tkscale(frame10, label = "Parameter 8 :", command = replot,
        from = 0, to = 20, showvalue = 1, variable = sp8,
        state = "disabled", resolution = 0.01, orient = "horiz", relief = "groove"),
        fill = "x", expand = 1, padx = 3, ipadx = 30, pady = 2, ipady = 2, side = "left")
    tkpack(entry.sp8, side = "right")

    frame2 <- tkframe(right.frm, relief = "groove", borderwidth = 2)
    tkpack(tklabel(frame2, text = "Function"))
    for (i in c("Double_Normal", "Double_Logistic")) {
        tmp <- tkradiobutton(frame2, command = redraw, text = i,
            value = i, variable = kernel)
        tkpack(tmp, anchor = "w")
    }

    OnOK <- function() {
        replot()
    }
    OnQuit <- function() {
        tclvalue(done) <- 2
    }
    OnClear <- function() {
        assign("selfit.tmp", list(), envir = selfit.env)
        plot(seq(2, maxLength, l=2), seq(0,1,l=2), type='n',
             xlab="length bins", ylab="selectivity")
    }
    OnSave <- function() {
        k <- as.character(tclObj(kernel))
        if (k == "Double_Logistic") {
		p7 <- as.numeric(tclObj(sp7))
            p8 <- as.numeric(tclObj(sp8))
        }
        if (k == "Double_Normal") {
            p7 <- NULL
            p8 <- NULL
        }
        minL <- as.numeric(tclObj(minLB))
        maxL <- as.numeric(tclObj(maxLB))
        p1 <- as.numeric(tclObj(sp1))
        p2 <- as.numeric(tclObj(sp2))
        p3 <- as.numeric(tclObj(sp3))
        p4 <- as.numeric(tclObj(sp4))
        p5 <- as.numeric(tclObj(sp5))
        p6 <- as.numeric(tclObj(sp6))

        aux <- list(model = k, sp = c(p1,p2,p3,p4,p5,p6,p7,p8),
                    min.dist = minL, max.dist = maxL)

        assign("selfit.tmp",
               c(get("selfit.tmp", envir = selfit.env), list(aux)),
               envir = selfit.env)
        replot()
    }

    tkpack(frame0, frame1, frame3, frame4, frame5, frame6,
           frame7, frame8, frame9, frame10, fill = "x")
    tkpack(frame2, fill = "x")
    tkpack(left.frm, right.frm, side = "left", anchor = "n")
    c.but <- tkbutton(base, text = "Clear", command = function() {
        OnClear()
    })

    q.but <- tkbutton(base, text = "Quit", command = OnQuit)
    save.but <- tkbutton(base, text = "Save", command = OnSave)
    tkpack(spec.frm)
    tkpack(q.but, side = "right")
    tkpack(c.but, side = "left")
    tkpack(save.but, side = "right")
    replot()
    tkbind(entry.sp7, "<Return>", function() {
        replot()
    })
    tkbind(entry.sp8, "<Return>", function() {
        replot()
    })
    tkbind(base, "<Destroy>", function() tclvalue(done) <- 2)
    tkwait.variable(done)
    tkdestroy(base)
    if (!silent) {
        fit <- get("selfit.tmp", envir = selfit.env)
        return(fit)
    }
    else return(invisible())
} # end selfit function

