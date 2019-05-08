#' Plot movement rates from model output
#' 
#' Plots estimated movement rates in final year for each area/seaon with movement as
#' reported in Report.sso. If movement is time-varying, an additional figure shows
#' pattern across years.
#' 
#' 
#' @param replist list created by \code{\link{SS_output}}
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param subplots which subplots to create
#' @param plotdir where to put the plots (uses model directory by default)
#' @param colvec vector of colors for each movement rate in the plot
#' @param ylim optional input for y range of the plot. By default plot ranges
#' from 0 to 10\% above highest movement rate (not including fish staying in an
#' area).
#' @param legend add a legend designating which color goes with which pair of
#' areas?
#' @param legendloc location passed to legend function (if used)
#' @param moveseas choice of season for which movement rates are shown
#' @param min.move.age Minimum age of movement (in future will come from Report file)
#' @param pwidth width of plot
#' @param pheight height of plot
#' @param punits units for PNG file
#' @param res resolution for PNG file
#' @param ptsize point size for PNG file
#' @param cex.main Character expansion parameter for plot titles
#' @param verbose Print information on function progress.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_output}}, \code{\link{SSplotMovementRates}},
#' @examples
#' 
#'   \dontrun{
#'     SSplotMovementRates(myreplist)
#'   }
#' 
SSplotMovementRates <-
  function(replist, plot=TRUE, print=FALSE, subplots=1:2,
           plotdir="default",
           colvec="default", ylim="default", 
           legend=TRUE, legendloc="topleft",
           moveseas="all", min.move.age=0.5,
           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1,
           verbose=TRUE)
{
  #if(verbose) cat("Running SSplotMovementRates function\n")

  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  if(plotdir=="default") plotdir <- replist$inputs$dir
  
  # get values from replist
  accuage    <- replist$accuage
  move       <- replist$movement
  nseasons   <- replist$nseasons
  min.move.age   <- min.move.age # need to get min.move.age into repfile somewhere
  seasdur    <- replist$seasdurations
  parameters <- replist$parameters
  accuage    <- replist$accuage
  nareas     <- replist$nareas
  MGparmAdj  <- replist$MGparmAdj

  # some empty value to be replaced in subplot 2
  moveByYr   <- NULL 
  moveinfo   <- NULL

  # subplot 1: movement in end year
  if(1 %in% subplots){
    if(verbose) cat("  running subplot 1: movement rates in final year\n")
    
    if(moveseas[1]=="all") moveseas <- sort(unique(move$Seas))
    for(iseas in moveseas){
      move2   <- move[move$Seas==moveseas[iseas] &
                      move$Source_area!=move$Dest_area,]
      
      if(nrow(move2)==0){
        if(verbose) cat("Skipping movement rate plot: no movement in season",moveseas[iseas],"\n")
      }else{
        move3 <- move2[,-(1:6)]
        
        if(colvec[1]=="default") colvec=rich.colors.short(nrow(move2))
        if(ylim[1]=="default") ylim=c(0,1.1*max(move))
        main <- paste("Movement rates\n(fraction moving per year in season ",moveseas[iseas],")",sep="")
        # bundle plot as function below
        move.endyr.fn <- function(){
          matplot(0:accuage,t(move3),
                  type='l',lwd=3,col=colvec,
                  ylab="Movement rate",xlab="Age (years)",
                  main=main,
                  cex.main=cex.main)
          abline(h=0,col='grey')
          if(legend){
            legend(legendloc,lwd=3,bty="n",
                   col=colvec,lty=1:nrow(move2),
                   legend=paste("area",move2$Source_area,"to area",move2$Dest_area)
                   )
          }
        }
        if(plot) move.endyr.fn()
        if(print){
          file <- "move1_movement_rates.png"
          caption <- main
          plotinfo <- pngfun(file=file, caption=caption)
          move.endyr.fn()
          dev.off()
        }
      }
    } # end loop over seasons
  } # end subplot 1

  # subplot 2: time-varying movement
  if(2 %in% subplots){
    # subset some report values
    movepars <- parameters[grep("Move",replist$parameters$Label),]
    MGparmAdj <- MGparmAdj[,c(1,grep("MoveParm",names(MGparmAdj)))]
    # exclude forecast years (values were reported as zeros)
    MGparmAdj <- MGparmAdj[MGparmAdj$Yr <= replist$endyr,]
    time <- any(apply(MGparmAdj[,-1], 2, function(x){any(x!=x[1])}))
    if(time){
      warning("plot of time-varying movement rates not currently working")
    if(FALSE){
      if(verbose) cat("  running subplot 2: time-varying movement rates\n")
      moveinfo <- move[,1:6]
      moveinfo$LabelBase2 <- paste("seas_",moveinfo$Seas,"_GP_",moveinfo$GP,
                                   "from_",moveinfo$Source,"to_",moveinfo$Dest,sep="")
      moveinfo <- moveinfo[moveinfo$LabelBase2 %in% substring(movepars$Label,12),]
      ## print(moveinfo)
      nmoves <- nrow(moveinfo)
      if(verbose) cat("  N movement rates:",nmoves,"\n")
      if(nareas > 2){
        cat("  WARNING: time-varying movement plots not yet configured",
            "for models with N areas > 2\n")
      }else{
        yrvec <- replist$startyr:replist$endyr
        nyrs <- length(yrvec)

        ## if(nmoves*2 != nrow(movepars)){
        ##   cat("warning!: In SSplotMovementRates function.\n
        ##                Problem with number of parameters.\n
        ##                2*nrow(moveinfo)=",2*nrow(moveinfo),"\n,
        ##                nrow(movepars)  =",nrow(movepars),"\n")
        ## }

        movecalc <- function(min.move.age, accuage, minage, maxage,
                             valueA, valueB, from, to, seasdur) {
          # subfunction to calculate movement rates
          # similar to one in the "movepars" function.
          # in the future, these could be generalized and stand-alone in the r4ss package
          veclengths <- unique(c(length(minage),length(maxage),
                                 length(valueA),length(valueB),
                                 length(from),length(to)))
          if(length(veclengths)!=1){
            stop("Error! input vectors  minage, maxage, valueA, valueB, from, and to need to all have the same length.")
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
                movemat1[ipar,iage] <-
                  valueA[ipar] + (agevec[iage]-minage[ipar])*temp1[ipar]
              }
            }
          }
          movemat1 <- exp(movemat1)
          movemat1[from!=to, ] <- (1/nseasons)*movemat1[from!=to, ]
          movemat2 <- movemat1/matrix(apply(movemat1,2,sum),npars,nages,byrow=T)
          names <- paste("from_",from,"to_",to,sep="")
          # fix movement at 0 for when from and to areas don't match
          movemat2[,0:accuage < min.move.age] <- from==to
          rownames(movemat2) <- names

          return(movemat2)
        } # end movecalc subfunction

        # make an array of movement rates by source area, age, destination area, and year
        moveByYr <- array(NA,
                          dim=c(accuage+1, nyrs, nmoves),
                          dimnames=list(age=0:accuage,
                              yr=yrvec, movement=1:nmoves))
        for(iyr in 1:nyrs){
          y <- yrvec[iyr]
          for(imove in 1:nmoves){
            LabelA <- paste("MoveParm_A_",moveinfo$LabelBase2[imove],sep="")
            LabelB <- paste("MoveParm_B_",moveinfo$LabelBase2[imove],sep="")
            seas <- moveinfo$Seas[imove]
            basevalueA <- movepars$Value[movepars$Label==LabelA]
            basevalueB <- movepars$Value[movepars$Label==LabelB]
            valueA <- MGparmAdj[[LabelA]][MGparmAdj$Yr==y]
            valueB <- MGparmAdj[[LabelB]][MGparmAdj$Yr==y]
            # fill in array
            moveByYr[ , iyr, imove] <-
              movecalc(min.move.age = min.move.age,
                       accuage  = accuage,
                       minage   = rep(moveinfo$minage[imove],2),
                       maxage   = rep(moveinfo$maxage[imove],2),
                       valueA   = c(valueA,0),
                       valueB   = c(valueB,0),
                       from     = rep(moveinfo$Source_area[imove],2),
                       to       = c(moveinfo$Dest_area[imove],moveinfo$Source_area[imove]),
                       seasdur  = seasdur[seas]
                       )
          } # end loop over movement definitions
        } # end loop over years to calculate moveByYr array

        # make plots
        cat("Warning! Time-varying movement plots are experimental and might be totally wrong\n")
        for(imove in 1:nmoves){
          Source_area <- moveinfo$Source_area[imove]
          Dest_area <- moveinfo$Dest_area[imove]
          movetable <- moveByYr[, , imove]
          ### not sure why following line was present, removing on 10 May 2018
          #movetable <- moveByYr[1, ,imove,] 
          main <- paste("Time-varying movement from area", Source_area,
                        "to area", Dest_area)
          move.mountains.fn <- function(){
            mountains(zmat=t(movetable),xvec=0:accuage,yvec=yrvec,xlab='Age',ylab='Year')
            title(main=main,cex.main=cex.main)
          }
          
          if(plot) move.mountains.fn()
          if(print){
            file <- paste0("move2_time-varying_movement_rates_",
                           Source_area, "to", Dest_area, ".png")
            caption <- main
            plotinfo <- pngfun(file=file, caption=caption)
            move.mountains.fn()
            dev.off()
          }
        }
      } # end check for Nareas > 2
    } # end if(FALSE) turning off section that isn't working
    } # end check for time-varying movement
  } # end subplot 2
  returnlist <- list()
  if(!is.null(moveByYr))
    returnlist <- list(moveinfo=moveinfo, moveByYr=moveByYr)
  if(!is.null(plotinfo)){
    plotinfo$category <- "Move"
    returnlist$plotinfo <- plotinfo
  }
  return(invisible(returnlist))
}
