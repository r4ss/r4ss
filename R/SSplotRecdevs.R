#' Plot recruitment deviations
#' 
#' Plot recruitment deviations and associated quantities including derived
#' measures related to bias adjustment.
#' 
#' 
#' @param replist list created by \code{SSoutput}
#' @param subplots vector controlling which subplots to create
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param add add to existing plot (not yet implemented)
#' @param uncertainty include plots showing uncertainty?
#' @param forecastplot include points from forecast years?
#' @param col1 first color used
#' @param col2 second color used
#' @param col3 third color used
#' @param col4 fourth color used
#' @param legendloc location of legend. see ?legend for more info
#' @param labels vector of labels for plots (titles and axis labels)
#' @param pwidth width of plot
#' @param pheight height of plot
#' @param punits units for PNG file
#' @param res resolution for PNG file
#' @param ptsize point size for PNG file
#' @param cex.main character expansion for plot titles
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param verbose report progress to R GUI?
#' @author Ian Taylor, Ian Stewart
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_fitbiasramp}}
SSplotRecdevs <-
  function(replist, subplots=1:3, plot=TRUE, print=FALSE, add=FALSE,
           uncertainty=TRUE,forecastplot=FALSE,
           col1="black",col2="blue",col3="green3",col4="red",
           legendloc="topleft",
           labels=c("Year",                        #1
             "Asymptotic standard error estimate", #2
             "Log recruitment deviation",          #3
             "Bias adjustment fraction, 1 - stddev^2 / sigmaR^2"), #4
           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,
           cex.main=1, plotdir="default",
           verbose=TRUE)
{
  # Plot of recrecruitment deviations,  asymptotic error check, and bias adjustment

  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL
  if(plotdir=="default") plotdir <- replist$inputs$dir

  parameters <- replist$parameters
  recruit    <- replist$recruit
  startyr    <- replist$startyr
  endyr      <- replist$endyr
  sigma_R_in <- replist$sigma_R_in


  recdevEarly <- parameters[substring(parameters$Label,1,13) %in% c("Early_RecrDev"),]
  early_initage <- parameters[substring(parameters$Label,1,13) %in% c("Early_InitAge"),]
  main_initage <- parameters[substring(parameters$Label,1,12) %in% c("Main_InitAge"),]
  recdev <- parameters[substring(parameters$Label,1,12) %in% c("Main_RecrDev"),]
  recdevFore <- parameters[substring(parameters$Label,1,8)=="ForeRecr",]
  recdevLate <- parameters[substring(parameters$Label,1,12)=="Late_RecrDev",]

  if(nrow(recdev)==0 || max(recdev$Value)==0){
    if(verbose) cat("Skipped SSplotrecdevs - no rec devs estimated\n")
  }else{
    if(nrow(recdev)>0){
      # early
      recdev$Yr <- as.numeric(substring(recdev$Label,14))
      if(nrow(recdevEarly)>0){
        recdevEarly$Yr <- as.numeric(substring(recdevEarly$Label,15))
      }else{
        recdevEarly$Yr <- integer(0) # empty value to add column to data.frame with 0 rows
      }
      if(nrow(early_initage)>0){
        early_initage$Yr <- startyr - as.numeric(substring(early_initage$Label,15))
        recdevEarly <- rbind(early_initage,recdevEarly)
      }
      # main
      if(nrow(main_initage)>0){
        main_initage$Yr <- startyr - as.numeric(substring(main_initage$Label,14))
        recdev <- rbind(main_initage,recdev)
      }
      # forecast
      if(nrow(recdevFore)>0){
        recdevFore$Yr <- as.numeric(substring(recdevFore$Label,10))
      }else{
        recdevFore$Yr <- NULL
      }
      if(nrow(recdevLate)>0){
        recdevLate$Yr <- as.numeric(substring(recdevLate$Label,14))
        recdevFore <- rbind(recdevLate,recdevFore)
      }

      Yr <- c(recdevEarly$Yr,recdev$Yr,recdevFore$Yr)
      if(forecastplot){
        goodyrs <- rep(TRUE,length(Yr))
      }else{
        goodyrs <- Yr<=endyr+1 # TRUE/FALSE of in range or not
      }
      xlim <- range(Yr[goodyrs],na.rm=TRUE)
      ylim <- range(c(recdevEarly$Value,recdev$Value,recdevFore$Value)[goodyrs],
                    na.rm=TRUE)

      recdevfunc <- function(uncertainty){
        # recdevs with uncertainty intervals
        alldevs <- rbind(recdevEarly, recdev, recdevFore)[goodyrs,]

        colvec <- c(rep(col2,nrow(recdevEarly)),
                    rep(col1,nrow(recdev)),
                    rep(col2,nrow(recdevFore)))[goodyrs]
        ## alldevs$Parm_StDev[is.na(alldevs$Parm_StDev)] <- 0
        val <- alldevs$Value[goodyrs]
        Yr <- alldevs$Yr[goodyrs]
        if(uncertainty){
          std <- alldevs$Parm_StDev[goodyrs]
          recdev_hi <- val + 1.96*std
          recdev_lo <- val - 1.96*std
          ylim <- range(recdev_hi,recdev_lo,na.rm=TRUE)
        }else{
          ylim <- range(val)
        }
        plot(Yr,Yr,type="n",xlab=labels[1],
             ylab=labels[3],ylim=ylim)
        abline(h=0,col="grey")
        if(uncertainty) arrows(Yr,recdev_lo,Yr,recdev_hi,length=0.03,code=3,angle=90,lwd=1.2,col=colvec)
        lines(Yr,val,lty=3)
        points(Yr,val,pch=16,col=colvec)

      }

      # the following code only applies when uncertainty was computed
      if(uncertainty){
        recdevfunc3 <- function()
        {
          # std. dev. of recdevs
          par(mar=par("mar")[c(1:3,2)])
          ymax <- 1.1*max(recdev$Parm_StDev,recdevEarly$Parm_StDev,recdevFore$Parm_StDev,sigma_R_in,na.rm=TRUE)
          plot(recdev$Yr,recdev$Parm_StDev,xlab=labels[1],
               main="Recruitment deviation variance",cex.main=cex.main,
               ylab=labels[2],xlim=xlim,ylim=c(0,ymax),type="b")
          if(nrow(recdevEarly)>0)
              lines(recdevEarly$Yr,recdevEarly$Parm_StDev,type="b",col=col2)
          if(forecastplot & nrow(recdevFore)>0)
              lines(recdevFore$Yr,recdevFore$Parm_StDev,type="b",col=col2)
          abline(h=0,col="grey")
          abline(h=sigma_R_in,col=col4)

          ## # bias correction (2nd axis, scaled by ymax)
          ## lines(recruit$year,ymax*recruit$biasadj,col=col3)
          ## abline(h=ymax*1,col=col3,lty=3)
          ## ypts <- pretty(0:1)
          ## axis(side=4,at=ymax*ypts,label=ypts)
          ## mtext("Bias adjustment fraction",side=4,line=3,cex=par()$cex)
        }
      } # end if uncertainty==TRUE

      if(plot){ # if plotting to screen or PDF
        if(1 %in% subplots) recdevfunc(uncertainty=FALSE)
        if(uncertainty){
          if(2 %in% subplots) recdevfunc(uncertainty=TRUE)
          if(3 %in% subplots) recdevfunc3()
        }
      }
      if(print){ # if printing to PNG files
        if(1 %in% subplots){
          file <- "recdevs1_points.png"
          caption <- "Recruitment deviations"
          plotinfo <- pngfun(file=file, caption=caption)
          recdevfunc(uncertainty=FALSE)
          dev.off()
        }
        if(uncertainty){
          if(2 %in% subplots){
            file <- "recdevs2_withbars.png"
            caption <- "Recruitment deviations with 95% intervals"
            plotinfo <- pngfun(file=file, caption=caption)
            recdevfunc(uncertainty=TRUE)
            dev.off()
          }
          if(3 %in% subplots){
            file <- "recdevs3_varcheck.png"
            caption <-
              paste("Recruitment deviations variance check.<br>",
                    "See later figure of transformed variance values for comparison",
                    "with bias adjustment settings in the model.")
            plotinfo <- pngfun(file=file, caption=caption)
            recdevfunc3()
            dev.off()
          }
        } # end if uncertinaty
      } # end if print
    } # end if nrow(recdevs)>0
  } # end if max(recdev)>0
  if(!is.null(plotinfo)) plotinfo$category <- "RecDev"
  return(invisible(plotinfo))
} # end function
