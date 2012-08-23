SSplotDiscard <-
  function(replist,subplots=1:2,
           plot=TRUE,print=FALSE,
           plotdir="default",
           fleets="all",
           fleetnames="default",
           datplot=FALSE,
           labels=c("Year",
           "Discard fraction",
           "Total discards",
           "for"),
           yhi=1,
           col1="blue", col2="red",
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           verbose=TRUE)
{
  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  # get stuff from replist
  nfishfleets     <- replist$nfishfleets
  discard         <- replist$discard
  FleetNames      <- replist$FleetNames
  DF_discard      <- replist$DF_discard   # used in SSv3.11
  discard_type    <- replist$discard_type # used in SSv3.11
  discard_spec    <- replist$discard_spec # used in SSv3.20
  if(fleetnames[1]=="default") fleetnames <- FleetNames
  if(plotdir=="default") plotdir <- replist$inputs$dir

  # if discards exist
  if(!is.na(discard) && nrow(discard)>0){
    if(fleets[1]=="all") fleets <- 1:nfishfleets
    fleets <- intersect(fleets,discard$FleetNum)
    for(FleetNum in fleets){
      # table available beginning with SSv3.20 has fleet-specific discard specs
      if(!is.null(discard_spec)){ 
        DF_discard <- discard_spec$errtype[discard_spec$Fleet==FleetNum]
      }
      usedisc <- discard[discard$FleetNum==FleetNum,]
      FleetName <- usedisc$FleetName[1]

      yr <- as.numeric(usedisc$Yr)
      ob <- as.numeric(usedisc$Obs)
      std <- as.numeric(usedisc$Std_use)
      if(DF_discard == -2){ # lognormal with std as interpreted as the standard error (in log space) of the observation
        liw <- ob - qlnorm(0.025,log(ob),std)
        uiw <- qlnorm(0.975,log(ob),std) - ob
      }
      if(DF_discard == -1){ # normal with std as std
        liw <- ob - qnorm(0.025,ob,std)
        uiw <- qnorm(0.975,ob,std) - ob
      }
      if(DF_discard == 0){  # normal with std interpreted as CV
        liw <- ob - qnorm(0.025,ob,std*ob)
        uiw <- qnorm(0.975,ob,std*ob) - ob
      }
      if(DF_discard > 0){ # t-distribution with DF_discard = degrees of freedom
        liw <- -std*qt(0.025,DF_discard) # quantile of t-distribution
        uiw <- std*qt(0.975,DF_discard) # quantile of t-distribution
      }
      liw[(ob-liw)<0] <- ob[(ob-liw)<0] # no negative limits
      xlim <- c((min(yr)-3),(max(yr)+3))
      if(!is.na(discard_type)){ # SSv3.11
        if(grepl("as_fraction",discard_type)){
          # discards as a fraction
          title <- paste("Discard fraction for",FleetName)
          ylab <- "Discard fraction"
        }else{
          # discards in same units as catch, or in numbers (should distinguish in the future)
          title <- paste("Total discard for",FleetName)
          ylab <- "Total discards"
        }
      }else{ # SSv3.20 and beyond
        ## 1:  discard_in_biomass(mt)_or_numbers(1000s)_to_match_catchunits_of_fleet
        ## 2:  discard_as_fraction_of_total_catch(based_on_bio_or_num_depending_on_fleet_catchunits)
        ## 3:  discard_as_numbers(1000s)_regardless_of_fleet_catchunits
        discard_units <- discard_spec$units[discard_spec$Fleet==FleetNum]
        if(discard_units==1){
          # type 1: biomass or numbers
          #         someday could make labels more specific based on catch units
          title <- paste("Total discard for",FleetName)
          ylab <- "Total discards"
        }
        if(discard_units==2){
          # type 2: discards as fractions
          title <- paste("Discard fraction for",FleetName)
          ylab <- "Discard fraction"
        }
        if(discard_units==3){
          # type 3: discards as numbers
          title <- paste("Total discard for",FleetName)
          ylab <- "Total discards (1000's)"
        }
      }

      # wrap up plot command in function
      dfracfunc <- function(addfit){
        plotCI(x=yr,y=ob,uiw=uiw,liw=liw,ylab=ylab,xlab=labels[1],main=title,
               ylo=0,yhi=yhi,col=col2,sfrac=0.001,lty=1,xlim=xlim,
               ymax=max(usedisc$Exp,na.rm=TRUE))
        abline(h=0,col="grey")
        if(addfit) points(yr,usedisc$Exp,col=col1,pch="-",cex=2)
      }

      # make plots
      if(!datplot) subplots <- setdiff(subplots,1) # don't do subplot 1 if datplot=FALSE
      for(isubplot in subplots){ # loop over subplots (data only or with fit)
        if(isubplot==1) addfit <- FALSE else addfit <- TRUE
        if(plot) dfracfunc(addfit=addfit)
        if(print) {
          if(datplot){
            file <- paste(plotdir,"discard_data",FleetName,".png",sep="")
          }else{
            file <- paste(plotdir,"discard_fit",FleetName,".png",sep="")
          }
          caption <- title
          plotinfo <- pngfun(file=file, caption=caption)
          dfracfunc(addfit=addfit)
          dev.off()
        }
      } # end loop over subplots
    } # discard series
    #if(verbose) cat("Finished discard plot\n")
  }
  if(!is.null(plotinfo)) plotinfo$category <- "Discard"
  return(invisible(plotinfo))
} # end of function
