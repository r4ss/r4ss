SSplotYield <-
  function(replist,
           subplots=1:2,
           add=FALSE,plot=TRUE,print=FALSE,
           labels=c("Relative depletion", #1
             "Equilibrium yield (mt)",    #2
             "Total biomass (mt)",        #3
             "Surplus production (mt)"),  #4
           cex.main=1,
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,
           plotdir="default",
           verbose=TRUE)
{
  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  equil_yield <- replist$equil_yield
  nareas      <- replist$nareas
  nseasons    <- replist$nseasons
  timeseries  <- replist$timeseries
  SS_versionshort <- replist$SS_versionshort
  if(is.null(SS_versionshort)) SS_versionshort <- "older than SS-V3.20"
  
  # test if data is available
  if(!is.null(equil_yield[1,1]) && !is.na(equil_yield[1,1])){
    # function for yeild curve
    yieldfunc <- function(){
      plot(equil_yield$Depletion,equil_yield$Catch,xlab=labels[1],ylab=labels[2],
           type="l",lwd=2,col="blue")
      abline(h=0,col="grey")
      abline(v=0,col="grey")}

    # make plot
    if(1 %in% subplots){
      if(plot){yieldfunc()}
      if(print){
        pngfun(file=paste(plotdir,"23_yield.png",sep=""))
        yieldfunc()
        dev.off()}
    }
    if(verbose) cat("Finished plot 23: yield curve\n")
  }

  ts <- timeseries
  ts$Yr <- ts$Yr + (ts$Seas-1)/nseasons

  # get total biomass and total catch (across areas)
  arearows <- ts$Area==1
  Bio_all <- ts$Bio_all[arearows]
  if(SS_versionshort=="SS-V3.20") stringB <- "sel(B)" else stringB <- "enc(B)"
  
  totcatchmat <- as.matrix(ts[arearows, substr(names(ts),1,nchar(stringB))==stringB])

  if(nareas > 1){
    for(iarea in 2:nareas){
      arearows <- ts$Area==iarea
      Bio_all <- ts$Bio_all[arearows]
      totcatchmat <- totcatchmat + as.matrix(ts[arearows, substr(names(ts),1,nchar(stringB))==stringB])
    }
  }

  ls <- nrow(totcatchmat)
  sprodfunc <- function(){
    totcatch <- 0
    totcatch[3:ls] <- rowSums(totcatchmat)[3:ls]
    sprod <- NA
    sprod[3:(ls-1)] <- Bio_all[4:ls] - Bio_all[3:(ls-1)] + totcatch[3:(ls-1)]
    sprodgood <- !is.na(sprod)
    Bio_all_good <- Bio_all[sprodgood]
    sprod_good <- sprod[sprodgood]
    xlim <- c(0,max(Bio_all_good,na.rm=TRUE))
    ylim <- c(min(0,sprod_good,na.rm=TRUE),max(sprod_good,na.rm=TRUE))
    plot(Bio_all_good,sprod_good,ylim=ylim,xlim=xlim,xlab=labels[3],ylab=labels[4],type="l",col="black")

    # make arrows
    old_warn <- options()$warn      # previous setting
    options(warn=-1)                # turn off "zero-length arrow" warning
    s <- seq(length(sprod_good)-1)
    arrows(Bio_all_good[s],sprod_good[s],Bio_all_good[s+1],sprod_good[s+1],length=0.06,angle=20,col="black",lwd=1.2)
    options(warn=old_warn)  #returning to old value
    
    abline(h=0,col="grey")
    abline(v=0,col="grey")
    points(Bio_all_good[1],sprod_good[1],col="blue",pch=19)
  } # end sprodfunc

  if(2 %in% subplots){
    if(plot){sprodfunc()}
    if(print){
      pngfun(file=paste(plotdir,"23_surplus_prod.png",sep=""))
      sprodfunc()
      dev.off()}
  }
  if(verbose) cat("Finished plot 23: Surplus production\n")
} # end function
