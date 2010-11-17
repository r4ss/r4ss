SSchangeYears <-
  function(replist,nseasons=12,newstartyr,newstartseas,verbose=TRUE)
{
  # function for converting artificial seasons as years into real calendar years
  replist2 <- replist

  yradd <- newstartyr + (newstartseas-1)/nseasons - replist$startyr/nseasons
  
  getseas <- function(yrvec){
    newyrs <- yrvec / nseasons + yradd
    seas <- floor(1 + nseasons*(newyrs %% 1))
    return(seas)
  }
  getyr <- function(yrvec){
    newyrs <- floor(yrvec / nseasons + yradd)
  }

  # calculate new year values
  replist2$startyr            <- getyr(replist$startyr)
  replist2$endyr              <- getyr(replist$endyr)
  replist2$MGparmAdj$Yr       <- getyr(as.numeric(replist$MGparmAdj$Yr))
  replist2$SelSizeAdj$Yr      <- getyr(replist$SelSizeAdj$Yr)
  replist2$growthseries$Yr    <- getyr(replist$growthseries$Yr)
  replist2$sizeselex$year     <- getyr(replist$sizeselex$year)
  replist2$ageselex$year      <- getyr(replist$ageselex$year)
  replist2$timeseries$Yr      <- getyr(replist$timeseries$Yr)
  replist2$sprseries$Year     <- getyr(replist$sprseries$Year)
  replist2$recruit$year       <- getyr(replist$recruit$year)
  replist2$cpue$Yr            <- getyr(replist$cpue$Yr)
  replist2$natage$Yr          <- getyr(replist$natage$Yr)
  replist2$natlen$Yr          <- getyr(replist$natlen$Yr)
  replist2$catage$Yr          <- getyr(replist$catage$Yr)
  replist2$dbase$Yr           <- getyr(replist$dbase$Yr)
  replist2$recruitpars$Yr     <- getyr(replist$recruitpars$Yr)

  # for composition data
  replist2$lendbase$Yr      <- getyr(replist$lendbase$Yr)
  replist2$sizedbase$Yr     <- getyr(replist$sizedbase$Yr)
  replist2$agedbase$Yr      <- getyr(replist$agedbase$Yr)
  replist2$condbase$Yr      <- getyr(replist$condbase$Yr)
  replist2$ghostagedbase$Yr <- getyr(replist$ghostagedbase$Yr)
  replist2$ladbase$Yr       <- getyr(replist$ladbase$Yr)
  replist2$wadbase$Yr       <- getyr(replist$wadbase$Yr)
  replist2$tagdbase1$Yr     <- getyr(replist$tagdbase1$Yr)
  replist2$tagdbase2$Yr     <- getyr(replist$tagdbase2$Yr)

  # calculate new season values
  replist2$nseasons <- nseasons
  replist2$growthseries$Seas    <- getseas(replist$growthseries$Yr)
  replist2$ageselex$Seas        <- getseas(replist$ageselex$year)
  replist2$timeseries$Seas      <- getseas(replist$timeseries$Yr)
  replist2$cpue$Seas            <- getseas(replist$cpue$Yr)
  replist2$natage$Seas          <- getseas(replist$natage$Yr)
  replist2$natlen$Seas          <- getseas(replist$natlen$Yr)
  replist2$catage$Seas          <- getseas(replist$catage$Yr)

  # for composition data
  replist2$lendbase$Seas      <- getseas(replist$lendbase$Yr)
  replist2$sizedbase$Seas     <- getseas(replist$sizedbase$Yr)
  replist2$agedbase$Seas      <- getseas(replist$agedbase$Yr)
  replist2$condbase$Seas      <- getseas(replist$condbase$Yr)
  replist2$ghostagedbase$Seas <- getseas(replist$ghostagedbase$Yr)
  replist2$ladbase$Seas       <- getseas(replist$ladbase$Yr)
  replist2$wadbase$Seas       <- getseas(replist$wadbase$Yr)
  replist2$tagdbase1$Seas     <- getseas(replist$tagdbase1$Yr)
  replist2$tagdbase2$Seas     <- getseas(replist$tagdbase2$Yr)


  # fix stuff
  replist2$timeseries$Yr[1] <- replist2$startyr - 2
  replist2$timeseries$Yr[2] <- replist2$startyr - 1
  replist2$timeseries$Seas[1:2] <- 1

  # converting artificial seasons as years into real calendar years
  if(verbose){
    cat("converting year labels using formula: new year = old year/",nseasons," + ",yradd,"\n",sep="")
    cat("old and new timeseries values:\n")
    df <- cbind(replist$timeseries[c(2,4)],replist2$timeseries[c(2,4,3)])
    names(df) <- paste(c(rep("Old_",2),rep("New_",2),""),names(df),sep="")
    rownames(df) <- 1:nrow(df)
    df <- df[c(1:5,-(6:1)+nrow(df)),]
    df[6,] <- "..."
    rownames(df)[6] <- "..."
    print(df)
    cat("\nNote: check your results, the function may not work right.\nAlso, units for ages are still in number of seasons, not in years\n")
  }
  return(invisible(replist2))
}
