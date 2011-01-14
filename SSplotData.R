SSplotData <- function(replist,col="grey",datatypes="all",fleets="all"){

  ### get info from replist
  # dimensions
  startyr       <- replist$startyr
  endyr         <- replist$endyr
  nfleets       <- replist$nfleets
  fleetnames    <- replist$FleetNames
  
  # catch
  catch <- SSplotCatch(replist,plot=F,print=F,verbose=F)
  if(is.null(catch$totcatchmat2)) catch <- catch$totcatchmat else
                                  catch <- catch$totcatchmat2
  # index
  cpue          <- replist$cpue
  
  # composition data
  lendbase      <- replist$lendbase
  sizedbase     <- replist$sizedbase
  agedbase      <- replist$agedbase
  condbase      <- replist$condbase
  ghostagedbase <- replist$ghostagedbase
  ghostcondbase <- replist$ghostcondbase
  ghostlendbase <- replist$ghostlendbase
  ladbase       <- replist$ladbase
  wadbase       <- replist$wadbase
  tagdbase1     <- replist$tagdbase1
  tagdbase2     <- replist$tagdbase2

  typenames <- c("catch","cpue","lendbase","sizedbase","agedbase","condbase",
                 "ghostagedbase","ghostcondbase","ghostlendbase","ladbase",
                 "wadbase","tagdbase1","tagdbase2")

  # loop over types to make a database of years with comp data
  ntypes <- 0
  typetable <- as.data.frame(matrix(NA,nrow=0,ncol=5))
  for(itype in 1:length(typenames)){
    dat <- get(typenames[itype])
    typename <- typenames[itype]
    if(!is.null(dat) && !is.na(dat) && nrow(dat)>0){
      ntypes <- ntypes+1
      for(ifleet in 1:nfleets){
        # identify years from different data types
        if(typename=="catch") allyrs <- dat$Yr[dat[,ifleet]>0]
        if(typename=="cpue") allyrs <- dat$Yr[dat$FleetNum==ifleet]
        if(length(grep("dbase",typename))>1) allyrs <- dat$Yr
        # expand table of years with data
        yrs <- sort(unique(floor(allyrs)))
        if(length(yrs)>0)
          typetable <- rbind(typetable,
                             data.frame(yr=yrs,fleet=ifleet,
                                        itype=ntypes,typename=typename))
      }
    }
  }
  # typetable is full data frame of all fleets and data types
  # typetable2 has been subset according to requested choices
  
  # subset by fleets and data types if requested
  if(fleets[1]=="all") fleets <- 1:nfleets
  if(datatypes[1]=="all") datatypes <- typenames
  typetable2 <- typetable[typetable$fleet %in% fleets &
                          typetable$typename %in% datatypes,]
  
  return(invisible(typetable2))
  
}
