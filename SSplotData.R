SSplotData <- function(replist,col="grey",datatypes="all"){

  ### get info from replist
  # dimensions
  startyr       <- replist$startyr
  endyr         <- replist$endyr
  nfleets       <- replist$nfleets
  fleetnames    <- replist$FleetNames
  
  # catch
  catch <- SSplotCatch(replist,plot=F,print=F,verbose=F)
  if(!is.null(catch$totcatchmat2)) catch <- catch$totcatchmat else
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

  ntypes <- 0
  typetable <- as.data.frame(matrix(NA,nrow=0,ncol=5))
  for(itype in 1:length(typenames)){
    dat <- get(typenames[itype])
    typename <- typenames[itype]
    print(typename)
    print(head(dat))
    if(!is.null(dat) && !is.na(dat) && nrow(dat)>0){
      ntypes <- ntypes+1
      for(ifleet in 1:nfleets){
        if(typename=="catch") allyrs <- dat$Yr[dat[,ifleet]>0]
        if(typename=="cpue") allyrs <- dat$Yr[dat$FleetNum==ifleet]
        if(length(grep("dbase",typename))>1) allyrs <- dat$Yr
        yrs <- sort(unique(floor(allyrs)))
        typetable <- rbind(typetable,
                           data.frame(yr=yrs,ifleet=ifleet,
                                      itype=ntypes,typename=typename))
      }
    }
  }
  return(typetable)
}
