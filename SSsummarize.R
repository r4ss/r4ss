SSsummarize <- function(biglist,
                        keyvec=NULL,
                        numvec=NULL,
                        selfactor=c("Lsel"),
                        selfleet=NULL,
                        selyr="min",
                        selgender=1){
  ## subfunction to extract recruitment deviations from parameter list
  biglistkeys <- substring(names(biglist),8)

  ## check inputs in keyvec or numvec
  # too many inputs
  if(!is.null(keyvec) & !is.null(numvec))
    stop("don't input both 'keyvec' and 'numvec'")
  if(!is.null(keyvec) & is.null(numvec)){
    # if only keyvec is supplied, check to make sure it works
    if(length(setdiff(keyvec,biglistkeys)) > 0)
      stop("'keyvec' should include strings that follow 'replist_' in names(biglist)")
    n <- length(keyvec)
  }
  if(is.null(keyvec) & !is.null(numvec)){
    # if only numvec is supplied, check to make sure it works
    if(length(setdiff(numvec,1:length(biglist))) > 0)
      stop("'numvec' should include indices of elements of biglist")
    n <- length(numvec)
  }
  # no inputs to subset elements
  if(is.null(keyvec) & is.null(numvec)){
    keyvec <- substring(names(biglist),8)
    n <- length(biglist)
  }

  # loop over outputs to create list of parameters, derived quantities, and years
  parnames <- NULL
  dernames <- NULL
  allyears <- NULL

  for(imodel in 1:n){
    element <- (1:length(biglist))[biglistkeys==keyvec[imodel]]
    if(length(element)!=1)
      stop("error with keyvec, element =",element,"\n",
           "keyvec[i] =",keyvec[imodel])
    stats <- biglist[[element]]
    parnames <- union(parnames,stats$parameters$Label)
    dernames <- union(dernames,stats$derived_quants$LABEL)
    allyears <- union(allyears,stats$timeseries$Yr)
  }
  allyears <- sort(allyears) # not actually getting any timeseries stuff yet

  # objects to store quantities
  sel        <- NULL
  selexlist  <- list()
  pars <- parstds <- parphases <- as.data.frame(matrix(NA,nrow=length(parnames),ncol=n))
  quants <- quantstds <- as.data.frame(matrix(NA,nrow=length(dernames),ncol=n))
  growth     <- NULL
  maxgrad    <- NULL
  ## SpawnBio <- depl  <- as.data.frame(matrix(NA,nrow=length(allyears),ncol=n))

  # notes about what runs were used
  sim        <- NULL
  keyvec2    <- NULL
  listnames  <- NULL

  warn <- FALSE # flag for whether filter warning has been printed or not

  # loop over outputs within biglist
  for(imodel in 1:n){
    element <- (1:length(biglist))[biglistkeys==keyvec[imodel]]
    stats <- biglist[[element]]
    listname <- names(biglist)[element]

    key <- as.character(stats$key)
    if(length(key)==0 | is.null(key)) key <- imodel
    keyvec2 <- c(keyvec2,key)
    cat("imodel=",imodel,"/",n,", element=",element,",",substring("      ",nchar(i)+nchar(element)),"got ", listname,sep="")

    # gradient
    maxgrad <- c(maxgrad, stats$maximum_gradient_component)

    # selex
    if(FALSE){
      seltemp <- stats$sizeselex
      if(is.null(selfactor)) selfactor <- unique(seltemp$Factor)
      if(is.null(selfleet))  selfleet  <- unique(seltemp$Fleet)
      if(is.null(selgender)) selgender <- unique(seltemp$gender)
      if(is.null(selyr))   selyr   <- unique(seltemp$year)
      if(selyr[1]=="min")  selyr   <- min(seltemp$year)
      if(selyr[1]=="max")  selyr   <- max(seltemp$year)

      for(iselfactor in 1:length(selfactor)){
        for(iselfleet in 1:length(selfleet)){
          for(iselyr in 1:length(selyr)){
            for(iselgender in 1:length(selgender)){
              seltemp_i <- seltemp[seltemp$Factor==selfactor[iselfactor] &
                                   seltemp$Fleet==selfleet[iselfleet] &
                                   seltemp$gender==selgender[iselgender] &
                                   seltemp$year==selyr[iselyr],]
              mylabel <- seltemp_i$label
              myname <- paste("sizeselex_",mylabel,sep="")
              seltemp_i2 <- as.numeric(seltemp_i[-(1:5)])
              selexlist[[myname]] <- rbind(selexlist[[myname]],seltemp_i2)
              rownames(selexlist[[myname]])[nrow(selexlist[[myname]])] <- key
            }
          }
        }
      }
    }
    
    ## growth
    growthtemp <- stats$growthseries
    imorphf <- ifelse(max(stats$morph_indexing$Index)==10,3,1)
    growthtemp <- growthtemp[growthtemp$Morph==imorphf,-(1:4)]
    growth <- cbind(growth, as.numeric(growthtemp[nrow(growthtemp),]))

    ## compile parameters
    parstemp <- stats$parameters
    # print(head(parstemp))
    
    for(ipar in 1:nrow(parstemp)){
      pars[parnames==parstemp$Label[ipar], imodel] <- parstemp$Value[ipar]
      parstds[parnames==parstemp$Label[ipar], imodel] <- parstemp$Parm_StDev[ipar]
      parphases[parnames==parstemp$Label[ipar], imodel] <- parstemp$Phase[ipar]
    }
    cat(",  N active pars=",sum(!is.na(parstemp$Active_Cnt)),"\n",sep="")
    
    ## compile derived quantities
    quantstemp <- stats$derived_quants
    for(iquant in 1:nrow(quantstemp)){
      quants[dernames==quantstemp$LABEL[iquant], imodel] <- quantstemp$Value[iquant]
      quantstds[dernames==quantstemp$LABEL[iquant], imodel] <- quantstemp$StdDev[iquant]
    }
  } # end loop over keys

  if(!setequal(keyvec,keyvec2)){
    cat("problem with keys!\nkeyvec:\n")
    print(keyvec)
    cat("keyvec2:\n")
    print(keyvec2)
  }else{
    names(pars) <- names(parstds) <- keyvec2
    names(quants) <- names(quantstds) <- keyvec2
    ## names(SpawnBio) <- names(depl) <- keyvec2
  }
  pars$Label <- parstds$Label <- parphases$Label <- parnames
  quants$Label <- quantstds$Label <- dernames

  # extract year values from labels for some parameters associated with years
  pars$Yr <- NA
  for(ipar in 1:nrow(pars)){
    substrings <- strsplit(as.character(pars$Label[ipar]),"_")[[1]]
    yr <- substrings[substrings %in% allyears][1]
    pars$Yr[ipar] <- ifelse(is.null(yr), NA, as.numeric(yr))
  }

  # identify parameters that are recruitment deviations
  pars$recdev <- FALSE
  pars$recdev[grep("RecrDev",pars$Label)] <- TRUE
  pars$recdev[grep("InitAge",pars$Label)] <- TRUE

  # if there are any initial age parameters, figure out what year they're from
  InitAgeRows <- grep("InitAge",pars$Label)
  temp <- unlist(strsplit(pars$Label[InitAgeRows],"InitAge_")) # separate out values from string
  InitAgeVals <- as.numeric(temp[seq(2,length(temp),2)]) # get odd entries in above separation
  InitAgeYrs <- matrix(NA,nrow=length(InitAgeRows),ncol=n)
  for(imodel in 1:n){
    modelpars <- pars[,imodel]
    devyears <- pars$Yr[!is.na(modelpars) & pars$recdev]
    if(any(!is.na(devyears))) minyr <- min(devyears,na.rm=TRUE) else minyr <- NA
    good <- !is.na(modelpars[InitAgeRows])
    if(!is.na(minyr) & minyr>0 & any(good)) InitAgeYrs[good,imodel] <- minyr - InitAgeVals[good]
  }
  # check for differences in assignment of initial ages
  if(max(apply(InitAgeYrs,1,sd,na.rm=TRUE),na.rm=TRUE)>0){
    cat("warning: years for InitAge parameters are differ between models, use InitAgeYrs matrix\n")
  }else{
    pars$Yr[InitAgeRows] <- apply(InitAgeYrs,1,max,na.rm=TRUE)
  }
  recdevs <- pars[pars$recdev,]
  print(ncol(recdevs))
  recdevs <- recdevs[order(recdevs$Yr),1:(n+2)]
  
  mylist <- list()
  mylist$listnames  <- names(biglist)
  mylist$keyvec     <- keyvec
  mylist$maxgrad    <- maxgrad
  #mylist            <- c(mylist,selexlist)
  mylist$pars       <- pars
  mylist$parstds    <- parstds
  mylist$parphases  <- parphases
  mylist$quants     <- quants
  mylist$quantstds  <- quantstds
  mylist$recdevs    <- recdevs
  mylist$growth     <- growth
  mylist$InitAgeYrs <- InitAgeYrs

  #mylist$lbinspop   <- as.numeric(names(stats$sizeselex)[-(1:5)])

  if(FALSE){
    mysummary <- SSsummarize(mymodels)
    recdevs <- mysummary$recdevs
    plot(0,xlim=range(recdevs$Yr),ylim=range(recdevs[,1:n],na.rm=TRUE),type='n')
    for(i in 1:n){
      yvec <- recdevs[,i]
      xvec <- recdevs$Yr[!is.na(yvec)]
      yvec <- yvec[!is.na(yvec)]
      lines(xvec,yvec,lwd=1,col=rich.colors.short(n)[i],type='o')
    }
  }
  
  
  return(mylist)
} # end function


