SSsummarize <-
function(biglist,
         keyvec=NULL,
         numvec=NULL,
         selfactor=c("Lsel"),
         selfleet=NULL,
         selyear="min",
         selgender=1,
         growthyr=1971)
{
  ## subfunction to extract recruitment deviations from parameter list
  getrecdevs <- function(replist){
    parmat <- replist$parameters
    name2 <- "InitF_1fishery1"
    if(length(grep(name2,parmat$Label))==0) name2 <- "InitF_1fleet1"
    rowrange <- (grep("SR_autocorr",parmat$Label)+1):(grep(name2,parmat$Label)-1)
    val <- parmat$Value[rowrange]
    std <- parmat$Parm_StDev[rowrange]
    return(data.frame(val=val,std=std))
  }

  biglistkeys <- substring(names(biglist),9)

  ## check inputs in keyvec or numvec
  # too many inputs
  if(!is.null(keyvec) & !is.null(numvec)){
    print("don't input both 'keyvec' and 'numvec'",quote=FALSE)
    return()
  }
  if(!is.null(keyvec) & is.null(numvec)){
    # if only keyvec is supplied, check to make sure it works
    if(length(setdiff(keyvec,biglistkeys)) > 0){
      print("'keyvec' should include strings that follow 'replist_' in names(biglist)",quote=FALSE)
      return()
    }
    n <- length(keyvec)
  }
  if(is.null(keyvec) & !is.null(numvec)){
    # if only numvec is supplied, check to make sure it works
    if(length(setdiff(numvec,1:length(biglist))) > 0){
      print("'numvec' should include indices of elements of biglist",quote=FALSE)
      return()
    }
    n <- length(numvec)
  }
  # no inputs to subset elements
  if(is.null(keyvec) & is.null(numvec)){
    keyvec <- substring(names(biglist),9)
    n <- length(biglist)
  }

  # objects to store quantities
  bio        <- NULL
  depl       <- NULL
  sel        <- NULL
  selexlist  <- list()
  recdevs    <- NULL
  recdevstds <- NULL
  pars       <- NULL
  parstds    <- NULL
  quants     <- NULL
  quantstds  <- NULL
  biasadj    <- NULL
  growth     <- NULL
  maxgrad    <- NULL

  # notes about what runs were used
  sim        <- NULL
  keyvec2    <- NULL
  listnames  <- NULL

  # loop over outputs within biglist
  for(i in 1:n)
  {
    element <- (1:length(biglist))[biglistkeys==keyvec[i]]
    if(length(element)!=1){
      print(paste("error with keyvec, element =",element),quote=FALSE)
      print(paste("keyvec[i] =",keyvec[i]),quote=FALSE)
      return()
    }
    stats <- biglist[[element]]
    listname <- names(biglist)[element]

    key <- as.character(stats$key)
    if(is.null(key)) key <- i
    keyvec2 <- c(keyvec2,key)
    print(paste("i=",i,"/",n,", element=",element,",",substring("      ",nchar(i)+nchar(element)),"got ", listname,sep=""),quote=FALSE)

    # time series stuff
    biotemp <- stats$timeseries$SpawnBio
    bio <- cbind(bio, biotemp)
    depl <- cbind(depl, biotemp/biotemp[1])

    # gradient
    maxgrad <- c(maxgrad, stats$maximum_gradient_component)

    # selex
    seltemp <- stats$sizeselex
    if(is.null(selfactor)) selfactor <- unique(seltemp$Factor)
    if(is.null(selfleet))  selfleet  <- unique(seltemp$Fleet)
    if(is.null(selgender)) selgender <- unique(seltemp$gender)
    if(is.null(selyear))   selyear   <- unique(seltemp$year)
    if(selyear[1]=="min")  selyear   <- min(seltemp$year)
    if(selyear[1]=="max")  selyear   <- max(seltemp$year)

    for(iselfactor in 1:length(selfactor)){
      for(iselfleet in 1:length(selfleet)){
        for(iselyear in 1:length(selyear)){
          for(iselgender in 1:length(selgender)){
            seltemp_i <- seltemp[seltemp$Factor==selfactor[iselfactor] &
                                 seltemp$Fleet==selfleet[iselfleet] &
                                 seltemp$gender==selgender[iselgender] &
                                 seltemp$year==selyear[iselyear],]
            mylabel <- seltemp_i$label
            myname <- paste("sizeselex_",mylabel,sep="")
            seltemp_i2 <- as.numeric(seltemp_i[-(1:5)])
            selexlist[[myname]] <- rbind(selexlist[[myname]],seltemp_i2)
            rownames(selexlist[[myname]])[nrow(selexlist[[myname]])] <- key
          }
        }
      }
    }

    if(3==4){
        # array of selex
        if(sumtrendfits){
            if(isimchoice==3 & ifitchoice==2){
                fitseltime <- stats$sizeselex[stats$sizeselex$Factor=="Lsel" & stats$sizeselex$Fleet==1 & stats$sizeselex$gender==1,]
                fitseltime <- fitseltime[,-(1:5)]
                fitselarrayCB[,,isim] <- as.matrix(fitseltime)
            }
            if(isimchoice==3 & ifitchoice==2){
                fitseltime2 <- stats$sizeselex[stats$sizeselex$Factor=="Lsel" & stats$sizeselex$Fleet==2 & stats$sizeselex$gender==1,]
                fitseltime2 <- fitseltime2[,-(1:5)]
                fitselarrayCB2[,,isim] <- as.matrix(fitseltime2)
            }
            if(isimchoice==1 & ifitchoice==2){
                fitseltime <- stats$sizeselex[stats$sizeselex$Factor=="Lsel" & stats$sizeselex$Fleet==1 & stats$sizeselex$gender==1,]
                fitseltime <- fitseltime[,-(1:5)]
                fitselarrayAB[,,isim] <- as.matrix(fitseltime)
            }
            if(isimchoice==1 & ifitchoice==2){
                fitseltime2 <- stats$sizeselex[stats$sizeselex$Factor=="Lsel" & stats$sizeselex$Fleet==2 & stats$sizeselex$gender==1,]
                fitseltime2 <- fitseltime2[,-(1:5)]
                fitselarrayAB2[,,isim] <- as.matrix(fitseltime2)
            }
        }
    }
    ## recdevs
    recdev <- getrecdevs(stats)
    recdevs <- cbind(recdevs, recdev$val)
    recdevstds <- cbind(recdevstds, recdev$std)

    ## growth
    growthtemp <- stats$growthseries
    imorphf <- ifelse(max(stats$morph_indexing$Index)==10,3,1)
    growth <- cbind(growth, as.numeric(growthtemp[growthtemp$Morph==imorphf &
                                                  growthtemp$Yr==growthyr,-(1:4)]))

    ## all pars
    parstemp <- stats$parameters
    pars <- rbind(pars, parstemp$Value)
    parstds <- rbind(parstds, parstemp$Parm_StDev)

    derquantnames <- c("SPB_Virgin" ,   "SPB_Initial",    "SPB_1971"     ,  "SPB_1972"    ,
                       "SPB_1973"   ,   "SPB_1974"   ,    "SPB_1975"     ,  "SPB_1976"    ,
                       "SPB_1977"   ,   "SPB_1978"   ,    "SPB_1979"     ,  "SPB_1980"    ,
                       "SPB_1981"   ,   "SPB_1982"   ,    "SPB_1983"     ,  "SPB_1984"    ,
                       "SPB_1985"   ,   "SPB_1986"   ,    "SPB_1987"     ,  "SPB_1988"    ,
                       "SPB_1989"   ,   "SPB_1990"   ,    "SPB_1991"     ,  "SPB_1992"    ,
                       "SPB_1993"   ,   "SPB_1994"   ,    "SPB_1995"     ,  "SPB_1996"    ,
                       "SPB_1997"   ,   "SPB_1998"   ,    "SPB_1999"     ,  "SPB_2000"    ,
                       "SPB_2001"   ,   "Recr_Virgin",    "Recr_Initial" ,  "Recr_1971"   ,
                       "Recr_1972"  ,   "Recr_1973"  ,    "Recr_1974"    ,  "Recr_1975"   ,
                       "Recr_1976"  ,   "Recr_1977"  ,    "Recr_1978"    ,  "Recr_1979"   ,
                       "Recr_1980"  ,   "Recr_1981"  ,    "Recr_1982"    ,  "Recr_1983"   ,
                       "Recr_1984"  ,   "Recr_1985"  ,    "Recr_1986"    ,  "Recr_1987"   ,
                       "Recr_1988"  ,   "Recr_1989"  ,    "Recr_1990"    ,  "Recr_1991"   ,
                       "Recr_1992"  ,   "Recr_1993"  ,    "Recr_1994"    ,  "Recr_1995"   ,
                       "Recr_1996"  ,   "Recr_1997"  ,    "Recr_1998"    ,  "Recr_1999"   ,
                       "Recr_2000"  ,   "Recr_2001"  ,    "SPRratio_2001",  "F_2001"      ,
                       # "Bratio_1971",
                       "Bratio_1972",    "Bratio_1973"  ,  "Bratio_1974" ,
                       "Bratio_1975",   "Bratio_1976",    "Bratio_1977"  ,  "Bratio_1978" ,
                       "Bratio_1979",   "Bratio_1980",    "Bratio_1981"  ,  "Bratio_1982" ,
                       "Bratio_1983",   "Bratio_1984",    "Bratio_1985"  ,  "Bratio_1986" ,
                       "Bratio_1987",   "Bratio_1988",    "Bratio_1989"  ,  "Bratio_1990" ,
                       "Bratio_1991",   "Bratio_1992",    "Bratio_1993"  ,  "Bratio_1994" ,
                       "Bratio_1995",   "Bratio_1996",    "Bratio_1997"  ,  "Bratio_1998" ,
                       "Bratio_1999",   "Bratio_2000",    "Bratio_2001"  ,  "Bzero_again" ,
                       "Bzero_again")

    ## almost all derived quantities
    quantstemp <- stats$derived_quants
    #quantstemp <- stats$derived_quants[stats$derived_quants$LABEL %in% derquantnames,]
    quants <- rbind(quants, quantstemp$Value)
    quantstds <- rbind(quants, quantstemp$Parm_StDev)

    ## root mean square error in fits
    biasadjtemp <- stats$recruit$biasadj
    biasadj <- cbind(biasadj,biasadjtemp)
    #} # end loop over sims
  } # end loop over keys

  if(!setequal(keyvec,keyvec2)){
    print("problem with keys!",quote=FALSE)
    print("keyvec:",quote=FALSE)
    print(keyvec)
    print("keyvec2:",quote=FALSE)
    print(keyvec2)
  }
  pars <- as.data.frame(pars)
  names(pars) <- parstemp$Label
  quants <- as.data.frame(quants)
  names(quants) <- quantstemp$LABEL

  # clean up the row or column names
  colnames(bio) <- keyvec
  colnames(depl) <- keyvec

  mylist <- list()
  mylist$listnames  <- names(biglist)
  mylist$keyvec     <- keyvec
  mylist$maxgrad    <- maxgrad
  mylist$bio        <- bio
  mylist$depl       <- depl
  mylist            <- c(mylist,selexlist)
  mylist$recdevs    <- recdevs
  mylist$recdevstds <- recdevstds
  mylist$pars       <- pars
  mylist$parstds    <- parstds
  mylist$quants     <- quants
  mylist$quantstds  <- quantstds
  mylist$biasadj    <- biasadj
  mylist$growth     <- growth

  mylist$lbinspop   <- as.numeric(names(stats$sizeselex)[-(1:5)])
  mylist$hessian    <- apply(mylist$recdevstds,2,max) > 0

  return(mylist)
} # end function


