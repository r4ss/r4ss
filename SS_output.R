SS_output <-
  function(dir="C:/myfiles/mymodels/myrun/", model="ss3",
           repfile="Report.sso", compfile="CompReport.sso",covarfile="covar.sso",
           ncols=200, forecast=TRUE, warn=TRUE, covar=TRUE,
           checkcor=TRUE, cormax=0.95, cormin=0.01, printhighcor=10, printlowcor=10,
           verbose=TRUE, printstats=TRUE,hidewarn=FALSE, NoCompOK=FALSE, aalmaxbinrange=0)
{
  ################################################################################
  #
  # SS_output
  # This function comes with no warranty or guarantee of accuracy
  #
  # Purpose: To import content from Stock SYnthesis model run.
  # Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
  #          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
  #          and other contributors to http://code.google.com/p/r4ss/
  # Returns: a list containing elements of Report.sso and/or covar.sso,
  #          formatted as R objects, and optional summary statistics to R console
  # General: Updated for Stock Synthesis version 3.10; R version 2.8.1
  # Notes:   See users guide for documentation: http://code.google.com/p/r4ss/wiki/Documentation
  # Required packages: none
  #
  ################################################################################

  codedate <- "November 2, 2010"

  if(verbose){
    print(paste("R function updated:",codedate),quote=FALSE)
    print("Check for new code and report problems at http://code.google.com/p/r4ss/",quote=FALSE)
  }

  flush.console()

  #################################################################################
  ## embedded functions: matchfun and matchfun2
  #################################################################################

  matchfun <- function(string, obj=rawrep[,1], substr1=TRUE)
  {
    # return a line number from the report file (or other file)
    # sstr controls whether to compare subsets or the whole line
    match(string, if(substr1){substring(obj,1,nchar(string))}else{obj} )
  }

  matchfun2 <- function(string1,adjust1,string2,adjust2,cols="all",matchcol1=1,matchcol2=1,
    objmatch=rawrep,objsubset=rawrep,substr1=TRUE,substr2=TRUE)
  {
    # return a subset of values from the report file (or other file)
    # subset is defined by character strings at the start and end, with integer
    # adjustments of the number of lines to above/below the two strings
    line1 <- match(string1,if(substr1){substring(objmatch[,matchcol1],1,nchar(string1))}else{objmatch[,matchcol1]})
    line2 <- match(string2,if(substr2){substring(objmatch[,matchcol2],1,nchar(string2))}else{objmatch[,matchcol2]})
    if(is.na(line1) | is.na(line2)) return("absent")
    if(cols[1]!="all"){ out <- objsubset[(line1+adjust1):(line2+adjust2),cols]
       }else{           out <- objsubset[(line1+adjust1):(line2+adjust2), ]}
    return(out)
  }

  # get info on output files created by Stock Synthesis
  dir <- paste(dir,"/",sep="")
  shortrepfile <- repfile
  repfile <- paste(dir,repfile,sep="")

  parfile <- paste(dir,model,".par",sep="")
  if(!file.exists(parfile)){
    if(!hidewarn) print(paste("Some stats skipped because the .par file not found:",parfile),quote=FALSE)
    parfile <- NA
  }

  # read three rows to get start time and version number from rep file
  if(file.exists(repfile)){
    if(file.info(repfile)$size>0){
      if(verbose) print(paste("Getting header info from",repfile),quote=FALSE)
    }else{
      print(paste("!Error: report file is empty:",repfile),quote=FALSE)
      return()
    }
  }else{
    print(paste("!Error: can't find report file,", repfile),quote=FALSE)
    return()
  }
  rephead <- readLines(con=repfile,n=10)

  # warn if SS version used to create rep file is too old or too new for this code
  SS_version <- rephead[1]
  SS_versionshort <- toupper(substr(SS_version,1,8))
  if(!(SS_versionshort %in% c("SS-V3.10","SS-V3.11"))){
    print(paste("! Warning, this function tested on SS-V3.11b. You are using",substr(SS_version,1,9)),quote=FALSE)
  }else{
    if(verbose) print(paste("You're using",SS_versionshort,"which should work with this R code."),quote=FALSE)
  }

  findtime <- function(lines){
    # quick function to get model start time from SS output files
    time <- strsplit(lines[grep("ime",lines)],"ime: ")[[1]]
    if(length(time)<2) return() else return(time[2])
  }
  repfiletime <- findtime(rephead)
  if(verbose) print(paste("Report file time:",repfiletime),quote=FALSE)

  corfile <- NA
  if(covar){
    # .cor file
    if(!is.na(parfile)){
      corfile <- sub(".par",".cor",parfile)
      if(!file.exists(corfile)){
        print(paste("Some stats skipped because the .cor file not found:",corfile),quote=FALSE)
        corfile <- NA
      }
    }
    # CoVar.sso file
    covarfile <- paste(dir,covarfile,sep="")
    if(!file.exists(covarfile)){
      print("covar file not found. Change input to covar=FALSE, or modify 'covarfile' input.",quote=FALSE)
      return()
    }

    # time check for CoVar file
    covarhead <- readLines(con=covarfile,n=2)
    covartime <- findtime(covarhead)
    # the conversion to R time class below may no longer be necessary as strings should match
    if(is.null(covartime) || is.null(repfiletime)){
      print("problem comparing the file creation times:",quote=FALSE)
      print(paste("  Report.sso:",repfiletime),quote=FALSE)
      print(paste("  covar.sso:",covartime),quote=FALSE)
    }else{
      if( covartime != repfiletime){
        print(paste("!Error: ",shortrepfile,"and",covarfile,"were from different model runs. Change input to covar=FALSE"),quote=FALSE)
        print(paste("covar time:",covartime),quote=FALSE)
        return()
      }
    }
  }

  # time check for CompReport file
  compfile <- paste(dir,compfile,sep="")
  if(file.exists(compfile)){
    comphead <- readLines(con=compfile,n=2)
    comptime <- findtime(comphead)
    if(is.null(comptime) || is.null(repfiletime)){
      print("problem comparing the file creation times:",quote=FALSE)
      print(paste("  Report.sso:",repfiletime),quote=FALSE)
      print(paste("  CompReport.sso:",comptime),quote=FALSE)
    }else{
      if(comptime != repfiletime){
        print(paste(shortrepfile,"and",compfile,"were from different model runs."),quote=FALSE)
        print(paste("CompReport time:",comptime),quote=FALSE)
        return()
      }
    }
    comp <- TRUE
  }else{
    print(paste("Missing ",compfile,". Change the compfile input or rerun model to get the file.",sep=""),quote=FALSE)
    #return()
    if(NoCompOK) comp <- FALSE else return()
  }

  # read report file
  if(verbose) print("Reading full report file",quote=FALSE)
  flush.console()
  rawrep <- read.table(file=repfile,col.names=1:ncols,fill=TRUE,quote="",colClasses="character",nrows=-1,comment.char="")

  # check empty columns
  emptytest <- function(x){ sum(!is.na(x) & x=="")/length(x) }
  nonblanks <- apply(rawrep,2,emptytest) < 1
  maxnonblank = max(0,(1:ncols)[nonblanks==TRUE])
  if(maxnonblank==ncols){
    print(      "! Warning, all columns are used and some data may have been missed,",quote=FALSE)
    print(paste("  increase 'ncols' input above current value (ncols=",ncols,")",sep=""),quote=FALSE)
    return(NULL)
  }
  if((maxnonblank+1)==ncols & verbose){ print("Got all columns.",quote=FALSE)}
  if((maxnonblank+1)<ncols){ if(verbose) print(paste("Got all columns. To speed code, future reads of this model may use ncols=",maxnonblank+1,sep=""),quote=FALSE)}
  if(verbose) print("Got Report file",quote=FALSE)
  flush.console()

  # read forecast report file
  if(forecast){
    forcastname <- paste(dir,"Forecast-report.sso",sep="")
    temp <- file.info(forcastname)$size
    if(is.na(temp) | temp==0){
      print("!Error: the Forecase-report.sso file is empty.",quote=FALSE)
      print("        Change input to 'forecast=FALSE' or rerun model with forecast turned on.",quote=FALSE)
      return()
    }
    rawforcast1 <- read.table(file=forcastname,col.names=1:ncols,fill=TRUE,quote="",colClasses="character",nrows=-1)
    endyield <- matchfun("MSY_not_calculated",rawforcast1[,1])
    if(is.na(endyield)) yesMSY <- TRUE else yesMSY <- FALSE
    if(yesMSY) endyield <- matchfun("findFmsy",rawforcast1[,10])
    yieldraw <- rawforcast1[(matchfun("Btarget",rawforcast1[,10])):endyield,]
    yielddat <- yieldraw[c(3:(as.numeric(length(yieldraw[,1])-1))),c(4,7)]
    colnames(yielddat) <- c("Catch","Depletion")
    yielddat$Catch <- as.numeric(yielddat$Catch)
    yielddat$Depletion <- as.numeric(yielddat$Depletion)
    yielddat <- yielddat[order(yielddat$Depletion,decreasing = FALSE),]
    if(verbose) print("Got forecast file",quote=FALSE)
  }else{if(verbose) print("You skipped the forecast file",quote=FALSE)}
  flush.console()

  # read warnings file
  if(warn){
    warnname <- paste(dir,"warning.sso",sep="")
    if(!file.exists(warnname)){
      print("warning.sso file not found",quote=FALSE)
      warn <- NA
    }else{
      warn <- readLines(warnname,warn=FALSE)
      warnstring <- warn[grep("N warnings: ",warn)]
      if(length(warnstring)>0){
        nwarn <- as.numeric(strsplit(warnstring,"N warnings: ")[[1]][2])
        textblock <- c(paste("were", nwarn, "warnings"),paste("was", nwarn, "warning"))[1+(nwarn==1)]
        if(verbose) print(paste("Got warning file. There", textblock, "in", warnname),quote=FALSE)
      }else{
        print("warning.sso file is missing the string 'N warnings'!",quote=FALSE)
        nwarn <- NA
      }
    }
  }else{
    if(verbose) print("You skipped the warnings file",quote=FALSE)
    nwarn <- NA
  }
  if(verbose) print("Finished reading files",quote=FALSE)
  flush.console()

  # Useful dimensions
  rawselex <- matchfun2("LEN_SELEX",6,"AGE_SELEX",-1)
  rawselex <- rawselex[,rawselex[1,]!=""]
  names(rawselex)<- rawselex[1,]
  selex <- rawselex[-1,]
  for(icol in (1:ncol(selex))[!(names(selex) %in% c("Factor","label"))]) selex[,icol] <- as.numeric(selex[,icol])
  nfleets <- length(unique(selex$Fleet))
  nfishfleets <- max(selex$Fleet[selex$Factor=="Ret"])
  nsexes <- length(unique(as.numeric(selex$gender)))
  FleetNames <- matchfun2("FleetNames",1,"FleetNames",nfleets,cols=2)

  # more dimensions
  tempaccu <- as.character(rawrep[matchfun("Natural_Mortality")+1,-(1:5)])
  accuage <- max(as.numeric(tempaccu[tempaccu!=""]))
  ncpue <- sum(as.numeric(rawrep[matchfun("INDEX_1")+1+1:nfleets,11]))
  begin <- matchfun("TIME_SERIES")+2
  end  <- matchfun("SPR_series")-1
  nareas <- max(as.numeric(rawrep[begin:end,1]))
  startyr <- min(as.numeric(rawrep[begin:end,2]))+2  # this is the 'initial' year not including
  temptime <- rawrep[begin:end,2:3]
  endyr <- max(as.numeric(temptime[temptime[,2]=="TIME",1])) # this is the beginning of the last year of the normal timeseries
  nseasons <- max(as.numeric(rawrep[(begin+3):end,4]))
  seasfracs <- (0:(nseasons-1))/nseasons

  # compositions
  if(comp){   # skip this stuff if no CompReport.sso file
    allbins <- read.table(file=compfile, col.names=1:ncols, fill=TRUE, colClasses="character", skip=3, nrows=15)
    #lbins is data length bins
    lbins <- as.numeric(allbins[7,-1])
    lbins <- lbins[!is.na(lbins)]
    nlbins <- length(lbins)
    #lbinspop is Pop_len_mid used for selex and bio quantities
    lbinspop <- as.numeric(allbins[3,-1])
    lbinspop <- lbinspop[!is.na(lbinspop)]
    nlbinspop <- length(lbinspop)
    Lbin_method <- as.numeric(allbins[matchfun("Method_for_Lbin_definition",allbins[,1]),2])
    # read composition database
    rawcompdbase <- read.table(file=compfile, col.names=1:21, fill=TRUE, colClasses="character", skip=18, nrows=-1)
    names(rawcompdbase) <- rawcompdbase[1,]
    compdbase <- rawcompdbase[2:(nrow(rawcompdbase)-2),] # subtract header line and last 2 lines
    compdbase <- compdbase[compdbase$Obs!="",]
    compdbase$Like[compdbase$Like=="_"] <- NA
    compdbase$effN[compdbase$effN=="_"] <- NA
    for(i in (1:ncol(compdbase))[!(names(compdbase) %in% c("Kind"))]) compdbase[,i] <- as.numeric(compdbase[,i])

    # configure seasons
    if(nseasons>1) compdbase$YrSeasName <- paste(floor(compdbase$Yr),"s",compdbase$Seas,sep="") else compdbase$YrSeasName <- compdbase$Yr

    # deal with Lbins
    compdbase$Lbin_range <- compdbase$Lbin_hi - compdbase$Lbin_lo
    compdbase$Lbin_mid <- 0.5*(compdbase$Lbin_lo + compdbase$Lbin_hi)

    # divide into objects by kind
    Lbin_range <- compdbase$Lbin_range
    if(is.null(Lbin_range)){ # if/else required to avoid warning if no comp data at all
      notconditional <- TRUE
      conditional <- FALSE
    }else{
      notconditional <- !is.na(Lbin_range) & Lbin_range >  aalmaxbinrange
      conditional    <- !is.na(Lbin_range) & Lbin_range <= aalmaxbinrange
    }
    lendbase         <- compdbase[compdbase$Kind=="LEN" & compdbase$N > 0,]
    sizedbase        <- compdbase[compdbase$Kind=="SIZE" & compdbase$N > 0,]
    agedbase         <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0 & notconditional,]
    condbase         <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0 & conditional,]
    ghostagedbase    <- compdbase[compdbase$Kind=="AGE" & compdbase$N < 0 & notconditional,]
    compdbase$Kind[compdbase$Kind=="L@A" & compdbase$Ageerr < 0] <- "W@A"

    if(is.null(compdbase$N)){
      good <- TRUE
    }else{
      good <- !is.na(compdbase$N)
    }
    ladbase          <- compdbase[compdbase$Kind=="L@A" & good,]
    wadbase          <- compdbase[compdbase$Kind=="W@A" & good,]
    tagdbase1        <- compdbase[compdbase$Kind=="TAG1",]
    tagdbase2        <- compdbase[compdbase$Kind=="TAG2",]
    # consider range of bins for conditional age at length data

    if(verbose){
        print(paste("CompReport file separated by this code as follows (rows = Ncomps*Nbins):"),quote=FALSE)
        print(paste("  ",nrow(lendbase), "rows of length comp data,"),quote=FALSE)
        print(paste("  ",nrow(sizedbase),"rows of generalized size comp data,"),quote=FALSE)
        print(paste("  ",nrow(agedbase), "rows of age comp data,"),quote=FALSE)
        print(paste("  ",nrow(condbase), "rows of conditional age-at-length data, and"),quote=FALSE)
        print(paste("  ",nrow(ghostagedbase),"rows of ghost fleet age comp data"),quote=FALSE)
        print(paste("  ",nrow(ladbase),  "rows of mean length at age data"),quote=FALSE)
        print(paste("  ",nrow(wadbase),  "rows of mean weight at age data"),quote=FALSE)
        print(paste("  ",nrow(tagdbase1),"rows of 'TAG1' comp data"),quote=FALSE)
        print(paste("  ",nrow(tagdbase2),"rows of 'TAG2' comp data"),quote=FALSE)
    }
    Lbin_ranges <- as.data.frame(table(agedbase$Lbin_range))
    names(Lbin_ranges)[1] <- "Lbin_hi-Lbin_lo"
    if(length(unique(agedbase$Lbin_range)) > 1){
      print("Warning!: different ranges of Lbin_lo to Lbin_hi found in age comps.",quote=FALSE)
      print(Lbin_ranges)
      print("  consider increasing 'aalmaxbinrange' to designate",quote=FALSE)
      print("  some of these data as conditional age-at-length",quote=FALSE)
    }
    # convert bin indices to true lengths
    if(nrow(agedbase)>0){
      agebins <- sort(unique(agedbase$Bin[!is.na(agedbase$Bin)]))
    }else{
      agebins <- NA
    }
    nagebins <- length(agebins)
  }else{
    # if comp option is turned off
    lbins <- NA
    nlbins <- NA
    temp <- rawrep[grep("NUMBERS_AT_LENGTH",rawrep[,1])+1,]
    lbinspop <- as.numeric(temp[temp!=""][-(1:11)])
    nlbinspop <- length(lbinspop)
    agebins <- NA
    nagebins <- NA
    Lbin_method <- 2
  }


  # info on growth morphs
  endcode <- "SIZEFREQ_TRANSLATION" #(this section heading not present in all models)
  if(is.na(matchfun(endcode))) endcode <- "MOVEMENT"
  morph_indexing <- matchfun2("MORPH_INDEXING",1,endcode,-1,cols=1:9)
  names(morph_indexing) <- morph_indexing[1,]
  morph_indexing <- morph_indexing[-1,]
  for(i in 1:ncol(morph_indexing)) morph_indexing[,i] <- as.numeric(morph_indexing[,i])
  ngpatterns <- max(morph_indexing$Gpattern)

  # set mainmorphs as those morphs with the earliest birth season
  # and the largest fraction of the submorphs (should equal middle morph when using sub-morphs)
  temp <- morph_indexing[morph_indexing$Bseas==min(morph_indexing$Bseas) &
                                        morph_indexing$Sub_Morph_Dist==max(morph_indexing$Sub_Morph_Dist),]
  mainmorphs <- min(temp$Index[temp$Gender==1])
  if(nsexes==2) mainmorphs <- c(mainmorphs, min(temp$Index[temp$Gender==2]))
  if(length(mainmorphs)==0) print("!Error with morph indexing in SS_output function.",quote=FALSE)

  # forecast
  if(forecast){
    grab  <- rawforcast1[,1]
    nforecastyears <- as.numeric(rawforcast1[grab %in% c("N_forecast_yrs:"),2])
    nforecastyears <- nforecastyears[1]
  }else{
    nforecastyears <- NA
  }
  if(verbose) print("Finished dimensioning",quote=FALSE)
  flush.console()

  # stats list: items that are output to the GUI (if printstats==T) for a quick summary of results
  stats <- list()
  stats$SS_version <- SS_version
  stats$Run_time <- paste(as.character(matchfun2("StartTime",0,"StartTime",0,cols=1:6)),collapse=" ")

  tempfiles  <- as.data.frame(rawrep[4:5,1:2],row.names = NULL)
  tempfiles <- matchfun2("Data_File",0,"Control_File",0,cols=1:2)
  stats$Files_used <- paste(c(tempfiles[1,],tempfiles[2,]),collapse=" ")

  stats$Nwarnings <- nwarn
  if(length(warn)>20) warn <- c(warn[1:20],paste("Note:",length(warn)-20,"additional lines truncated. Look in warning.sso file to see full list."))
  stats$warnings <- warn

  # likelihoods
  rawlike <- matchfun2("LIKELIHOOD",2,"Fleet:",-2,cols=1:3)
  like <- data.frame(signif(as.numeric(rawlike[,2]),digits=7))
  names(like) <- "values"
  rownames(like) <- rawlike[,1]
  like$lambdas <- rawlike[,3]
  stats$likelihoods_used <- like

  like2 <- matchfun2("Fleet:",0,"Input_Variance_Adjustment",-1,cols=1:(2+nfleets))
  names(like2) <- like2[1,]
  stats$likelihoods_raw_by_fleet <- like2[2:length(like2[,1]),]

  # parameters
  rawpars <- matchfun2("PARAMETERS",1,"DERIVED_QUANTITIES",-1,cols=1:16)
  names(rawpars) <- rawpars[1,]
  rawpars <- rawpars[-1,]
  parameters <- rawpars
  parameters[parameters=="_"] <- NA
  for(i in (1:ncol(parameters))[!(names(parameters)%in%c("Label","Status"))]) parameters[,i] = as.numeric(parameters[,i])

  if(!is.na(parfile)){
    parline <- read.table(parfile,fill=TRUE,comment.char="",nrows=1)
  }else{
    parline <- matrix(NA,1,16)

  }
  stats$N_estimated_parameters <- parline[1,6]

  pars <- rawpars[!(rawpars$Phase %in% c("_","")),]
  pars[pars=="_"] <- NA
  for(i in (1:ncol(pars))[!(names(pars)%in%c("Label","Status"))]) pars[,i] = as.numeric(pars[,i])
  pars$Afterbound <- ""
  pars$checkdiff <- pars$Value - pars$Min
  pars$checkdiff2 <- pars$Max - pars$Value
  pars$checkdiff3 <- abs(pars$Value-(pars$Max-(pars$Max-pars$Min)/2))
  pars$Afterbound[pars$checkdiff < 0.001 | pars$checkdiff2 < 0.001 | pars$checkdiff2 < 0.001] <- "CHECK"
  pars$Afterbound[!pars$Afterbound %in% "CHECK"] <- "OK"

  stats$table_of_phases <- table(pars$Phase)
  pars <- pars[pars$Phase %in% 0:100,]
  stats$estimated_non_rec_devparameters <- pars[,c(2,3,5:15)]

  # read covar.sso file
  if(covar){
    CoVar <- read.table(covarfile,header=TRUE,colClasses=c(rep("numeric",4),rep("character",4),"numeric"),skip=3)
    if(verbose) print("Got covar file.",quote=FALSE)
    stdtable <- CoVar[CoVar$Par..j=="Std",c(7,9,5)]
    names(stdtable) = c("name","std","type")
    N_estimated_parameters2 <- sum(stdtable$type=="Par")

    if(is.na(stats$N_estimated_parameters)){
      stats$N_estimated_parameters <- N_estimated_parameters2
    }else{
      if(stats$N_estimated_parameters!=N_estimated_parameters2){
        print("!warning:",quote=FALSE)
        print(paste(" ",stats$N_estimated_parameters,"estimated parameters indicated by",parfile),quote=FALSE)
        print(paste(" ",N_estimated_parameters2,"estimated parameters shown in",covarfile),quote=FALSE)
        print(paste("  returning the second value,",N_estimated_parameters2),quote=FALSE)
        stats$N_estimated_parameters <- N_estimated_parameters2
      }
    }
    Nstd <- sum(stdtable$std>0)

    if(Nstd<=1){
      print(paste("Too few estimated quantities in covar file (n=",Nstd,"). Change input to covar=FALSE.",sep=""),quote=FALSE)
      return()
    }
    if(checkcor==TRUE & stats$N_estimated_parameters > 1)
    {
      corfilter <- CoVar[CoVar$all.i!=CoVar$all.j & CoVar$Par..i=="Par" & CoVar$Par..j=="Par" & !substr(CoVar$label.i,1,8)=="ForeRecr" & !substr(CoVar$label.j,1,8)=="ForeRecr",]
      rangecor <- range(abs(corfilter$corr))
      corstats <- list()
      corstats$cormessage1 <- paste("Range of abs(parameter correlations) is",min(rangecor),"to",max(rangecor))
      # search for high or low correlations in covar file
      highcor <- CoVar[CoVar$all.i!=CoVar$all.j & CoVar$Par..i=="Par" & CoVar$Par..j=="Par" & !substr(CoVar$label.i,1,8)=="ForeRecr" & !substr(CoVar$label.j,1,8)=="ForeRecr" & abs(CoVar$corr) >= cormax, names(CoVar)%in%c("label.i", "label.j", "corr")]
      lowcorcandidates <- CoVar[CoVar$all.i!=CoVar$all.j & CoVar$Par..i=="Par" & CoVar$Par..j=="Par" & !substr(CoVar$label.i,1,8)=="ForeRecr" & !substr(CoVar$label.j,1,8)=="ForeRecr" & abs(CoVar$corr) <= cormin, names(CoVar)%in%c("label.i", "label.j", "corr")]
      lowcortestlist <- data.frame(unique(c(lowcorcandidates$label.i,lowcorcandidates$label.j)))
      lowcortestlist$name <- as.character(lowcortestlist[,1])
      nlowcor <- 0
      lowcor <- 0
      if(nrow(lowcortestlist)>0)
      {
       lowcortestlist$max <- NA
       for(i in 1:length(lowcortestlist[,1]))
       {
        lowcortestlist$max[i] <- max(corfilter$corr[corfilter$label.i == lowcortestlist$name[i]],corfilter$corr[corfilter$label.j == lowcortestlist$name[i]])
       }
       lowcor <- lowcortestlist[abs(lowcortestlist$max) <= cormin,2:3]
       nlowcor <- nrow(lowcor)
      }
      nhighcor <- nrow(highcor)
      if(printhighcor>0){
        if(nhighcor==0) textblock <- "No correlations"
        if(nhighcor==1) textblock <- "1 correlation"
        if(nhighcor>1)  textblock <- paste(nhighcor,"correlations")
        corstats$cormessage2 <-paste(textblock, " above threshold (cormax=", cormax,")",sep="")
        if(nhighcor>0 & nhighcor<=printhighcor){
          row.names(highcor) = paste("   ",1:nhighcor)
          corstats$cormessage3 <- highcor
        }
        if(nhighcor>0 & nhighcor>printhighcor){
          highcorsub <- highcor[order(-abs(highcor$corr)),]
          highcorsub <- highcorsub[1:printhighcor,]
          row.names(highcorsub) <- paste("   ",1:printhighcor)
          corstats$cormessage4 <- paste("Highest",printhighcor,
          "parameter correlations above threshold (to print more, increase 'printhighcor' input):")
          corstats$cormessage5 <- highcorsub
        }
      }else{
        corstats$cormessage6 <- "High correlations not reported. To report, change 'printhighcor' input to a positive value."
      }

      if(printlowcor>0){
        if(nlowcor==0) textblock <- "No uncorrelated parameters"
        if(nlowcor==1) textblock <- "1 uncorrelation"
        if(nlowcor>1)  textblock <- paste(nlowcor,"uncorrelated parameters")
        corstats$cormessage7 <- paste(textblock, " below threshold (cormin=", cormin,")",sep="")
        if(nlowcor>0 & nlowcor<=printlowcor){
          corstats$cormessage8 <-lowcor
        }
        if(nlowcor>0 & nlowcor>printlowcor){
          lowcorsub <- lowcor[order(abs(lowcor$max)),]
          lowcorsub <- lowcorsub[1:printlowcor,]
          corstats$cormessage9 <- paste("Lowest",printlowcor,
          "parameters uncorrelations below threshold (to print more, increase 'printlowcor' input):")
          corstats$cormessage10 <-lowcorsub
        }
      }else{
        corstats$cormessage11 <-"Uncorrelated parameters not reported. To report, change 'printlowcor' input to a positive value."
      }
    }else{if(verbose) print("You skipped the correlation check",quote=FALSE)}
  }else{if(verbose) print("You skipped the covar file",quote=FALSE)}
  flush.console()

  # derived quantities
  rawder <- matchfun2("DERIVED_QUANTITIES",4,"MGparm_By_Year_after_adjustments",-1,cols=1:3)
  names(rawder) <- rawder[1,]
  der <- rawder[-1,]
  der[der=="_"] <- NA
  for(i in 2:3) der[,i] = as.numeric(der[,i])

  managementratiolabels <- matchfun2("DERIVED_QUANTITIES",1,"DERIVED_QUANTITIES",3,cols=1:2)
  names(managementratiolabels) <- c("Ratio","Label")

  # time varying parameters
  MGparmAdj <- matchfun2("MGparm_By_Year_after_adjustments",2,"selparm(Size)_By_Year_after_adjustments",-1)
  if(nrow(MGparmAdj)>2){
    MGparmAdj <- MGparmAdj[,MGparmAdj[1,]!=""]
    names(MGparmAdj) <- c("Yr",parameters$Label[1:(ncol(MGparmAdj)-1)])
  }else{
    MGparmAdj <- NA
  }
  SelSizeAdj <- matchfun2("selparm(Size)_By_Year_after_adjustments",2,"selparm(Age)_By_Year_after_adjustments",-1)
  if(nrow(SelSizeAdj)>2){
    SelSizeAdj <- SelSizeAdj[,apply(SelSizeAdj,2,emptytest)<1]
    SelSizeAdj[SelSizeAdj==""] <- NA
    for(icol in 1:ncol(SelSizeAdj)) SelSizeAdj[,icol] <- as.numeric(SelSizeAdj[,icol])
    names(SelSizeAdj) <- c("FleetSvy","Yr",paste("Par",1:(ncol(SelSizeAdj)-2),sep=""))
  }else{
    SelSizeAdj <- NA
  }
  SelAgeAdj <- matchfun2("selparm(Age)_By_Year_after_adjustments",2,"RECRUITMENT_DIST",-1)
  if(nrow(SelAgeAdj)>2){
    SelAgeAdj <- SelAgeAdj[,apply(SelAgeAdj,2,emptytest)<1]
    SelAgeAdj[SelAgeAdj==""] <- NA
    if(SelAgeAdj[1,1]=="RECRUITMENT_DIST"){
      SelAgeAdj <- NA
    }else{
      for(icol in 1:ncol(SelAgeAdj)) SelAgeAdj[,icol] <- as.numeric(SelAgeAdj[,icol])
      names(SelAgeAdj) <- c("FleetSvy","Yr",paste("Par",1:(ncol(SelAgeAdj)-2),sep=""))
    }
  }else{
    SelAgeAdj <- NA
  }

  # recruitment distribution
  recruitment_dist <- matchfun2("RECRUITMENT_DIST",1,"MORPH_INDEXING",-1)[,1:6]
  names(recruitment_dist) <- recruitment_dist[1,]
  recruitment_dist <- recruitment_dist[-1,]
  for(i in 1:6) recruitment_dist[,i] <- as.numeric(recruitment_dist[,i])
  
  
  # gradient
  if(covar & !is.na(corfile)) stats$log_det_hessian <- read.table(corfile,nrows=1)[1,10]
  stats$maximum_gradient_component <- as.numeric(matchfun2("Convergence_Level",0,"Convergence_Level",0,cols=2))

  # sigma_R
  srhead <- matchfun2("SPAWN_RECRUIT",0,"SPAWN_RECRUIT",10,cols=1:6)
  rmse_table <- as.data.frame(srhead[-(1:9),1:5])
  for(icol in 2:5) rmse_table[,icol] <- as.numeric(rmse_table[,icol])
  names(rmse_table) <- srhead[9,1:5]
  names(rmse_table)[4] <- "RMSE_over_sigmaR"

  stats$sigma_R_in <- as.numeric(srhead[4,1])
  stats$rmse_table <- rmse_table

  rawvartune <- matchfun2("INDEX_1",1,"INDEX_1",(nfleets+1),cols=1:21)
  names(rawvartune) <- rawvartune[1,]
  rawvartune <- rawvartune[2:length(rawvartune[,1]),]
  rawvartune[,1] <- rawvartune[,21]
  vartune <- rawvartune[,c(1,8,11,13,16,18)]
  vartune <- vartune[vartune$N > 0,]
  stats$index_variance_tuning_check <- vartune

  rawlenntune <- matchfun2("FIT_AGE_COMPS",-(nfleets+1),"FIT_AGE_COMPS",-1,cols=1:10)
  names(rawlenntune) <- rawlenntune[1,]
  rawlenntune <- rawlenntune[2:length(rawlenntune[,1]),]
  rawlenntune[,1] <- rawlenntune[,10]
  lenntune <- rawlenntune[,c(1,2,4,5,6,8,9)]
  lenntune <- lenntune[lenntune$N > 0,]
  stats$Length_comp_Eff_N_tuning_check <- lenntune

  rawagentune <- matchfun2("FIT_SIZE_COMPS",-(nfleets+1),"FIT_SIZE_COMPS",-1,cols=1:10)
  names(rawagentune) <- rawagentune[1,]
  rawagentune <- rawagentune[2:length(rawagentune[,1]),]
  rawagentune[,1] <- rawagentune[,10]
  agentune <- rawagentune[,c(1,2,4,5,6,8,9)]
  agentune <- agentune[agentune$N > 0,]
  stats$Age_comp_Eff_N_tuning_check <- agentune

  if(verbose) print("Finished primary run statistics list",quote=FALSE)
  flush.console()

  # data return object
  returndat <- list()

  returndat$nfleets     <- nfleets
  returndat$nfishfleets <- nfishfleets
  returndat$nsexes      <- nsexes
  returndat$ngpatterns  <- ngpatterns
  returndat$mainmorphs  <- mainmorphs
  returndat$lbins       <- lbins
  returndat$Lbin_method <- Lbin_method
  returndat$nlbins      <- nlbins
  returndat$lbinspop    <- lbinspop
  returndat$nlbinspop   <- nlbinspop
  returndat$agebins     <- agebins
  returndat$nagebins    <- nagebins
  returndat$accuage     <- accuage
  returndat$nareas      <- nareas
  returndat$startyr     <- startyr
  returndat$endyr       <- endyr
  returndat$nseasons    <- nseasons
  returndat$seasfracs   <- seasfracs
  returndat$nforecastyears <- nforecastyears
  returndat$morph_indexing <- morph_indexing
  returndat$MGparmAdj   <- MGparmAdj
  returndat$SelSizeAdj  <- SelSizeAdj
  returndat$SelAgeAdj   <- SelAgeAdj
  returndat$recruitment_dist <- recruitment_dist
  
  # Static growth
  begin <- matchfun("N_Used_morphs",rawrep[,6])+1
  rawbio <- rawrep[begin:(begin+nlbinspop),1:8]
  names(rawbio) <- rawbio[1,]
  biology <- rawbio[-1,]
  for(i in 1:ncol(biology)) biology[,i] <- as.numeric(biology[,i])

  # determine fecundity type
  FecType <- 0
  if("Eggs/kg_slope_wt_Fem" %in% parameters$Label){
    FecType <- 1
    FecPar1name <- "Eggs/kg_inter_Fem"
    FecPar2name <- "Eggs/kg_slope_wt_Fem"
  }
  if("Eggs_exp_len_Fem" %in% parameters$Label){
    FecType <- 2
    FecPar1name <- "Eggs_scalar_Fem"
    FecPar2name <- "Eggs_exp_len_Fem"
  }
  if("Eggs_exp_wt_Fem" %in% parameters$Label){
    FecType <- 3
    FecPar1name <- "Eggs_scalar_Fem"
    FecPar2name <- "Eggs_exp_wt_Fem"
  }
  returndat$biology <- biology
  returndat$FecType <- FecType
  returndat$FecPar1name <- FecPar1name
  returndat$FecPar2name <- FecPar2name

  returndat$FecPar1 <- parameters$Value[parameters$Label==FecPar1name]
  returndat$FecPar2 <- parameters$Value[parameters$Label==FecPar2name]

  rawgrow <- matchfun2("Biology_at_age",1,"MEAN_BODY_WT(begin)",-1,cols=1:18)
  names(rawgrow) <- rawgrow[1,]
  growdat <- rawgrow[-1,]
  for(i in 1:ncol(growdat)) growdat[,i] <- as.numeric(growdat[,i])
  nmorphs <- max(growdat$Morph)
  midmorphs <- c(c(0,nmorphs/nsexes)+ceiling(nmorphs/nsexes/2))
  returndat$endgrowth <- growdat

  # mean body weight
  rawmean_body_wt <- matchfun2("MEAN_BODY_WT(begin)",1,"MEAN_SIZE_TIMESERIES",-1,cols=1:(accuage+4))
  names(rawmean_body_wt) <- rawmean_body_wt[1,]
  mean_body_wt <- rawmean_body_wt[-1,]
  for(i in 1:ncol(mean_body_wt)) mean_body_wt[,i] <- as.numeric(mean_body_wt[,i])
  returndat$mean_body_wt <- mean_body_wt

  # Time-varying growth
  rawgrow <- matchfun2("MEAN_SIZE_TIMESERIES",1,"mean_size_Jan_1_for_gender",-1,cols=1:(4+accuage+1))
  growthvaries <- FALSE
  if(length(rawgrow)>1){
    names(rawgrow) <- rawgrow[1,]
    growdat <- rawgrow[-1,]
    for(i in 1:ncol(growdat)) growdat[,i] <- as.numeric(growdat[,i])
    growdat <- growdat[growdat$Beg==1 & growdat$Yr < endyr,]
    if(nseasons > 1) growdat <- growdat[growdat$Seas==1,]
    if(length(unique(growdat$Yr))>1) growthvaries <- TRUE
    returndat$growthseries <- growdat
    returndat$growthvaries <- growthvaries
  }

  # Length selex and retention
  if(!forecast) selex <- selex[selex$year <= endyr,]
  returndat$sizeselex <- selex

  # Age based selex
  rawageselex <- matchfun2("AGE_SELEX",4,"ENVIRONMENTAL_DATA",-1)
  rawageselex <- rawageselex[,rawageselex[1,]!=""]
  names(rawageselex)<- rawageselex[1,]
  ageselex <- rawageselex[-1,]
  if(!forecast) ageselex <- ageselex[ageselex$year <= endyr,]
  for(icol in (1:ncol(ageselex))[!(names(ageselex) %in% c("factor","label"))]) ageselex[,icol] <- as.numeric(ageselex[,icol])
  returndat$ageselex <- ageselex

  # time series
  rawts <- matchfun2("TIME_SERIES",1,"SPR_series",-1,cols=1:ncols)
  tsfull <- rawts[,rawts[1,]!=""]
  names(tsfull) <- tsfull[1,]
  tsfull <- tsfull[-1,]
  tsfull[tsfull=="_"] <- NA
  for(i in (1:ncol(tsfull))[names(tsfull)!="Era"]) tsfull[,i] = as.numeric(tsfull[,i])
  returndat$timeseries <- tsfull

  # stats and dimensions
  tsfull$Yr <- tsfull$Yr + (tsfull$Seas-1)/nseasons
  ts <- tsfull[tsfull$Yr <= endyr+1,]
  tsyears <- ts$Yr[ts$Seas==1]
  # Depletion
  if(nareas > 1)
  {
    tsspaw_bio <- ts$SpawnBio[ts$Seas==1 & ts$Area==1]
    for(a in 2:nareas){tsspaw_bio <- tsspaw_bio + ts$SpawnBio[ts$Seas==1 & ts$Area==a]}
  }
  if(nareas == 1){tsspaw_bio <- ts$SpawnBio[ts$Seas==1]}
  if(nsexes==1) tsspaw_bio <- tsspaw_bio/2
  depletionseries <- tsspaw_bio/tsspaw_bio[1]
  stats$SBzero <- tsspaw_bio[1]
  # if(nsexes==1) stats$SBzero <- stats$SBzero/2
  stats$current_depletion <- depletionseries[length(depletionseries)] # doesn't work for spatial models

  # total landings
  ls <- nrow(ts)-1
  totretainedmat <- as.matrix(ts[,substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
  ts$totretained <- 0
  ts$totretained[3:ls] <- rowSums(totretainedmat)[3:ls]

  # total catch
  totcatchmat <- as.matrix(ts[,substr(names(ts),1,nchar("enc(B)"))=="enc(B)"])
  ts$totcatch <- 0
  ts$totcatch[3:ls] <- rowSums(totcatchmat)[3:ls]

  # harvest rates
  F_method <- as.numeric(rawrep[matchfun("F_Method"),2])
  returndat$F_method <- F_method
  if(F_method==1){
    stringmatch <- "Hrate:_"
  }else{stringmatch <- "F:_"}
  Hrates <- as.matrix(ts[,substr(names(ts),1,nchar(stringmatch))==stringmatch])
  fmax <- max(Hrates)
  #stats$fmax <- fmax
  #stats$endyrcatch <- ts$totcatch[ls]
  #stats$endyrlandings <- ts$totretained[ls]

  # depletion
  depletion_method <- as.numeric(rawrep[matchfun("Depletion_method"),2])
  depletion_basis <- rawrep[matchfun("B_ratio_denominator"),2]
  if(depletion_basis=="no_depletion_basis"){
    depletion_basis <- "none"
  }else{
    depletion_basis <- as.numeric(strsplit(depletion_basis,"%*",fixed=TRUE)[[1]][1])/100
  }
  returndat$depletion_method <- depletion_method
  returndat$depletion_basis <- depletion_basis

  ## discard fractions ###

  # degrees of freedom for T-distribution (or indicator 0, -1, -2 for other distributions)
  DF_discard <- rawrep[matchfun("DISCARD_OUTPUT"),3]
  if(length(grep("T_distribution",DF_discard))>0)
    DF_discard <- as.numeric(strsplit(DF_discard,"=_")[[1]][2])
  if(length(grep("_normal_with_Std_in_as_CV",DF_discard))>0)     DF_discard <- 0
  if(length(grep("_normal_with_Std_in_as_stddev",DF_discard))>0) DF_discard <- -1
  if(length(grep("_lognormal",DF_discard))>0)                    DF_discard <- -2

  rawdisc <- matchfun2("DISCARD_OUTPUT",1,"MEAN_BODY_WT_OUTPUT",-1)
  discard_type <- rawdisc[1,1]
  rawdisc <- rawdisc[-1,]
  if(!((length(rawdisc[,1])) == 1))
  {
    rawdisc <- rawdisc[,rawdisc[1,]!=""]
    names(rawdisc) <- rawdisc[1,]
    discard <- rawdisc[-1,]
    for(icol in 2:ncol(discard)) discard[,icol] <- as.numeric(discard[,icol])
  }else{
    discard <- NA
  }
  returndat$discard <- discard
  returndat$discard_type <- discard_type
  returndat$DF_discard <- DF_discard

  ## Average body weight observations
  # degrees of freedom for T-distribution
  DF_mnwgt <- rawrep[matchfun("MEAN_BODY_WT_OUTPUT"),2]
  DF_mnwgt <- as.numeric(strsplit(DF_mnwgt,"=_")[[1]][2])

  rawmnwgt <- matchfun2("MEAN_BODY_WT_OUTPUT",1,"FIT_LEN_COMPS",-1,cols=1:10)
  mnwgt <- NA
  if(nrow(rawmnwgt)>1)
  {
    names(rawmnwgt) <- rawmnwgt[1,]
    mnwgt <- rawmnwgt[-1,]
    for(i in 2:ncol(mnwgt)) mnwgt[,i] <- as.numeric(mnwgt[,i])
  } # if mean weight data exists
  returndat$mnwgt <- mnwgt
  returndat$DF_mnwgt <- DF_mnwgt

  # Yield and SPR time-series
  rawspr <- matchfun2("SPR_series",5,"SPAWN_RECRUIT",-1,cols=1:(22+2*nmorphs))
  names(rawspr) <- rawspr[1,]
  rawspr[rawspr=="_"] <- NA
  rawspr[rawspr=="&"] <- NA
  spr <- rawspr[-1,]
  for(i in (1:ncol(spr))[!(names(spr)%in%c("Actual:","More_F(by_morph):"))]) spr[,i] <- as.numeric(spr[,i])
  spr <- spr[spr$Year <= endyr,]
  spr$spr <- spr$SPR
  returndat$sprseries <- spr
  stats$last_years_sprmetric <- spr$spr[length(spr$spr)]

  if(forecast){
   returndat$equil_yield <- yielddat
   # stats$spr_at_msy <- as.numeric(rawforcast[33,2])
   # stats$exploit_at_msy <- as.numeric(rawforcast[35,2])
   # stats$bmsy_over_VLHbzero <- as.numeric(rawforcast[38,3])
   # stats$retained_msy <- as.numeric(rawforcast[43,5])
  }else{if(verbose) print("You skipped the equilibrium yield data",quote=FALSE)}
  flush.console()

  returndat$managementratiolabels <- managementratiolabels
  returndat$B_ratio_denominator <- as.numeric(strsplit(managementratiolabels$Label[3],"%")[[1]][1])/100

  # Spawner-recruit curve
  rawsr <- matchfun2("SPAWN_RECRUIT",11,"INDEX_2",-1,cols=1:9)
  names(rawsr) <- rawsr[1,]
  rawsr[rawsr=="_"] <- NA
  rawsr <- rawsr[-(1:2),] # remove header rows
  sr <- rawsr[-(1:2),] # remove rows for Virg and Init
  for(i in 1:(ncol(sr)-1)) sr[,i] <- as.numeric(sr[,i])
  returndat$recruit <- sr

  # CPUE/Survey series
  rawcpue <- matchfun2("INDEX_2",1,"INDEX_2",ncpue+1,cols=1:10)
  if(ncpue>0)
  {
    names(rawcpue) <- rawcpue[1,]
    cpue <- rawcpue[-1,]
    for(i in 2:ncol(cpue)) cpue[,i] <- as.numeric(cpue[,i])
    cpue$FleetName <- NA
    cpue$FleetNum <- NA
    for(i in 1:nrow(cpue))
    {
      cpue$FleetNum[i] <- strsplit(cpue$Fleet[i],"_")[[1]][1]
      cpue$FleetName[i] <- substring(cpue$Fleet[i],nchar(cpue$FleetNum[i])+2)
    }
  }else{
    cpue <- NA
  }
  returndat$cpue <- cpue

# temporary split in the code to work with 3.11 and 3.10
if(SS_versionshort==c("SS-V3.11")){
  # Numbers at age
  rawnatage <- matchfun2("NUMBERS_AT_AGE",1,"NUMBERS_AT_LENGTH",-1,cols=1:(12+accuage),substr1=FALSE)
  if(length(rawnatage)>1){
    names(rawnatage) <- rawnatage[1,]
    rawnatage <- rawnatage[-1,]
    for(i in (1:ncol(rawnatage))[!(names(rawnatage) %in% c("Beg/Mid","Era"))]) rawnatage[,i] = as.numeric(rawnatage[,i])
    returndat$natage <- rawnatage
  }

  # Numbers at length
  rawnatlen <- matchfun2("NUMBERS_AT_LENGTH",1,"CATCH_AT_AGE",-1,cols=1:(11+nlbinspop),substr1=FALSE)
  if(length(rawnatlen)>1){
    names(rawnatlen) <- rawnatlen[1,]
    rawnatlen <- rawnatlen[-1,]
    for(i in (1:ncol(rawnatlen))[!(names(rawnatlen) %in% c("Beg/Mid","Era"))]) rawnatlen[,i] = as.numeric(rawnatlen[,i])
    returndat$natlen <- rawnatlen
  }
}else{
  # Numbers at age
  rawnatage <- matchfun2("NUMBERS_AT_AGE",1,"NUMBERS_AT_LENGTH",-1,cols=1:(11+accuage),substr1=FALSE)
  if(length(rawnatage)>1){
    names(rawnatage) <- rawnatage[1,]
    rawnatage <- rawnatage[-1,]
    for(i in (1:ncol(rawnatage))[names(rawnatage)!="Era"]) rawnatage[,i] = as.numeric(rawnatage[,i])
    returndat$natage <- rawnatage
  }

  # Numbers at length
  rawnatlen <- matchfun2("NUMBERS_AT_LENGTH",1,"CATCH_AT_AGE",-1,cols=1:(10+nlbinspop),substr1=FALSE)
  if(length(rawnatlen)>1){
    names(rawnatlen) <- rawnatlen[1,]
    rawnatlen <- rawnatlen[-1,]
    for(i in (1:ncol(rawnatlen))[names(rawnatlen)!="Era"]) rawnatlen[,i] = as.numeric(rawnatlen[,i])
    returndat$natlen <- rawnatlen
  }
}
  
  # Movement
  movement <- matchfun2("MOVEMENT",1,"EXPLOITATION",-1,cols=1:(7+accuage),substr1=FALSE)
  names(movement) <- c(movement[1,1:6],paste("age",movement[1,-(1:6)],sep=""))
  movement <- movement[-1,]
  for(i in 1:ncol(movement)) movement[,i] <- as.numeric(movement[,i])
  returndat$movement <- movement

  # reporting rates
  tagreportrates <- matchfun2("Reporting_Rates_by_Fishery",1,
                              "See_composition_data_output_for_tag_recapture_details",-1,
                              cols=1:3)
  if(tagreportrates[[1]][1]!="absent"){
    names(tagreportrates) <- tagreportrates[1,]
    tagreportrates <- tagreportrates[-1,]
    for(i in 1:ncol(tagreportrates)) tagreportrates[,i] <- as.numeric(tagreportrates[,i])
    returndat$tagreportrates <- tagreportrates
  }else{
    returndat$tagreportrates <- NA
  }
  
  # tag recapture table
  tagrecap <- matchfun2("TAG_Recapture",1,
                        "Tags_Alive",-1,
                        cols=1:10)
  if(tagrecap[[1]][1]!="absent"){
    tagfirstperiod <- tagrecap[1,1]
    tagaccumperiod <- tagrecap[2,1]
    names(tagrecap) <- tagrecap[4,]
    tagrecap <- tagrecap[-(1:4),]
    for(i in 1:ncol(tagrecap)) tagrecap[,i] <- as.numeric(tagrecap[,i])
    returndat$tagrecap <- tagrecap
    returndat$tagfirstperiod 
    returndat$tagaccumperiod 
  }else{
    returndat$tagrecap <- NA
    returndat$tagfirstperiod <- NA
    returndat$tagaccumperiod <- NA
  }

  # tags alive
  tagsalive <- matchfun2("Tags_Alive",1,
                        "Total_recaptures",-1,
                        cols=1:ncols)
  if(tagsalive[[1]][1]!="absent"){
    tagcols <- max((1:ncols)[apply(tagsalive,2,function(x){any(x!="")})])
    tagsalive <- tagsalive[,1:tagcols]
    names(tagsalive) <- c("TG",paste("period",0:(tagcols-2),sep=""))
    for(i in 1:ncol(tagsalive)) tagsalive[,i] <- as.numeric(tagsalive[,i])
    returndat$tagsalive <- tagsalive
  }else{
    returndat$tagsalive <- NA
  }

  # total recaptures
  tagtotrecap <- matchfun2("Total_recaptures",1,
                           "Reporting_Rates_by_Fishery",-1,
                           cols=1:ncols)
  if(tagtotrecap[[1]][1]!="absent"){
    tagcols <- max((1:ncols)[apply(tagtotrecap,2,function(x){any(x!="")})])
    tagtotrecap <- tagtotrecap[,1:tagcols]
    names(tagtotrecap) <- c("TG",paste("period",0:(tagcols-2),sep=""))
    for(i in 1:ncol(tagtotrecap)) tagtotrecap[,i] <- as.numeric(tagtotrecap[,i])
    returndat$tagtotrecap <- tagtotrecap
  }else{
    returndat$tagtotrecap <- NA
  }
  
  # age-length matrix
  rawALK <- matchfun2("AGE_LENGTH_KEY",4,"AGE_AGE_KEY",-1,cols=1:(accuage+2))
  if(length(rawALK)>1){
    ALK = array(NA,c(nlbinspop,accuage+1,nmorphs))
    starts <- grep("Morph:",rawALK[,3])+2
    ends <- grep("mean",rawALK[,1])-1
    for(i in 1:nmorphs){
      ALKtemp <- rawALK[starts[i]:ends[i],-1]
      for(icol in 1:(accuage+1)) ALKtemp[,icol] <- as.numeric(ALKtemp[,icol])
      ALK[,,i] <- as.matrix(ALKtemp)
    }
    returndat$ALK <- ALK
  }

  # ageing error matrices
  rawAAK <- matchfun2("AGE_AGE_KEY",1,"SELEX_database",-1,cols=1:(accuage+2))
  if(length(rawAAK)>1){
    starts <- grep("KEY:",rawAAK[,1])
    returndat$N_ageerror_defs <- N_ageerror_defs <- length(starts)
    if(N_ageerror_defs > 0)
    {
      nrowsAAK <- nrow(rawAAK)/nsexes - 3
      AAK = array(NA,c(N_ageerror_defs,nrowsAAK,accuage+1))
      age_error_mean <- age_error_sd <- data.frame(age=0:accuage)
      for(i in 1:N_ageerror_defs){
        AAKtemp <- rawAAK[starts[i] + 2 + 1:nrowsAAK,-1]
        # what about 2-sex model?
        for(icol in 1:(accuage+1)) AAKtemp[,icol] <- as.numeric(AAKtemp[,icol])
        AAK[i,,] <- as.matrix(AAKtemp)
        age_error_mean[[paste("type",i,sep="")]] <- as.numeric((rawAAK[starts[i] + 1,-1]))
        age_error_sd[[paste("type",i,sep="")]] <- as.numeric((rawAAK[starts[i] + 2,-1]))
      }
      returndat$AAK <- AAK
      returndat$age_error_mean <- age_error_mean
      returndat$age_error_sd <- age_error_sd
    }
  }

  # catch at age
  catage <- matchfun2("CATCH_AT_AGE",1,"BIOLOGY",-1)
  if(catage[[1]][1]=="absent"){
    catage <- NA
    print("! Warning: no catch-at-age numbers because 'detailed age-structured reports' turned off in starter file.",quote=F)
  }else{
    catage <- catage[,apply(catage,2,emptytest)<1]
    names(catage) <- catage[1,]
    catage <- catage[-1,]
    for(icol in (1:ncol(catage))[substr(names(catage),1,2)!="XX" & names(catage)!="Era"]){
      catage[,icol] <- as.numeric(catage[,icol])
    }
  }
  returndat$catage <- catage
  
  # adding stuff to list which gets returned by function
  if(comp){
    returndat$lendbase      <- lendbase
    returndat$sizedbase     <- sizedbase
    returndat$agedbase      <- agedbase
    returndat$condbase      <- condbase
    returndat$ghostagedbase <- ghostagedbase
    returndat$ladbase       <- ladbase
    returndat$wadbase       <- wadbase
    returndat$tagdbase1     <- tagdbase1
    returndat$tagdbase2     <- tagdbase2
  }

  returndat$derived_quants <- der
  returndat$parameters <- parameters
  returndat$FleetNames <- FleetNames
  returndat$repfiletime <- repfiletime
  returndat$SRRtype <- as.numeric(rawrep[matchfun("SPAWN_RECRUIT"),3]) # type of stock recruit relationship

  if(covar){
    returndat$CoVar    <- CoVar
    if(stats$N_estimated_parameters > 1){returndat$highcor  <- highcor}
    if(stats$N_estimated_parameters > 1){returndat$lowcor   <- lowcor}
    returndat$stdtable <- stdtable
  }
  returndat <- c(returndat,stats)

  # process annual recruit devs
  recdevEarly   <- parameters[substring(parameters$Label,1,13)=="Early_RecrDev",]
  early_initage <- parameters[substring(parameters$Label,1,13)=="Early_InitAge",]
  main_initage  <- parameters[substring(parameters$Label,1,12)=="Main_InitAge",]
  recdev        <- parameters[substring(parameters$Label,1,12)=="Main_RecrDev",]
  recdevFore    <- parameters[substring(parameters$Label,1, 8)=="ForeRecr",]
  recdevLate    <- parameters[substring(parameters$Label,1,12)=="Late_RecrDev",]

  if(nrow(recdev)>0){
    recdev$Yr        <- as.numeric(substring(recdev$Label,14))
  }
  if(nrow(recdevEarly)>0){
    recdevEarly$Yr   <- as.numeric(substring(recdevEarly$Label,15))
  }
  if(nrow(early_initage)>0){
    early_initage$Yr <- startyr - as.numeric(substring(early_initage$Label,15))
    recdevEarly <- rbind(early_initage,recdevEarly)
  }
  if(nrow(main_initage)>0){
    main_initage$Yr  <- startyr - as.numeric(substring(main_initage$Label,14))
    recdev <- rbind(main_initage,recdev)
  }
  if(nrow(recdevFore)>0)
    recdevFore$Yr <- as.numeric(substring(recdevFore$Label,10))
  if(nrow(recdevLate)>0)
    recdevLate$Yr <- as.numeric(substring(recdevLate$Label,14))
  if(nrow(recdevFore)>0 & nrow(recdevLate)>0)
    recdevFore <- rbind(recdevLate,recdevFore)

  Yr <- c(recdevEarly$Yr,recdev$Yr,recdevFore$Yr)
  recruitpars <- rbind(if(nrow(recdevEarly)>0){recdevEarly}else{NULL},
                       if(nrow(recdevEarly)>0){recdev}else{NULL},
                       if(nrow(recdevEarly)>0){recdevFore}else{NULL})
  returndat$recruitpars <- recruitpars
  # process adjustments to recruit devs
  RecrDistpars <- parameters[substring(parameters$Label,1,8)=="RecrDist",]
  returndat$RecrDistpars <- RecrDistpars

  # print list of statistics
  if(printstats){
    print("Statistics shown below (to turn off, change input to printstats=FALSE)",quote=FALSE)

    # remove scientific notation (only for display, not returned values, which were added to returndat already)
    stats$likelihoods_used <- format(stats$likelihoods_used,scientific=20)
    stats$estimated_non_rec_devparameters <- format(stats$estimated_non_rec_devparameters,scientific=20)
    print(stats)
    if(covar){
      if(stats$N_estimated_parameters > 1){print(corstats, quote=FALSE)}else{print("Too few estimated parameters to report correlations")}
    }
  }

  # return the inputs to this function so they can be used by SSplots or other functions
  inputs <- list()
  inputs$dir      <- dir
  inputs$model    <- model
  inputs$repfile  <- repfile
  inputs$forecast <- forecast
  inputs$warn     <- warn
  inputs$covar    <- covar
  inputs$verbose  <- verbose

  returndat$inputs <- inputs

  if(verbose) print("completed SS_output",quote=FALSE)
  invisible(returndat)

} # end function
