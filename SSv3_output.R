SSv3_output <- function(
  dir="C:\\myfiles\\mymodels\\myrun\\", model="ss3",
  repfile="Report.sso", compfile="CompReport.sso",covarfile="CoVar.sso",
  ncols=200, forecast=T, warn=T, covar=T,
  checkcor=T, cormax=0.95, cormin=0.01, printhighcor=10, printlowcor=10,
  verbose=T, printstats=T,hidewarn=F, NoCompOK=F)
{
################################################################################
#
# SSv3_output
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: To import content from SSv3 model run.
# Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
#          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
#          and other contributors to http://code.google.com/p/r4ss/
# Returns: a list containing elements of Report.sso and/or CoVar.sso,
#          formatted as R objects, and optional summary statistics to R console
# General: Updated for Stock Synthesis version 3.03A; R version 2.8.1
# Notes:   See users guide for documentation: http://code.google.com/p/r4ss/wiki/Documentation
# Required packages: none
#
################################################################################

codedate <- "September 1, 2009"

if(verbose){
  print(paste("R function updated:",codedate),quote=F)
  print("Check for new code and report problems at http://code.google.com/p/r4ss/",quote=F)
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

# get info on output files created by SSv3
dir <- paste(dir,"/",sep="")
shortrepfile <- repfile
repfile <- paste(dir,repfile,sep="")

parfile <- paste(dir,model,".par",sep="")
if(!file.exists(parfile)){
  if(!hidewarn) print(paste("Some stats skipped because the .par file not found:",parfile),quote=F)
  parfile <- NA
}

# read three rows to get start time and version number from rep file
if(file.exists(repfile)){
  if(verbose) print(paste("Getting header info from",repfile),quote=F)
}else{
  print(paste("!Error: can't find report file,", repfile),quote=F)
  return()
}
rephead <- readLines(con=repfile,n=3)

# warn if SS version used to create rep file is too old or too new for this code
SS_version <- rephead[1]
SS_versionshort <- toupper(substr(SS_version,1,9))
if(!(SS_versionshort %in% paste("SS-V3.0",c("3A","3B","4-"),sep=""))){
  print(paste("! Warning, this function tested on SS-V3.03A to SS-V3.04. You are using",substr(SS_version,1,9)),quote=F)
}else{
  if(verbose) print(paste("You're using",SS_versionshort,"which should work with this R code."),quote=F)
}

findtime <- function(lines){
  # quick function to get model start time from SSv3 output files
  time <- strsplit(lines[grep('ime',lines)],'ime: ')[[1]]
  if(length(time)<2) return() else return(time[2])
}
repfiletime <- findtime(rephead)
print(paste("Report file time:",repfiletime),quote=F)

corfile <- NA
if(covar){
  # .cor file
  if(!is.na(parfile)){
    corfile <- sub(".par",".cor",parfile)
    if(!file.exists(corfile)){
      print(paste("Some stats skipped because the .cor file not found:",corfile),quote=F)
      corfile <- NA
    }
  }
  # CoVar.sso file
  covarfile <- paste(dir,covarfile,sep="")
  if(!file.exists(covarfile)){
    print("CoVar file not found. Change input to covar=F, or modify 'covarfile' input.",quote=F)
    return()
  }

  # time check for CoVar file
  covarhead <- readLines(con=covarfile,n=2)
  covartime <- findtime(covarhead)
  # the conversion to R time class below may no longer be necessary as strings should match
  covartime2 <- as.POSIXlt(covartime, format="%a %b %d %H:%M:%S %Y")
  repfiletime2 <- as.POSIXlt(repfiletime, format="%a %b %d %H:%M:%S %Y")
  difftimelimit <- 300
  if(abs(as.numeric(difftime(covartime2,repfiletime2,units="secs")))>difftimelimit){
    print(paste("!Error: ",shortrepfile,"and",covarfile,"were modified more than",difftimelimit,"seconds apart. Change input to covar=F"),quote=F)
    print(paste("CoVar time:",covartime),quote=F)
    return()
  }
}

# time check for CompReport file
compfile <- paste(dir,compfile,sep="")
if(file.exists(compfile)){
  comphead <- readLines(con=compfile,n=2)
  comptime <- findtime(comphead)
  if(comptime != repfiletime){
    print(paste(shortrepfile,"and",compfile,"have different time values. Check the input filenames."),quote=F)
    print(paste("CompReport time:",comptime),quote=F)
    return()
  }
  comp <- T
}else{
  print(paste("Missing ",compfile,". Change the compfile input or rerun model to get the file.",sep=""),quote=F)
  #return()
  if(NoCompOK) comp <- F else return()
}

# read report file
if(verbose) print("Reading full report file",quote=F)
flush.console()
rawrep <- read.table(file=repfile,col.names=1:ncols,fill=T,quote="",colClasses="character",nrows=-1,comment.char="")

# check empty columns
nonblanks <- rep(NA,ncols)
for(icol in 1:ncols){
  temp <- unique(rawrep[,icol]!="")
  # temp <- unique(!is.na(rawrep[,icol]))
  nonblanks[icol] <- max(c(0,temp[!is.na(temp)]))
}
maxnonblank = max(c(0,(1:ncols)[nonblanks==1]))
if(maxnonblank==ncols){
  print(      "! Warning, all columns are used and some data may have been missed,",quote=F)
  print(paste("  increase 'ncols' input above current value (ncols=",ncols,")",sep=""),quote=F)
  return(NULL)
}
if((maxnonblank+1)==ncols & verbose){ print("Got all columns.",quote=F)}
if((maxnonblank+1)<ncols){ if(verbose) print(paste("Got all columns. To speed code, future reads of this model may use ncols=",maxnonblank+1,sep=""),quote=F)}
if(verbose) print("Got Report file",quote=F)
flush.console()

# read .CoVar file
if(covar){
  CoVar <- read.table(covarfile,header=T,colClasses=c(rep("numeric",4),rep("character",4),"numeric"),skip=3)
  if(verbose) print("Got CoVar file.",quote=F)
  stdtable <- CoVar[CoVar$Par..j=="Std",c(7,9,5)]
  names(stdtable) = c('name','std','type')
  Nstd <- sum(stdtable$std>0)

  if(Nstd<=1){
    print(paste("Too few estimated quantities in CoVar file (n=",Nstd,"). Change input to covar=F.",sep=""),quote=F)
    return()
  }
  if(checkcor==T)
  {
    corfilter <- CoVar[CoVar$all.i!=CoVar$all.j & CoVar$Par..i=="Par" & CoVar$Par..j=="Par" & !substr(CoVar$label.i,1,8)=="ForeRecr" & !substr(CoVar$label.j,1,8)=="ForeRecr",]
    rangecor <- range(abs(corfilter$corr))
    corstats <- list()
    corstats$cormessage1 <- paste("Range of abs(parameter correlations) is",min(rangecor),"to",max(rangecor))
    # search for high or low correlations in CoVar file
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
  }else{if(verbose) print("You skipped the correlation check",quote=F)}
}else{if(verbose) print("You skipped the CoVar file",quote=F)}
flush.console()

# read forecast report file
if(forecast){
  forcastname <- paste(dir,"Forecast-report.sso",sep="")
  temp <- file.info(forcastname)$size
  if(is.na(temp) | temp==0){
    print("!Error: the Forecase-report.sso file is empty.",quote=F)
    print("        Change input to 'forecast=F' or rerun model with forecast turned on.",quote=F)
    return()
  }
  rawforcast1 <- read.table(file=forcastname,col.names=1:ncols,fill=T,quote="",colClasses="character",nrows=-1)
  endyield <- matchfun("MSY_not_calculated",rawforcast1[,1])
  if(is.na(endyield)) yesMSY <- TRUE else yesMSY <- FALSE
  if(yesMSY) endyield <- matchfun("findFmsy",rawforcast1[,10])
  yieldraw <- rawforcast1[(matchfun("Btarget",rawforcast1[,10])):endyield,]
  yielddat <- yieldraw[c(3:(as.numeric(length(yieldraw[,1])-1))),c(4,7)]
  colnames(yielddat) <- c("Catch","Depletion")
  yielddat$Catch <- as.numeric(yielddat$Catch)
  yielddat$Depletion <- as.numeric(yielddat$Depletion)
  yielddat <- yielddat[order(yielddat$Depletion,decreasing = FALSE),]
  if(verbose) print("Got forecast file",quote=F)
}else{if(verbose) print("You skipped the forecast file",quote=F)}
flush.console()

# read warnings file
if(warn){
  warnname <- paste(dir,"warning.sso",sep="")
  if(!file.exists(warnname)){
    print("warning.sso file not found",quote=F)
    warn <- NA
  }else{
    warn <- readLines(warnname,warn=F)
    warnstring <- warn[grep("N warnings: ",warn)]
    if(length(warnstring)>0){
      nwarn <- as.numeric(strsplit(warnstring,"N warnings: ")[[1]][2])
      textblock <- c(paste("were", nwarn, "warnings"),paste("was", nwarn, "warning"))[1+(nwarn==1)]
      if(verbose) print(paste("Got warning file. There", textblock, "in", warnname),quote=F)
    }else{
      print("warning.sso file is missing the string 'N warnings'!")
    }
  }
}else{
  if(verbose) print("You skipped the warnings file",quote=F)
}
if(verbose) print("Finished reading files",quote=F)
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

if(comp){   # skip this stuff if no CompReport.sso file
  allbins <- read.table(file=compfile, col.names=1:ncols, fill=T, colClasses="character", skip=3, nrows=15)
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
  rawcompdbase <- read.table(file=compfile, col.names=1:21, fill=T, colClasses="character", skip=18, nrows=-1)
  names(rawcompdbase) <- rawcompdbase[1,]
  compdbase <- rawcompdbase[2:(nrow(rawcompdbase)-2),] # subtract header line and last 2 lines
  compdbase <- compdbase[compdbase$Obs!="",]
  for(i in (1:ncol(compdbase))[!(names(compdbase) %in% c("effN","Kind"))]) compdbase[,i] <- as.numeric(compdbase[,i])
  lendbase   <- compdbase[compdbase$Kind=="LEN" & compdbase$N > 0,]
  agedbase   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0,]
  latagebase <- compdbase[compdbase$Kind=="L@A" & compdbase$N > 0,]
  lendbase$effN <- as.numeric(lendbase$effN)
  agedbase$effN <- as.numeric(agedbase$effN)
  agebins <- sort(unique(agedbase$Bin[!is.na(agedbase$Bin)]))
  nagebins <- length(agebins)
}else{
  lbins <- NA
  nlbins <- NA
  lbinspop <- NA
  nlbinspop <- NA
  agebins <- NA
  nagebins <- NA
  compdbase <- NA
  agedbase <- NA
  latagebase <- NA
  Lbin_method <- 2
}
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

morph_indexing <-matchfun2("MORPH_INDEXING",1,"MOVEMENT",-1,cols=1:9)
names(morph_indexing) <- morph_indexing[1,]
morph_indexing <- morph_indexing[-1,]
for(i in 1:ncol(morph_indexing)) morph_indexing[,i] <- as.numeric(morph_indexing[,i])
ngpatterns <- max(morph_indexing$Gpattern)

if(forecast){
  grab  <- rawforcast1[,1]
  nforecastyears <- as.numeric(rawforcast1[grab %in% c("N_forecast_yrs:"),2])
  nforecastyears <- nforecastyears[1]
}else{
  nforecastyears <- NA
}
if(verbose) print("Finished dimensioning",quote=F)
flush.console()

# stats list: items that are output to the GUI (if printstats==T) for a quick summary of results
stats <- list()
stats$SS_version <- SS_version
stats$Run_time <- paste(as.vector(rawrep[2,1:6]),collapse=" ")

tempfiles  <- as.data.frame(rawrep[4:5,1:2],row.names = NULL)
tempfiles <- matchfun2("Data_File",0,"Control_File",0,cols=1:2)
stats$Files_used <- paste(c(tempfiles[1,],tempfiles[2,]),collapse=" ")

stats$warnings <- warn

rawlike <- matchfun2("LIKELIHOOD",2,"Fleet:",-2,cols=1:3)
like <- data.frame(signif(as.numeric(rawlike[,2]),digits=7))
names(like) <- "values"
rownames(like) <- rawlike[,1]
like$lambdas <- rawlike[,3]
stats$likelihoods_used <- like

like2 <- matchfun2("Fleet:",0,"Input_Variance_Adjustment",-1,cols=1:(2+nfleets))
names(like2) <- like2[1,]
stats$likelihoods_raw_by_fleet <- like2[2:length(like2[,1]),]

rawpars <- matchfun2("PARAMETERS",1,"DERIVED_QUANTITIES",-1,cols=1:14)
names(rawpars) <- rawpars[1,]
rawpars <- rawpars[-1,]
allpars <- rawpars
allpars[allpars=="_"] <- NA
for(i in (1:ncol(allpars))[!(names(allpars)%in%c("Label","Status"))]) allpars[,i] = as.numeric(allpars[,i])

if(!is.na(parfile)){ parline <- read.table(parfile,fill=T,comment.char='',nrows=1)
}else{ parline <- matrix(NA,1,16) }
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
pars <- pars[pars$Phase %in% 0:25,]
stats$estimated_non_rec_devparameters <- pars[,c(2,3,5:15)]

rawder <- matchfun2("DERIVED_QUANTITIES",4,"MGParm_Block_Assignments",-1,cols=1:3)
names(rawder) <- rawder[1,]
der <- rawder[-1,]
der[der=="_"] <- NA
for(i in 2:3) der[,i] = as.numeric(der[,i])

managementratiolabels <- matchfun2("DERIVED_QUANTITIES",1,"DERIVED_QUANTITIES",3,cols=1:2)
names(managementratiolabels) <- c("Ratio","Label")

if(covar & !is.na(corfile)) stats$log_det_hessian <- read.table(corfile,nrows=1)[1,10]
stats$maximum_gradient_component <- parline[1,16]
stats$sigma_R_in <- as.numeric(rawrep[(matchfun("SPAWN_RECRUIT")+3),1])
stats$sigma_R_out <- as.numeric(rawrep[(matchfun("N_est")+1),2])

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

rawagentune <- matchfun2("LEN_SELEX",-(nfleets+1),"LEN_SELEX",-1,cols=1:10)
names(rawagentune) <- rawagentune[1,]
rawagentune <- rawagentune[2:length(rawagentune[,1]),]
rawagentune[,1] <- rawagentune[,10]
agentune <- rawagentune[,c(1,2,4,5,6,8,9)]
agentune <- agentune[agentune$N > 0,]
stats$Age_comp_Eff_N_tuning_check <- agentune

if(verbose) print("Finished primary run statistics list",quote=F)
flush.console()

# data return object
returndat <- list()

returndat$nfleets     <- nfleets
returndat$nfishfleets <- nfishfleets
returndat$nsexes      <- nsexes
returndat$ngpatterns  <- ngpatterns
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

# Static growth
begin <- matchfun("N_Used_morphs",rawrep[,6])+1
if(comp){
  rawbio <- rawrep[begin:(begin+nlbinspop),1:8]
  names(rawbio) <- rawbio[1,]
  bio <- rawbio[-1,]
  for(i in 1:ncol(bio)) bio[,i] <- as.numeric(bio[,i])
}else{
  bio <- NA
}
returndat$biology <- bio

rawgrow <- matchfun2("Biology_at_age",1,"MEAN_BODY_WT(begin)",-1,cols=1:18)
names(rawgrow) <- rawgrow[1,]
growdat <- rawgrow[-1,]
for(i in 1:ncol(growdat)) growdat[,i] <- as.numeric(growdat[,i])
nmorphs <- max(growdat$Morph)
midmorphs <- c(c(0,nmorphs/nsexes)+ceiling(nmorphs/nsexes/2))
if(nseasons > 1){growdat <- growdat[growdat$Seas==1,]}
returndat$endgrowth <- growdat

# Time-varying growth
 rawgrow <- matchfun2("MEAN_SIZE_TIMESERIES",1,"mean_size_Jan_1_for_gender",-1,cols=1:(4+accuage+1))
 if(length(rawgrow)>1){
   names(rawgrow) <- rawgrow[1,]
   growdat <- rawgrow[-1,]
   for(i in 1:ncol(growdat)) growdat[,i] <- as.numeric(growdat[,i])
   growdat <- growdat[growdat$Beg==1 & growdat$Yr < endyr,]
   if(nseasons > 1) growdat <- growdat[growdat$Seas==1,]
   returndat$growthseries <- growdat
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
   if(nsexes==1) stats$SBzero <- stats$SBzero/2
   stats$current_depletion <- depletionseries[length(depletionseries)]

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
   depletion_basis <- as.numeric(strsplit(depletion_basis,"%*",fixed=T)[[1]][1])/100
 }
 returndat$depletion_method <- depletion_method
 returndat$depletion_basis <- depletion_basis

# discard fractions ###

 # degrees of freedom for T-distribution
 DF_discard <- rawrep[matchfun("DISCARD_OUTPUT"),2]
 DF_discard <- as.numeric(strsplit(DF_discard,"=_")[[1]][2])

 rawdisc <- matchfun2("DISCARD_OUTPUT",1,"MEAN_BODY_WT_OUTPUT",-1)
 discard_type <- rawdisc[1,1]
 rawdisc <- rawdisc[-1,]
 if(!((length(rawdisc[,1])) == 1))
 {
   rawdisc <- rawdisc[,rawdisc[1,]!=""]
   names(rawdisc) <- rawdisc[1,]
   discard <- rawdisc[-1,]
 }else{
   discard <- NA
 }
 returndat$discard <- discard
 returndat$discard_type <- discard_type
 returndat$DF_discard <- DF_discard

# Average body weight observations

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

#testing: bad

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
 }else{if(verbose) print("You skipped the equilibrium yield data",quote=F)}
 flush.console()

 returndat$managementratiolabels <- managementratiolabels

# Spawner-recruit curve
 rawsr <- matchfun2("SPAWN_RECRUIT",7,"N_est",-1,cols=1:9)
 names(rawsr) <- rawsr[1,]
 rawsr[rawsr=="_"] <- NA
 rawsr <- rawsr[-(1:2),] # remove header rows
 sr <- rawsr[-(1:2),] # remove rows for Virg and Init
 for(i in 1:(ncol(sr)-1)) sr[,i] <- as.numeric(sr[,i])
 returndat$recruit <- sr

# CPUE/Survey series
 rawcpue <- matchfun2("INDEX_2",1,"INDEX_2",ncpue+1,cols=1:10)
 if(ncpue>0){
   names(rawcpue) <- rawcpue[1,]
   cpue <- rawcpue[-1,]
   for(i in 2:ncol(cpue)) cpue[,i] <- as.numeric(cpue[,i])
   cpue$FleetName <- NA
   cpue$FleetNum <- NA
   for(i in 1:nrow(cpue)){
     cpue$FleetNum[i] <- strsplit(cpue$Fleet[i],"_")[[1]][1]
     cpue$FleetName[i] <- substring(cpue$Fleet[i],nchar(cpue$FleetNum[i])+2)}
 }else{cpue <- NA}
 returndat$cpue <- cpue

# Numbers at age
 rawnatage <- matchfun2("NUMBERS_AT_AGE",1,"CATCH_AT_AGE",-1,cols=1:(11+accuage),substr1=FALSE)
 if(length(rawnatage)>1){
   names(rawnatage) <- rawnatage[1,]
   rawnatage <- rawnatage[-1,]
   for(i in (1:ncol(rawnatage))[names(rawnatage)!="Era"]) rawnatage[,i] = as.numeric(rawnatage[,i])
   returndat$natage <- rawnatage
 }

# Movement
 movement <- matchfun2("MOVEMENT",1,"EXPLOITATION",-1,cols=1:(7+accuage),substr1=FALSE)
 names(movement) <- c(movement[1,1:6],paste("age",movement[1,-(1:6)],sep=""))
 movement <- movement[-1,]
 for(i in 1:ncol(movement)) movement[,i] <- as.numeric(movement[,i])
 returndat$movement <- movement

if(comp){
  # age-length matrix
  rawALK <- matchfun2("AGE_LENGTH_KEY",4,"AGE_AGE_KEY",-1,cols=1:(accuage+2))
  if(length(rawALK)>1){
    ALK = array(NA,c(nmorphs,nlbinspop,accuage+1))
    starts <- grep("Morph:",rawALK[,3])+2
    ends <- grep("mean",rawALK[,1])-1
    for(i in 1:nmorphs){
      ALKtemp <- rawALK[starts[i]:ends[i],-1]
      for(icol in 1:(accuage+1)) ALKtemp[,icol] <- as.numeric(ALKtemp[,icol])
      ALK[i,,] <- as.matrix(ALKtemp)
    }
    returndat$ALK <- ALK
  }
}

# ageing error matrices
 rawAAK <- matchfun2("AGE_AGE_KEY",1,"SELEX_database",-1,cols=1:(accuage+2))
 if(length(rawAAK)>1){
   starts <- grep("KEY:",rawAAK[,1])
   N_ageerror_defs <- length(starts)
   if(N_ageerror_defs > 0)
   {
     nrowsAAK <- nrow(rawAAK)/nsexes - 3
     AAK = array(NA,c(N_ageerror_defs,nrowsAAK,accuage+1))
     age_error_sd = 0:accuage
     for(i in 1:N_ageerror_defs){
       AAKtemp <- rawAAK[starts[i] + 1 + 1:nrowsAAK,-1]
       # what about 2-sex model?
       for(icol in 1:(accuage+1)) AAKtemp[,icol] <- as.numeric(AAKtemp[,icol])
       AAK[i,,] <- as.matrix(AAKtemp)
       age_error_sd <- cbind(age_error_sd,as.numeric((rawAAK[starts[i] + 2,-1])))
     }
     returndat$AAK <- AAK
   }
 }

 returndat$composition_database <- compdbase

 returndat$derived_quants <- der
 returndat$parameters <- allpars
 returndat$FleetNames <- FleetNames
 returndat$repfiletime <- repfiletime
 returndat$SRRtype <- as.numeric(rawrep[matchfun("SPAWN_RECRUIT"),3]) # type of stock recruit relationship
 
 if(covar){
   returndat$CoVar    <- CoVar
   returndat$highcor  <- highcor
   returndat$lowcor   <- lowcor
   returndat$stdtable <- stdtable
 }
 returndat <- c(returndat,stats)


# print list of statistics
 if(printstats){
   print("Statistics shown below (to turn off, change input to printstats=F)",quote=F)

   # remove scientific notation (only for display, not returned values, which were added to returndat already)
   stats$likelihoods_used <- format(stats$likelihoods_used,scientific=20)
   stats$estimated_non_rec_devparameters <- format(stats$estimated_non_rec_devparameters,scientific=20)
   print(stats)
   if(covar){
     print(corstats, quote=F)
   }
 }

# return the inputs to this function so they can be used by SSv3_plots or other functions
 inputs <- list()
 inputs$dir      <- dir
 inputs$model    <- model
 inputs$repfile  <- repfile
 inputs$forecast <- forecast
 inputs$warn     <- warn
 inputs$covar    <- covar
 inputs$verbose  <- verbose

 returndat$inputs <- inputs

 if(verbose) print("completed SSv3_output",quote=F)
 invisible(returndat)

} # end function
