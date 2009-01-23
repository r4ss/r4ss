SSv3_output <- function(
         dir="C:\\myfiles\\mymodels\\myrun\\", model="default", repfile="Report.SSO", 
         ncols=200, forecast=T, warn=T, covar=T, checkcor=T, cormax=0.95, cormin=0.01, printhighcor=10, printlowcor=10,
         verbose=T, printstats=T, return="Yes")
{
################################################################################
#
# SSv3_output BETA January 22, 2008.
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: To import content from SSv3 model run.
# Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
#          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
# Returns: a list containing elements of Report.SSO and/or CoVar.SSO,
#          formatted as R objects, and optional summary statistics to R console
# General: Updated for Stock Synthesis version 3.02b January, 2008; R version 2.8.1
# Notes:   See users guide for documentation: http://code.google.com/p/r4ss/wiki/Documentation
# Required packages: none
#
################################################################################

if(verbose) print("running SSv3_output:",quote=F)
flush.console()

# Defining internal functions: matchfun and matchfun2
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
repfiletime <- file.info(repfile)$mtime
if(model=="default")
{
  shortparfiles <- dir(dir,pattern=".par",full.names=F)
  allparfiles <- dir(dir,pattern=".par",full.names=T)
  if(length(allparfiles)==0){
    print(paste("Some stats skipped because there are no .par files found in directory:",dir),quote=F)
    parfile <- NA
  }
  if(length(allparfiles)==1){
    parfile <- allparfiles
    model <- strsplit(shortparfiles,split='.par')[[1]]
  }
  if(length(allparfiles)>1){
    print("Multiple .par files found in directory. To get stats from parfile and corfile, specify model name in input (i.e. model='SS3')",quote=F)
    parfile <- NA
  }
}else{
  parfile <- paste(dir,model,".par",sep="")
  if(!file.exists(parfile)){
    print(paste("Some stats skipped because the .par file not found:",parfile),quote=F)
    parfile <- NA
  }
}

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
  # CoVar.SSO file
  covarfile <- paste(dir,"CoVar.SSO",sep="")
  if(!file.exists(covarfile)){
    print("CoVar.SSO not found. Change input to covar=F.",quote=F)
    return()
  }
  
  # time check
  covartime <- file.info(covarfile)$mtime
  difftimelimit <- 10
  if(abs(as.numeric(difftime(covartime,repfiletime,units="secs")))>difftimelimit){
    print(paste(shortrepfile,"and CoVar.SSO were modified more than",difftimelimit,"seconds apart. Change input to covar=F"),quote=F)
    return()
  }
}

# read report file
if(verbose) print(paste("reading",repfile),quote=F)
flush.console()
rawrep <- read.table(file=repfile,col.names=1:ncols,fill=T,quote="",colClasses="character",nrows=-1)
SS_version <- rawrep[1,1]
if(substr(SS_version,1,9)!="SS-V3.02B"){
  print(paste("! Warning, this function is built for SS-V3.02B. You are using",substr(SS_version,1,9)),quote=F) 
}

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
if((maxnonblank+1)==ncols){ print("Got all columns.",quote=F)}
if((maxnonblank+1)<ncols){ print(paste("Got all columns. To speed code, future reads of this model may use ncols=",maxnonblank+1,sep=""),quote=F)}
if(verbose) print("Got Report file",quote=F)
flush.console()

# read .CoVar file
if(covar){
  CoVar <- read.table(covarfile,header=T,colClasses=c(rep("numeric",4),rep("character",4),"numeric"))
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
    lowcortestlist$max <- NA
    for(i in 1:length(lowcortestlist[,1]))
    {
      lowcortestlist$max[i] <- max(corfilter$corr[corfilter$label.i == lowcortestlist$name[i]],corfilter$corr[corfilter$label.j == lowcortestlist$name[i]])
    }
    lowcor <- lowcortestlist[abs(lowcortestlist$max) <= cormin,2:3]
    nhighcor <- nrow(highcor)   
    nlowcor <- nrow(lowcor)   
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
        corstats$cormessag5 <- highcorsub
      }
    }else{
      corstats$cormessag6 <- "High correlations not reported. To report, change 'printhighcor' input to a positive value."
    }
    if(printlowcor>0){
      if(nlowcor==0) textblock <- "No uncorrelated parameters"  
      if(nlowcor==1) textblock <- "1 uncorrelation"  
      if(nlowcor>1)  textblock <- paste(nlowcor,"uncorrelated parameters")  
      corstats$cormessag7 <- paste(textblock, " below threshold (cormin=", cormin,")",sep="")
      if(nlowcor>0 & nlowcor<=printlowcor){
        corstats$cormessag8 <-lowcor
      }
      if(nlowcor>0 & nlowcor>printlowcor){
        lowcorsub <- lowcor[order(abs(lowcor$max)),]
        lowcorsub <- lowcorsub[1:printlowcor,]
        corstats$cormessag9 <- paste("Lowest",printlowcor,
        "parameters uncorrelations below threshold (to print more, increase 'printlowcor' input):")
        corstats$cormessag10 <-lowcorsub
      }
    }else{
      corstats$cormessag11 <-"Uncorrelated parameters not reported. To report, change 'printlowcor' input to a positive value."
    }
  }else{if(verbose) print("You skipped the correlation check",quote=F)}
}else{if(verbose) print("You skipped the CoVar file",quote=F)}
flush.console()

# read forecast report file
if(forecast){
  forcastname <- paste(dir,"Forecast-report.SSO",sep="")
  rawforcast1 <- read.table(file=forcastname,col.names=c(seq(1,ncols,by=1)),fill=T,quote="",colClasses="character",nrows=-1)
  #rawforcast <- rawforcast1[(matchfun("Management_report",rawforcast1[,1]):(length(rawforcast1[,1]))),]
  if(verbose) print("Got forecast file",quote=F)
}else{if(verbose) print("You skipped the forecast file",quote=F)}
flush.console()

# read warnings file
if(warn){
  warnname <- paste(dir,"warning.SSO",sep="")
  if(!file.exists(warnname)){
    print("warning.SSO file not found",quote=F)
    warn <- NA
  }else{
    warn <- readLines(warnname,warn=F)
    nwarn <- length(warn)
    textblock <- c(paste("were", nwarn, "warnings"),paste("was", nwarn, "warning"))[1+(nwarn==1)]
    if(verbose) print(paste("Got warning file. There", textblock, "in", warnname),quote=F)
  }
}else{
  if(verbose) print("You skipped the warnings file",quote=F)
}
if(verbose) print("Finished reading files",quote=F)
flush.console()

# Useful dimensions
getdim <- matchfun2("LEN_SELEX",2,"RETENTION",-1)
nfleets <- length(unique(as.numeric(getdim[,1])))
nfishfleets <- max(as.numeric(as.vector(matchfun2("RETENTION",2,"DISCARD_MORT",-1,cols=1))))
nsexes <- length(unique(as.numeric(getdim[,3])))
allbins <- matchfun2("Size_Bins_pop", 1, "Size_Bins_pop", 8, cols=1:ncols)

#lbins is data length bins
lbins <- as.numeric(allbins[6,-1])
lbins <- lbins[!is.na(lbins)]
nlbins <- length(lbins)

#lbinspop is Pop_len_mid used for selex and bio quantities
lbinspop <- as.numeric(allbins[3,-1])
lbinspop <- lbinspop[!is.na(lbinspop)]
nlbinspop <- length(lbinspop)

FleetNames <- matchfun2("FleetNames",1,"FleetNames",nfleets,cols=2)

# read composition database
rawcompdbase <- matchfun2("Composition_Database",1,"SELEX_database",-1,cols=1:19)
names(rawcompdbase) <- rawcompdbase[1,]
compdbase <- rawcompdbase[-1,]
compdbase <- compdbase[compdbase$Obs!="",]
for(i in (1:ncol(compdbase))[!(names(compdbase) %in% c("effN","Kind"))]) compdbase[,i] <- as.numeric(compdbase[,i])

lendbase   <- compdbase[compdbase$Kind=="LEN" & compdbase$N > 0,]
agedbase   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0,]
latagebase <- compdbase[compdbase$Kind=="L@A" & compdbase$N > 0,]
Lbin_method <- as.numeric(rawrep[matchfun("Method_for_Lbin"),2])
lendbase$effN <- as.numeric(lendbase$effN)
agedbase$effN <- as.numeric(agedbase$effN)
agebins <- unique(agedbase$Bin[!is.na(agedbase$Bin)])
nagebins <- length(agebins)
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
stats$Files_used <- paste(c(tempfiles[1,],tempfiles[2,]),collapse=" ")

stats$warnings <- warn

rawlike <- matchfun2("LIKELIHOOD",2,"Fleet:",-2,cols=1:3)
like <- data.frame(signif(as.numeric(rawlike[,2]),digits=7))
names(like) <- "values"
rownames(like) <- rawlike[,1]
like$lambdas <- rawlike[,3]
stats$used_likelihoods <- like

like2 <- matchfun2("Fleet:",0,"Input_Variance_Adjustment",-1,cols=1:(2+nfleets))
names(like2) <- like2[1,]
stats$raw_likelihoods_by_fleet <- like2[2:length(like2[,1]),]

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
if("dimensions" %in% return | return=="Yes"){
  returndat$nfleets     <- nfleets
  returndat$nfishfleets <- nfishfleets
  returndat$nsexes      <- nsexes
  returndat$lbins       <- lbins
  returndat$lbins       <- lbins
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
}
if(return=="Yes") returndat$morph_indexing <- morph_indexing

# Static growth
begin <- matchfun("N_Used_morphs",rawrep[,6])+1
rawbio <- rawrep[begin:(begin+nlbinspop),1:7]
names(rawbio) <- rawbio[1,]
bio <- rawbio[-1,]
for(i in 1:ncol(bio)) bio[,i] <- as.numeric(bio[,i])
if("biology" %in% return | return=="Yes") returndat$biology <- bio

rawgrow <- matchfun2("Biology_at_age",1,"MEAN_BODY_WT(begin)",-1,cols=1:18)
names(rawgrow) <- rawgrow[1,]
growdat <- rawgrow[-1,]
for(i in 1:ncol(growdat)) growdat[,i] <- as.numeric(growdat[,i])
nmorphs <- max(growdat$Morph)
midmorphs <- c(c(0,nmorphs/nsexes)+ceiling(nmorphs/nsexes/2))
if(nseasons > 1){growdat <- growdat[growdat$Seas==1,]}
if("endgrowth" %in% return | return=="Yes") returndat$endgrowth <- growdat

# Time-varying growth
 rawgrow <- matchfun2("MEAN_SIZE_TIMESERIES",1,"mean_size_Jan_1_for_gender",-1,cols=1:(4+accuage+1))
 if(length(rawgrow)>1){
   names(rawgrow) <- rawgrow[1,]
   growdat <- rawgrow[-1,]
   for(i in 1:ncol(growdat)) growdat[,i] <- as.numeric(growdat[,i])
   growdat <- growdat[growdat$Beg==1 & growdat$Yr < endyr,]
   if(nseasons > 1) growdat <- growdat[growdat$Seas==1,]
   if("growthseries" %in% return | return=="Yes") returndat$growthseries <- growdat
 }
 
# Length selex and retention
 rawselex <- matchfun2("LEN_SELEX",1,"RETENTION",-1,cols=1:(nlbinspop+4))
 names(rawselex)<- rawselex[1,]
 selex <- rawselex[-1,]
 if(!forecast) selex <- selex[selex$year <= endyr,]
 for(icol in c(1:3,5:ncol(selex))) selex[,icol] = as.numeric(selex[,icol])
 if("sizeselex" %in% return | return=="Yes") returndat$sizeselex <- selex

 rawret <- matchfun2("RETENTION",1,"DISCARD_MORT",-1,cols=1:(nlbinspop+4))
 names(rawret) <- rawret[1,]
 rawret <- rawret[-1,]
 if(!forecast) rawret <- rawret[rawret$year <= endyr,]
 if("retention" %in% return | return=="Yes") returndat$retention <- rawret

# Age based selex
 rawageselex <- rawrep[(matchfun("AGE_SELEX")+1):(matchfun("Average_size_selex_at_age_in_endyear")-1),1:(accuage+5)]
 names(rawageselex)<- rawageselex[1,]
 ageselex <- rawageselex[-1,]
 if(!forecast) ageselex <- ageselex[ageselex$year <= endyr,]
 for(icol in c(1:3,5:ncol(ageselex))) ageselex[,icol] = as.numeric(ageselex[,icol])
 if("ageselex" %in% return | return=="Yes") returndat$ageselex <- ageselex

# time series
 rawts <- matchfun2("TIME_SERIES",1,"SPR_series",-1,cols=1:ncols)
 tsfull <- rawts[,rawts[1,]!=""]
 names(tsfull) <- tsfull[1,]
 tsfull <- tsfull[-1,]
 tsfull[tsfull=="_"] <- NA
 for(i in (1:ncol(tsfull))[names(tsfull)!="Era"]) tsfull[,i] = as.numeric(tsfull[,i])
 if("timeseries" %in% return | return=="Yes") returndat$timeseries <- tsfull

# stats and dimensions
 tsfull$Yr <- tsfull$Yr + (tsfull$Seas-1)/nseasons
 ts <- tsfull[tsfull$Yr <= endyr+1,]
 tsyears <- ts$Yr[ts$Seas==1]
 tsspaw_bio <- ts$SpawnBio[ts$Seas==1]
 if(nsexes==1) tsspaw_bio <- tsspaw_bio/2
 depletionseries <- tsspaw_bio/tsspaw_bio[1]
 stats$SBzero <- ts$SpawnBio[1]
 if(nsexes==1) stats$SBzero <- stats$SBzero/2
 tsspaw_bio <- ts$SpawnBio[ts$Seas==1]
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
 if(return=="Yes") returndat$F_method <- F_method
 if(F_method==1){
   stringmatch <- "Hrate:_"
 }else{stringmatch <- "F:_"}
 Hrates <- as.matrix(ts[,substr(names(ts),1,nchar(stringmatch))==stringmatch])
 fmax <- max(Hrates)
 #stats$fmax <- fmax
 #stats$endyrcatch <- ts$totcatch[ls]
 #stats$endyrlandings <- ts$totretained[ls]

# depletion
 depletion_basis <- as.numeric(rawrep[matchfun("depletion_basis"),2])
 depletion_level <- as.numeric(strsplit(rawrep[matchfun("depletion_basis"),4],"*",fixed=T)[[1]][1])
 if(return=="Yes"){
   returndat$depletion_basis <- depletion_basis
   returndat$depletion_level <- depletion_level
 }

# Average body weight observations
 rawmnwgt <- matchfun2("MEAN_BODY_WT",1,"FIT_LEN_COMPS",-1,cols=1:10)
 names(rawmnwgt) <- rawmnwgt[1,]
 mnwgt <- NA
 if(nrow(rawmnwgt)>1)
 {
   names(rawmnwgt) <- rawmnwgt[1,]
   mnwgt <- rawmnwgt[-1,]
   for(i in 1:ncol(mnwgt)) mnwgt[,i] <- as.numeric(mnwgt[,i])
 } # if mean weight data exists
 if(return=="Yes") returndat$mnwgt <- mnwgt

# Yield and SPR time-series
 rawspr <- matchfun2("SPR_series",4,"SPAWN_RECRUIT",-1,cols=1:(20+2*nmorphs))
 names(rawspr) <- rawspr[1,]
 rawspr[rawspr=="_"] <- NA
 spr <- rawspr[-1,]
 for(i in (1:ncol(spr))[!(names(spr)%in%c("Actual:","More_F(by_morph):"))]) spr[,i] <- as.numeric(spr[,i])
 spr <- spr[spr$Year <= endyr,]
 spr$spr <- spr$SPR
 if("sprseries" %in% return | return=="Yes") returndat$sprseries <- spr
 stats$last_years_sprmetric <- spr$spr[length(spr$spr)]

 #if(forecast){
  # stats$spr_at_msy <- as.numeric(rawforcast[33,2])
  # stats$exploit_at_msy <- as.numeric(rawforcast[35,2])
  # stats$bmsy_over_VLHbzero <- as.numeric(rawforcast[38,3])
  # stats$retained_msy <- as.numeric(rawforcast[43,5])
 #}else{if(verbose) print("You skipped the MSY statistics",quote=F)}
 #flush.console()

 if("managementratiolabels" %in% return | return=="Yes") returndat$managementratiolabels <- managementratiolabels


# Spawner-recruit curve
 rawsr <- matchfun2("SPAWN_RECRUIT",7,"N_est",-1,cols=1:9)
 names(rawsr) <- rawsr[1,]
 rawsr[rawsr=="_"] <- NA
 rawsr <- rawsr[-(1:2),] # remove header rows
 sr <- rawsr[-(1:2),] # remove rows for Virg and Init
 for(i in 1:(ncol(sr)-1)) sr[,i] <- as.numeric(sr[,i])
 if("recruit" %in% return | return=="Yes") returndat$recruit <- sr

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
 if("cpue" %in% return | return=="Yes") returndat$cpue <- cpue

 # Numbers at age
 if("natage" %in% return | return=="Yes"){
   rawnatage <- matchfun2("NUMBERS_AT_AGE",1,"CATCH_AT_AGE",-1,cols=1:(11+accuage),substr1=FALSE)
   if(length(rawnatage)>1){
     names(rawnatage) <- rawnatage[1,]
     rawnatage <- rawnatage[-1,]
     for(i in (1:ncol(rawnatage))[names(rawnatage)!="Era"]) rawnatage[,i] = as.numeric(rawnatage[,i])
     returndat$natage <- rawnatage
   }
 }

# Movement
if(return=="Yes"){
  movement <- matchfun2("MOVEMENT",1,"EXPLOITATION",-1,cols=1:(7+accuage),substr1=FALSE)
  names(movement) <- c(movement[1,1:6],paste("age",movement[1,-(1:6)],sep=""))
  movement <- movement[-1,]
  for(i in 1:ncol(movement)) movement[,i] <- as.numeric(movement[,i])
  returndat$movement <- movement
}

# return list of statistics
 if(printstats){
   print("Statistics shown below (to turn off, change input to printstats=F)",quote=F)
   print(stats)
   if(covar){
     print(corstats, quote=F)
     }
 }

# age-length matrix
 if("ALK" %in% return | return=="Yes"){
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
 if("AGE_AGE_KEY" %in% return | return=="Yes"){
   rawAAK <- matchfun2("AGE_AGE_KEY",1,"Size_Bins_pop",-1,cols=1:(accuage+2))
   if(length(rawAAK)>1){
     starts <- grep("KEY:",rawAAK[,1])
     N_ageerror_defs <- length(starts)
     if(N_ageerror_defs > 0)
     {
       AAK = array(NA,c(N_ageerror_defs,nagebins,accuage+1))
       age_error_sd = 0:accuage
       for(i in 1:N_ageerror_defs){
         AAKtemp <- rawAAK[starts[i] + 1 + 1:nagebins,-1]
         # what about 2-sex model?
         for(icol in 1:(accuage+1)) AAKtemp[,icol] <- as.numeric(AAKtemp[,icol])
         AAK[i,,] <- as.matrix(AAKtemp)
         age_error_sd <- cbind(age_error_sd,as.numeric((rawAAK[starts[i] + 2,-1])))
       }
       returndat$AAK <- AAK
     }
   }
 }

 if("compdbase" %in% return | return=="Yes") returndat$composition_database <- compdbase

 if(return=="Yes"){
   returndat$derived_quants <- der
   returndat$parameters <- allpars
   returndat$FleetNames <- FleetNames
 }
 if(covar){
   if("covar" %in% return | return=="Yes") returndat$CoVar <- CoVar
   if("highcor" %in% return | return=="Yes") returndat$highcor <- highcor
   if("lowcor" %in% return | return=="Yes") returndat$lowcor <- lowcor
   if("stdtable" %in% return | return=="Yes") returndat$stdtable <- stdtable
 }
 if("stats" %in% return | return=="Yes") returndat <- c(returndat,stats)
  
 # return the inputs to this function so they can be used by SSv3_plots or other functions
 if("inputs" %in% return | return=="Yes"){
   inputs <- list()
   inputs$dir      <- dir
   inputs$model    <- model
   inputs$repfile  <- repfile
   inputs$forecast <- forecast
   inputs$warn     <- warn
   inputs$covar    <- covar
   inputs$verbose  <- verbose
   returndat$inputs <- inputs
 }         
 
 if(verbose) print("completed SSv3.output",quote=F)
 if(return!="No"){invisible(returndat)}

} # end function
