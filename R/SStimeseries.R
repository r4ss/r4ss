#' A function to create a time-series table from an SS Report.sso file
#'
#' Reads the Report.sso within the directory and creates a time-series
#' table as required by the current Terms of Refernce for West Coast 
#' groundfish.  Table includes the historical and the forecast years for 
#' the model.  Works with Stock Synthesis versions 3.24U and later.
#'
#' @param dir Locates the directory of the files to be read in, double
#' backslashes (or forwardslashes) and quotes necessary.
#' @param plotdir Directory where the table will be saved.  The default 
#' saves the table to the dir location where the Report.sso file is located.
#' @return A csv file containing a time-series of total biomass, summary 
#' biomass, spawning biomass or output, relative depletion, total dead catch
#' the SPR, and the exploitation.
#' @author Chantel Wetzel
#' @export
#'
SStimeseries <- function(dir,  plotdir = 'default'){
	# Create a time-series table 
	# Based on PFMC groundfish assessment required table
	# All the quantities are found using a readLines command, not the SS_output function.
	# The SS_output function is run to get indexing values for the model. 
	wd        <- paste(dir, "/Report.sso", sep="")
	base      <- readLines(wd)	
	rawrep <- read.table(file= wd , col.names = 1:400, fill = TRUE, quote = "", 
								colClasses = "character", nrows = -1, comment.char = "")

	if (plotdir == 'default') { csv.dir = paste0(dir,"/tables/") }
	if (plotdir != 'default') { csv.dir = paste0(plotdir,"/tables/")}
	dir.create(csv.dir) 

	SS_versionCode 		<- base[grep("#V",base)]
  	SS_version 			<- base[grep("Stock_Synthesis",base)]
  	SS_version 			<- SS_version[substring(SS_version,1,2)!="#C"] # remove any version numbering in the comments
  	SS_versionshort 	<- toupper(substr(SS_version,1,8))
  	SS_letter           <- toupper(substr(SS_version,9,9))
  	SS_versionNumeric 	<- as.numeric(substring(SS_versionshort,5))

  	if (SS_versionNumeric <= 3.24){
  		if (SS_letter != "U" && SS_letter != "V" && SS_letter != "Z"){
  			print(":::::::::::::::::::::WARNING::::::::::::::::::::::::::::")
  			print("This code is designed for SS version 3.24U or later.")
  			print("The catches will not be correctly pulled for older versions.")
  			print("The other values may be correct, but double-check.")
  			print(":::::::::::::::::::::WARNING::::::::::::::::::::::::::::")
  		}
  	} 	

	# Function to force R to print correct decimal places
	print.numeric<-function(x, digits) { formatC(x, digits = digits, format = "f") }
	comma <- function(x, digits=0) { formatC(x, big.mark=",", digits, format = "f") }

	emptytest <- function(x){ sum(!is.na(x) & x=="")/length(x) }

	matchfun <- function(string, obj=rawrep[,1], substr1=TRUE)
	{
  		# return a line number from the report file (or other file)
  		# sstr controls whether to compare subsets or the whole line
  		match(string, if(substr1){substring(obj,1,nchar(string))}else{obj} )
	}

	matchfun2 <- function(string1,adjust1,string2,adjust2,cols="nonblank",matchcol1=1,matchcol2=1,
    objmatch=rawrep,objsubset=rawrep,substr1=TRUE,substr2=TRUE,header=FALSE)
  	{
  	  # return a subset of values from the report file (or other file)
  	  # subset is defined by character strings at the start and end, with integer
  	  # adjustments of the number of lines to above/below the two strings
  	  line1 <- match(string1,
  	                 if(substr1){
  	                   substring(objmatch[,matchcol1],1,nchar(string1))
  	                 }else{
  	                   objmatch[,matchcol1]
  	                 })
  	  line2 <- match(string2,
  	                 if(substr2){
  	                   substring(objmatch[,matchcol2],1,nchar(string2))
  	                 }else{
  	                   objmatch[,matchcol2]
  	                 })
  	  if(is.na(line1) | is.na(line2)) return("absent")
	
  	  if(is.numeric(cols))    out <- objsubset[(line1+adjust1):(line2+adjust2),cols]
  	  if(cols[1]=="all")      out <- objsubset[(line1+adjust1):(line2+adjust2),]
  	  if(cols[1]=="nonblank"){
  	    # returns only columns that contain at least one non-empty value
  	    out <- objsubset[(line1+adjust1):(line2+adjust2),]
  	    out <- out[,apply(out,2,emptytest) < 1]
  	  }
  	  if(header && nrow(out)>0){
  	    out[1,out[1,]==""] <- "NoName"
  	    names(out) <- out[1,]
  	    out <- out[-1,]
  	  }
  	  return(out)
  	}

	rawdefs <- matchfun2("DEFINITIONS",1,"LIKELIHOOD",-1)

  	# Determine the number of fishing fleets
  	if (SS_versionNumeric >= 3.3){
  		# version 3.30
  		defs 		 <- rawdefs[-(1:3),apply(rawdefs[-(1:3),],2,emptytest)<1]
    	defs[defs==""] <- NA
    	FleetNames   <- as.character(defs[grep("fleet_names",defs$X1),-1])
    	FleetNames   <- FleetNames[!is.na(FleetNames)]
    	fleet_ID     <- 1: length(FleetNames)
    	fleet_type   <- as.numeric(defs[4:(3+length(fleet_ID)),1])
    	nfleets      <- sum(fleet_type <= 2 )
  	}
  	if (SS_versionNumeric < 3.3){
  		# version 3.20 - 3.24
  		# Code will work for 3.24U version and higher
  		# During testing on older models the catches are not pulled correctly, but
  		# all other quantities were.
  		defs 		 	<- rawdefs[-(1:3),apply(rawdefs[-(1:3),],2,emptytest)<1]
  		defs[defs==""] 	<- NA
    	lab 		 	<- defs$X1
  		catch_units  	<- as.numeric(defs[grep("Catch_units",lab),-1])
    	IsFishFleet  	<- !is.na(catch_units)
    	nfleets      	<- sum(IsFishFleet)
  	}

  	# Indexing in order to determine model years
	begin <- matchfun(string = "TIME_SERIES", obj = rawrep[,1])+2
	end   <- matchfun(string = "SPR_series",  obj = rawrep[,1])-1
	
	startyr  <- min(as.numeric(rawrep[begin:end,2]))+2
	temptime <- rawrep[begin:end,2:3]
	endyr    <- max(as.numeric(temptime[temptime[,2]=="TIME",1]))
	foreyr   <- max(as.numeric(temptime[temptime[,2]=="FORE",1]))
	hist     <- startyr:endyr
	fore     <- (endyr+1):foreyr
	all      <- startyr:foreyr

	# Determine if the model has multiple areas
	# The quantities by areas are summed into total values (e.g. spawning biomass summed across all areas)
  	nareas <- max(as.numeric(rawrep[begin:end,1]))
  	if ( nareas > 1) { print(paste0("Patience: There are ", nareas, " areas that are being pulled and combined.")) }

	smry.all = tot.bio.all = recruits.all = 0
	for (a in 1:nareas){
		smry        = mapply(function(x) smry   = as.numeric(strsplit(base[grep(paste(a, x,"TIME",sep=" "),base)]," ")[[1]][6]), x = hist)
		smry.1      = mapply(function(x) smry.1 = as.numeric(strsplit(base[grep(paste(a, x,"FORE",sep=" "),base)]," ")[[1]][6]), x = fore)
		
		tot.bio     = mapply(function(x) tot.bio   = as.numeric(strsplit(base[grep(paste(a, x,"TIME",sep=" "),base)]," ")[[1]][5]), x = hist)
		tot.bio.1   = mapply(function(x) tot.bio.1 = as.numeric(strsplit(base[grep(paste(a, x,"FORE",sep=" "),base)]," ")[[1]][5]), x = fore)

		recruits    = mapply(function(x) out = as.numeric(strsplit(base[grep(paste(a, x,"TIME",sep=" "),base)]," ")[[1]][8]), x = hist)
		recruits.1  = mapply(function(x) out = as.numeric(strsplit(base[grep(paste(a, x,"FORE",sep=" "),base)]," ")[[1]][8]), x = fore)
		
		smry.all    = smry.all + c(smry,smry.1)
		tot.bio.all = tot.bio.all + c(tot.bio, tot.bio.1)
		recruits.all = recruits.all + c(recruits, recruits.1)
	}
	

	spr_type = strsplit(base[grep("SPR_report_basis",base)]," ")[[1]][3]
	#if (spr_type != "1-SPR") { 
	#	print(":::::::::::::::::::::::::::::::::::WARNING:::::::::::::::::::::::::::::::::::::::")
	#	print(paste("The SPR is being reported as", spr_type, "."))
	#    print("West coast groundfish assessments typically report 1-SPR in the time-series table") 
	#    print(":::::::::::::::::::::::::::::::::::WARNING:::::::::::::::::::::::::::::::::::::::")  }


	adj.spr.all  = mapply(function(x) out = as.numeric(strsplit(base[grep(paste("SPRratio_",x,sep=""),base)]," ")[[1]][3]), x = all)
	ssb.all      = mapply(function(x) out = as.numeric(strsplit(base[grep(paste("SPB_",x,sep=""),base)]," ")     [[1]][3]), x = all)
	ssb.virgin   = as.numeric(strsplit(base[grep("SPB_Virgin",base)]," ") [[1]][3])
	
	depl.all     = mapply(function(x) out = as.numeric(strsplit(base[grep(paste("Bratio_",x,sep=""),base)]," ")[[1]][3]), x = (startyr + 1):foreyr)
	depl.all     = c(ssb.all[1] / ssb.virgin, depl.all)
	
	temp    = strsplit(base[grep("fleet_names",base)]," ")[[1]]
	names   = temp[3:(3 + nfleets - 1)]

	# Determine the number of fishery fleets with catch and sum all mortality across fleets.
	if (nfleets != length(names)) { 
		print("WARNING: The number of fishing fleets does not match the number of fishing fleets with names.")}

	catch   = numeric(length(hist))
	if (SS_versionNumeric < 3.3 ) { xx = 12}
	if (SS_versionNumeric >= 3.3) { xx = 14}

	for (a in 1:nfleets){
		temp = mapply(function(x) out = as.numeric(strsplit(base[grep(paste(a, names[a], x,sep=" "),base)]," ")[[1]][xx]), x = hist)
		catch = catch + temp
	}
	fore.catch = mapply(function(x) out = as.numeric(strsplit(base[grep(paste("ForeCatch_",x,sep=""),base)]," ")[[1]][3]), x = fore)
	catch.all    = c(catch, fore.catch)

	print("Catch includes estimated discards for total dead.")
	print("Exploitation = Total catch (including discards) divided by the summary biomass.")
	exp.all = catch.all / smry.all 
	
	ts.table = data.frame(all,
				comma(tot.bio.all,0),
				comma(ssb.all,0),
				comma(smry.all,0),
				print(depl.all*100,1),
				comma(recruits.all,0),
				catch.all,
				print(adj.spr.all,3),
				exp.all)
	
	colnames(ts.table) = c("Year", "Total Biomass (mt)", "Spawning Biomass", 
		"Total Biomass XX+ (mt)", "Depletion (%)", 
		"Age-0 Recruits", "Total Catch (mt)", spr_type, "Relative Exploitation Rate")
	write.csv(ts.table, file = paste0(csv.dir,"TimeSeries.csv"), row.names = FALSE)	
}
