#' A function to create a executive summary tables from an SS Report.sso file
#'
#' Reads the Report.sso within the directory and creates executive summary
#' tables as required by the current Terms of Reference for West Coast 
#' groundfish.  Works with Stock Synthesis versions 3.24U and later.
#' Additionally, historical catch and numbers at ages tables are created.
#'
#' @param dir Locates the directory of the files to be read in, double
#' backslashes (or forwardslashes) and quotes necessary.
#' @param plotdir Directory where the table will be saved.  The default 
#' saves the table to the dir location where the Report.sso file is located.
#' @param quant to calculate confidence intervals, default is set at 0.95
#' @param es.only =  only the executive summary tables will be produced, default is false which
#' will return all executive summary tables, historical catches, and numbers-at-ages
#' @param nsex This will allow the user to calculate single sex values based on the new sex 
#' specification (-1) in SS for single sex models. Default value is FALSE. TRUE will not divide by 2.
#' @return A csv files containing executive summary tables.
#' @author Chantel Wetzel
#' @export
#'
SSexecutivesummary <- function (dir, plotdir = 'default', quant = 0.95, es.only = FALSE, nsex = FALSE)
{
  # Check to make sure dir is a dir
  if(!is.character(dir) || !file.info(dir)$isdir){
    stop("Input 'dir' should be a directory")
  }
  # Make sure dir contains the report file
  repfile <- file.path(dir, "Report.sso")
  if(is.na(file.info(repfile)$size)){
    stop("Report.sso not found in 'dir': ", dir)
  }
  
	#Read in the base model using r4ss
	wd        <- paste(dir, "/Report.sso", sep="")
	base      <- readLines(wd)	
	rawrep    <- read.table(file= wd , col.names = 1:400, fill = TRUE, quote = "", 
								colClasses = "character", nrows = -1, comment.char = "")

	if (plotdir == 'default') { csv.dir = paste0(dir,"/tables/") }
	if (plotdir != 'default') { csv.dir = paste0(plotdir,"/tables/")}
	dir.create(csv.dir, showWarnings = FALSE) 

	SS_versionCode 		<- base[grep("#V",base)]
  	SS_version 			<- base[grep("Stock_Synthesis",base)]
  	SS_version 			<- SS_version[substring(SS_version,1,2)!="#C"] # remove any version numbering in the comments
  	SS_versionshort 	<- toupper(substr(SS_version,1,8))
  	SS_versionNumeric 	<- as.numeric(substring(SS_versionshort,5))
  	if(SS_versionNumeric == 3.3){ 
  		add <- as.numeric(toupper(substr(SS_version, 9, 11)))
  		SS_versionNumeric = SS_versionNumeric + 0.1*add
  	}

	#=================================================================================================
	# Function Sections
	#=================================================================================================

	# Function to force R to print correct decimal place
	print.numeric  <- function(x, digits) { formatC(x, digits = digits, format = "f") }
	comma 		   <- function(x, digits=0) { formatC(x, big.mark=",", digits, format = "f") }
	emptytest      <- function(x){ sum(!is.na(x) & x=="")/length(x) }

	# Funtion to calculate confidence intervals
	getDerivedQuant.fn <- function(dat, label, yrs, quant, divisor=1) {
          # modify old header to new value
        names(dat)[names(dat)=="LABEL"] <- "Label"
    	allYrs <- suppressWarnings(as.numeric(substring(dat$Label[substring(dat$Label,1,3)=="SSB"],5,8)))
    	allYrs <- allYrs[!is.na(allYrs)]
    	finalYr <- as.numeric(substring(dat$Label[substring(dat$Label,1,8)=="OFLCatch"],10,13))[1]
    	if(is.null(yrs)) {
    	    yrs <- allYrs
    	}
    	if(yrs[1]<0) {
    	    yrs <- (finalYr+yrs):finalYr
    	}
    	out <- dat[dat$Label%in%paste(label,yrs,sep="_"),]
    	out.value <- out$Value <- out$Value/divisor
    	out$StdDev <- out$StdDev/divisor
    	if(label=="Recr") {   #use lognormal
    	    out.lower <- exp(log(out.value)-qnorm(1-(1-quant)/2)*sqrt(log((out$StdDev/out.value)^2+1)))
    	    out.upper <- exp(log(out.value)+qnorm(1-(1-quant)/2)*sqrt(log((out$StdDev/out.value)^2+1)))
    	}
    	else {
    	    out.lower <- out.value-qnorm(1-(1-quant)/2)*out$StdDev
    	    out.upper <- out.value+qnorm(1-(1-quant)/2)*out$StdDev
    	}
    	return(data.frame(Year=yrs,Value=out.value,LowerCI=out.lower,UpperCI=out.upper))
	}

	# Function to pull values from the read in report file and calculate the confidence intervals
	Get.Values <- function(dat, label, yrs, quant, single = FALSE){
	
	    if (label == "Recr") { label = " Recr"}
	    
	    if(!single){
	        dq = mapply(function(x) out = as.numeric(strsplit(dat[grep(paste(label,"_",x,sep=""),dat)]," ")[[1]][3]), x = yrs)
	        sd = mapply(function(x) out = as.numeric(strsplit(dat[grep(paste(label,"_",x,sep=""),dat)]," ")[[1]][4]), x = yrs)
	        if (label == "Main_RecrDev" || label == "Late_RecrDev" || label == "ForeRecr") {
	            zz = ifelse(SS_versionNumeric < 3.3, 10, 11)
            	#sd = mapply(function(x) out = as.numeric(strsplit(dat[grep(paste(label,"_",x,sep=""),dat)]," ")[[1]][zz]), x = yrs)
            	sd = mapply(function(x) out = strsplit(dat[grep(paste(label,"_",x,sep=""),dat)]," ")[[1]][zz], x = yrs)
            	if (sd[1] == "NA") { sd = rep(0, length(dq)) }
            	sd = as.numeric(sd)
	        }
	    }
	
	    if(single){
	        dq = as.numeric(strsplit(dat[grep(label,dat)]," ")[[1]][3])
	        sd = as.numeric(strsplit(dat[grep(label,dat)]," ")[[1]][4])
	    }
	
	    if(label == " Recr" || label == "Recr_virgin"){
	        low = exp(log(dq) - qnorm(1-(1-quant)/2) * sqrt(log(1 + (sd/dq) * (sd/dq))))
	        high= exp(log(dq) + qnorm(1-(1-quant)/2) * sqrt(log(1 + (sd/dq) * (sd/dq))))
	    }
	    if(label != " Recr" && label != "Recr_virgin"){
	        low = dq - qnorm(1-(1-quant)/2)*sd
	        high= dq + qnorm(1-(1-quant)/2)*sd	    		
	    }
	
	    if (!single) { return(data.frame(yrs, dq, low, high)) }
	    if (single)  { return(data.frame(dq, low, high)) }
	}


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

  	#=================================================================================================
	# Determine the model version and dimensions of the model
	#=================================================================================================

	rawdefs <- matchfun2("DEFINITIONS",1,"LIKELIHOOD",-1)

  	# Determine the number of fishing fleets
  	if (SS_versionNumeric >= 3.313){
  		# version 3.30
  		defs 		 <- rawdefs[-(1:3),apply(rawdefs[-(1:3),],2,emptytest)<1]
    	defs[defs==""] <- NA
    	FleetNames   <- as.character(defs[grep("Fleet_name",defs$X1),-1])
    	FleetNames   <- FleetNames[!is.na(FleetNames)]
    	fleet_ID     <- 1: length(FleetNames)
    	fleet_type   <- as.numeric(defs[grep("Fleet_type",defs$X1),-1])# as.numeric(defs[4:(3+length(fleet_ID)),1])
    	nfleets      <- sum(fleet_type[!is.na(fleet_type)] <= 2 )
  	}

  	if (SS_versionNumeric < 3.313 & SS_versionNumeric >= 3.3){
  		# version 3.30
  		defs 		 <- rawdefs[-(1:3),apply(rawdefs[-(1:3),],2,emptytest)<1]
    	defs[defs==""] <- NA
    	FleetNames   <- as.character(defs[grep("fleet_names",defs$X1),-1])
    	FleetNames   <- FleetNames[!is.na(FleetNames)]
    	fleet_ID     <- 1: length(FleetNames)
    	fleet_type   <- as.numeric(defs$X1[4:dim(defs)[1]])
    	nfleets      <- sum(fleet_type[!is.na(fleet_type)] <= 2 )
  	}

  	if (SS_versionNumeric < 3.3){
  		# version 3.20 - 3.24
  		defs 		 	<- rawdefs[-(1:3),apply(rawdefs[-(1:3),],2,emptytest)<1]
  		defs[defs==""] 	<- NA
    	lab 		 	<- defs$X1
  		catch_units  	<- as.numeric(defs[grep("Catch_units",lab),-1])
    	IsFishFleet  	<- !is.na(catch_units)
    	nfleets      	<- sum(IsFishFleet)
  	}

  	begin <- matchfun(string = "TIME_SERIES", obj = rawrep[,1])+2
	end   <- matchfun(string = "SPR_series",  obj = rawrep[,1])-1
	
	temptime <- rawrep[begin:end,2:3]
	endyr    <- max(as.numeric(temptime[temptime[,2]=="TIME",1]))
	startyr  <- min(as.numeric(rawrep[begin:end,2]))+2
	foreyr   <- max(as.numeric(temptime[temptime[,2]=="FORE",1]))
	hist     <- (endyr - 11):(endyr + 1)
	fore     <- (endyr+1):foreyr
	all      <- startyr:foreyr

	#======================================================================
	# Determine number of areas in the model
	#======================================================================
  	nareas   <- max(as.numeric(rawrep[begin:end,1]))

  	#======================================================================
	# Determine the fleet name and number for fisherie with catch
	#======================================================================
  	begin <- matchfun(string = "CATCH", obj = rawrep[,1])+2
    end   <- matchfun(string = "TIME_SERIES", obj = rawrep[,1])-1
    temp  <- rawrep[begin:end, 1:18]
    names <- unique(temp[,2]) # This is a list of fishery names with catch associated with them
    fleet.num <- unique(temp[,1])

 	#======================================================================
	# Find summary age
	#======================================================================
	ts        <- matchfun2("TIME_SERIES", -1,"Area", -1) 
    smry.age  <- as.numeric(toupper(substr(ts[2,2],14,15)))

    #======================================================================
    # Two-sex or Singl-sex model
    #======================================================================
    selex <- matchfun2("LEN_SELEX",6,"AGE_SELEX",-1,header=TRUE)
    nsexes <- ifelse(SS_versionNumeric < 3.3, 
    					length(unique(as.numeric(selex$gender))),
    					length(unique(as.numeric(selex$Sex))))

    sexfactor = 2
    if (nsex) {sexfactor = 1}


    #======================================================================
    # Determine the number of growth patterns
    #======================================================================
    find.morph <- matchfun2("MORPH_INDEXING", 1, "MOVEMENT", -2, header=TRUE)
    nmorphs <- dim(find.morph)[1] / nsexes
	
	#======================================================================
	#ES Table a  Catches from the fisheries
	#======================================================================
		xx = ifelse(SS_versionNumeric < 3.3, 12, 15)
		catch = NULL; total.catch = total.dead = 0
		ind = hist[1:(length(hist)-1)]

		if(SS_versionNumeric >= 3.313) {
			for(a in 1:nareas){
				for (i in 1:nfleets){
					killed = mapply(function(x) killed = as.numeric(strsplit(base[grep(paste(fleet.num[i], names[i], nareas[a], x, sep=" "),base)]," ")[[1]][xx]), x = ind)
					input.catch = mapply(function(x) input.catch = as.numeric(strsplit(base[grep(paste(fleet.num[i], names[i], nareas[a], x, sep=" "),base)]," ")[[1]][xx+1]), x = ind)
					total.dead  = total.dead + killed
					total.catch = total.catch + input.catch
					catch = cbind(catch, input.catch)
				}
				es.a = data.frame(ind, comma(catch, digits = 2), comma(total.catch, digits = 2), comma(total.dead, digits = 2))
				colnames(es.a) = c("Years", names, "Total Catch", "Total Dead")
				write.csv(es.a, paste0(csv.dir, "/a_Catches_Area", nareas[a], "_ExecutiveSummary.csv"), row.names = F)
			}
		}

		if(SS_versionNumeric < 3.313 & SS_versionNumeric >= 3.24) {
			for (i in 1:nfleets){
				killed = mapply(function(x) killed = as.numeric(strsplit(base[grep(paste(fleet.num[i], names[i], x, sep=" "),base)]," ")[[1]][xx]), x = ind)
				input.catch = mapply(function(x) input.catch = as.numeric(strsplit(base[grep(paste(fleet.num[i], names[i], x, sep=" "),base)]," ")[[1]][xx+1]), x = ind)
				total.dead  = total.dead + killed
				total.catch = total.catch + input.catch
				catch = cbind(catch, input.catch)
			}
			es.a = data.frame(ind, comma(catch, digits = 2), comma(total.catch, digits = 2), comma(total.dead, digits = 2))
			colnames(es.a) = c("Years", names, "Total Catch", "Total Dead")
			write.csv(es.a, paste0(csv.dir, "/a_Catches_ExecutiveSummary.csv"), row.names = F)
		}

		if(SS_versionNumeric < 3.24) {
			begin <- matchfun(string = "TIME_SERIES", obj = rawrep[,1])+2
			end   <- matchfun(string = "SPR_series",  obj = rawrep[,1])-1
			temp = rawrep[begin:end, ]
		    find = as.numeric(temp[, 2])
			grab = which(find %in% ind)
			xx = 15
			for (i in 1:nfleets){
				killed = as.numeric(temp[grab, xx])
				input.catch = as.numeric(temp[grab, xx + 1])
				total.dead  = total.dead + killed
				total.catch = total.catch + input.catch
				catch = cbind(catch, input.catch)
				xx = xx + 8
			}
			es.a = data.frame(ind, comma(catch, digits = 2), comma(total.catch, digits = 2), comma(total.dead, digits = 2))
			colnames(es.a) = c("Years", 1:nfleets, "Total Catch", "Total Dead")
			write.csv(es.a, paste0(csv.dir, "/a_Catches_ExecutiveSummary.csv"), row.names = F)
		}



	#======================================================================
	#ES Table b Spawning Biomass and Depletion
	#======================================================================
		sb.name = ifelse(SS_versionNumeric < 3.313, "SPB", "SSB")
		ssb =  Get.Values(dat = base, label = sb.name, hist, quant )
		if (nsexes == 1) { ssb$dq = ssb$dq / sexfactor ; ssb$low = ssb$low / sexfactor ; ssb$high = ssb$high / sexfactor }
		depl = Get.Values(dat = base, label = "Bratio" , hist, quant )
		for (i in 1:length(hist)){ dig = ifelse(ssb[i,2] < 100, 1, 0)}
		es.b =  data.frame(hist, 
				  		  comma(ssb$dq,digits = dig), paste0(comma(ssb$low,digits = dig), "\u2013", comma(ssb$high,digits = dig)),
				  		  print(100*depl$dq, digits = 1), paste0(print(100*depl$low,digits = 1), "\u2013", print(100*depl$high,digits = 1)))
		colnames(es.b) = c("Years", "Spawning Output", "95% Asymptotic Interval", "Estimated Depletion (%)", "95% Asymptotic Interval")

		write.csv(es.b, paste0(csv.dir, "/b_SSB_ExecutiveSummary.csv"), row.names = F)
	
	#======================================================================
	#ES Table c Recruitment
	#======================================================================
		parameters   <- matchfun2("PARAMETERS",1,"DERIVED_QUANTITIES", -1, header=TRUE)
		recdevMain   <- parameters[substring(parameters$Label,1,12)=="Main_RecrDev",]

        recdevLate   <- parameters[substring(parameters$Label,1,12)=="Late_RecrDev",]
        temp         <- toupper(substr(recdevLate$Label,14,17))
  		late.yrs     <- as.numeric(temp) 

		recdevFore   <- parameters[substring(parameters$Label,1, 8)=="ForeRecr",]
		temp	     <- toupper(substr(recdevFore$Label,10,13))
  		fore.yrs     <- as.numeric(temp)
  		ind          <- fore.yrs <= max(hist)
  		fore.yrs     <- fore.yrs[ind] 

  		end 		 <- ifelse(length(late.yrs) == 0, fore.yrs - 1, late.yrs - 1)

		recruits     = Get.Values(dat = base, label = "Recr" , hist, quant )
		if (dim(recdevMain)[1] != 0){
			recdevs      = Get.Values(dat = base, label = "Main_RecrDev", yrs = hist[1]:end, quant )
			devs = cbind(recdevs$dq, recdevs$low, recdevs$high) 

			if (length(late.yrs) > 0 ){
				late.recdevs = Get.Values(dat = base, label = "Late_RecrDev", yrs = late.yrs, quant )
				devs = cbind(c(recdevs$dq, late.recdevs$dq), c(recdevs$low, late.recdevs$low), c(recdevs$high, late.recdevs$high)) 
			}			
			
			if(length(fore.yrs) > 0){
				fore.recdevs = Get.Values(dat = base, label = "ForeRecr", yrs = fore.yrs, quant )	
				if (length(late.yrs) > 0){
					devs = cbind(c(recdevs$dq, late.recdevs$dq, fore.recdevs$dq), 
								 c(recdevs$low, late.recdevs$low, fore.recdevs$low), 
								 c(recdevs$high, late.recdevs$high, fore.recdevs$high))					
				}

				if (length(late.yrs) == 0){
					devs = cbind(c(recdevs$dq,    fore.recdevs$dq), 
								 c(recdevs$low,   fore.recdevs$low), 
								 c(recdevs$high,  fore.recdevs$high))					
				}

			}		
			# Zero out the sd for years where devs were not estimated
			devs.out = data.frame(print(devs[,1], digits = 3), paste0(print(devs[,2],digits = 3), "\u2013", print(devs[,3], digits = 3)))			
		}

		if (dim(recdevMain)[1] == 0) { devs.out = data.frame(rep(0, length(hist)), rep(0, length(hist))) }
		for (i in 1:length(hist)){ dig = ifelse(recruits[i,2] < 100, 1, 0)}

		es.c = data.frame(hist,
						  comma(recruits$dq, dig), paste0(comma(recruits$low, dig), "\u2013", comma(recruits$high, dig)), 
						  devs.out )

		colnames(es.c) = c("Years", "Recruitment", "95% Asymptotic Interval", "Recruitment Deviations", "95% Asymptotic Interval")

		write.csv(es.c, paste0(csv.dir, "/c_Recr_ExecutiveSummary.csv"), row.names = F)	
	
	#======================================================================
	#ES Table d 1-SPR (%)
	#======================================================================
		spr.name = ifelse(SS_versionNumeric >= 3.301, "SPR_report_basis", "SPR_ratio_basis")
		spr_type = strsplit(base[grep(spr.name,base)]," ")[[1]][3]
		#if (spr_type != "1-SPR") { 
		#	print(":::::::::::::::::::::::::::::::::::WARNING:::::::::::::::::::::::::::::::::::::::")
		#	print(paste("The SPR is being reported as", spr_type, "."))
	    #	print("West coast groundfish assessments typically report 1-SPR in the executive summary") 
	   	#	print(":::::::::::::::::::::::::::::::::::WARNING:::::::::::::::::::::::::::::::::::::::")  }

		adj.spr = Get.Values(dat = base, label = "SPRratio" , hist, quant)
		f.value = Get.Values(dat = base, label = "F" , hist, quant)
		es.d = data.frame(hist,
						  print(adj.spr$dq*100,2), paste0(print(adj.spr$low*100,2), "\u2013", print(adj.spr$high*100,2)),
						  print(f.value$dq,3),     paste0(print(f.value$low,3),     "\u2013", print(f.value$high,3)))
		colnames(es.d) = c("Years", paste0("Estimated ", spr_type, " (%)"), "95% Asymptotic Interval", "Harvest Rate (proportion)", "95% Asymptotic Interval")

		write.csv(es.d, paste0(csv.dir, "/d_SPR_ExecutiveSummary.csv"), row.names = F)
	
	
	#======================================================================
	#ES Table e Reference Point Table
	#======================================================================
		# Find the values within the forecast file 
		rawforecast  <- readLines(paste0(dir, "/forecast.ss"))	
		rawstarter   <- readLines(paste0(dir, "/starter.ss"))
		spr          <- as.numeric(strsplit(rawforecast[grep("SPR target",rawforecast)]," ")[[1]][1])

		if (SS_versionNumeric < 3.313){
			sb.unfished = "SSB_Unfished"
			smry.unfished = "SmryBio_Unfished"
			recr.unfished = "Recr_Unfished"
			yield.btgt = "TotYield_Btgt"
			yield.spr  = "TotYield_SPRtgt"
			yield.msy = "TotYield_MSY"

		} else {
			sb.unfished = "SSB_unfished"
			smry.unfished = "SmryBio_unfished"
			recr.unfished = "Recr_unfished"
			yield.btgt = "Dead_Catch_Btgt"
			yield.spr  = "Dead_Catch_SPR"
			yield.msy  = "Dead_Catch_MSY"
		}

		ssb.virgin = Get.Values(dat = base, label = sb.unfished,      hist, quant, single = TRUE)
		smry.virgin= Get.Values(dat = base, label = smry.unfished,  hist, quant, single = TRUE)
		rec.virgin = Get.Values(dat = base, label = recr.unfished,     hist, quant, single = TRUE)
		final.depl = 100*depl[dim(depl)[1],2:4]     
		b.target   = Get.Values(dat = base, label = "SSB_Btgt",    	     hist, quant, single = TRUE)
		spr.btarg  = Get.Values(dat = base, label = "SPR_Btgt",    	     hist, quant, single = TRUE)
		f.btarg    = Get.Values(dat = base, label = "Fstd_Btgt", 	     hist, quant, single = TRUE)
		yield.btarg= Get.Values(dat = base, label = yield.btgt,   hist, quant, single = TRUE)
		b.spr 	   = Get.Values(dat = base, label = "SSB_SPR",  	     hist, quant, single = TRUE)
		f.spr      = Get.Values(dat = base, label = "Fstd_SPR", 		 hist, quant, single = TRUE)
		yield.spr  = Get.Values(dat = base, label = yield.spr,    hist, quant, single = TRUE)
		b.msy 	   = Get.Values(dat = base, label = "SSB_MSY", 		     hist, quant, single = TRUE)
		spr.msy    = Get.Values(dat = base, label = "SPR_MSY", 		     hist, quant, single = TRUE)
		f.msy 	   = Get.Values(dat = base, label = "Fstd_MSY", 	     hist, quant, single = TRUE)
		msy 	   = Get.Values(dat = base, label = yield.msy,    hist, quant, single = TRUE)

		# Convert spawning quantities for single-sex models
		if (nsexes == 1){
			ssb.virgin = ssb.virgin / sexfactor 
			b.target = b.target / sexfactor
			b.spr = b.spr / sexfactor
			b.msy = b.msy / sexfactor
		}

	
		es.e =  matrix(c(
				comma(ssb.virgin$dq,       dig),  paste0(comma(ssb.virgin$low,      dig), 	"\u2013", comma(ssb.virgin$high,      dig)),
				comma(smry.virgin$dq,      dig),  paste0(comma(smry.virgin$low,     dig),	"\u2013", comma(smry.virgin$high,     dig)),
				comma(ssb$dq[dim(ssb)[1]], dig),  paste0(comma(ssb$low[dim(ssb)[1]],dig), 	"\u2013", comma(ssb$high[dim(ssb)[1]],dig)),
				comma(rec.virgin$dq,       dig),  paste0(comma(rec.virgin$low,      dig), 	"\u2013", comma(rec.virgin$high,      dig)),
				print(final.depl$dq,         2),  paste0(print(final.depl$low,      2), 	"\u2013", print(final.depl$high,      2)),
				"",    "",  
				comma(b.target$dq,     dig),   	  paste0(comma(b.target$low,    dig),   	"\u2013", comma(b.target$high,	  dig)),
				print(spr.btarg$dq,    3),  	  paste0(print(spr.btarg$low,     3),  	    "\u2013", print(spr.btarg$high,	  3)),
				print(f.btarg$dq,      3),    	  paste0(print(f.btarg$low,       3),       "\u2013", print(f.btarg$high,	  3)),
				comma(yield.btarg$dq,  dig),	  paste0(comma(yield.btarg$low, dig),	    "\u2013", comma(yield.btarg$high, dig)),
				"",    "", 	  
				comma(b.spr$dq,		dig),     	  paste0(comma(b.spr$low, 	  dig),      	"\u2013", comma(b.spr$high, 	dig)),
				print(spr,			  3),         " NA ",	  
				print(f.spr$dq,		  3),     	  paste0(print(f.spr$low,	    3),      	"\u2013", print(f.spr$high,		  3)),
				comma(yield.spr$dq, dig), 		  paste0(comma(yield.spr$low, dig),  	    "\u2013", comma(yield.spr$high,	dig)),
				"",    "", 				  
				comma(b.msy$dq,		dig),     	  paste0(comma(b.msy$low,	dig),      	    "\u2013", comma(b.msy$high,		dig)),
				print(spr.msy$dq,	  3),   	  paste0(print(spr.msy$low,   3),    	    "\u2013", print(spr.msy$high,	  3)),
				print(f.msy$dq,		  3),     	  paste0(print(f.msy$low,     3),      	    "\u2013", print(f.msy$high,		  3)),
				comma(msy$dq, 		dig),         paste0(comma(msy$low,		dig),        	"\u2013", comma(msy$high,		dig))
				), ncol=2, byrow=T ) 

		es.e = noquote(es.e)
		
		colnames(es.e) = c("Estimate", "95% Asymptotic Interval")
		rownames(es.e) = c("Unfished Spawning Biomass (mt)",
						   paste0("Unfished Age ", smry.age, "+ Biomass (mt)"),
						   paste0("Spawning Biomass", " (", hist[length(hist)], ")"),
						   "Unfished Recruitment (R0)",
						   paste0("Depletion ", "(", hist[length(hist)], ")"),
						   "Reference Points Based SB40%",
						   "Proxy Spawning Biomass (SB40%)",
						   "SPR resulting in SB40%",
						   "Exploitation Rate Resulting in SB40%",
						   "Yield with SPR Based On SB40% (mt)",
						   "Reference Points based on SPR proxy for MSY",
						   "Proxy spawning biomass (SPR50)",
						   "SPR50",
						   "Exploitation rate corresponding to SPR50",
						   "Yield with SPR50 at SBSPR (mt)",
						   "Reference points based on estimated MSY values",
						   "Spawning biomass at MSY (SBMSY)",
						   "SPRMSY",
						   "Exploitation rate corresponding to SPRMSY",
						   "MSY (mt)")

		write.csv(es.e, paste0(csv.dir, "/e_ReferencePoints_ExecutiveSummary.csv"))
	
	#======================================================================
	# ES Table f is the historical harvest 
	#======================================================================
		ind = hist
		ofl = rep("fill_in", length(ind))
		abc = rep("fill_in", length(ind))
		acl = rep("fill_in", length(ind))
		catch = c(comma(total.catch, digits = 2), "NA")
		dead  = c(comma(total.dead,  digits = 2), "NA")
		es.f = data.frame(ind, ofl, abc, acl, catch, dead) 
		colnames(es.f) = c("Years", "OFL", "ABC", "ACL", "Landings", "Total Dead")

		write.csv(es.f, paste0(csv.dir, "/f_Manage_ExecutiveSummary.csv"), row.names = F)
	
	#======================================================================
	#ES Table g  Predicted Quantities
	#======================================================================
		ofl.fore =  Get.Values(dat = base, label = "OFLCatch" ,  yrs = fore, quant)
		abc.fore =  Get.Values(dat = base, label = "ForeCatch" , yrs = fore, quant)
		ssb.fore  = Get.Values(dat = base, label =  sb.name,       yrs = fore, quant)
		depl.fore = Get.Values(dat = base, label = "Bratio",     yrs = fore, quant)

		if (nsexes == 1) { 
			ssb.fore$dq = ssb.fore$dq / sexfactor; ssb.fore$low = ssb.fore$low / sexfactor; ssb.fore$high = ssb.fore$high / sexfactor}
		
		smry.fore = 0
		for(a in 1:nareas){
			temp = mapply(function(x) temp = as.numeric(strsplit(base[grep(paste(a, x,"FORE",sep=" "),base)]," ")[[1]][6]),
		                                       x = fore)
			smry.fore = smry.fore + temp
		}

		es.g = data.frame(fore, 
						  comma(ofl.fore$dq, 2),
						  comma(abc.fore$dq, 2),
						  comma(smry.fore,   2),
						  comma(ssb.fore$dq, 2),
						  print(depl.fore$dq*100,2))
		colnames(es.g) = c("Year", "Predicted OFL (mt)", "ABC Catch (mt)", paste0("Age ", smry.age, "+ Biomass (mt)"), "Spawning Biomass (mt)", "Depletion (%)")

		write.csv(es.g, paste0(csv.dir, "/g_Projections_ExecutiveSummary.csv"), row.names = F)

	#======================================================================
	#ES Table h decision table
	#======================================================================
		# To be done later

	
	#======================================================================
	#ES Table i the summary table
	#======================================================================
		ind = length(hist)-1
		smry = 0
		for(a in 1:nareas){
			temp = mapply(function(x) temp = as.numeric(strsplit(base[grep(paste(a, x,"TIME",sep=" "),base)]," ")[[1]][6]), x = hist[1:ind])
			smry = smry + temp
		}
		
		smry = c(smry, smry.fore[1])
		
		es.i = matrix(c(hist,
			   c(print(adj.spr$dq[1:(length(hist)-1)],2), "NA"),
			   c(print(f.value$dq[1:(length(hist)-1)],2), "NA"),
			   comma(smry,   dig),
			   comma(ssb$dq, dig),
			   paste0(comma(ssb$low, dig), "\u2013", comma(ssb$high, dig)),
			   comma(recruits$dq, dig),
			   paste0(comma(recruits$low, dig), "\u2013", comma(recruits$high, dig)),
			   print(depl$dq*100, 1),
			   paste0(print(depl$low*100,1), "\u2013", print(depl$high*100,1))), 
			   ncol = length(hist), byrow = T)
		
		es.i = noquote(es.i)
		
		rownames(es.i) = c(" Years",
						   "1-SPR",
						   "Exploitation_Rate",
						   paste0("Age ", smry.age, "+ Biomass (mt)"),
						   "Spawning Biomass (mt)", 
						   "95% Confidence Interval",
						   "Recruitment",
						   "95% Confidence Interval",
						   "Depletion (%)",
						   "95% Confidence Interval")

		write.csv(es.i, paste0(csv.dir, "/i_Summary_ExecutiveSummary.csv"))
	

	if (es.only == FALSE){
	#======================================================================
	# Total Catch when discards are estimated
	#======================================================================
		xx = ifelse(SS_versionNumeric < 3.3, 12, 15)
		total.dead = total.catch = 0 
		catch = NULL
		ind = startyr:endyr
		if (SS_versionNumeric >= 3.313){
			for(a in 1:nareas){
			  	for (i in 1:nfleets){
			  		killed = mapply(function(x) killed = as.numeric(strsplit(base[grep(paste(fleet.num[i], names[i], nareas[a], x, sep=" "),base)]," ")[[1]][xx]), x = ind)
			  		input.catch = mapply(function(x) input.catch = as.numeric(strsplit(base[grep(paste(fleet.num[i], names[i], nareas[a], x, sep=" "),base)]," ")[[1]][xx+1]), x = ind)
			  		total.dead = total.dead + killed
			  		total.catch = total.catch + input.catch
			  		catch = cbind(catch, input.catch)
			  	}
				mortality = data.frame(ind, comma(catch, 2), comma(total.catch,2), comma(total.dead,2))
				colnames(mortality) = c("Year",names, "Total Catch", "Total Dead")
		
				write.csv(mortality, paste0(csv.dir, "/_CatchesAllYrs_Area", nareas[a], ".csv"), row.names = F)
			}
		}

		if (SS_versionNumeric < 3.313 & SS_versionNumeric >= 3.24){
			#for(a in 1:nareas){
			  	for (i in 1:nfleets){
			  		killed = mapply(function(x) killed = as.numeric(strsplit(base[grep(paste(fleet.num[i], names[i],x, sep=" "),base)]," ")[[1]][xx]), x = ind)
			  		input.catch = mapply(function(x) input.catch = as.numeric(strsplit(base[grep(paste(fleet.num[i], names[i], x, sep=" "),base)]," ")[[1]][xx+1]), x = ind)
			  		total.dead = total.dead + killed
			  		total.catch = total.catch + input.catch
			  		catch = cbind(catch, input.catch)
			  	}
				mortality = data.frame(ind, comma(catch, 2), comma(total.catch,2), comma(total.dead,2))
				colnames(mortality) = c("Year",names, "Total Catch", "Total Dead")
		
				write.csv(mortality, paste0(csv.dir, "/_CatchesAllYrs_Area.csv"), row.names = F)
			#}
		}

		if(SS_versionNumeric < 3.24) {
			begin <- matchfun(string = "TIME_SERIES", obj = rawrep[,1])+2
			end   <- matchfun(string = "SPR_series",  obj = rawrep[,1])-1
			temp = rawrep[begin:end, ]
		    find = as.numeric(temp[, 2])
			grab = which(find %in% ind)
			xx = 15
			for (i in 1:nfleets){
				killed = as.numeric(temp[grab, xx])
				input.catch = as.numeric(temp[grab, xx + 1])
				total.dead  = total.dead + killed
				total.catch = total.catch + input.catch
				catch = cbind(catch, input.catch)
				xx = xx + 8
			}
			es.a = data.frame(ind, comma(catch, digits = 2), comma(total.catch, digits = 2), comma(total.dead, digits = 2))
			colnames(es.a) = c("Years", 1:nfleets, "Total Catch", "Total Dead")
			write.csv(es.a, paste0(csv.dir, "/a_Catches_ExecutiveSummary.csv"), row.names = F)
		}

	
	#======================================================================
	#Numbers at age
	#======================================================================
	if ( nareas > 1) { print(paste0("Patience: There are ", nareas, " areas that are being pulled and combined to create the numbers-at-age tables.")) }

		if(SS_versionNumeric < 3.30) { 
			maxAge = length(strsplit(base[grep(paste("1 1 1 1 1 1", startyr,sep=" "),base)]," ")[[1]]) - 14
			
			if (nsexes == 1) {
				natage.f = natage.m = 0
				for(a in 1:nareas){
					temp = mapply(function(x) temp = as.numeric(strsplit(base[grep(paste(a,"1 1 1 1", x,sep=" "),base)]," ")[[1]][14:(14+maxAge)]), x = startyr:endyr)
					natage.f = natage.f + t(temp) 
				}
				
				colnames(natage.f) = 0:maxAge
				rownames(natage.f) <- startyr:endyr 
			
				write.csv(natage.f, paste0(csv.dir, "/_natage.csv"))
			}
	
			if (nsexes == 2) {
				natage.f = natage.m = 0
				for(a in 1:nareas){
					for (b in 1:nmorphs){
						n = b
						temp = mapply(function(x) temp = as.numeric(strsplit(base[grep(paste(a, b, "1 1 1", n, x,sep=" "),base)]," ")[[1]][14:(14+maxAge)]), x = startyr:endyr)
						natage.f = natage.f + t(temp) 
						n = ifelse(nmorphs ==1, nsexes, b + nsexes) 
						temp = mapply(function(x) temp = as.numeric(strsplit(base[grep(paste(a, b, "2 1 1", n, x,sep=" "),base)]," ")[[1]][14:(14+maxAge)]), x = startyr:endyr)
						natage.m = natage.m + t(temp) 
					}
				}
				
				colnames(natage.f) = 0:maxAge; colnames(natage.m) = 0:maxAge		
				rownames(natage.f) <- startyr:endyr ; rownames(natage.m) <- startyr:endyr
			
				write.csv(natage.f, paste0(csv.dir, "/_natage_f.csv"))
				write.csv(natage.m, paste0(csv.dir, "/_natage_m.csv"))	
			}
		} # SS v3.24 verions loop

		# Check to see if numbers-at-age is calculated 
		if(SS_versionNumeric >= 3.30) {
			check = as.numeric(strsplit(rawstarter[grep("detailed output", rawstarter)]," ")[[1]][1])
			if (check == 2) { "Detailed age-structure set in starter file set = 2 which does not create numbers-at-age table."}

			if (check != 2){
				maxAge = length(strsplit(base[grep(paste("1 1 1 1 1 1 1", startyr,sep=" "),base)]," ")[[1]]) - 14
				
				if (nsexes == 1) {
					natage.f = natage.m = 0
					for(a in 1:nareas){
						temp = mapply(function(x) temp = as.numeric(strsplit(base[grep(paste(a,"1 1 1 1 1 1", x,sep=" "),base)]," ")[[1]][14:(14+maxAge)]), x = startyr:endyr)
						natage.f = natage.f + t(temp) 
					}
					
					colnames(natage.f) = 0:maxAge
					rownames(natage.f) <- startyr:endyr 
			
					write.csv(natage.f, paste0(csv.dir, "/_natage.csv"))
				}
	
				if (nsexes == 2) {
					natage.f = natage.m = 0
					for(a in 1:nareas){
						for (b in 1:nmorphs){
							n = b
							temp = mapply(function(x) temp = as.numeric(strsplit(base[grep(paste(a, b, "1 1 1 1", n, x,sep=" "),base)]," ")[[1]][14:(14+maxAge)]), x = startyr:endyr)
							natage.f = natage.f + t(temp) 
							n = ifelse(nmorphs ==1, nsexes, b + nsexes) 
							temp = mapply(function(x) temp = as.numeric(strsplit(base[grep(paste(a, b, "2 1 1 1", n, x,sep=" "),base)]," ")[[1]][14:(14+maxAge)]), x = startyr:endyr)
							natage.m = natage.m + t(temp) 
						}
					}
					
					colnames(natage.f) = 0:maxAge; colnames(natage.m) = 0:maxAge		
					rownames(natage.f) <- startyr:endyr ; rownames(natage.m) <- startyr:endyr
			
					write.csv(natage.f, paste0(csv.dir, "/_natage_f.csv"))
					write.csv(natage.m, paste0(csv.dir, "/_natage_m.csv"))	
				}					
			} #check loop
		} #  SS version 3.30

	} #nareas
}
