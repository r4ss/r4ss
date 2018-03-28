# calculate some basic risk metrics
#    prob SB increases in year compared to current year
#    prob SB in compareYrs above a B0 reference pt
#    prob SPR exceeds a reference point (meaning greater fishing intensity)
# a start to generalizing this to many other metrics
#' @param ... Each entry can be a single model or a list of the models indicating an ensemble (combined using the ensemble package). Each model or list entered is a row in the risk table.
#' @param currYr the year for comparison
#' @param compareYrs the years to compare to current year (can compare previous or future years)
#' @param BratioRefPt would be nice to have this work with multiple values!!!
#' @param mcmc
#' @param SOMETHING FOR ARGUMENTS IN COMBINE ENSEMBLE
#' 
riskMetrics.fn <- function(...,currYr,compareYrs,BratioRefPt=0.3,SPRrefPt=1-0.46,mcmc=F) {

	models <- eval(substitute(list(...)))
	nRows <- length(models) #this is the number of different rows for the table

	for(i in 1:nRows) {
		if(any(names(models[[i]])=="Data_File")) {
			models[[i]] <- list(models[[i]]) #a single model entered, but make it in same format for multiple
		}
	}

	#assumes that all models are from same version of SS (or at least 3.2 vs 3.3)
	# if(models[[1]][[1]]$SS_versionNumeric >= 3.3) {
	# 	params <- c("SSB","Bratio","SPRratio")
	# } else {
	# 	params <- c("SPB","Bratio","SPRratio")
	# }

	params <- c("SSB","Bratio","SPRratio")

	#Spawning Biomass risks
	SBrisks <- vector(mode="list",length=nRows)
	for(i in 1:nRows) {
		SBsamples <- combineEnsemble.fn(models[[i]],param=params[1],element="derived_quants",yrs=c(currYr,sort(unique(compareYrs))),totN=1e6,useCov=TRUE)
		currSBsamples <- SBsamples[,1]
		SBrisks[[i]] <- apply(SBsamples, 2, function(x,curr) {sum(x<curr)/length(curr)},curr=currSBsamples)
		SBrisks[[i]] <- SBrisks[[i]][-1] #I kept this column in to make sure apply could be used when only 1 compare yr is specified		
	}

	#BratioRisks (only works for models that use static B0 from SS)
	BratioRisks <- vector(mode="list",length=nRows)
	for(i in 1:nRows) {
		BratSamples <- combineEnsemble.fn(models[[i]],param=params[2],element="derived_quants",yrs=c(currYr,sort(unique(compareYrs))),totN=1e6,useCov=TRUE)
		BratioRisks[[i]] <- apply(BratSamples, 2, function(x,y) {sum(x<y)/length(x)},y=BratioRefPt)
		BratioRisks[[i]] <- BratioRisks[[i]][-1] #I kept this column in to make sure apply could be used when only 1 compare yr is specified		
	}

	#SPRrisks
	SPRrisks <- vector(mode="list",length=nRows)
	for(i in 1:nRows) {
		SprSamples <- combineEnsemble.fn(models[[i]],param=params[3],element="derived_quants",yrs=c(currYr,sort(unique(compareYrs))),totN=1e6,useCov=TRUE)
		SPRrisks[[i]] <- apply(SprSamples, 2, function(x,y) {sum(x<y)/length(x)},y=SPRrefPt)
	}

	return(list(SBrisks,BratioRisks,SPRrisks))
}


if(F) {

	#Testing by Allan Hicks

	#install.packages("devtools")
	#install.packages("mvtnorm")
	#devtools::install_github("allanhicks/Ensemble")
	#devtools::install_github("r4ss/r4ss")

	library(Ensemble)

	library(r4ss)
	library(MASS)
	library(corpcor)

	setwd("C:/OneDrive/IPHC/Assessment")

	longAAF  <- SS_output("2015/2015Models/BlueLine/Final_2015_long_areasasfleets_model",verbose=F)
	models <- list(longAAF)
	set.seed(42)
	xx <- combineEnsemble.fn(models,param="SSB",element="derived_quants",yrs=2016:2019,totN=1e6,useCov=TRUE)
	apply(xx,2,median)
	sum(xx[,"SPB_2017"]<xx[,"SPB_2016"])/nrow(xx)

	set.seed(42)
	riskMetrics.fn(longAAF,currYr=2016,compareYrs=2017:2019)
#the problem is that SSB is used in derived_quants and SPB is used in covar

	setwd("C:/OneDrive/Hake/hake2018")
	hake  <- SS_output("Hake2018model",verbose=F,covar=T)

	models <- list(hake)
	set.seed(42)
	xx <- combineEnsemble.fn(models,param="SSB",element="derived_quants",yrs=2018:2020,totN=1e6,useCov=TRUE)
	sum(xx[,"SSB_2019"]<xx[,"SSB_2018"])/nrow(xx)

	riskMetrics.fn(hake,hake,currYr=2018,compareYrs=2019,BratioRefPt=0.40,SPRrefPt=1)

test.fn <- function(...,fn=mean) {
	dots <- match.call(expand.dots = FALSE)$...
    object <- as.list(substitute(list(...)))[-1L]

    print(is.list(list(...)))
	print(dots)
	print(length(object))
	return(eval(substitute(list(...))))
}
x1 <- data.frame(a=1:3,b=1:3)
x2 <- data.frame(b=4:8,c=4:8)
x <- test.fn(x1,x2)



x <- test.fn(longAAF,hake)

#test 2017 halibut model
	setwd("C:/OneDrive/IPHC/Assessment")
	shortAAF  <- SS_output("2017/2017Models/Short_AAF",verbose=F)
	shortCW  <- SS_output("2017/2017Models/Short_CW",verbose=F)
	longAAF  <- SS_output("2017/2017Models/Long_AAF",verbose=F)
	longCW  <- SS_output("2017/2017Models/Long_CW",verbose=F)

	riskMetrics.fn(list(shortAAF,shortCW,longAAF,longCW),currYr=2018,compareYrs=2019:2021)




}