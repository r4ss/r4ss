# calculate some basic risk metrics
#    prob SB increases in year compared to current year
#    prob SB in compareYrs above a B0 reference pt
#    prob SPR exceeds a reference point (meaning greater fishing intensity)
# a start to generalizing this to many other metrics
#' @param models a single model or a list of the models to include. If multiple models, they are combined using the ensemble package
#' @param currYr the year for comparison
#' @param compareYrs the years to compare to current year (can compare previous or future years)
#' @param mcmc
#' @param SOMETHING FOR ARGUMENTS IN COMBINE ENSEMBLE
#' 
riskMetrics.fn <- function(models,currYr,compareYrs,mcmc=F) {

	if(any(names(models)=="Data_File")) {
		models <- list(models) #a single model entered, but make it in same format for multiple
	}

	#assumes that all models are from same version of SS (or at least 3.2 vs 3.3)
	if(models[[1]]$SS_versionNumeric >= 3.3) {
		params <- c("SSB","Bratio","SPRratio")
	} else {
		params <- c("SPB","Bratio","SPRratio")
	}

	SBsamples <- combineEnsemble.fn(models,param=params[1],element="derived_quants",yrs=c(currYr,sort(unique(compareYrs))),totN=1e6,useCov=TRUE)
	currSBsamples <- SBsamples[,1]
	SBrisks <- apply(SBsamples, 2, function(x,curr) {sum(x<curr)/length(curr)},curr=currSBsamples)
	SBrisks <- SBrisks[-1] #I kept this column in to make sure apply could be used when only 1 compare yr is specified



	return(SBrisks)
}


if(F) {

	#Testing by Allan Hicks

	#install.packages("devtools")
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
	xx <- combineEnsemble.fn(models,param="SPB",element="derived_quants",yrs=2016:2019,totN=1e6,useCov=TRUE)
	apply(xx,2,median)
	sum(xx[,"SPB_2017"]<xx[,"SPB_2016"])/nrow(xx)

	set.seed(42)
	riskMetrics.fn(longAAF,currYr=2016,compareYrs=2017:2019)


	setwd("C:/OneDrive/Hake/hake2018")
	hake  <- SS_output("Hake2018model",verbose=F,covar=T)

	models <- list(hake)
	set.seed(42)
	xx <- combineEnsemble.fn(models,param="SSB",element="derived_quants",yrs=2016:2019,totN=1e6,useCov=TRUE)
	sum(xx[,"SSB_2019"]<xx[,"SSB_2018"])/nrow(xx)

	riskMetrics.fn(hake,currYr=2018,compareYrs=2019)



}