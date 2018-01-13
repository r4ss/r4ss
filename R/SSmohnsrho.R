##' Calculate Mohn's Rho values for select quantities
##' Function calculates a rho value for the ending year for each retrospective
##' relative to the reference model and a "Wood's Hole Mohn's Rho" which is based
##' on all years between the reference and the retrospective run.
##'
## Grab all the results 
##' @param summaryoutput List created by \code{SSsummarize}. The expected order for the 
##' models are the full reference model, the retro -1, retro -2, and so forth.
##' @param endyrvec Single year or vector of years representing the
##' final year of values to show for each model. 
##' @param startyr Single year used to calculate the start of the Wood's Hole
##' Mohn's Rho value across all years.
##'
##' @author Chantel R. Wetzel
##'
##' @export

SSmohnsrho <- function(summaryoutput, endyrvec = NULL, startyr = NULL){

	print ("The expected order of model in the summary output are the reference model followed by retro -1, retro -2, and so forth.")

	N = summaryoutput$n
	if(length(endyrvec) == 0) { endyrvec = rev( (summaryoutput$endyrs[N]-N+1) :summaryoutput$endyrs[N]) }

	if(length(startyr) == 0) { startyr = summaryoutput$startyr[1] }
	
	
	mohnSSB = mohnRec = mohnBratio = mohnF = numeric()
	mohnSSB.all = mohnRec.all = mohnBratio.all = mohnF.all = numeric()

	# Mohn's Rho Calculation for the terminal year for each of the retrospectives relative to the reference model
	# Rho = sum over y [ (X_y,retro - X_y,ref) / X_y,ref ]
	for(i in 1:(N-1))
	{
	  ind = which(summaryoutput$SpawnBio$Yr == endyrvec[i])
	  mohnSSB[i] = (summaryoutput$SpawnBio[ind, i+1] - summaryoutput$SpawnBio[ind, 1]) / summaryoutput$SpawnBio[ind, 1] 

	  ind = which(summaryoutput$recruits$Yr == endyrvec[i])
	  mohnRec[i] = (summaryoutput$recruits[ind, i + 1] - summaryoutput$recruits[ind, 1]) / summaryoutput$recruits[ind, 1] 

	  ind = which(summaryoutput$Bratio$Yr == endyrvec[i])
	  mohnBratio[i] = (summaryoutput$Bratio[ind, i + 1] - summaryoutput$Bratio[ind, 1]) / summaryoutput$Bratio[ind, 1] 

	  ind = which(summaryoutput$Fvalue$Yr == endyrvec[i])
	  mohnF[i] = (summaryoutput$Fvalue[ind, i + 1] - summaryoutput$Fvalue[ind, 1]) / summaryoutput$Fvalue[ind, 1] 
	}

	# Wood's Hole Mohn's Rho Calculation for all years for each of the retrospectives relative to the reference model
	# Rho = sum over y [ (X_y,retro - X_y,ref) / X_y,ref ]
	# This rho value is then scaled acording to the number of model year for comparison between the one year and
	# all year calculation
	# Rho = Rho / Number of Years
	for(i in 1:(N-1))
	{
	  ind = which(summaryoutput$SpawnBio$Yr == startyr):which(summaryoutput$SpawnBio$Yr == endyrvec[i])
	  mohnSSB.all[i] =  sum( (summaryoutput$SpawnBio[ind, i+1] - summaryoutput$SpawnBio[ind, 1]) / 
	  					 summaryoutput$SpawnBio[ind, 1] ) / length(ind)

	  ind = which(summaryoutput$recruits$Yr == startyr):which(summaryoutput$recruits$Yr == endyrvec[i])
	  mohnRec.all[i] = sum( (summaryoutput$recruits[ind, i + 1] - summaryoutput$recruits[ind, 1]) / 
	  					  summaryoutput$recruits[ind, 1] ) / length(ind)

	  ind = which(summaryoutput$Bratio$Yr == startyr+1):which(summaryoutput$Bratio$Yr == endyrvec[i])
	  mohnBratio.all[i] = sum( (summaryoutput$Bratio[ind, i + 1] - summaryoutput$Bratio[ind, 1]) / 
	  					  summaryoutput$Bratio[ind, 1] ) / length(ind)

	  ind = which(summaryoutput$Fvalue$Yr == startyr):which(summaryoutput$Fvalue$Yr == endyrvec[i])
	  mohnF.all[i] = sum( (summaryoutput$Fvalue[ind, i + 1] - summaryoutput$Fvalue[ind, 1]) / 
	  					  summaryoutput$Fvalue[ind, 1] ) / length(ind)
	}

  mohn.out = list()
  mohn.out$SSB = sum(mohnSSB)
  mohn.out$Rec = sum(mohnRec)
  mohn.out$Bratio = sum(mohnBratio)
  mohn.out$F = sum(mohnF)

  mohn.out$SSB.all = sum(mohnSSB.all)
  mohn.out$Rec.all = sum(mohnRec.all)
  mohn.out$Bratio.all = sum(mohnBratio.all)
  mohn.out$F.all = sum(mohnF.all)
  return(mohn.out)
}
