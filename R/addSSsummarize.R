#' Add a model to the list of models to compare
#' 
#' Adds specified quantities from any model to the list of models returned from
#' \code{\link{SSsummarize}} for further comparison.
#' 
#' @param origModels A list of models created by \code{\link{SSsummarize}}.
#' @param newModels A list of models to add to the originals models list.  Each
#' new model is an element of the list, and is a list itself with possible
#' components described in the details below.
#' @return Returns list as is returned from \code{\link{SSsummarize}}, but
#' contains additions for the new models.
#' @note This function was made to compare TINSS results and SS results, and
#' assumed that you would always start with a list of SS models output from
#' \code{\link{SSsummarize}}.  It has not been tested to see how it works when
#' starting with an empty list.
#' @author Allan Hicks
#' @export
#' @seealso \code{\link{SSsummarize}} \code{\link{SSplotComparisons}}
#' @keywords data manip list
#' @examples
#' 
#'   \dontrun{
#'   ######################################
#'   #DO NOT RUN
#' tinss1 <- list(npars=A$fit$npar,maxgrad=A$fit$maxgrad,nsexes=1,
#'                #note, there is an estimated parameter called sd_sbt,
#'                #      but it is a single value
#'                SpawnBio=data.frame(c(1964,1965,A$yrs),
#'                                    c(A$sbo,A$sbo,A$sbt)*1e6,0,
#'                                    qnorm(0.025,c(A$so,A$so,A$sbt)*1e6,0),
#'                                    qnorm(0.975,c(A$so,A$so,A$sbt)*1e6,0)),
#'                Bratio=data.frame(A$yrs,A$sbt/A$sbo,0,
#'                                  qnorm(0.025,A$sbt/A$sbo,0),
#'                                  qnorm(0.975,A$sbt/A$sbo,0)),
#'                SPRratio=data.frame(A$yr,A$spr,0,qnorm(0.025,A$spr,0),
#'                                    qnorm(0.975,A$spr,0)),
#'                recruits=data.frame(A$yr,A$nt[,1]*1e6,0,qnorm(0.025,A$nt[,1]*1e5,0),
#'                                    qnorm(0.975,A$nt[,1]*1e6,0)),
#'                #I'm not sure exactly what wt are,
#'                #   but it is important to line them up correctly
#'                recdevs=data.frame(A$recYrs,A$wt),  
#'                indices = data.frame(A$iyr,1e6*A$yt,1e6*A$qbt,
#'                                     rep(A$q,length(A$iyr)),rep(0.4,length(A$iyr)),
#'                                     rep(0,length(A$iyr)),rep(1,length(A$iyr)))
#'                )
#'   tinss <- list(tinss1,tinss1)   #can add more models here
#' 
#' 
#'   #add TINSS model to SS models already summarized                
#'   SSnTINSS <- addSSsummarize(models,tinss)
#'   mcmcInd <- seq(burnin+1,nrow(A$mc.sbt),thin)
#'   SSnTINSS$mcmc[[2]] <- data.frame(A$mc.sb0[mcmcInd],
#'                                    A$mc.sbt[mcmcInd,],
#'                                    A$mc.depl[mcmcInd,],
#'                                    A$mc.spr[mcmcInd,],
#'                                    A$mc.rt[mcmcInd,],
#'                                    log(A$mcmc[mcmcInd,"Ro"]*1e6),
#'                                    A$mcmc[mcmcInd,"msy"]*1e6)  
#'   names(SSnTINSS$mcmc[[2]]) <-
#'     c("SPB_Virgin",paste("SPB",A$yrs,sep="_"),
#'       paste("Bratio",A$yrs,sep="_"),
#'       paste("SPRratio",A$yr,sep="_"),
#'       paste("Recr",A$yr,sep="_"),"SR_R0","TotYield_MSY")
#'   modelnames <- c("SS", "TINSS","TINSS.MLE")
#'   
#'   SSplotComparisons(SSnTINSS, legendlabels=modelnames,
#'                     subplot=2,endyr=2011,mcmcVec=c(T,T,F))
#'   title(main="MCMC")
#'   SSplotComparisons(SSnTINSS, legendlabels=modelnames,
#'                     subplot=4,endyr=2011,mcmcVec=c(T,T,F))
#'   title(main="MCMC")
#'   ###############################################
#'   }
#' 
addSSsummarize <- function(origModels,newModels) {
########################################################################
# This function adds the specified components to a model summary object
# If there are no original models, it will create an object that can be used with r4ss model comparisons
#
# Written by Allan Hicks, 1/12/2011, allan.hicks@noaa.gov
#
# origModels is the list of original models that are being added to
#   it is the result of the SSsumarize function
#   if origModels is NULL, then a new list will be returned that can be used in r4ss model comparisons
# 
# newModels is a list of models to be added to the origModels
#   each new model is an element of the list, and is a list itself
#   each new model is a list with possible components named:
#       npars:          the number of parameters in the model
#       maxgrad:        the maximum gradient component (if used)
#       nsexes:         the number of sexes
#       likelihoods:    likelihoods from the model. A data.frame with the 2nd column as names, which matches on names from origModels
#                           SS uses the following names
#                            TOTAL
#                            Equil_catch
#                            Survey
#                            Length_comp
#                            Age_comp
#                            Recruitment
#                            recast_Recruitment
#                            Parm_priors
#                            Parm_softbounds
#                            Parm_devs
#                            Crash_Pen
#                            Size_at_age
#       likelambdas     NOT IMPLEMENTED
#       #pars#:         NOT IMPLEMENTED YET FOR DIFFICULTY IN MATCHING PARAMETERS
#       #parsSD#:       NOT IMPLEMENTED YET FOR DIFFICULTY IN MATCHING PARAMETERS
#       #parsphases#:   NOT IMPLEMENTED YET FOR DIFFICULTY IN MATCHING PARAMETERS
#       SpawnBio:       Spawning biomass matrix 
#                           1st column is year
#                           2nd column is spawning biomass in same units as original models (SS reports female spawning biomass)
#                           3rd column is the standard deviation of estimated spawning biomass
#                           4th column is a lower bound of the confidence interval to be plotted (say from an MCMC)
#                           5th column is an upper bound of the confidence interval to be plotted (say from an MCMC)
#       Bratio:         Depletion matrix
#                           1st column is year
#                           2nd column is depletion
#                           3rd column is the standard deviation of depletion (optional)
#                           4th column is a lower bound of the confidence interval to be plotted (say from an MCMC)
#                           5th column is an upper bound of the confidence interval to be plotted (say from an MCMC)
#       SPRratio:         SPR ratio matrix
#                           1st column is year
#                           2nd column is depletion
#                           3rd column is the standard deviation (optional)
#                           4th column is a lower bound of the confidence interval to be plotted (say from an MCMC)
#                           5th column is an upper bound of the confidence interval to be plotted (say from an MCMC)
#       recruits:       Recruitment matrix
#                           1st column is year
#                           2nd column is recruitment as in original models (SS reports age-0 recruits)
#                           3rd column is the standard deviation (optional)
#                           4th column is a lower bound of the confidence interval to be plotted (say from an MCMC)
#                           5th column is an upper bound of the confidence interval to be plotted (say from an MCMC)
#       recdevs:        Recruitment deviate matrix
#                           1st column is year
#                           2nd column is deviate (matched with original models)
#       growth:         #NOT IMPLEMENTED
#       indices:        Matrix of fits to indices
#                           1st column is year
#                           2nd column is observed index (data)
#                           3rd column is expected index (prediction)
#                           4th column is catchability coefficient (q)
#                           5th column is standard error of index (total used in fitting)
#                           6th column is a likelihood for this point, or enter any value to make sure it plots, or enter NA not to plot the estimate
#                           
#       InitAgeYrs      #NOT IMPLEMENTED

    if(!is.list(newModels[[1]])) stop("The new models needs to be a list of lists. A list of new models, each with a list of quantities to enter.")

    cat("Adding",length(newModels),"new models to",origModels$n,"original models\n")
    
    if(is.null(origModels)) {
        stop("Not implemented yet for no original models\n")
        #origModels <- list()
    }
    
    addColumn <- function(origDF,newDF,matchOrig,matchNew,nOrig,valNew=1) {
        #adds a column for the new model to the dataframe and matches using the original column labeled with matchOrig and the new 
        #nOrig is the number of original models
        if(is.null(newDF)) {
            newDF <- data.frame(rep(NA,nrow(origDF)),origDF[,matchOrig])
            valNew <- 1  
            matchNew <- 2
        }
        x.add <- newDF[!(newDF[,matchNew] %in% origDF[,matchOrig]),]
        if(nrow(x.add)>0) { #add additional rows for missing names
            origDFnrow <- nrow(origDF)
            for(i in 1:nrow(x.add)) {origDF <- rbind(origDF,NA)} #add neessary number of rows of NA
            origDF[,matchOrig] <- c(origDF[1:origDFnrow,matchOrig],as.character(x.add[,matchNew]))
        }
        x.ind <- match(origDF[,matchOrig],newDF[,matchNew])   #should have all rows of newDF and origDF
        out <- cbind(origDF[,1:nOrig],newDF[x.ind,valNew],origDF[,(nOrig+1):ncol(origDF)])
        colnames(out) <- c(1:(nOrig+1),colnames(origDF)[(nOrig+1):ncol(origDF)])
        out
    }
    
    addNewModel.fn <- function(x,models) {
        #adds a single model to the origModels
        #models is the set of models formatted as in SSsummarize
        #written as a function to use lapply, avoid loops and make it easier when origModels=NULL
        n <- models$n
        models$n <- n+1
        if(!is.null(x$npars))   { models$npars[n+1] <- x$npars }else{ models$npars[n+1] <- NA }
        if(!is.null(x$listnames))   { models$npars[n+1] <- x$listnames }else{ models$npars[n+1] <- NA }
        if(!is.null(x$keyvec))   { models$npars[n+1] <- x$keyvec }else{ models$npars[n+1] <- NA }
        if(!is.null(x$maxgrad)) { models$maxgrad[n+1] <- x$maxgrad }else{ models$maxgrad[n+1] <- NA }
        if(!is.null(x$nsexes))  { models$nsexes[n+1] <- x$nsexes }else{ cat("nsexes must be defined! Inserting a value of 3.\n"); models$nsexes[n+1] <- 3}
        
        models$pars <- addColumn(models$pars,NULL,"Label",2,n)   ###NEED TO IMPLEMENT
        models$parsSD <- addColumn(models$parsSD,NULL,"Label",2,n)   ###NEED TO IMPLEMENT
        models$parphases <- addColumn(models$parphases,NULL,"Label",2,n)   ###NEED TO IMPLEMENT
        models$quants <- addColumn(models$quants,NULL,"Label",2,n)   ###NEED TO IMPLEMENT
        models$quantsSD <- addColumn(models$quantsSD,NULL,"Label",2,n)   ###NEED TO IMPLEMENT        
        models$likelihoods <- addColumn(models$likelihoods,x$likelihoods,"Label",2,n)
        models$likelambdas <- addColumn(models$likelambdas,NULL,"Label",2,n)   ###NEED TO IMPLEMENT
        #add Spawning Biomasses, note that it is only the column that changes
        if(!is.null(x$SpawnBio)) print("adding SpawnBio")
        if(!is.null(x$SpawnBio)){if(ncol(x$SpawnBio)<5) {stop("There must be at least 5 columns in SpawnBio: Yr, SB, SD, Lower, Upper\n")}}
        models$SpawnBio <- addColumn(models$SpawnBio,x$SpawnBio,"Yr",1,n,2)
        models$SpawnBio$Yr <- as.numeric(models$SpawnBio$Yr)
        models$SpawnBio <- models$SpawnBio[order(models$SpawnBio[,"Yr"]),]
        models$SpawnBioSD <- addColumn(models$SpawnBioSD,x$SpawnBio,"Yr",1,n,3)
        models$SpawnBioSD$Yr <- as.numeric(models$SpawnBioSD$Yr)
        models$SpawnBioSD <- models$SpawnBioSD[order(models$SpawnBioSD[,"Yr"]),]
        models$SpawnBioLower <- addColumn(models$SpawnBioLower,x$SpawnBio,"Yr",1,n,4)
        models$SpawnBioLower$Yr <- as.numeric(models$SpawnBioLower$Yr)
        models$SpawnBioLower <- models$SpawnBioLower[order(models$SpawnBioLower[,"Yr"]),]
        models$SpawnBioUpper <- addColumn(models$SpawnBioUpper,x$SpawnBio,"Yr",1,n,5)
        models$SpawnBioUpper$Yr <- as.numeric(models$SpawnBioUpper$Yr)
        models$SpawnBioUpper <- models$SpawnBioUpper[order(models$SpawnBioUpper[,"Yr"]),]
        #add Bratio (depletion), note that it is only the column that changes
        if(!is.null(x$Bratio)) {
            if(ncol(x$Bratio)<5) {stop("There must be at least 5 columns in Bratio: Yr, SB, SD, Lower, Upper\n")}
            print("adding Bratio")
        }
        models$Bratio <- addColumn(models$Bratio,x$Bratio,"Yr",1,n,2)
        models$Bratio$Yr <- as.numeric(models$Bratio$Yr)
        models$Bratio <- models$Bratio[order(models$Bratio[,"Yr"]),]
        models$BratioSD <- addColumn(models$BratioSD,x$Bratio,"Yr",1,n,3)
        models$BratioSD$Yr <- as.numeric(models$BratioSD$Yr)
        models$BratioSD <- models$BratioSD[order(models$BratioSD[,"Yr"]),]
        models$BratioLower <- addColumn(models$BratioLower,x$Bratio,"Yr",1,n,4)
        models$BratioLower$Yr <- as.numeric(models$BratioLower$Yr)
        models$BratioLower <- models$BratioLower[order(models$BratioLower[,"Yr"]),]
        models$BratioUpper <- addColumn(models$BratioUpper,x$Bratio,"Yr",1,n,5)
        models$BratioUpper$Yr <- as.numeric(models$BratioUpper$Yr)
        models$BratioUpper <- models$BratioUpper[order(models$BratioUpper[,"Yr"]),]
        #add recruits        
        models$recruits <- addColumn(models$recruits,x$recruits,"Yr",1,n,2)
        models$recruitsSD <- addColumn(models$recruitsSD,x$recruits,"Yr",1,n,3)
        models$recruitsLower <- addColumn(models$recruitsLower,x$recruits,"Yr",1,n,4)
        models$recruitsUpper <- addColumn(models$recruitsUpper,x$recruits,"Yr",1,n,5)
        models$recdevs <- addColumn(models$recdevs,x$recdevs,"Yr",1,n,2)
        #add SPR
        models$SPRratio <- addColumn(models$SPRratio,x$SPRratio,"Yr",1,n,2)
        models$SPRratioSD <- addColumn(models$SPRratioSD,x$SPRratio,"Yr",1,n,3)
        models$SPRratioLower <- addColumn(models$SPRratioLower,x$SPRratio,"Yr",1,n,4)
        models$SPRratioUpper <- addColumn(models$SPRratioUpper,x$SPRratio,"Yr",1,n,5)


        models$growth <- cbind(models$growth,NA)  #NEED TO IMPLEMENT
        
        #Indices
        if(!is.null(x$indices)) {
            newIndices <- as.data.frame(cbind(NA,x$indices[,1],NA,NA,x$indices[,2],x$indices[,3],x$indices[,4],NA,x$indices[,5],NA,x$indices[,6],NA,NA,NA,NA,n+1,n+1))
            names(newIndices) <- names(models$indices)
            models$indices <- rbind(models$indices,newIndices)
        }
        
        #models$InitAgeYrs <- cbind(models$InitAgeYrs,NA)  #NEED TO IMPLEMENT
        
        return(models)
    }
    
    for(ii in 1:length(newModels)) {  #lapply doesn't work because it returns a separate list for each newModel
        origModels <- addNewModel.fn(newModels[[ii]],models=origModels)
    }
        
    return(origModels)
}

if(F) {

    likes <- c(4242,5555,9933,3377)
    likes <- data.frame(likes=likes,label=c("Survey","Other","TOTAL","Other2"))
    SB <- data.frame(c(1971,1972,1973),c(3333,5555,7777),c(100,200,300),c(1111,3333,5555),c(5555,7777,9999))
    BR <- data.frame(c(1971,1972,1973),c(0.3333,0.5555,0.7777),c(0.100,.200,.300),c(.1111,.3333,.5555),c(.5555,.7777,.9999))
    newModel1 <- list(npars=42,likelihoods=likes,SpawnBio=SB,Bratio=BR)  #data.frame(1,"NatM_p_1_Fem_GP_1"))
    likes <- c(4233,5544,9988,3344)
    likes <- data.frame(likes=likes,label=c("Recruitment","Other","TOTAL","Other2"))
    newModel2 <- list(npars=42,likelihoods=likes)
    newModels <- list(newModel1,newModel2)
    tmp <- addSSsummarize(mysummary,newModels)

        x.ind <- match(models$likelihoods[,"Label"],names(x$likelihoods))
        models$likelihoods <- cbind(models$likelihoods[,1:n],x$likelihoods[x.ind],models$likelihoods[,n+1])
        colnames(models$likelihoods) <- c(1:(n+1),"Label")
        x.add <- x$likelihoods[!(names(x$likelihoods) %in% models$likelihoods[,"Label"])]
        if(length(x.add)>0) { #add additional rows for missing names
            tmp <- as.data.frame(matrix(NA,nrow=length(x.add),ncol=ncol(models$likelihoods),dimnames=list(NULL,colnames(models$likelihoods))))
            tmp[,n+1] <- x.add
            tmp[,"Label"] <- names(x.add)
            models$likelihoods <- rbind(models$likelihoods,tmp)
        }

    addNewModel.fn <- function(x,models) {
        #adds a single model to the origModels
        #models is the set of models formatted as in SSsummarize
        #written as a function to use lapply, avoid loops and make it easier when origModels=NULL
        n <- models$n
        models$n <- n+1
        if(!is.null(x$npars))   { models$npars[n+1] <- x$npars }else{ models$npars[n+1] <- NA }
        if(!is.null(x$maxgrad)) { models$maxgrad[n+1] <- x$maxgrad }else{ models$maxgrad[n+1] <- NA }
        if(!is.null(x$nsexes))  { models$nsexes[n+1] <- x$nsexes }else{ models$nsexes[n+1] <- NA }
        
        models$likelihoods <- addColumn(models$likelihoods,x$likelihoods,"Label",2,n)
        
        return(models)
    }

}
