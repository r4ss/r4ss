#' Plot cumulative catch by cohort.
#' 
#' Cumulative catch contributions for each cohort are plotted based on
#' estimated catch-at-age matrix and weight-at-age values by fleet.  Curves are
#' shown in units of both numbers and biomass.
#' 
#' 
#' @param replist List created by \code{SS_output}
#' @param subplots Vector controlling which subplots to create
#' @param add Add to existing plot? (not yet implemented)
#' @param plot Plot to active plot device?
#' @param print Print to PNG files?
#' @param cohortcols Vector of colors to show for each cohort. Default is range
#' of colors shade indicating time period.
#' @param cohortfrac What fraction of the cohorts to include in plot. If value
#' < 1 is used, then cohorts are filtered to only include those with the
#' highest maximum cumulative catch. Value will be overridden by
#' \code{cohortvec}.
#' @param cohortvec Optional vector of birth years for cohorts to include in
#' plot. Value overrides \code{cohortfrac}.
#' @param cohortlabfrac What fraction of the cohorts to label in plot. By
#' default, top 10\% of cohorts are labeled. Value will be overridden by
#' \code{cohortlabvec}.
#' @param cohortlabvec Optional vector of birth years for cohorts to label in
#' plot. Value overrides \code{cohortlabfrac}.
#' @param lwd Line width
#' @param plotdir Directory where PNG or PDF files will be written. By default
#' it will be the directory where the model was run.
#' @param xlab x-label for all plots
#' @param labels Vector of labels for plots (titles and axis labels)
#' @param pwidth Width of plot
#' @param pheight Height of plot
#' @param punits Units for PNG file
#' @param res Resolution for PNG file
#' @param ptsize point size for PNG file
#' @param cex.main Character expansion for plot titles (no titles in this
#' function yet)
#' @param verbose Report progress to R console?
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}}
SSplotCohorts <-
  function(replist,subplots=1:2,add=FALSE,
           plot=TRUE,print=FALSE,
           cohortcols="default",
           cohortfrac=1,
           cohortvec=NULL,
           cohortlabfrac=0.1,
           cohortlabvec=NULL,
           lwd=3, 
           plotdir="default",
           xlab="Year",
           labels=c("Age",
             "Cumulative catch by cohort (in numbers x1000)",
             "Cumulative catch by cohort (x1000 mt)"),
           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,
           cex.main=1, # note: no plot titles yet implemented
           verbose=TRUE)
{
  # plot catch-at-age contributions by cohort in units of numbers and biomass

  subplot_names <- c("1: catch by cohort")  
  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  catage           <- replist$catage
  wtatage          <- replist$wtatage
  nseasons         <- replist$nseasons
  nsexes           <- replist$nsexes
  nfleets          <- replist$nfleets
  nfishfleets      <- replist$nfishfleets
  catch_units      <- replist$catch_units
  startyr          <- replist$startyr
  endyr            <- replist$endyr
  accuage          <- replist$accuage
  growthvaries     <- replist$growthvaries
  growdat          <- replist$endgrowth
  SS_versionshort  <- toupper(substr(replist$SS_version,1,8))
  
#  if(nfishfleets==1 & verbose) cat("  Note: skipping stacked plots of catch for single-fleet model\n")
  if(is.null(wtatage)){
    cat("Warning: no weight-at-age data in replist$wtatage\n",
        "        plots of cohort contributions will be in numbers only\n")
    subplots <- setdiff(subplots,2) # removing subplot 2 from the list
  }else{
    if(nseasons>1)
      cat("Warning: plots of catch by cohort might not work for seasonal models.\n")
  }
  
  if(plotdir=="default") plotdir <- replist$inputs$dir

  # vector of cohort birth years
  yrs <- startyr:endyr
  ages <- 0:accuage
  nages <- length(ages)
  cohorts <- (startyr-accuage):endyr
  ncohorts <- length(cohorts)

  #catcohort <- matrix(NA,nrow=nyrs,ncol=ncohorts)
  catcohort_fltsex <- array(data = NA,
                            dim = c(ncohorts,nages,nfishfleets,nsexes),
                            dimnames = c("cohort","age","fleet","gender"))
  # same dimension array to store biomass values
  wtatage_fltsex <- catcohort_fltsex

  
  for(icohort in 1:ncohorts){          # loop over cohorts (designated birth year)
    cohort <- cohorts[icohort]
    for(iage in ages+1){               # loop over ages
      a <- iage-1 # age starts at 0 but index starts at 1
      y <- cohort+a
      if(y %in% yrs){                  # check if y is in range of years
        for(ifleet in 1:nfishfleets){  # loop over fleets
          f <- ifleet # index = fleet number in current SS but making general
          for(isex in 1:nsexes){       # loop over genders
            ### testing:
            ##   cat('y=',y,' a=',a,' f=',f,' isex=',isex,' cohort=',cohort,'\n',sep='')

            # copy values from catage to catcohort_fltsex
            # summation could include multiple seasons or morphs within a year
            catcohort_fltsex[icohort,iage,ifleet,isex] <-
              sum(catage[catage$Fleet==f & catage$Yr==y & catage$Gender==isex,
                         names(catage)==y-cohort])
            # get assocated weight value
            if(is.null(wtatage)){
              w <- 0 # dummy value to keep code from breaking when wtatage not available
            }else{
              w <- wtatage[[paste("X",a,sep="")]][abs(wtatage$yr)==y &
                                                  wtatage$fleet==f &
                                                  wtatage$gender==isex]
            }
            wtatage_fltsex[icohort,iage,ifleet,isex] <- w
          } # end loop over genders
        } # end loop over fleets
      } # end check for y in yrs
    } # end loop over ages
  } # end loop over cohorts

  # note: "B" notation indicates biomass instead of numbers
  catcohortN <- apply(catcohort_fltsex,1:2,sum)
  catcohortB <- apply(catcohort_fltsex*wtatage_fltsex,1:2,sum)
  rownames(catcohortN) <- cohorts
  colnames(catcohortN) <- ages
   
  
  ### calculate cumulative cohort contributions
  # make temporary matrices with NAs replaced by zeros to do calculations
  tempN <- catcohortN  
  tempB <- catcohortB 
  tempN[is.na(tempN)] <- 0
  tempB[is.na(tempB)] <- 0
  # empty matrix for cumulatives
  cumcatcohortN <- 0*tempN
  cumcatcohortB <- 0*tempB
  
  for(icohort in 1:ncohorts){
    cumcatcohortN[icohort,] <- cumsum(tempN[icohort,])
    cumcatcohortB[icohort,] <- cumsum(tempB[icohort,])
  }
  # restoring NA values for unobserved age/cohort combinations
  cumcatcohortN[is.na(catcohortN)] <- NA 
  cumcatcohortB[is.na(catcohortB)] <- NA 

  # figure out which are the biggest cohorts to show and to label (by numbers)
  cohortmaxN <- apply(cumcatcohortN, 1, max, na.rm=TRUE)
  
  if(is.null(cohortvec)){
    cohortvecN <- cohorts[cohortmaxN >= quantile(cohortmaxN,1-cohortfrac)]
  }else{
    cohortvecN <- cohortvec[cohortvec %in% cohorts]
  }
  if(is.null(cohortlabvec)){
    bigcohortsN <- cohorts[cohortmaxN >= quantile(cohortmaxN,1-cohortlabfrac)]
  }else{
    bigcohortsN <- cohorts[cohorts %in% cohortlabvec]
  }
  bigcohortsN <- bigcohortsN[bigcohortsN %in% cohortvecN]
  maxagesN <- pmin(endyr - bigcohortsN, accuage) # max observed age is accumulator age or earlier
  maxvecN <- cohortmaxN[cohorts %in% bigcohortsN]

  # figure out which are the biggest cohorts to show and to label (by biomass)
  cohortmaxB <- apply(cumcatcohortB,1, max, na.rm=TRUE)
  if(is.null(cohortvec)){
    cohortvecB <- cohorts[cohortmaxB >= quantile(cohortmaxB,1-cohortfrac)]
  }else{
    cohortvecB <- cohortvec[cohortvec %in% cohorts]
  }
  if(is.null(cohortlabvec)){
    bigcohortsB <- cohorts[cohortmaxB >= quantile(cohortmaxB,1-cohortlabfrac)]
  }else{
    bigcohortsB <- cohorts[cohorts %in% cohortlabvec]
  }
  bigcohortsB <- bigcohortsB[bigcohortsB %in% cohortvecB]
  maxagesB <- pmin(endyr - bigcohortsB, accuage) # max observed age is accumulator age or earlier
  maxvecB <- cohortmaxB[cohorts %in% bigcohortsB]

  # set colors
  if(cohortcols[1]=="default"){
    cohortcolsN <- rich.colors.short(length(cohortvecN),alpha=.7)
    cohortcolsB <- rich.colors.short(length(cohortvecB),alpha=.7)
  }else{
    cohortcolsN <- cohortcolsB <- cohortcols
  }

  plotfun <- function(isubplot){
    if(isubplot==1){
      # make plot of cumulative numbers by cohort
      matplot(x=0:accuage,t(cumcatcohortN[cohorts %in% cohortvecN,]),
              xlab="Age",ylab=labels[2],type='l',
              xlim=c(0,1.1*accuage),col=cohortcolsN,lty=1,lwd=lwd)
      ## print(cbind(bigcohorts,maxages,maxvec))
      points(x=maxagesN,y=maxvecN,pch=16,cex=.5)
      text(x=maxagesN,y=maxvecN,labels=bigcohortsN,pos=4)
    }
    if(isubplot==2){
      # make plot of cumulative biomass by cohort
      matplot(x=0:accuage,t(cumcatcohortB[cohorts %in% cohortvecB,])/1000,
              xlab="Age",ylab=labels[3],type='l',
              xlim=c(0,1.1*accuage),col=cohortcolsN,lty=1,lwd=lwd)
      ## print(cbind(bigcohorts,maxages,maxvec))
      points(x=maxagesB,y=maxvecB/1000,pch=16,cex=.5)
      text(x=maxagesB,y=maxvecB/1000,labels=bigcohortsB,pos=4)
    }  
  }
  
  if(plot) for(isubplot in subplots) plotfun(isubplot)

  if(print){
    for(isubplot in subplots){
      if(isubplot==1){
        file <- paste("catch_cohort_numbers.png",sep="")
        caption <- labels[2]
      }
      if(isubplot==2){
        file <- paste("catch_cohort_biomass.png",sep="")
        caption <- labels[3]
      }
      plotinfo <- pngfun(file=file, caption=caption)
      plotfun(isubplot)
      dev.off()
    }
  }
  
  returnlist <-
    list(catcohort_fltsex=catcohort_fltsex,
         wtatage_fltsex=wtatage_fltsex,
         catcohortN=catcohortN,
         catcohortB=catcohortB,
         cumcatcohortN=cumcatcohortN,
         cumcatcohortB=cumcatcohortB)
 
  returnlist$plotinfo <- plotinfo
  return(invisible(returnlist))
}
