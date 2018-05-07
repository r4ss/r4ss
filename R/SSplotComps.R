#' Plot composition data and fits.
#'
#' Plot composition data and fits from Stock Synthesis output.  Multi-figure
#' plots depend on \code{make_multifig}.
#'
#'
#' @param replist list created by \code{SSoutput}
#' @param subplots vector controlling which subplots to create
#' @param kind indicator of type of plot can be "LEN", "SIZE", "AGE", "cond",
#' "GSTAGE", "L[at]A", or "W[at]A".
#' @param sizemethod if kind = "SIZE" then this switch chooses which of the
#' generalized size bin methods will be plotted.
#' @param aalyear Years to plot multi-panel conditional age-at-length fits for
#' all length bins; must be in a "c(YYYY,YYYY)" format. Useful for checking the
#' fit of a dominant year class, critical time period, etc. Default=-1.
#' @param aalbin The length bin for which multi-panel plots of the fit to
#' conditional age-at-length data will be produced for all years.  Useful to
#' see if growth curves are ok, or to see the information on year classes move
#' through the conditional data. Default=-1.
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param fleets optional vector to subset fleets for which plots will be made
#' @param fleetnames optional vector of fleet names to put in the labels
#' @param sexes which sexes to show plots for. Default="all" which will include
#' males, females, and unsexed. This option is not fully implemented for all
#' plots.
#' @param yupper upper limit on ymax for polygon/histogram composition plots
#' @param datonly make plots of data without fits as well as data with fits?
#' @param samplesizeplots make sample size plots?
#' @param compresidplots make plots of residuals for fit to composition data?
#' @param bub make bubble plot for numbers at age or size?
#' @param showyears Add labels for years to sample size plots?
#' @param showsampsize add sample sizes to plot
#' @param showeffN add effective sample sizes to plot
#' @param aggregates_by_mkt separate plots of aggregates across years
#' into different plots for each market category (retained, discarded)?
#' @param sampsizeline show line for input sample sizes on top of conditional
#' age-at-length plots (TRUE/FALSE, still in development)
#' @param effNline show line for effective sample sizes on top of conditional
#' age-at-length plots (TRUE/FALSE, still in development)
#' @param minnbubble number of unique x values before adding buffer. see
#' ?bubble3 for more info.
#' @param pntscalar This scalar defines the maximum bubble size for bubble
#' plots. This option is still available but a better choice is to use cexZ1
#' which allow the same scaling throughout all plots.
#' @param scalebubbles scale data-only bubbles by sample size, not just
#' proportion within sample? Default=FALSE.
#' @param cexZ1 Character expansion (cex) for point associated with value of 1.
#' @param bublegend Add legend with example bubble sizes to bubble plots.
#' @param colvec Vector of length 3 with colors for females, males, unsexed fish
#' @param linescol Color for lines on top of polygons
#' @param axis1 position of bottom axis values
#' @param axis2 position of left size axis values
#' @param red What color to use for females in bubble plots (default is slightly
#' transparent red)
#' @param blue What color to use for males in bubble plots (default is slightly
#' transparent blue)
#' @param pwidth default width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param pheight default height width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param punits units for \code{pwidth} and \code{pheight}. Can be "px"
#' (pixels), "in" (inches), "cm" or "mm". Default="in".
#' @param ptsize point size for plotted text in plots printed to files (see
#' help("png") in R for details). Default=12.
#' @param res resolution of plots printed to files. Default=300
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param cex.main character expansion parameter for plot titles
#' @param linepos should lines be added before points (linepos=1) or after
#' (linepos=2)?
#' @param fitbar show fit to bars instead of points
#' @param do.sqrt scale bubbles based on sqrt of size vector. see ?bubble3 for
#' more info.
#' @param smooth add loess smoother to observed vs. expected index plots and
#' input vs. effective sample size?
#' @param cohortlines optional vector of birth years for cohorts for which to
#' add growth curves to numbers at length bubble plots
#' @param labels vector of labels for plots (titles and axis labels)
#' @param printmkt show market categories in plot titles?
#' @param printsex show sex in plot titles?
#' @param maxrows maximum (or fixed) number or rows of panels in the plot
#' @param maxcols maximum (or fixed) number or columns of panels in the plot
#' @param maxrows2 maximum number of rows for conditional age at length plots
#' @param maxcols2 maximum number of columns for conditional age at length
#' plots
#' @param rows number or rows to return to as default for next plots to come or
#' for single plots
#' @param cols number or cols to return to as default for next plots to come or
#' for single plots
#' @param andre_oma Outer margins passed to Andre's multi-panel conditional
#' age-at-length plots.
#' @param andrerows Number of rows of Andre's conditional age-at-length plots
#' within each page. Default=3.
#' @param fixdims fix the dimensions at maxrows by maxcols or resize based on
#' number of years of data
#' @param fixdims2 fix the dimensions at maxrows by maxcols in aggregate plots
#' or resize based on number of fleets
#' @param maxneff the maximum value to include on plots of input and effective
#' sample size. Occasionally a calculation of effective N blows up to very
#' large numbers, rendering it impossible to observe the relationship for other
#' data. Default=5000.
#' @param verbose return updates of function progress to the R GUI?
#' @param scalebins Rescale expected and observed proportions by dividing by
#' bin width for models where bins have different widths? Caution!: May not
#' work correctly in all cases.
#' @param addMeans Add parameter means in addition to medians for MCMC
#' posterior distributions in which the median and mean differ.
#' @param mainTitle Logical indicating if a title for the plot should be produced
#' @param \dots additional arguments that will be passed to the plotting.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{make_multifig}}
SSplotComps <-
  function(replist, subplots=c(1:21,24), #subplots=1:13,
           kind="LEN", sizemethod=1, aalyear=-1, aalbin=-1, plot=TRUE, print=FALSE,
           fleets="all", fleetnames="default", sexes="all",
           yupper=0.4,
           datonly=FALSE, samplesizeplots=TRUE, compresidplots=TRUE, bub=FALSE,
           showyears=TRUE, showsampsize=TRUE, showeffN=TRUE, aggregates_by_mkt=FALSE,
           sampsizeline=FALSE,effNline=FALSE,
           minnbubble=3, pntscalar=NULL,
           scalebubbles=FALSE,cexZ1=1.5,bublegend=TRUE,
           colvec=c(rgb(1,0,0,.7),rgb(0,0,1,.7),rgb(.1,.1,.1,.7)),
           linescol=c(rgb(0,.5,0,.7),rgb(.8,0,0,.7),rgb(0,0,.8,.7)),
           axis1=NULL,axis2=NULL,
           blue=rgb(0,0,1,0.7),red=rgb(1,0,0,0.7),
           pwidth=6.5, pheight=5.0, punits="in", ptsize=10, res=300,
           plotdir="default", cex.main=1, linepos=1, fitbar=FALSE,
           do.sqrt=TRUE, smooth=TRUE, cohortlines=c(),
           labels = c("Length (cm)",           #1
                      "Age (yr)",              #2
                      "Year",                  #3
                      "Observed sample size",  #4
                      "Effective sample size", #5
                      "Proportion",            #6
                      "cm",                    #7
                      "Frequency",             #8
                      "Weight",                #9
                      "Length",                #10
                      "(mt)",                  #11
                      "(numbers x1000)",       #12
                      "Stdev (Age) (yr)",      #13
                      "Conditional AAL plot, "), #14
           printmkt=TRUE,printsex=TRUE,
           maxrows=6,maxcols=6,maxrows2=2,maxcols2=4,rows=1,cols=1,
           andre_oma=c(3,0,3,0), andrerows=3,
           fixdims=TRUE,fixdims2=FALSE,maxneff=5000,verbose=TRUE,
           scalebins=FALSE,addMeans=TRUE,mainTitle=FALSE,...)
{
  ################################################################################
  # SSplotComps
  ################################################################################

  ###### current definitions of subplots
  ###
  ### { # loop over fleets
  ### subplot 1: multi-panel composition plot
  ### subplot 2: single panel bubble plot for numbers at length or age
  ### subplot 3: multi-panel bubble plots for conditional age-at-length
  ### subplot 4: multi-panel plot of fit to conditional age-at-length for specific years
  ### subplot 5: Pearson residuals for A-L key
  ### subplot 6: multi-panel plot of point and line fit to conditional
  ###            age-at-length for specific length bins
  ### subplot 7: sample size plot
  ### subplot 8: Andre's mean age and std. dev. in conditional AAL
  ### subplot 9: by fleet aggregating across years
  ### } # end loop over fleets
  ### subplot 10: by fleet aggregating across years within each season
  ### subplot 11: by fleet aggregating across seasons within a year
  ### subplot 12: bubble plot comparison of length or age residuals
  ###             across fleets within sex/partition


  ###### new definitions of subplots
  ###
  ### { # loop over fleets
  ### subplot 1: multi-panel composition plot
  ### subplot 2: single panel bubble plot for numbers at length or age
  ### subplot 3: multi-panel bubble plots for conditional age-at-length
  ### subplot 4: multi-panel plot of fit to conditional age-at-length for specific years
  ### subplot 5: Pearson residuals for A-L key
  ### subplot 6: multi-panel plot of point and line fit to conditional
  ###            age-at-length for specific length bins
  ### subplot 7: sample size plot
  ### NEW subplot 8: TA1.8 Francis weighting plot
  ### NEW subplot 9: TA1.8 Francis weighting plot for conditional data
  ### subplot 10: Andre's mean age and std. dev. in conditional AAL
  ### } # end loop over fleets
  ### subplot 21: by fleet aggregating across years
  ### subplot 22: by fleet aggregating across years within each season
  ### subplot 23: by fleet aggregating across seasons within a year
  ### subplot 24: bubble plot comparison of length or age residuals
  ###             across fleets within partition




  if(!exists("make_multifig")) stop("you are missing the function 'make_mulitifig'")
  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  SS_versionNumeric <- replist$SS_versionNumeric

  lendbase      <- replist$lendbase
  sizedbase     <- replist$sizedbase
  agedbase      <- replist$agedbase
  condbase      <- replist$condbase
  ghostagedbase <- replist$ghostagedbase
  ghostlendbase <- replist$ghostlendbase
  ladbase       <- replist$ladbase
  wadbase       <- replist$wadbase
  tagdbase1     <- replist$tagdbase1
  tagdbase2     <- replist$tagdbase2

  nfleets       <- replist$nfleets
  nseasons      <- replist$nseasons
  seasfracs     <- replist$seasfracs
  FleetNames    <- replist$FleetNames
  nsexes        <- replist$nsexes
  accuage       <- replist$accuage

  Age_tuning    <- replist$Age_comp_Eff_N_tuning_check

  # define a variety of titles and labels
  titles <- NULL
  titlemkt <- ""
  if(plotdir=="default"){
    plotdir <- replist$inputs$dir
  }

  # sort out which fleets will be included
  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  }else{
    if(length(intersect(fleets,1:nfleets))!=length(fleets)){
      stop("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
    }
  }
  if(fleetnames[1]=="default"){
    fleetnames <- FleetNames
  }

  # sort out which sexes will be included, and associated labels
  if(sexes[1]=="all"){
    sexes <- 0:nsexes # this can be used to subset stuff below
  }
  if(nsexes==1){
    sexes <- 0:nsexes
  }
  if(nsexes==1 | length(sexes)>1){
    # single-sex models, or models with all sexes shown
    # on the same plot, don't need sex-specific title
    titlesex <- ""
    filesex  <- ""
  }
  if(nsexes>1 & length(sexes)==1){
    # multi-sex models with only 1 sex shown need
    # title and filename info
    if(sexes==0){
      titlesex <- "sexes combined, "
      filesex  <- "sex0"
    }
    if(sexes==1){
      titlesex <- "female, "
      filesex  <- "sex1"
    }
    if(sexes==2){
      titlesex <- "male, "
      filesex  <- "sex2"
    }
  }
  titlesex <- ifelse(printsex,titlesex,"")

  # a few quantities related to data type and plot number
  if(kind=="LEN"){
    dbase_kind <- lendbase
    kindlab=labels[1]
    if(datonly){
      filenamestart <- "comp_lendat_"
      titledata <- "Length comp data, "
    }else{
      filenamestart <- "comp_lenfit_"
      titledata <- "Length comps, "
    }
  }
  if(kind=="GSTLEN"){
    dbase_kind <- ghostlendbase
    kindlab=labels[1]
    if(datonly){
      filenamestart <- "comp_gstlendat_"
      titledata <- "Ghost length comp data, "
    }else{
      filenamestart <- "comp_gstlenfit_"
      titledata <- "Ghost length comps, "
    }
  }
  if(kind=="SIZE"){
    dbase_kind <- sizedbase[sizedbase$method==sizemethod,]
    sizeunits <- unique(dbase_kind$units)
    if(length(sizeunits)>1)
      stop("!error with size units in generalized size comp plots:\n",
           "    more than one unit value per method.\n")
    if(sizeunits %in% c("in","cm"))
      kindlab <- paste(labels[10]," (",sizeunits,")",sep="")
    if(sizeunits %in% c("lb","kg"))
      kindlab <- paste(labels[9]," (",sizeunits,")",sep="")
    if(datonly){
      filenamestart <- "comp_sizedat_"
      titledata <- "Size comp data, "
    }else{
      filenamestart <- "comp_sizefit_"
      titledata <- "Size comps, "
    }
  }
  if(kind=="AGE"){
    dbase_kind <- agedbase
    kindlab=labels[2]
    if(datonly){
      filenamestart <- "comp_agedat_"
      titledata <- "Age comp data, "
    }else{
      filenamestart <- "comp_agefit_"
      titledata <- "Age comps, "
    }
  }
  if(kind=="cond"){
    dbase_kind <- condbase
    kindlab=labels[2]
    if(datonly){
      filenamestart <- "comp_condAALdat_"
      titledata <- "Conditional age-at-length data, "
    }else{
      filenamestart <- "comp_condAALfit_"
      titledata <- "Conditional age-at-length, "
    }
  }
  if(kind=="GSTAGE"){
    dbase_kind <- ghostagedbase
    kindlab=labels[2]
    if(datonly){
      filenamestart <- "comp_gstagedat_"
      titledata <- "Ghost age comp data, "
    }else{
      filenamestart <- "comp_gstagefit_"
      titledata <- "Ghost age comps, "
    }
  }
  if(kind=="GSTcond"){
    dbase_kind <- ghostagedbase
    kindlab=labels[2]
    if(datonly){
      filenamestart <- "comp_gstCAALdat_"
      titledata <- "Ghost conditional age-at-length data, "
    }else{
      filenamestart <- "comp_gstCAALfit_"
      titledata <- "Ghost conditional age-at-length comps, "
    }
  }
  if(kind=="L@A"){
    dbase_kind <- ladbase[ladbase$N!=0,] # remove values with 0 sample size
    kindlab=labels[2]
    filenamestart <- "comp_LAAfit_"
    titledata <- "Mean length at age, "
    dbase_kind$SD <- dbase_kind$Lbin_lo/dbase_kind$N
  }
  if(kind=="W@A"){
    dbase_kind <- wadbase[wadbase$N!=0,] # remove values with 0 sample size
    kindlab=labels[2]
    filenamestart <- "comp_WAAfit_"
    titledata <- "Mean weight at age, "
  }
  if(!(kind%in%c("LEN","SIZE","AGE","cond","GSTAGE","GSTLEN","L@A","W@A"))){
    stop("Input 'kind' to SSplotComps is not right.")
  }

  # partition group is used by some aggregate plots (subplot 21+)
  # either equal to partition or constant across all samples
  if(nrow(dbase_kind)>0){
    if(aggregates_by_mkt){
      dbase_kind$Part_group <- dbase_kind$Part
    }else{
      dbase_kind$Part_group <- -1 # code for all partitions combined
    }
  }


  # Add asterix to indicate super periods and then remove rows labeled "skip".
  # It would be better to somehow show the range of years, but that seems difficult.
  if(any(dbase_kind$SuprPer=="Sup" & dbase_kind$Used=="skip")){
    cat("Note: removing super-period composition values labeled 'skip'\n",
        "     and designating super-period values with a '*'\n")
    dbase_kind <- dbase_kind[dbase_kind$SuprPer=="No" | dbase_kind$Used!="skip",]
    dbase_kind$YrSeasName <- paste(dbase_kind$YrSeasName,ifelse(dbase_kind$SuprPer=="Sup","*",""),sep="")
  }
  ageerr_warning <- TRUE

  # subset data based on requested range of fleets and sexes
  dbase_kind <- dbase_kind[dbase_kind$Fleet %in% fleets & dbase_kind$sex %in% sexes,]

  # loop over fleets
  for(f in fleets){
    # check for the presence of data
    if(length(dbase_kind$Obs[dbase_kind$Fleet==f])>0){
      dbasef <- dbase_kind[dbase_kind$Fleet==f,]
      #get mean sample quantities to show in conditional age-at-length figures
      if(kind %in% c("cond","GSTcond") && f %in% Age_tuning$Fleet){
        #### these values not to be trusted in the presence of ghost data:
        ## HarmEffNage <- Age_tuning$"HarMean(effN)"[Age_tuning$Fleet==f]
        ## MeanNage    <- Age_tuning$"mean(inputN*Adj)"[Age_tuning$Fleet==f]
        HarmEffNage <- NULL
        MeanNage <- NULL
      }else{
        HarmEffNage <- NULL
        MeanNage <- NULL
      }
      # loop over sex combinations
      ## for(k in (1:3)[testor])
      ## {
      ##   if(k==1){dbase_k <- dbasef[dbasef$sex==1 & dbasef$Pick_sex==0,]}
      ##   if(k==2){dbase_k <- dbasef[dbasef$sex==1 & dbasef$Pick_sex %in% c(1,3),]}
      ##   if(k==3){dbase_k <- dbasef[dbasef$sex==2,]}
      ##   sex <- ifelse(k==3, 2, 1)
      ##   if(sex %in% sexes){
      ##     cat('sex',sex,'\n')

      dbase_k <- dbasef
      # loop over partitions (discard, retain, total)
      for(j in unique(dbase_k$Part)){
        dbase <- dbase_k[dbase_k$Part==j,]
        # dbase is the final data.frame used in the individual plots
        # it is subset based on the kind (age, len, age-at-len), fleet, sex, and partition

        ## # starting with SSv3.24a, the Yr.S column is already in the output, otherwise fill it in
        ## if(!"Yr.S" %in% names(dbase)){
        ##   # add fraction of season to distinguish between samples
        ##   dbase$Yr.S <- dbase$Yr + (0.5/nseasons)*dbase$Seas
        ## }
        # check for multiple ageing error types within a year to plot separately
        max_n_ageerr <- max(apply(table(dbase$Yr.S,dbase$Ageerr)>0,1,sum))

        if(max_n_ageerr > 1){
          if(ageerr_warning){
            cat("Note: multiple samples with different ageing error types within fleet/year.\n",
                "     Plots label '2005a3' indicates ageing error type 3 for 2005 sample.\n",
                "     Bubble plots may be misleading with overlapping bubbles.\n")
            ageerr_warning <- FALSE
          }
          # add 1/1000 of a year for each ageing error type to distinguish between types within a year
          dbase$Yr.S <- dbase$Yr.S + dbase$Ageerr/1000
          dbase$YrSeasName <- paste(dbase$YrSeasName,"a",dbase$Ageerr,sep="")
        }
        ## dbase$Yr.S[dbase_k$Pick_sex==1] <- dbase$Yr.S[dbase_k$Pick_sex==1] + 1e-6
        ## dbase$Yr.S[dbase_k$Pick_sex==2] <- dbase$Yr.S[dbase_k$Pick_sex==2] + 2e-6

        ## assemble pieces of plot title
        # market category
        if(j==0) titlemkt <- "whole catch, "
        if(j==1) titlemkt <- "discard, "
        if(j==2) titlemkt <- "retained, "
        titlemkt <- ifelse(printmkt,titlemkt,"")

        # plot bars for data only or if input 'fitbar=TRUE'
        if(datonly | fitbar) bars <- TRUE else bars <- FALSE

        # aggregating identifiers for plot titles and filenames
        title_sexmkt <- paste(titlesex,titlemkt,sep="")
        filename_fltsexmkt <- paste("flt",f,filesex,"mkt",j,sep="")

        ### subplot 1: multi-panel composition plot
        if(1 %in% subplots & kind!="cond"){ # for age or length comps, but not conditional AAL
          caption <- paste(titledata,title_sexmkt, fleetnames[f],sep="") # total title
          if(mainTitle) {
            ptitle <- caption
          } else {
            ptitle <- ""
          }
          titles <- c(ptitle,titles) # compiling list of all plot titles
          tempfun <- function(ipage,...){
            sexvec <- dbase$sex
            # a function to combine a bunch of repeated commands
            if(!(kind %in% c("GSTAGE","GSTLEN","L@A","W@A"))){
              # test for Dirichlet-Multinomial likelihood
              if("DM_effN" %in% names(dbase) && any(!is.na(dbase$DM_effN))){
                # Dirichlet-Multinomial likelihood
                make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,
                              linesx=dbase$Bin,linesy=dbase$Exp,
                              sampsize=dbase$N,
                              effN=dbase$DM_effN,
                              showsampsize=showsampsize,showeffN=showeffN,
                              sampsize_label="N input=",
                              effN_label="N adj.=",
                              bars=bars,linepos=(1-datonly)*linepos,
                              nlegends=3,
                              legtext=list(dbase$YrSeasName,"sampsize","effN"),
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                              maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                              fixdims=fixdims,ipage=ipage,scalebins=scalebins,
                              colvec=colvec, linescol=linescol, axis1=axis1, axis2=axis2,
                              sexvec=sexvec, yupper=yupper, ...)
              }else{
                # standard multinomial likelihood
                make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,
                              linesx=dbase$Bin,linesy=dbase$Exp,
                              sampsize=dbase$N,effN=dbase$effN,
                              showsampsize=showsampsize,showeffN=showeffN,
                              sampsize_label="N adj.=",
                              effN_label="N eff.=",
                              bars=bars,linepos=(1-datonly)*linepos,
                              nlegends=3,
                              legtext=list(dbase$YrSeasName,"sampsize","effN"),
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                              maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                              fixdims=fixdims,ipage=ipage,scalebins=scalebins,
                              colvec=colvec, linescol=linescol, axis1=axis1, axis2=axis2,
                              sexvec=sexvec, yupper=yupper, ...)
              }                
            }
            if(kind=="GSTAGE"){
              make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,linesx=dbase$Bin,linesy=dbase$Exp,
                            sampsize=dbase$N,effN=dbase$effN,
                            showsampsize=FALSE,showeffN=FALSE,
                            bars=bars,linepos=(1-datonly)*linepos,
                            nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
                            main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                            maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                            fixdims=fixdims,ipage=ipage,scalebins=scalebins,
                            colvec=colvec, linescol=linescol, axis1=axis1, axis2=axis2,
                            sexvec=sexvec, yupper=yupper, ...)
            }
            if(kind=="GSTLEN"){
              make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,linesx=dbase$Bin,linesy=dbase$Exp,
                            sampsize=dbase$N,effN=dbase$effN,showsampsize=FALSE,showeffN=FALSE,
                            bars=bars,linepos=(1-datonly)*linepos,
                            nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
                            main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                            maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                            fixdims=fixdims,ipage=ipage,scalebins=scalebins,
                            colvec=colvec, linescol=linescol, axis1=axis1, axis2=axis2,
                            sexvec=sexvec,...)
            }
            if(kind %in% c("L@A","W@A")){
              make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,linesx=dbase$Bin,linesy=dbase$Exp,
                            ptsSD=dbase$SD,
                            sampsize=dbase$N,effN=0,showsampsize=FALSE,showeffN=FALSE,
                            nlegends=1,legtext=list(dbase$YrSeasName),
                            bars=bars,linepos=(1-datonly)*linepos,
                            main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=ifelse(kind=="W@A",labels[9],labels[1]),
                            maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                            fixdims=fixdims,ipage=ipage,scalebins=scalebins,
                            colvec=colvec, linescol=linescol, axis1=axis1, axis2=axis2,
                            sexvec=sexvec,...)
            }
          } # end tempfun

          if(plot) tempfun(ipage=0,...)
          if(print){ # set up plotting to png file if required
            npages <- ceiling(length(unique(dbase$Yr.S))/maxrows/maxcols)
            for(ipage in 1:npages){
              pagetext <- ""
              caption_count <- ""
              if(npages>1){
                pagetext <- paste0("_page", ipage)
                caption_count <- paste0(" (plot ",ipage," of ",npages,")")
              }
              caption_extra <- ""
              if(ipage==1){
                if("DM_effN" %in% names(dbase) && any(!is.na(dbase$DM_effN))){
                  # get Theta value for this fleet
                  ipar <- replist$age_data_info$ParmSelect[f]
                  Theta <- as.numeric(replist$Dirichlet_Multinomial_pars$Theta[ipar])
                  # note: in caption below, &#920 = Theta
                  caption_extra <-
                    paste0(".<br><br>'N input' is the input sample size. ",
                           "'N adj.' is the sample size after adjustment by the ",
                           "Dirichlet-Multinomial <i>&#920</i> parameter based on the ",
                           "formula N adj. = 1 / (1+<i>&#920</i>) + N * <i>&#920</i> / (1+<i>&#920</i>). ",
                           "<br><br>For this fleet, <i>&#920</i> = ", round(Theta, 3),
                           " and the sample size multiplier is approximately ",
                           "<i>&#920</i> / (1+<i>&#920</i>) = ", round(Theta / (1+Theta), 3),
                           "<br><br>For more info, see<br>",
                           "<blockquote>",
                           "Thorson, J.T., Johnson, K.F., ",
                           "Methot, R.D. and Taylor, I.G. 2017. ",
                           "Model-based estimates of effective sample size ",
                           "in stock assessment models using the ",
                           "Dirichlet-multinomial distribution. ",
                           "<i>Fisheries Research</i>",
                           "192: 84-93. ",
                           "<a href=https://doi.org/10.1016/j.fishres.2016.06.005>",
                           "https://doi.org/10.1016/j.fishres.2016.06.005</a>",
                           "</blockquote>")
                }else{
                  caption_extra <-
                    paste0(".<br><br>'N adj.' is the input sample size ",
                           "after data-weighting adjustment. ",
                           "N eff. is the calculated effective sample size used ",
                           "in the McAllister-Iannelli tuning method.")
                }
              }
              file <- paste(filenamestart,
                            filename_fltsexmkt,pagetext,".png",sep="")
              plotinfo <- pngfun(file=file,
                                 caption=paste0(caption, caption_count, caption_extra))
              tempfun(ipage=ipage,...)
              dev.off()
            }
          }
        } # end subplot 1

        # some things related to the next two bubble plots (single or multi-panel)
        if(datonly){
          z <- dbase$Obs
          if(scalebubbles){
            z <- dbase$N*dbase$Obs # if requested, scale by sample sizes
          }
          col <- rep("black",2)
          titletype <- titledata
          filetype <- "bub"
          allopen <- TRUE
        }else{
          z <- dbase$Pearson
          col <- rep(colvec[3],2)
          titletype <- "Pearson residuals, "
          filetype <- "resids"
          allopen <- FALSE
        }

        ### subplot 2: single panel bubble plot for numbers at length or age
        if(2 %in% subplots & bub & kind!="cond"){
          # get growth curves if requested
          if(length(cohortlines)>0){
            growdat <- replist$endgrowth
            growdatF <- growdat[growdat$Sex==1 & growdat$Morph==min(growdat$Morph[growdat$Sex==1]),]
            if(nsexes > 1){
              growdatM <- growdat[growdat$Sex==2 & growdat$Morph==min(growdat$Morph[growdat$Sex==2]),]
            }
          }
          # assemble caption that may also be used for plot title
          caption <- paste(titletype, title_sexmkt, fleetnames[f],sep="")
          caption <- paste(caption," (max=",round(max(z),digits=2),")",sep="")
          if(mainTitle) {
            ptitle <- caption
          } else {
            ptitle <- ""
          }
          titles <- c(ptitle,titles) # compiling list of all plot titles

          tempfun2 <- function(){
            xvals <- dbase$Yr.S
            # calculate smallest difference among years
            # which is used to adjust offsets males and females
            xdiff <- 0.1*sort(unique(diff(sort(unique(dbase$Yr.S)),
                                          na.rm=TRUE)))[1]
            # not sure what cases would have missing xdiff
            # from above calculation, but it definitely happens
            # with only one year of data, so setting default
            # to work for that case
            if(is.na(xdiff)){
              xdiff <- 0.1
            }
            cols <- rep(colvec[3],nrow(dbase))
            if(nsexes > 1){
              xvals[dbase$sex>0] <- dbase$Yr.S[dbase$sex>0] -
                (dbase$sex[dbase$sex>0]-1.5)*xdiff
              if(length(unique(dbase$Yr.S))==1){
                # if only one year, don't bother showing points
                # as mid-year values
                # this may not be ideal for seasonal models
                xvals[dbase$sex>0] <- floor(dbase$Yr.S[dbase$sex>0]) -
                  (dbase$sex[dbase$sex>0]-1.5)*xdiff
              }
              cols[dbase$sex>0] <- colvec[dbase$sex[dbase$sex>0]]
            }
            bubble3(x=xvals, y=dbase$Bin, z=z, xlab=labels[3],
                    ylab=kindlab, col=cols, cexZ1=cexZ1,
                    legend=bublegend,
                    las=1, main=ptitle, cex.main=cex.main, maxsize=pntscalar,
                    allopen=allopen, minnbubble=minnbubble)
            # add lines for growth of individual cohorts if requested
            if(length(cohortlines)>0){
              for(icohort in 1:length(cohortlines)){
                cat("  Adding line for",cohortlines[icohort],"cohort\n")
                if(kind=="LEN"){
                  if(nsexes>1){
                    lines(growdatF$Age_Mid+cohortlines[icohort],
                          growdatF$Len_Mid, col=colvec[1]) #females
                    lines(growdatM$Age_Mid+cohortlines[icohort],
                          growdatM$Len_Mid, col=colvec[2]) #males
                  }else{
                    lines(growdatF$Age_Mid+cohortlines[icohort],
                          growdatF$Len_Mid, col=colvec[3]) #single-sex growth
                  }
                }
                if(kind %in% c("AGE","GSTAGE")){
                  lines(0.5 + c(cohortlines[icohort],cohortlines[icohort]+accuage),
                        c(0,accuage),col=colvec[3],lty=3) # one-one line for age
                }
              }
            }
          }
          if(plot) tempfun2()
          if(print){ # set up plotting to png file if required
            pagetext <- ""
            if(npages>1){
              pagetext <- paste("_page",ipage,sep="")
              caption <- paste(caption, " (plot ",ipage," of ",npages,")",sep="")
            }
            if(length(grep("Pearson",caption))>0){
              caption <- paste(caption,
                               "<br> \nClosed bubbles are positive residuals",
                               "(observed > expected)",
                               "and open bubbles are negative residuals",
                               "(observed < expected).")
            }
            file <- paste(filenamestart,filetype,
                          filename_fltsexmkt,pagetext,".png",sep="")
            plotinfo <- pngfun(file=file, caption=caption)
            tempfun2()
            dev.off() # close device if png
          }
        } # end bubble plot
        ### subplot 3: multi-panel bubble plots for conditional age-at-length

        if(3 %in% subplots & kind=="cond"){
          # assemble caption that may also be used for plot title
          caption <- paste(titletype, title_sexmkt, fleetnames[f],sep="")
          caption <- paste(caption," (max=",round(max(z),digits=2),")",sep="")
          if(mainTitle) {
            ptitle <- caption
          } else {
            ptitle <- ""
          }
          titles <- c(ptitle,titles) # compiling list of all plot titles
          # calculate scaling of lines showing effect and input sample size
          sampsizeline.old <- sampsizeline
          effNline.old <- effNline
          if(is.logical(sampsizeline) && sampsizeline){
            # scaling when displaying only adjusted input sample size
            sampsizeline <- max(dbase$Bin)/max(dbase$N,na.rm=TRUE)
            if(!datonly && is.logical(effNline) && effNline){
              # scaling when displaying both input and effective
              sampsizeline <- effNline  <- max(dbase$Bin)/max(dbase$N,dbase$effN,na.rm=TRUE)
              cat("  Fleet ",f," ",titlesex,"adj. input & effective N in red & green scaled by ",effNline,"\n",sep="")
            }else{
              cat("  Fleet ",f," ",titlesex,"adj. input N in red scaled by ",sampsizeline,"\n",sep="")
            }
          }
          # function to make plots
          tempfun3 <- function(ipage,...){
            sexvec <- dbase$sex
            col.index <- sexvec
            col.index[col.index==0] <- 3
            cols <- colvec[col.index]
            yrvec <- dbase$Yr.S + dbase$sex*1e-6
            make_multifig(ptsx=dbase$Bin,ptsy=dbase$Lbin_mid,yr=yrvec,size=z,
                          sampsize=dbase$N,showsampsize=showsampsize,effN=dbase$effN,
                          showeffN=FALSE,
                          cexZ1=cexZ1,
                          bublegend=bublegend,
                          nlegends=1,legtext=list(dbase$YrSeasName),
                          bars=FALSE,linepos=0,main=ptitle,cex.main=cex.main,
                          xlab=labels[2],ylab=labels[1],ymin0=FALSE,maxrows=maxrows2,maxcols=maxcols2,
                          fixdims=fixdims,allopen=allopen,minnbubble=minnbubble,
                          #ptscol=col[1],ptscol2=col[2],
                          ptscol=cols,
                          ipage=ipage,scalebins=scalebins,
                          sampsizeline=sampsizeline,effNline=effNline,
                          sampsizemean=MeanNage,effNmean=HarmEffNage,
                          colvec=colvec, linescol=linescol, axis1=axis1, axis2=axis2,
                          sexvec=sexvec,...)
          }
          if(plot) tempfun3(ipage=0,...)
          if(print){ # set up plotting to png file if required
            npages <- ceiling(length(unique(dbase$Yr.S))*
                                length(unique(dbase$sex))/maxrows2/maxcols2)
            for(ipage in 1:npages){
              pagetext <- ""
              if(npages>1){
                pagetext <- paste("_page",ipage,sep="")
                caption <- paste(caption, " (plot ",ipage," of ",npages,")",sep="")
              }
              file <- paste(filenamestart,filetype,
                            filename_fltsexmkt,pagetext,".png",sep="")
              plotinfo <- pngfun(file=file, caption=caption)
              tempfun3(ipage=ipage,...)
              dev.off() # close device if png
            }
          }
          sampsizeline <- sampsizeline.old
          effNline <- effNline.old
        } # end conditional bubble plot
        #
        ### subplots 4 and 5: multi-panel plot of point and line fit to
        ###                   conditional age-at-length
        ###                   and Pearson residuals of A-L key for specific years
        if((4 %in% subplots | 5 %in% subplots) & aalyear[1] > 0 & kind=="cond"){
          for(y in 1:length(aalyear)){
            aalyr <- aalyear[y]
            if(length(dbase$Obs[dbase$Yr==aalyr])>0){
              ydbase <- dbase[dbase$Yr==aalyr,]
              sexvec <- ydbase$sex
              if(4 %in% subplots){
                ### subplot 4: multi-panel plot of fit to conditional age-at-length for specific years
                caption <- paste(aalyr," age-at-length bin, ",title_sexmkt,fleetnames[f],sep="")
                if(mainTitle) {
                  ptitle <- caption
                } else {
                  ptitle <- ""
                }
                titles <- c(ptitle,titles) # compiling list of all plot titles
                lenbinlegend <- paste(ydbase$Lbin_lo,labels[7],sep="")
                lenbinlegend[ydbase$Lbin_range>0] <- paste(ydbase$Lbin_lo,"-",ydbase$Lbin_hi,labels[7],sep="")
                tempfun4 <- function(ipage,...){ # temporary function to aid repeating the big function call
                  make_multifig(ptsx=ydbase$Bin,ptsy=ydbase$Obs,yr=ydbase$Lbin_lo,
                                linesx=ydbase$Bin,linesy=ydbase$Exp,
                                sampsize=ydbase$N,effN=ydbase$effN,showsampsize=showsampsize,showeffN=showeffN,
                                nlegends=3,legtext=list(lenbinlegend,"sampsize","effN"),
                                bars=FALSE,linepos=linepos,main=ptitle,cex.main=cex.main,
                                xlab=labels[2],ylab=labels[6],maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                                fixdims=fixdims,ipage=ipage,scalebins=scalebins,
                                sexvec=sexvec, yupper=yupper, ...)
                }
                if(plot) tempfun4(ipage=0,...)
                if(print){
                  npages <- ceiling(length(unique(ydbase$Yr.S))/maxrows/maxcols)
                  for(ipage in 1:npages){
                    pagetext <- ""
                    if(npages>1){
                      pagetext <- paste("_page",ipage,sep="")
                      caption <- paste(caption, " (plot ",ipage," of ",npages,")",sep="")
                    }
                    if(length(grep("Pearson",caption))>0){
                      caption <- paste(caption,
                                       "<br> \nClosed bubbles are positive residuals",
                                       "(observed > expected)",
                                       "and open bubbles are negative residuals",
                                       "(observed < expected).")
                    }
                    file <- paste0(filenamestart,filename_fltsexmkt,
                                   "_", aalyr, pagetext, ".png")
                    plotinfo <- pngfun(file=file, caption=caption)
                    tempfun4(ipage=ipage,...)
                    dev.off() # close device if print
                  }
                }
              } # end if 4 in subplots
              if(5 %in% subplots){
                ### subplot 5: Pearson residuals for A-L key
                z <- ydbase$Pearson
                col.index <- sexvec
                col.index[col.index==0] <- 3
                cols <- colvec[col.index]
                x.vec <- ydbase$Bin + ydbase$sex*1e-6
                caption <- paste(aalyr," Pearson residuals for A-L key, ",title_sexmkt,fleetnames[f],sep="")
                caption <- paste(caption," (max=",round(abs(max(z)),digits=2),")",sep="")
                if(mainTitle) {
                  ptitle <- caption
                } else {
                  ptitle <- ""
                }
                titles <- c(ptitle,titles) # compiling list of all plot titles
                tempfun5 <- function(){
                  bubble3(x=x.vec,y=ydbase$Lbin_lo,z=z,xlab=labels[2],
                          ylab=labels[1],col=cols,las=1,main=ptitle,
                          cex.main=cex.main,maxsize=pntscalar,
                          cexZ1=cexZ1,
                          legend=bublegend,
                          allopen=FALSE,minnbubble=minnbubble)
                }
                if(plot) tempfun5()
                if(print){
                  pagetext <- ""
                  if(npages>1){
                    pagetext <- paste("_page",ipage,sep="")
                    caption <- paste(caption, " (plot ",ipage," of ",npages,")",sep="")
                  }
                  if(length(grep("Pearson",caption))>0){
                    caption <- paste(caption,
                                     "<br> \nClosed bubbles are positive residuals",
                                     "(observed > expected)",
                                     "and open bubbles are negative residuals",
                                     "(observed < expected).")
                  }
                  file <- paste0(filenamestart,"yearresids_",
                                 filename_fltsexmkt,"_",aalyr,pagetext,".png")
                  plotinfo <- pngfun(file=file, caption=caption)
                  tempfun5()
                  dev.off() # close device if print
                }
              } # end if 5 in subplots
            }
          }
        }

        ### subplot 6: multi-panel plot of point and line fit to conditional
        ###            age-at-length for specific length bins
        if(6 %in% subplots & aalbin[1] > 0){
          badbins <- setdiff(aalbin, dbase$Lbin_hi)
          goodbins <- intersect(aalbin, dbase$Lbin_hi)
          if(length(goodbins)>0){
            if(length(badbins)>0){
              cat("Error! the following inputs for 'aalbin' do not match the Lbin_hi values for the conditional age-at-length data:",badbins,"\n",
                  "       the following inputs for 'aalbin' are fine:",goodbins,"\n")
            }
            for(ibin in 1:length(goodbins)){ # loop over good bins
              ilenbin <- goodbins[ibin]
              abindbase <- dbase[dbase$Lbin_hi==ilenbin,]
              if(nrow(abindbase)>0){ # check for data associated with this bin
                sexvec <- abindbase$sex
                caption <- paste0("Age-at-length ",ilenbin,labels[7],", ",title_sexmkt,fleetnames[f])
                if(mainTitle) {
                  ptitle <- caption
                } else {
                  ptitle <- ""
                }
                titles <- c(ptitle,titles) # compiling list of all plot titles
                tempfun6 <- function(ipage,...){ # temporary function to aid repeating the big function call
                  make_multifig(ptsx=abindbase$Bin,ptsy=abindbase$Obs,yr=abindbase$Yr.S,linesx=abindbase$Bin,linesy=abindbase$Exp,
                                sampsize=abindbase$N,effN=abindbase$effN,showsampsize=showsampsize,showeffN=showeffN,
                                nlegends=3,legtext=list(abindbase$YrSeasName,"sampsize","effN"),
                                bars=bars,linepos=(1-datonly)*linepos,
                                main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                                maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                                fixdims=fixdims,ipage=ipage,scalebins=scalebins,
                                sexvec=sexvec, ...)
                }
                if(plot) tempfun6(ipage=0,...)
                if(print){
                  npages <- ceiling(length(unique(abindbase$Yr.S))/maxrows/maxcols)
                  for(ipage in 1:npages){
                    pagetext <- ""
                    if(npages>1){
                      pagetext <- paste0("_page",ipage)
                      caption <- paste0(caption, " (plot ",ipage," of ",npages,")")
                    }
                    file <- paste0(filenamestart,filename_fltsexmkt,
                                  "_length",ilenbin,labels[7],pagetext,".png")
                    plotinfo <- pngfun(file=file, caption=caption)
                    tempfun6(ipage=ipage,...)
                    dev.off() # close device if print
                  }
                } # end print
              } # end if data
            } # end loop over length bins
          } # end if length(goodbins)>0
        } # end if plot requested

        ### subplot 7: sample size plot
        if(7 %in% subplots & samplesizeplots & !datonly &
           !("DM_effN" %in% names(dbase) && any(!is.na(dbase$DM_effN))) &
           !(kind %in% c("GSTAGE","GSTLEN","L@A","W@A"))){
          caption <- paste0("N-EffN comparison, ",titledata,title_sexmkt,fleetnames[f])
          if(mainTitle) {
            ptitle <- caption
          } else {
            ptitle <- ""
          }
          titles <- c(ptitle,titles) # compiling list of all plot titles
          lfitfunc <- function(){
            if(kind=="cond"){
              # trap nonrobust effective n's
              # should this only be for conditional age-at-length or all plots?
              dbasegood <- dbase[dbase$Obs>=0.0001 & dbase$Exp<0.99 &
                                 !is.na(dbase$effN) & dbase$effN<maxneff,]
            }else{
              dbasegood <- dbase
            }
            if(nrow(dbasegood)>0){
              # thinning out columns and removing rows with redundant information
              # (for the purposes of this function)
              dbasegood2 <- dbasegood[,c("YrSeasName","N","effN")]
              dbasegood2 <- unique(dbasegood2)
              plot(dbasegood2$N,dbasegood2$effN,xlab=labels[4],main=ptitle,
                   cex.main=cex.main,
                   ylim=c(0,1.15*max(dbasegood2$effN)),xlim=c(0,1.15*max(dbasegood2$N)),
                   col=colvec[3],pch=19,ylab=labels[5],xaxs="i",yaxs="i")
              # add labels for the years if requested
              if(showyears){
                par(xpd=TRUE) # allows the label to go over plot boundary
                text(x=dbasegood2$N,y=dbasegood2$effN,
                     dbasegood2$YrSeasName,adj=c(-0.2,0.5))
                par(xpd=FALSE) # restores default clipping
              }
              abline(0,1,col="black",lty=1)
              # add loess smoother if there's at least 6 points with a range greater than 2
              if(smooth & length(unique(dbasegood2$N)) > 6 & diff(range(dbasegood2$N))>2){
                old_warn <- options()$warn      # previous warnings setting
                options(warn=-1)                # turn off loess warnings
                psmooth <- loess(dbasegood2$effN~dbasegood2$N,degree=1)
                options(warn=old_warn)  #returning to old value
                lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")
              }
              if(addMeans){
                # vertical line with label for mean input sample size
                abline(v=mean(dbasegood2$N),lty="22",col='green3')
                text(x=mean(dbasegood2$N),y=0,
                     col='green3',"arithmetic mean",
                     srt=90, adj=c(-0.1,-0.3))
                # horizontal line with label for harmonic effective sample size
                abline(h=1/mean(1/dbasegood2$effN),lty="22",col='green3')
                text(x=0, y=1/mean(1/dbasegood2$effN),
                     col='green3',"harmonic mean",
                     adj=c(-0.1,-0.3))
              }
            }
          }
          if(plot) lfitfunc()
          if(print){ # set up plotting to png file if required
            file <- paste(filenamestart,"sampsize_",
                          filename_fltsexmkt,".png",sep="")
            plotinfo <- pngfun(file=file, caption=caption)
            lfitfunc()
            dev.off()
          }
        } # end subplot 7

        ### subplot 8: Chris Francis TA1.8 method for non-conditional data
        if(8 %in% subplots & kind %in% c("LEN","SIZE","AGE")){
          # convert "AGE" to "age" so that SSMethod.TA1.8 can find "agedbase", etc.
          kind2 <- tolower(kind)
          if(plot){
            tmp <- SSMethod.TA1.8(fit=replist, type=kind2,
                                  fleet=f, fleetnames=fleetnames, datonly=datonly)
          }
          if(print){ # set up plotting to png file if required
            file <- paste0(filenamestart,
                           "data_weighting_TA1.8_",fleetnames[f],".png")
            # not using pngfun because caption isn't available until after
            # plot is created
            # old command: plotinfo <- pngfun(file=file, caption=caption)
            png(filename=file.path(plotdir, file),width=pwidth,height=pheight,
                units=punits,res=res,pointsize=ptsize)
            # run function
            tmp <- SSMethod.TA1.8(fit=replist, type=kind2,
                                  fleet=f, fleetnames=fleetnames, datonly=datonly)
            # create caption
            caption <- paste0("Mean ", gsub("len","length",tolower(kind)),
                              " for ", fleetnames[f],
                              " with 95% confidence intervals",
                              " based on current samples sizes.")
            # add warning at top of caption if Dirichlet-Multinomial is used
            # regarldess of whether it is applied to this fleet/data combination
            if(!is.null(replist$Dirichlet_Multinomial_pars)){
              caption <-
                paste("WARNING: this figure is based on multinomial likelihood",
                      "and has not been updated to account for Dirichlet-Multinomial",
                      "likelihood and the sample size adjustment associated with",
                      "the estimated log(<i>&#920</i>) parameters.<br><br>", caption)
            }
            if(!datonly) {
              caption <- paste0(caption,
                                "<br>Francis data weighting method TA1.8:")
              if(!is.null(tmp[1])){
                vals <- paste0("thinner intervals (with capped ends) show ",
                               "result of further adjusting sample sizes ",
                               "based on suggested multiplier ",
                               "(with 95% interval) for ", kind2, " data from ",
                               fleetnames[f],":<br>",
                               round(tmp[1],4), " (",
                               round(tmp[2],4),"-",round(tmp[3],4),")")
              }else{
                vals <- "too few points to calculate adjustments."
              }
              caption <- paste(caption, vals, "<br><br>For more info, see<br>",
                               "<blockquote>Francis, R.I.C.C. (2011).",
                               "Data weighting in statistical fisheries stock assessment",
                               "models. <i>Can. J. Fish. Aquat. Sci.</i>",
                               "68: 1124-1138. ",
                               "<a href=https://doi.org/10.1139/f2011-025>",
                               "https://doi.org/10.1139/f2011-025</a>",
                               "</blockquote>")
            } # end test for datonly

            # add caption to the plotinfo table (normally done by pngfun)
            plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))

            dev.off() # close device if png
          } # end test for print to PNG option
        } # end subplot 8
        ### subplot 9: Chris Francis TA1.8 method for conditional data
        if(9 %in% subplots & kind=="cond" & (f %in% condbase$Fleet)){
          if(plot){
            SSMethod.Cond.TA1.8(fit=replist,
                                fleet=f, fleetnames=fleetnames, datonly=datonly)
          }
          if(print){ # set up plotting to png file if required
            file <- paste(filenamestart,
                          "data_weighting_TA1.8_condAge",fleetnames[f],".png",sep="")
            # not using pngfun because caption isn't available until after
            # plot is created
            # old command: plotinfo <- pngfun(file=file, caption=caption)
            png(filename=file.path(plotdir, file), width=pwidth, height=pheight,
                units=punits, res=res, pointsize=ptsize)
            # run function
            tmp <- SSMethod.Cond.TA1.8(fit=replist,
                                       fleet=f, fleetnames=fleetnames, datonly=datonly)
            # create caption
            caption <- paste0("Mean age from conditional data",
                              " (aggregated across length bins) for ",
                              fleetnames[f],
                              " with 95% confidence intervals ",
                              " based on current samples sizes.")
            if(!datonly){
              caption <- paste0(caption,
                                "<br>Francis data weighting method TA1.8:")
              if(!is.null(tmp[1])){
                vals <- paste0("thinner intervals (with capped ends) show ",
                               "result of further adjusting sample sizes ",
                               "based on suggested multiplier ",
                              "(with 95% interval) for ",
                              "conditional age-at-length data from ",
                              fleetnames[f],":<br>",
                              round(tmp[1],4), " (",
                              round(tmp[2],4),"-",round(tmp[3],4),")",
                              sep="")
              }else{
                vals <- "too few points to calculate adjustments."
              }
              caption <- paste(caption, vals, "<br><br>For more info, see<br>",
                               "<blockquote>Francis, R.I.C.C. (2011).",
                               "Data weighting in statistical fisheries stock assessment",
                               "models. <i>Can. J. Fish. Aquat. Sci.</i>",
                               "68: 1124-1138.</blockquote>")
            } # end test for datonly
            # add caption to the plotinfo table (normally done by pngfun)
            plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
            dev.off() # close device if png
          } # end test for print to PNG option
        }
        ### subplot 10: Andre's mean age and std. dev. in conditional AAL
        if(10 %in% subplots & kind=="cond" & length(unique(dbase$Bin)) > 1){
          caption1 <- paste(labels[14], title_sexmkt, fleetnames[f],sep="")
          if(mainTitle) {
            ptitle <- caption1
          } else {
            ptitle <- ""
          }
          andrefun <- function(ipage=0){
            Lens <-sort(unique(dbase$Lbin_lo))
            Yrs <- sort(unique(dbase$Yr.S))

            ymax <- 1.1*max(dbase$Bin,na.rm=TRUE)
            xmax <- max(condbase$Lbin_hi,na.rm=TRUE)
            xmin <- min(condbase$Lbin_lo,na.rm=TRUE)

            # do some stuff so that figures that span multiple pages can be output as separate PNG files
            npanels <- length(Yrs)
            npages <- npanels/andrerows
            panelrange <- 1:npanels
            if(npages > 1 & ipage!=0) panelrange <- intersect(panelrange, 1:andrerows + andrerows*(ipage-1))
            Yrs2 <- Yrs[panelrange]

            par(mfrow=c(andrerows,2),mar=c(2,4,1,1),oma=andre_oma)
            for (Yr in Yrs2){
              y <- dbase[dbase$Yr.S==Yr,]
              Size <- NULL; Size2 <- NULL
              Obs <- NULL; Obs2 <- NULL
              Pred <- NULL;  Pred2 <- NULL
              Upp <- NULL; Low <- NULL; Upp2 <- NULL; Low2 <- NULL
              for (Ilen in Lens){
                z <- y[y$Lbin_lo == Ilen,]
                if (length(z[,1]) > 0){
                  weightsPred <- z$Exp/sum(z$Exp)
                  weightsObs <- z$Obs/sum(z$Obs)
                  ObsV <- sum(z$Bin*weightsObs)
                  ObsV2 <- sum(z$Bin*z$Bin*weightsObs)
                  PredV <- sum(z$Bin*weightsPred)
                  PredV2 <- sum(z$Bin*z$Bin*weightsPred)
                  # Overdispersion on N
                  # NN <- z$N[1]*0.01 # Andre did this for reasons unknown
                  NN <- z$N[1]
                  if (max(z$Obs) > 1.0e-4 & NN>0){
                    Size <- c(Size,Ilen)
                    Obs <- c(Obs,ObsV)
                    Pred <- c(Pred,PredV)
                    varn <-sqrt(PredV2-PredV*PredV)/sqrt(NN)
                    Pred2 <- c(Pred2,varn)
                    varn <-sqrt(max(0,ObsV2-ObsV*ObsV))/sqrt(NN)
                    Obs2 <- c(Obs2,varn)
                    Low <- c(Low,ObsV-1.64*varn)
                    Upp <- c(Upp,ObsV+1.64*varn)
                    if (NN > 1){
                      Size2 <- c(Size2,Ilen)
                      Low2 <- c(Low2,varn*sqrt((NN-1)/qchisq(0.95,NN)))
                      Upp2 <- c(Upp2,varn*sqrt((NN-1)/qchisq(0.05,NN)))
                    }
                  }
                }
              }
              if (length(Obs) > 0){
                ## next line was replaced with setting at the top,
                ## for consistency across years
                #ymax <- max(Pred,Obs,Upp)*1.1
                plot(Size,Obs,type='n',xlab="",ylab="Age",xlim=c(xmin,xmax),ylim=c(0,ymax),yaxs="i")
                label <- ifelse(nseasons==1, floor(Yr), Yr)
                text(x=par("usr")[1],y=.9*ymax,labels=label,adj=c(-.5,0),font=2,cex=1.2)
                if(length(Low)>1) polygon(c(Size,rev(Size)),c(Low,rev(Upp)),col='grey95',border=NA)
                if(!datonly) lines(Size,Pred,col=4,lwd=3)
                points(Size,Obs,pch=16)
                lines(Size,Low,lty=3)
                lines(Size,Upp,lty=3)
                if(par("mfg")[1]==1){
                  title(main=ptitle,xlab=labels[1],outer=TRUE,line=1)
                }
                box()

                ymax2 <- max(Obs2,Pred2)*1.1
                plot(Size,Obs2,type='n',xlab=labels[1],ylab=labels[13],xlim=c(xmin,xmax),ylim=c(0,ymax2),yaxs="i")
                if(length(Low2)>1) polygon(c(Size2,rev(Size2)),c(Low2,rev(Upp2)),col='grey95',border=NA)
                if(!datonly) lines(Size,Pred2,col=4,lwd=3)
                points(Size,Obs2,pch=16)
                lines(Size2,Low2,lty=3)
                lines(Size2,Upp2,lty=3)
                if(!datonly & par("mfg")[1]==1){
                  legend('topleft',legend=c("Observed (with 90% interval)","Expected"),
                         bty='n',col=c(1,4),pch=c(16,NA),lty=c(NA,1),lwd=3)
                }
                box()

              } # end if data exist
            } # end loop over years
          } # end andrefun
          if(plot) andrefun()
          if(print){ # set up plotting to png file if required
            npages <- ceiling(length(unique(dbase$Yr.S))/andrerows)
            for(ipage in 1:npages){
              pagetext <- ""
              caption <- caption1
              if(npages>1){
                pagetext <- paste("_page",ipage,sep="")
                caption <- paste(caption, " (plot ",ipage," of ",npages,")",sep="")
              }
              if(ipage==1){
                # add more information only to first page of plots
                caption <- paste(caption,
                "\nThese plots show mean age and std. dev. in conditional A@L.<br>",
                "Left plots are mean A@L by size-class (obs. and pred.) ",
                "with 90% CIs based on adding 1.64 SE of mean to the data.<br>",
                "Right plots in each pair are SE of mean A@L (obs. and pred.) ",
                "with 90% CIs based on the chi-square distribution.",sep="")

              }
              file <- paste(filenamestart,"Andre_plots",
                            filename_fltsexmkt,pagetext,".png",sep="")
              plotinfo <- pngfun(file=file, caption=caption)
              andrefun(ipage=ipage)
              dev.off() # close device if png
            } # end loop over pages
          } # end test for print to PNG option
        } # end subplot 10
      } # end loop over partitions (index j)
      #        } # end test for whether sex in vector of requested sexes
      #      } # end loop over combined/not-combined sex
    } # end if data
  } # end loop over fleets

  ### subplot 21: by fleet aggregating across years
  if(21 %in% subplots & kind!="cond") # for age or length comps, but not conditional AAL
  {
    # check for the presence of data
    if(nrow(dbase_kind)>0)
    {
      # no longer subsetting by sex, so mapping directly over
      dbase_k <- dbase_kind
      # loop over partitions (discard, retain, total)
      for(j in unique(dbase_k$Part_group)){
        # dbase is the final data.frame used in the individual plots
        # it is subset based on the kind (age, len, age-at-len), fleet, sex, and partition
        dbase <- dbase_k[dbase_k$Part_group==j,]
        if(nrow(dbase)>0){
          # market category
          if(j ==-1) titlemkt <- ""
          if(j == 0) titlemkt <- "whole catch, "
          if(j == 1) titlemkt <- "discard, "
          if(j == 2) titlemkt <- "retained, "
          titlemkt <- ifelse(printmkt, titlemkt, "")

          # plot bars for data only or if input 'fitbar=TRUE'
          if(datonly | fitbar){
            bars <- TRUE
          }else{
            bars <- FALSE
          }

          ## aggregating identifiers for plot titles and filenames
          ## note: titlesex is set at the top of this function
          title_sexmkt <- paste(titlesex,titlemkt,sep="")
          filename_fltsexmkt <- paste(filesex)
          if(j > -1){ # add market category to filename if it's not a mix
            filename_fltsexmkt <- paste0(filename_fltsexmkt, "mkt",j)
          }
          caption <- paste(titledata,title_sexmkt, "aggregated across time by fleet",sep="") # total title

          if(mainTitle) {
            ptitle <- caption
          } else {
            ptitle <- ""
          }
          titles <- c(ptitle,titles) # compiling list of all plot titles

          Bins <- sort(unique(dbase$Bin))
          nbins <- length(Bins)
          df <- data.frame(N=dbase$N,
                           effN=dbase$effN,
                           obs=dbase$Obs*dbase$N,
                           exp=dbase$Exp*dbase$N)
          if("DM_effN" %in% names(dbase) && any(!is.na(dbase$DM_effN))){
            df$DM_effN <- dbase$DM_effN
          }
          agg <- aggregate(x=df,
                           by=list(bin=dbase$Bin, f=dbase$Fleet,
                               sex=dbase$sex, mkt=dbase$Part),
                           FUN=sum)
          agg <- agg[agg$f %in% fleets,]
          agg$obs <- agg$obs/agg$N
          agg$exp <- agg$exp/agg$N

          # note: sample sizes will be different for each bin if tail compression is used
          #       printed sample sizes in plot will be maximum, which may or may not
          #       represent sum of sample sizes over all years/ages
          for(f in unique(agg$f)){
            infleet <- agg$f==f
            agg$N[infleet] <- max(agg$N[infleet])
            if("DM_effN" %in% names(agg) && any(!is.na(agg$DM_effN))){
              agg$DM_effN[infleet] <- max(agg$DM_effN[infleet], na.rm=TRUE)
            }else{
              agg$effN[infleet] <- max(agg$effN[infleet], na.rm=TRUE)
            }
          }

          namesvec <- fleetnames[agg$f]

          # check for multiple market categories in a fleet to plot separately
          max_n_mkt <- max(apply(table(agg$f, agg$mkt)>0, 1, sum))
          if(max_n_mkt > 0){
            mktnames <- c("","(discards)","(retained)")
            namesvec <- paste(fleetnames[agg$f], mktnames[agg$mkt+1])
          }
          if(!(kind %in% c("GSTAGE","GSTLEN","L@A","W@A"))){
            # group remaining calculations as a function
            tempfun7 <- function(ipage,...){
              # test for Dirichlet-Multinomial likelihood
              if("DM_effN" %in% names(agg) && any(!is.na(agg$DM_effN))){
                # Dirichlet-Multinomial likelihood
                make_multifig(ptsx=agg$bin,ptsy=agg$obs,yr=paste(agg$f, agg$mkt),
                              linesx=agg$bin,linesy=agg$exp,
                              sampsize=agg$N,
                              effN=agg$DM_effN,
                              showsampsize=showsampsize,showeffN=showeffN,
                              sampsize_label="Sum of N input=",
                              effN_label="Sum of N adj.=",
                              bars=bars,linepos=(1-datonly)*linepos,
                              nlegends=3,
                              legtext=list(namesvec,"sampsize","effN"),
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                              maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                              fixdims=fixdims2,ipage=ipage,lwd=2,scalebins=scalebins,
                              sexvec=agg$sex, yupper=yupper, ...)
              }else{
                # standard multinomial likelihood
                make_multifig(ptsx=agg$bin,ptsy=agg$obs,yr=paste(agg$f, agg$mkt),
                              linesx=agg$bin,linesy=agg$exp,
                              sampsize=agg$N,effN=agg$effN,
                              showsampsize=showsampsize,showeffN=showeffN,
                              sampsize_label="Sum of N adj.=",
                              effN_label="Sum of N eff.=",
                              bars=bars,linepos=(1-datonly)*linepos,
                              nlegends=3,
                              legtext=list(namesvec,"sampsize","effN"),
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                              maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                              fixdims=fixdims2,ipage=ipage,lwd=2,scalebins=scalebins,
                              sexvec=agg$sex, yupper=yupper, ...)
              }
            }
            if(plot) tempfun7(ipage=0,...)
            if(print){ # set up plotting to png file if required
              npages <- ceiling(length(unique(agg$f))/maxrows/maxcols)
              for(ipage in 1:npages){
                if(max_n_mkt > 0){
                  caption <-
                    paste0(caption, ".\n <br> ",
                           "Labels 'retained' and 'discard' indicate",
                           " discarded or retained sampled for each fleet.",
                           " Panels without this designation represent",
                           " the whole catch.\n")
                }
                pagetext <- ""
                if(npages>1){
                  pagetext <- paste("_page",ipage,sep="")
                  caption <- paste(caption, "<br> (plot ",ipage," of ",npages,")",sep="")
                }
                file <- paste(filenamestart,filename_fltsexmkt,
                              pagetext,"_aggregated_across_time.png",sep="")
                plotinfo <- pngfun(file=file, caption=caption)
                tempfun7(ipage=ipage,...)
                dev.off()
              }
            } # end print function
          }else{
            # haven't configured this aggregated plot for other types
            ## if(kind=="GSTAGE"){
            ##   make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,linesx=dbase$Bin,linesy=dbase$Exp,
            ##                 sampsize=dbase$N,effN=dbase$effN,showsampsize=FALSE,showeffN=FALSE,
            ##                 bars=bars,linepos=(1-datonly)*linepos,
            ##                 nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
            ##                 main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
            ##                 maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
            ##                 fixdims=fixdims,ipage=ipage,...)
            ## }
            ## if(kind %in% c("L@A","W@A")){
            ##   make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,linesx=dbase$Bin,linesy=dbase$Exp,
            ##                 sampsize=dbase$N,effN=0,showsampsize=FALSE,showeffN=FALSE,
            ##                 nlegends=1,legtext=list(dbase$YrSeasName),
            ##                 bars=bars,linepos=(1-datonly)*linepos,
            ##                 main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=ifelse(kind=="W@A",labels[9],labels[1]),
            ##                 maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
            ##                 fixdims=fixdims,ipage=ipage,...)
            ## }
          }
        } # end test for presence of observations in this partition group
      } # end loop over partitions group
      #      } # end loop over combined/not-combined sex
    } # end if data
  } # end subplot 21

  ### subplot 22: by fleet aggregating across years within each season
  if(22 %in% subplots & kind!="cond" & nseasons>1) # for age or length comps, but not conditional AAL
  {
    dbasef <- dbase_kind[dbase_kind$Fleet %in% fleets,]
    if("DM_effN" %in% names(dbasef) && any(!is.na(dbasef$DM_effN))){
      warning("Sample sizes in plots by fleet aggregating across years within each season have not yet been updated to reflect Dirichlet-Multinomial likelihood")
    }
    # check for the presence of data
    if(nrow(dbasef)>0)
    {
      testor    <- length(dbasef$sex[dbasef$sex==1 & dbasef$Pick_sex==0 ])>0
      testor[2] <- length(dbasef$sex[dbasef$sex==1 & dbasef$Pick_sex %in% c(1,3)])>0
      testor[3] <- length(dbasef$sex[dbasef$sex==2])>0

      # loop over sex combinations
      for(k in (1:3)[testor])
      {
        if(k==1){dbase_k <- dbasef[dbasef$sex==1 & dbasef$Pick_sex==0,]}
        if(k==2){dbase_k <- dbasef[dbasef$sex==1 & dbasef$Pick_sex %in% c(1,3),]}
        if(k==3){dbase_k <- dbasef[dbasef$sex==2,]}
        sex <- ifelse(k==3, 2, 1)

        # loop over partitions (discard, retain, total)
        for(j in unique(dbase_k$Part))
        {
          # dbase is the final data.frame used in the individual plots
          # it is subset based on the kind (age, len, age-at-len), fleet, sex, and partition
          dbase <- dbase_k[dbase_k$Part==j,]
          if(nrow(dbase)>0){
            ## assemble pieces of plot title
            # sex
            if(k==1) titlesex <- "sexes combined, "
            if(k==2) titlesex <- "female, "
            if(k==3) titlesex <- "male, "
            titlesex <- ifelse(printsex,titlesex,"")

            # market category
            if(j==0) titlemkt <- "whole catch, "
            if(j==1) titlemkt <- "discard, "
            if(j==2) titlemkt <- "retained, "
            titlemkt <- ifelse(printmkt,titlemkt,"")

            # plot bars for data only or if input 'fitbar=TRUE'
            if(datonly | fitbar) bars <- TRUE else bars <- FALSE

            # aggregating identifiers for plot titles and filenames
            title_sexmkt <- paste(titlesex,titlemkt,sep="")
            filename_fltsexmkt <- paste("sex",k,"mkt",j,sep="")

            caption <- paste0(titledata, title_sexmkt,
                              "\naggregated within season by fleet") # total title
            if(mainTitle) {
              ptitle <- caption
            } else {
              ptitle <- ""
            }
            titles <- c(ptitle,titles) # compiling list of all plot titles

            Bins <- sort(unique(dbase$Bin))
            nbins <- length(Bins)
            df <- data.frame(N=dbase$N,
                             effN=dbase$effN,
                             obs=dbase$Obs*dbase$N,
                             exp=dbase$Exp*dbase$N)
            agg <- aggregate(x=df, by=list(bin=dbase$Bin,f=dbase$Fleet,s=dbase$Seas), FUN=sum)
            agg <- agg[agg$f %in% fleets,]
            if(any(agg$s<=0)){
              cat("super-periods may not work correctly in plots of aggregated comps\n")
              agg <- agg[agg$s > 0,]
            }
            agg$obs <- agg$obs/agg$N
            agg$exp <- agg$exp/agg$N
            # note: sample sizes will be different for each bin if tail compression is used
            #       printed sample sizes in plot will be maximum, which may or may not
            #       represent sum of sample sizes over all years/ages

            for(f in unique(agg$f)){ # loop over fleets
              for(s in unique(agg$s[agg$f==f])){ # loop over seasons within fleet
                infleetseas <- agg$f==f & agg$s==s
                agg$N[infleetseas] <- max(agg$N[infleetseas])
                agg$effN[infleetseas] <- max(agg$effN[infleetseas])
              }
            }
            agg$fseas <- agg$f + seasfracs[agg$s]

            namesvec <- paste(fleetnames[agg$f]," s",agg$s,sep="")

            # group remaining calculations as a function
            tempfun8 <- function(ipage,...){
              if(!(kind %in% c("GSTAGE","GSTLEN","L@A","W@A"))){
                make_multifig(ptsx=agg$bin,ptsy=agg$obs,yr=agg$fseas,
                              linesx=agg$bin,linesy=agg$exp,
                              sampsize=agg$N,effN=agg$effN,
                              showsampsize=showsampsize,showeffN=showeffN,
                              bars=bars,linepos=(1-datonly)*linepos,
                              nlegends=3,
                              legtext=list(namesvec,"sampsize","effN"),
                              main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                              maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                              fixdims=fixdims2,ipage=ipage,lwd=2,scalebins=scalebins,
                              yupper=yupper,...)
              }

         # haven't configured this aggregated plot for other types
              ## if(kind=="GSTAGE"){
              ##   make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,linesx=dbase$Bin,linesy=dbase$Exp,
              ##                 sampsize=dbase$N,effN=dbase$effN,showsampsize=FALSE,showeffN=FALSE,
              ##                 bars=bars,linepos=(1-datonly)*linepos,
              ##                 nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
              ##                 main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
              ##                 maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
              ##                 fixdims=fixdims,ipage=ipage,...)
              ## }
              ## if(kind %in% c("L@A","W@A")){
              ##   make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,linesx=dbase$Bin,linesy=dbase$Exp,
              ##                 sampsize=dbase$N,effN=0,showsampsize=FALSE,showeffN=FALSE,
              ##                 nlegends=1,legtext=list(dbase$YrSeasName),
              ##                 bars=bars,linepos=(1-datonly)*linepos,
              ##                 main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=ifelse(kind=="W@A",labels[9],labels[1]),
              ##                 maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
              ##                 fixdims=fixdims,ipage=ipage,...)
              ## }

            }
            if(plot) tempfun8(ipage=0,...)
            if(print){ # set up plotting to png file if required
              npages <- ceiling(length(unique(agg$fseas))/maxrows/maxcols)
              for(ipage in 1:npages)
              {
                pagetext <- ""
                if(npages>1){
                  pagetext <- paste("_page",ipage,sep="")
                  caption <- paste(caption, " (plot ",ipage," of ",npages,")",sep="")
                }
                file <- paste(filenamestart,filename_fltsexmkt,pagetext,
                              "_aggregated_within_season.png",sep="")

                plotinfo <- pngfun(file=file, caption=caption)
                tempfun8(ipage=ipage,...)
                dev.off()
              }
            } # end print function
          } # end test for presence of observations in this partition
        } # end loop over partitions
      } # end loop over combined/not-combined sex
    } # end if data
  } # end subplot 22

  ### subplot 23: by fleet aggregating across seasons within a year
  if(23 %in% subplots & kind!="cond" & nseasons>1){ # for age or length comps, but not conditional AAL
    # loop over fleets
    for(f in fleets){
      dbasef <- dbase_kind[dbase_kind$Fleet==f,]
      if("DM_effN" %in% names(dbasef) && any(!is.na(dbasef$DM_effN))){
        warning("Sample sizes in plots by fleet aggregating across seasons within a year have not yet been updated to reflect Dirichlet-Multinomial likelihood")
      }
      
      # check for the presence of data
      if(nrow(dbasef)>0){
        testor    <- length(dbasef$sex[dbasef$sex==1 & dbasef$Pick_sex==0 ])>0
        testor[2] <- length(dbasef$sex[dbasef$sex==1 & dbasef$Pick_sex %in% c(1,3)])>0
        testor[3] <- length(dbasef$sex[dbasef$sex==2])>0
        # loop over sex combinations
        for(k in (1:3)[testor]){
          if(k==1){dbase_k <- dbasef[dbasef$sex==1 & dbasef$Pick_sex==0,]}
          if(k==2){dbase_k <- dbasef[dbasef$sex==1 & dbasef$Pick_sex %in% c(1,3),]}
          if(k==3){dbase_k <- dbasef[dbasef$sex==2,]}
          sex <- ifelse(k==3, 2, 1)

          # loop over partitions (discard, retain, total)
          for(j in unique(dbase_k$Part)){
            # dbase is the final data.frame used in the individual plots
            # it is subset based on the kind (age, len, age-at-len), fleet, sex, and partition
            dbase <- dbase_k[dbase_k$Part==j,]
            if(nrow(dbase)>0){
              ## assemble pieces of plot title
              # sex
              if(k==1) titlesex <- "sexes combined, "
              if(k==2) titlesex <- "female, "
              if(k==3) titlesex <- "male, "
              titlesex <- ifelse(printsex,titlesex,"")

              # market category
              if(j==0) titlemkt <- "whole catch, "
              if(j==1) titlemkt <- "discard, "
              if(j==2) titlemkt <- "retained, "
              titlemkt <- ifelse(printmkt,titlemkt,"")

              # plot bars for data only or if input 'fitbar=TRUE'
              if(datonly | fitbar) bars <- TRUE else bars <- FALSE

              # aggregating identifiers for plot titles and filenames
              title_sexmkt <- paste(titlesex,titlemkt,sep="")
              filename_fltsexmkt <- paste("flt",f,"sex",k,"mkt",j,sep="")

              Bins <- sort(unique(dbase$Bin))
              nbins <- length(Bins)
              df <- data.frame(N=dbase$N,
                               effN=dbase$effN,
                               obs=dbase$Obs*dbase$N,
                               exp=dbase$Exp*dbase$N)
              agg <- aggregate(x=df, by=list(bin=dbase$Bin,f=dbase$Fleet,y=floor(dbase$Yr.S)), FUN=sum)
              agg <- agg[agg$f %in% fleets,]
              agg$obs <- agg$obs/agg$N
              agg$exp <- agg$exp/agg$N

              # note: sample sizes will be different for each bin if tail compression is used
              #       printed sample sizes in plot will be maximum, which may or may not
              #       represent sum of sample sizes over all years/ages
              for(f in unique(agg$f)){
                for(y in unique(agg$y[agg$f==f])){
                  infleetyr <- agg$f==f & agg$y==y
                  agg$N[infleetyr] <- max(agg$N[infleetyr])
                  agg$effN[infleetyr] <- max(agg$effN[infleetyr])
                }
              }
              agg$fy <- agg$f + agg$y/10000
              # total title
              caption <- paste(titledata,title_sexmkt,fleetnames[f],
                               "\naggregated across seasons within year",sep="")
              if(mainTitle) {
                ptitle <- caption
              } else {
                ptitle <- ""
              }

              # group remaining calculations as a function
              tempfun9 <- function(ipage,...){

                if(!(kind %in% c("GSTAGE","GSTLEN","L@A","W@A"))){
                  make_multifig(ptsx=agg$bin,ptsy=agg$obs,yr=agg$fy,
                                linesx=agg$bin,linesy=agg$exp,
                                sampsize=agg$N,effN=agg$effN,
                                showsampsize=showsampsize,showeffN=showeffN,
                                bars=bars,linepos=(1-datonly)*linepos,
                                nlegends=3,
                                legtext=list(agg$y,"sampsize","effN"),
                                main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                                maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                                fixdims=fixdims2,ipage=ipage,lwd=2,scalebins=scalebins,
                                yupper=yupper, ...)
                }

                # haven't configured this aggregated plot for other types
                ## if(kind=="GSTAGE"){
                ##   make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,linesx=dbase$Bin,linesy=dbase$Exp,
                ##                 sampsize=dbase$N,effN=dbase$effN,showsampsize=FALSE,showeffN=FALSE,
                ##                 bars=bars,linepos=(1-datonly)*linepos,
                ##                 nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
                ##                 main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                ##                 maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                ##                 fixdims=fixdims,ipage=ipage,...)
                ## }
                ## if(kind %in% c("L@A","W@A")){
                ##   make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,linesx=dbase$Bin,linesy=dbase$Exp,
                ##                 sampsize=dbase$N,effN=0,showsampsize=FALSE,showeffN=FALSE,
                ##                 nlegends=1,legtext=list(dbase$YrSeasName),
                ##                 bars=bars,linepos=(1-datonly)*linepos,
                ##                 main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=ifelse(kind=="W@A",labels[9],labels[1]),
                ##                 maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                ##                 fixdims=fixdims,ipage=ipage,...)
                ## }

              } # end tempfun

              if(plot) tempfun9(ipage=0,...)
              if(print){ # set up plotting to png file if required
                npages <- ceiling(length(unique(agg$fy))/maxrows/maxcols)
                for(ipage in 1:npages){
                  pagetext <- ""
                  if(npages>1){
                    pagetext <- paste("_page",ipage,sep="")
                    caption <- paste(caption, " (plot ",ipage," of ",npages,")",sep="")
                  }
                  file <- paste(filenamestart,filename_fltsexmkt,pagetext,
                                "_aggregated_across_seasons_within_year.png",sep="")
                  pngfun(file=file, caption=caption)
                  tempfun9(ipage=ipage,...)
                  dev.off()
                }
              } # end print function
            } # end test for presence of observations in this partition
          } # end loop over partitions
        } # end loop over combined/not-combined sex
      } # end if data
    } # end loop over fleets
  } # end subplot 23

  ### subplot 24: bubble plot comparison of length or age residuals
  ###             across fleets within sex/partition
  if(24 %in% subplots & kind %in% c("LEN","AGE")){

    # loop over partitions groups (everything, or separate discard, retain, total)
    for(j in unique(dbase_kind$Part_group)){
      # subset data.frame for this partition group and subset of fleets of interest
      dbase_parts <- dbase_kind[dbase_kind$Part_group==j,]
      # new column combining fleet and partition
      # where 3.1 is fleet=3, partition=1
      dbase_parts$FleetPart <- dbase_parts$Fleet + 0.1*dbase_parts$Part
      # table of info on each panel
      panel_table <- data.frame(FleetPart=sort(unique(dbase_parts$FleetPart)))
      # separate the pieces out again
      panel_table$Fleet <- floor(panel_table$FleetPart)
      # round below is necessary because values were coming out as
      # 1.9999999999999996 instead of 2
      panel_table$Part <- round(10*(panel_table$FleetPart - panel_table$Fleet))
      # fleet name to use for each panel
      panel_table$Name <- fleetnames[panel_table$Fleet]

      # check for multiple market categories in a fleet to plot separately
      max_n_mkt <- max(apply(table(panel_table$Fleet,
                                   panel_table$Part)>0, 1, sum))
      if(max_n_mkt > 1){
        # if multiple categories within a fleet, append to name
        mktnames <- c("","(discards)","(retained)")
        panel_table$Name <- paste(panel_table$Name,
                                  mktnames[panel_table$Part+1])
      }
      npanels <- nrow(panel_table)
      panelvec <- 1:npanels

      xlim <- range(dbase_parts$Yr.S) # set xlim based on range across all fleets
      xaxislab <- sort(unique(floor(dbase_parts$Yr.S))) # label with all years

      # get growth curves if requested
      if(length(cohortlines)>0){
        growdat <- replist$endgrowth
        growdatF <- growdat[growdat$Sex==1 &
                              growdat$Morph==min(growdat$Morph[growdat$Sex==1]),]
        if(nsexes > 1){
          growdatM <- growdat[growdat$Sex==2 &
                                growdat$Morph==min(growdat$Morph[growdat$Sex==2]),]
        }
      }
      # market category
      if(j ==-1) titlemkt <- ""
      if(j == 0) titlemkt <- "whole catch"
      if(j == 1) titlemkt <- "discard"
      if(j == 2) titlemkt <- "retained"
      titlemkt <- ifelse(printmkt,titlemkt,"")
      ## title_sexmkt <- paste(titlesex,titlemkt,sep="")

      caption_base <- paste0(titletype, titlemkt, ", comparing across fleets")
      # hack to remove ", ," from caption
      caption_base <- gsub(", ,", ", ", caption_base)
      if(mainTitle) {
        ptitle <- caption_base
      } else {
        ptitle <- ""
      }
      titles <- c(ptitle,titles) # compiling list of all plot titles
      # if not all market categories, append mkt value to filename
      filenamemkt <- ifelse(j > -1, paste("mkt",j,sep=""), "")

      multifleet.bubble.fun <- function(ipage=0){
        # a function to wrap up multi-fleet bubble plots
        # old graphics parameter settings
        par_old <- par()
        # multi-figure plot with as many rows as fleets, or the maxrows value
        par(mfrow=c(min(npanels,maxrows),1),
            mar=c(0.5,0,0,0),
            oma=c(4, 6, ifelse(mainTitle,3,1), 1))

        # set up some stuff for cases where there are more fleets than panels in one plot
        panelrange <- 1:npanels
        npages <- ceiling(npanels/maxrows) # how many pages of plots
        if(npages > 1 & ipage!=0) # range of which panels to print for each page
          panelrange <- intersect(panelrange, 1:maxrows + maxrows*(ipage-1))

        # loop over panels (fleet x partition)
        for(ipanel in panelvec[panelrange]){
          flt <- panel_table$Fleet[ipanel] # fleet number
          mkt <- panel_table$Part[ipanel] # market category
          # subset database of values
          dbase <- dbase_parts[dbase_parts$Fleet==flt &
                                 dbase_parts$Part==mkt,]
          # dbase is the final data.frame used in the individual plots
          # it is subset based on the kind (age, len, age-at-len), sex, and partition,
          ### not sure if multiple ageing error methods is supported at the moment,
          ### haven't tested -Ian 6/7/17
          # check for multiple ageing error types within a year to plot separately
          max_n_ageerr <- max(apply(table(dbase$Yr.S,dbase$Ageerr)>0,1,sum))

          if(max_n_ageerr > 1){
            if(ageerr_warning){
              cat("Note: multiple samples with different ageing error types within fleet/year.\n",
                  "     Plots label '2005a3' indicates ageing error type 3 for 2005 sample.\n",
                  "     Bubble plots may be misleading with overlapping bubbles.\n")
              ageerr_warning <- FALSE
            }
            # add 1/1000 of a year for each ageing error type to distinguish
            # between types within a year (may not work well for this plot)
            dbase$Yr.S <- dbase$Yr.S + dbase$Ageerr/(1000*max_n_ageerr)
            dbase$YrSeasName <- paste(dbase$YrSeasName,"a",dbase$Ageerr,sep="")
          }

          # calculate smallest difference among years
          # which is used to adjust offsets males and females
          xdiff <- 0.1*sort(unique(diff(sort(unique(dbase$Yr.S)),
                                        na.rm=TRUE)))[1]
          # not sure what cases would have missing xdiff
          # from above calculation, but it definitely happens
          # with only one year of data, so setting default
          # to work for that case
          if(is.na(xdiff)){
            xdiff <- 0.1
          }

          # define colors
          xvals <- dbase$Yr.S
          cols <- rep(colvec[3],nrow(dbase))
          if(nsexes > 1){
            xvals[dbase$sex>0] <- dbase$Yr.S[dbase$sex>0] -
              (dbase$sex[dbase$sex>0]-1.5)*xdiff
            if(length(unique(dbase$Yr.S))==1){
              # if only one year, don't bother showing points
              # as mid-year values
              # this may not be ideal for seasonal models
              xvals[dbase$sex>0] <- floor(dbase$Yr.S[dbase$sex>0]) -
                (dbase$sex[dbase$sex>0]-1.5)*xdiff
            }
            cols[dbase$sex>0] <- colvec[dbase$sex[dbase$sex>0]]
          }

          # determine bubble size and colors
          if(datonly){
            z <- dbase$Obs
            if(scalebubbles) z <- dbase$N*dbase$Obs # if requested, scale by sample sizes
            titletype <- titledata
            filetype <- "bub"
            allopen <- TRUE
          }else{
            z <- dbase$Pearson
            titletype <- "Pearson residuals, "
            filetype <- "resids"
            allopen <- FALSE
          }

          # make bubbles for a single fleet
          # this section is a modified version of tempfun2 above
          ylim <- range(dbase$Bin)
          ylim[2] <- ylim[2]+0.2*diff(ylim) # add buffer of 10% at the top for fleet name
          bubble3(x=xvals, y=dbase$Bin, z=z, col=cols, cexZ1=cexZ1,
                  legend=bublegend,
                  las=1,main="",cex.main=cex.main,maxsize=pntscalar,allopen=allopen,
                  xlim=xlim,ylim=ylim,axis1=FALSE)
          ### add label at top left of each panel
          legend('topleft', title=panel_table$Name[ipanel],
                 legend=NA, bty='n', cex=1.5)
          ### alternative way with legends on the side
          ### (probably not as good once the partition has been added)
          #mtext(namesvec[ipanel],side=2,line=4.5,cex=par()$cex)


          # add lines for growth of individual cohorts if requested
          if(length(cohortlines)>0){
            for(icohort in 1:length(cohortlines)){
              cat("  Adding line for",cohortlines[icohort],"cohort\n")
              if(kind=="LEN"){
                lines(growdatF$Age+cohortlines[icohort],
                      growdatF$Len_Mid, col=colvec[1]) #females
                if(nsexes>1){
                  lines(growdatM$Age+cohortlines[icohort],
                        growdatM$Len_Mid, col=colvec[2]) #males
                }
              }
              if(kind=="AGE"){
                lines(0.5 + c(cohortlines[icohort],cohortlines[icohort]+accuage),
                      c(0,accuage),col="red")
              }
            }
          }

          if(par()$mfg[1]==par()$mfg[3] | ipanel==tail(panelvec,1)){
            # label all years on x-axis of last panel
            axis(1,at=xaxislab)
          }else{
            # or just tick marks for other panels
            axis(1,at=xaxislab,labels=rep("",length(xaxislab)))
          }
          if(par()$mfg[1]==1)
            # add title after making first panel
            title(main=ptitle, outer=TRUE, xlab=labels[3], ylab=kindlab)
        } # end loop over fleets
        # restore previous graphics parameter settings
        par(mfcol=par_old$mfcol, mar=par_old$mar, oma=par_old$oma)
      } # end function wrapping up a single page of the residual comparison plot

          # make plots or write to PNG file
          if(length(panelvec)>0){
            if(plot) multifleet.bubble.fun(ipage=0)
            if(print){ # set up plotting to png file if required
              npages <- ceiling(nrow(panel_table)/maxrows)
              for(ipage in 1:npages){
                pagetext <- ""
                caption <- caption_base
                if(npages>1){
                  pagetext <- paste("_page",ipage,sep="")
                  caption <- paste0(caption, " (plot ",ipage," of ",npages,")")
                }
                if(ipage==1 & length(grep("Pearson",caption))>0){
                  caption <- paste(caption,
                                   "<br> \nClosed bubbles are positive residuals",
                                   "(observed > expected)",
                                   "and open bubbles are negative residuals",
                                   "(observed < expected).")
                }
                #### current scaling allows comparison across panels, so warning below
                #### has been turned off
                ## caption <- paste(caption,
                ##                  "<br>Note: bubble sizes are scaled to maximum within each panel.",
                ##                  "<br>Thus, comparisons across panels should focus on patterns, not bubble sizes.")
                file <- paste(filenamestart,filenamemkt,pagetext,
                              "_multi-fleet_comparison.png",sep="")
                plotinfo <- pngfun(file=file, caption=caption)
                multifleet.bubble.fun(ipage=ipage)
                dev.off()
              } # end loop over pages within printing PNG
            } # end printing to PNG files
          } # end test for non-zero number of fleets
        } # end loop over partitions
      ## } # end loop over sexes
    ## } # end loop over sex combinations
    # restore default single panel settings
    par(mfcol=c(rows,cols),mar=c(5,4,4,2)+.1,oma=rep(0,4))
  } # end subplot 24

  if(!is.null(plotinfo)) plotinfo$category <- "Comp"
  return(invisible(plotinfo))
} # end SSplotComps function
###########################
