#' Plot sex-ratio data and fits for two sex models
#'
#' Plot sex-ratio data and fits from Stock Synthesis output.  Multi-figure
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
#' @param printsex show gender in plot titles?
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
#' @param \dots additional arguments that will be passed to the plotting.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{make_multifig}}
#' @keywords hplot
SSplotSexRatio <-
  function(replist, subplots=1, #subplots=1:13,
           kind="LEN", sizemethod=1, aalyear=-1, aalbin=-1, plot=TRUE, print=FALSE,
           fleets="all", fleetnames="default", sexes="all",
           yupper=0.4,
           datonly=FALSE, samplesizeplots=TRUE, compresidplots=TRUE, bub=FALSE,
           showyears=TRUE, showsampsize=TRUE, showeffN=TRUE,
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
           scalebins=FALSE,addMeans=TRUE,...)
{
  ################################################################################
  # SSplotSexRatio
  ################################################################################

  ######  definitions of subplots
  ### Only one subplot is currently defined
  ### { # loop over fleets
  ### subplot 1: multi-panel composition plot
  ### } # end loop over fleets
  ###             across fleets within partition

  if(!exists("make_multifig2")) stop("you are missing the function 'make_mulitifig'")

  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
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
      titledata <- "length comp data, "
    }else{
      filenamestart <- "comp_lenfit_"
      titledata <- "length comps, "
    }
  }
  if(kind=="GSTLEN"){
    dbase_kind <- ghostlendbase
    kindlab=labels[1]
    if(datonly){
      filenamestart <- "comp_gstlendat_"
      titledata <- "ghost length comp data, "
    }else{
      filenamestart <- "comp_gstlenfit_"
      titledata <- "ghost length comps, "
    }
  }
  if(kind=="SIZE"){
    dbase_kind <- sizedbase[sizedbase$method==sizemethod,]
    sizeunits <- unique(dbase_kind$units)
    if(length(sizeunits)>1)
      stop("!error with size units in generalized size comp plots:\n",
           "    more than one unit value per method.\n")
    if(sizeunits %in% c("in","cm"))
      kindlab <- paste(labels[21]," (",sizeunits,")",sep="")
    if(sizeunits %in% c("lb","kg"))
      kindlab <- paste(labels[9]," (",sizeunits,")",sep="")
    if(datonly){
      filenamestart <- "comp_sizedat_"
      titledata <- "size comp data, "
    }else{
      filenamestart <- "comp_sizefit_"
      titledata <- "size comps, "
    }
  }
  if(kind=="AGE"){
    dbase_kind <- agedbase
    kindlab=labels[2]
    if(datonly){
      filenamestart <- "comp_agedat_"
      titledata <- "age comp data, "
    }else{
      filenamestart <- "comp_agefit_"
      titledata <- "age comps, "
    }
  }
  if(kind=="cond"){
    dbase_kind <- condbase
    kindlab=labels[2]
    if(datonly){
      filenamestart <- "comp_condAALdat_"
      titledata <- "conditional age-at-length data, "
    }else{
      filenamestart <- "comp_condAALfit_"
      titledata <- "conditional age-at-length, "
    }
  }
  if(kind=="GSTAGE"){
    dbase_kind <- ghostagedbase
    kindlab=labels[2]
    if(datonly){
      filenamestart <- "comp_gstagedat_"
      titledata <- "ghost age comp data, "
    }else{
      filenamestart <- "comp_gstagefit_"
      titledata <- "ghost age comps, "
    }
  }
  if(kind=="GSTcond"){
    dbase_kind <- ghostagedbase
    kindlab=labels[2]
    if(datonly){
      filenamestart <- "comp_gstCAALdat_"
      titledata <- "ghost conditional age-at-length data, "
    }else{
      filenamestart <- "comp_gstCAALfit_"
      titledata <- "ghost conditional age-at-length comps, "
    }
  }
  if(kind=="L@A"){
    dbase_kind <- ladbase[ladbase$N!=0,] # remove values with 0 sample size
    kindlab=labels[2]
    filenamestart <- "comp_LAAfit_"
    titledata <- "mean length at age, "
    dbase_kind$SD <- dbase_kind$Lbin_lo/dbase_kind$N
  }
  if(kind=="W@A"){
    dbase_kind <- wadbase[wadbase$N!=0,] # remove values with 0 sample size
    kindlab=labels[2]
    filenamestart <- "comp_WAAfit_"
    titledata <- "mean weight at age, "
  }
  if(!(kind%in%c("LEN","SIZE","AGE","cond","GSTAGE","GSTLEN","L@A","W@A"))){
    stop("Input 'kind' to SSplotComps is not right.")
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

  # subset data based on requested range of sexes
  dbase_kind <- dbase_kind[dbase_kind$sex %in% sexes,]
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
      # loop over genders combinations
      ## for(k in (1:3)[testor])
      ## {
      ##   if(k==1){dbase_k <- dbasef[dbasef$Gender==1 & dbasef$Pick_gender==0,]}
      ##   if(k==2){dbase_k <- dbasef[dbasef$Gender==1 & dbasef$Pick_gender %in% c(1,3),]}
      ##   if(k==3){dbase_k <- dbasef[dbasef$Gender==2,]}
      ##   sex <- ifelse(k==3, 2, 1)
      ##   if(sex %in% sexes){
      ##     cat('sex',sex,'\n')

      dbase_k <- dbasef

      # loop over partitions (discard, retain, total)
      for(j in unique(dbase_k$Part)){
        dbase <- dbase_k[dbase_k$Part==j,]
        # dbase is the final data.frame used in the individual plots
        # it is subset based on the kind (age, len, age-at-len), fleet, gender, and partition

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
        ## dbase$Yr.S[dbase_k$Pick_gender==1] <- dbase$Yr.S[dbase_k$Pick_gender==1] + 1e-6
        ## dbase$Yr.S[dbase_k$Pick_gender==2] <- dbase$Yr.S[dbase_k$Pick_gender==2] + 2e-6

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
          ptitle <- paste(titledata,title_sexmkt, fleetnames[f],sep="") # total title
          titles <- c(ptitle,titles) # compiling list of all plot titles
          tempfun <- function(ipage,...){
            sexvec <- dbase$sex
            # a function to combine a bunch of repeated commands
            if(!(kind %in% c("GSTAGE","GSTLEN","L@A","W@A"))){
              make_multifig2(dbase=dbase,
                            showsampsize=showsampsize,showeffN=showeffN,
                            bars=bars,linepos=(1-datonly)*linepos,
                            nlegends=3,legtext=list("yr","N","effN"),
                            main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                            maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                            fixdims=fixdims,ipage=ipage,scalebins=scalebins,
                            colvec=colvec, linescol=linescol, axis1=axis1, axis2=axis2,
                            sexvec=sexvec, yupper=yupper)
            }
            if(kind=="GSTAGE"){
              make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr.S,linesx=dbase$Bin,linesy=dbase$Exp,
                            sampsize=dbase$N,effN=dbase$effN,showsampsize=FALSE,showeffN=FALSE,
                            bars=bars,linepos=(1-datonly)*linepos,
                            nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
                            main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=labels[6],
                            maxrows=maxrows,maxcols=maxcols,rows=rows,cols=cols,
                            fixdims=fixdims,ipage=ipage,scalebins=scalebins,
                            colvec=colvec, linescol=linescol, axis1=axis1, axis2=axis2,
                            sexvec=sexvec, yupper=yupper)#, ...)
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
              caption <- ptitle
              pagetext <- ""
              if(npages>1){
                pagetext <- paste("_page",ipage,sep="")
                caption <- paste(caption, " (plot ",ipage," of ",npages,")",sep="")
              }
              file <- paste(plotdir,"/",filenamestart,
                            filename_fltsexmkt,pagetext,".png",sep="")
              plotinfo <- pngfun(file=file, caption=caption)
              tempfun(ipage=ipage,...)
              dev.off()
            }
          }
      } # end subplot 1

    } # end loop over partitions (index j)
      #        } # end test for whether gender in vector of requested sexes
      #      } # end loop over combined/not-combined genders
    } # end if data
  } # end loop over fleets

  if(!is.null(plotinfo)) plotinfo$category <- "Comp"
  return(invisible(plotinfo))
} # end embedded SSplotComps function
###########################
