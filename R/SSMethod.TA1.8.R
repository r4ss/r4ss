#' Apply Francis composition weighting method TA1.8 for conditional age-at-length fits
#'
#' Uses method TA1.8 (described in Appendix A of Francis 2011) to do
#' stage-2 weighting of composition data from a Stock Synthesis model.
#' 6-2-14 Added argument seas to deal with multi-season data
#' 12-3-14 Allowed type = 'size' to deal with generalized size comps
#' 12-3-14 Added argument method to allow the user to restrict which
#'         size-frequency methods are included
#'
#' Outputs a mutiplier, w (with bootstrap 95% confidence interval),
#' so that N2y = w x N1y, where N1y and N2y are the stage-1 and stage-2
#' multinomial sample sizes for the data set in year y.  Optionally
#' makes a plot of observed (with confidence limits, based on N1y)
#' and expected mean lengths (or ages).
#'
#' CAUTIONARY/EXPLANATORY NOTE.
#' The large number of options available in SS makes it very
#' difficult to be sure that what this function does is
#' appropriate for all combinations of options.  The following
#' notes might help anyone wanting to check or correct the code.
#' 1. The code first takes the appropriate database (lendbase, sizedbase,
#'    agedbase, or condbase) and removes un-needed rows.
#' 2. The remaining rows of the database are grouped into individual
#'    comps (indexed by vector indx) and relevant statistics (e.g.,
#'    observed and expected mean length or age), and ancillary data,
#'    are calculated for each comp (these are stored in pldat - one row
#'    per comp).
#' 3. If the data are to be plotted, the comps are grouped, with each
#'    group corresponding to a panel in the plot, and groups are indexed
#'    by plindx.
#' 4. A single multiplier is calculated to apply to all the comps.
#'
#' @param fit Stock Synthesis output as read by r4SS function SS_output
#' @param either 'len' (for length composition data), 'size' (for
#' generalized size composition data), 'age' (for age composition data),
#' or 'con' (for conditional age at length data)
#' @param fleet vector of one or more fleet numbers whose data are to
#' be analysed simultaneously (the output N multiplier applies
#' to all fleets combined)
#' @param part vector of one or more partition values; analysis is restricted
#' to composition data with one of these partition values.
#' Default is to include all partition values (0, 1, 2).
#' @param pick.gender vector of one or more values for Pick_gender; analysis is
#' restricted to composition data with one of these
#' Pick_gender values.  Ignored if type=='con'
#' @param seas string indicating how to treat data from multiple seasons
#' 'comb' - combine seasonal data for each year and plot against Yr
#' 'sep' - treat seasons separately, plotting against Yr.S
#' If is.null(seas) it is assumed that there is only one season in
#' the selected data (a warning is output if this is not true) and
#' option 'comb' is used.
#' @param method a vector of one or more size-frequency method numbers
#' (ignored unless type = 'size').
#' If !is.null(method), analysis is restricted to size-frequency
#' methods in this vector.  NB comps are separated by method
#' @param  plotit if TRUE, make an illustrative plot like one or more
#' panels of Fig. 4 in the Francis (2011).
#' @author Andre Punt, Chris Francis, Ian Taylor
#' @seealso \code{\link{SSMethod.Cond.TA1.8}}
#' @references Francis, R.I.C.C. (2011). Data weighting in statistical
#' fisheries stock assessment models. Canadian Journal of
#' Fisheries and Aquatic Sciences 68: 1124-1138.
#' @examples
#' \dontrun{
#' Nfleet <- length(myreplist$FleetNames)
#' for (Ifleet in 1:Nfleet)
#'   SSMethod.TA1.8(myreplist,"len",fleet=Ifleet,maxpanel=maxpanel)
#' for (Ifleet in 1:Nfleet)
#'   SSMethod.TA1.8(myreplist,"age",fleet=Ifleet,maxpanel=maxpanel)
#' for (Ifleet in 1:Nfleet)
#'   SSMethod.TA1.8(myreplist,"size",fleet=Ifleet,maxpanel=maxpanel)
#' for (Ifleet in 1:Nfleet)
#'   SSMethod.TA1.8(myreplist,"con",fleet=Ifleet,maxpanel=maxpanel)
#' for (Ifleet in 1:Nfleet)
#'   SSMethod.Cond.TA1.8(myreplist,fleet=Ifleet,maxpanel=maxpanel)
#' }
#' 
SSMethod.TA1.8 <-
  function(fit,type,fleet,part=0:2,pick.gender=0:3,seas=NULL,
           method=NULL,plotit=T,maxpanel=1000,FullDiagOut=F)
{
  # Check the type is correct and the pick.gender is correct
  is.in <- function (x, y)!is.na(match(x, y))
  if(!is.in(type,c('age','len','size','con')))stop('Illegal value for type')
  else if(sum(!is.in(pick.gender,c(0:3)))>0)
    stop('Unrecognised value for pick.gender')

  # Select the type of datbase
  dbase <- fit[[paste(type,'dbase',sep='')]]
  sel <- is.in(dbase$Fleet,fleet) & is.in(dbase$Part,part)
  if(type!='con')sel <- sel & is.in(dbase$'Pick_gender',pick.gender)
  if(type=='size' & !is.null(method))
    sel <- sel & is.in(dbase$method,method)
  if(sum(sel)==0) return()
  dbase <- dbase[sel,]
  if(is.null(seas)){
    seas <- 'comb'
    if(length(unique(dbase$Seas))>1)
      cat('Warning: combining data from multiple seasons\n')
  }

  gender.flag <- type!='con' & max(tapply(dbase$'Pick_gender',
                     dbase$Fleet,function(x)length(unique(x))))>1
  indx <- paste(dbase$Fleet,dbase$Yr,if(type=='con')dbase$'Lbin_lo' else
                '',if(seas=='sep')dbase$Seas else '')
  if(gender.flag)indx <- paste(indx,dbase$'Pick_gender')
  method.flag <- if(type=='size') length(unique(dbase$method))>1 else F
  if(method.flag)
    indx <- paste(indx,dbase$method)
  uindx <- unique(indx)
  if(length(uindx)==1)stop('Only one point to plot')

  pldat <- matrix(0,length(uindx),10,
                  dimnames=list(uindx,
                      c('Obsmn','Obslo','Obshi','semn','Expmn','Std.res',
                        'ObsloAdj','ObshiAdj','Fleet','Yr')))
  if(type=='con')pldat <- cbind(pldat,Lbin=0)
  if(gender.flag)pldat <- cbind(pldat,pick.gender=0)
  if(method.flag)pldat <- cbind(pldat,method=0)

  # Find the wieghting factor for this combination of factors
  for(i in 1:length(uindx)){  # each row of pldat is an individual comp
    subdbase <- dbase[indx==uindx[i],]
    xvar <- subdbase$Bin
    pldat[i,'Obsmn'] <- sum(subdbase$Obs*xvar)/sum(subdbase$Obs)
    pldat[i,'Expmn'] <- sum(subdbase$Exp*xvar)/sum(subdbase$Exp)
    pldat[i,'semn'] <- sqrt((sum(subdbase$Exp*xvar^2)/sum(subdbase$Exp)-
                             pldat[i,'Expmn']^2)/mean(subdbase$N))
    pldat[i,'Obslo'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']
    pldat[i,'Obshi'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']
    pldat[i,'Std.res'] <- (pldat[i,'Obsmn']-pldat[i,'Expmn'])/pldat[i,'semn']
    pldat[i,'Fleet'] <- mean(subdbase$Fleet)
    pldat[i,'Yr'] <- mean(if(seas=='comb')subdbase$Yr else subdbase$Yr.S)
    if(type=='con')pldat[i,'Lbin'] <- mean(subdbase$'Lbin_lo')
    if(gender.flag)
      pldat[i,'pick.gender'] <- mean(subdbase$'Pick_gender')
    if(method.flag)
      pldat[i,'method'] <- mean(subdbase$method)
  }
  Nmult <- 1/var(pldat[,'Std.res'],na.rm=T)

  # Find the adjusted confidence intervals
  for(i in 1:length(uindx)){
    pldat[i,'ObsloAdj'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']/sqrt(Nmult)
    pldat[i,'ObshiAdj'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']/sqrt(Nmult)
  }

  Nfleet <- length(unique(pldat[,'Fleet']))
  # make plot if requested
  if(plotit){
    plindx <- if(type=='con'){
      paste(pldat[,'Fleet'],pldat[,'Yr'])
    }else{
      pldat[,'Fleet']
    }
    if(gender.flag)plindx <- paste(plindx,pldat[,'pick.gender'])
    if(method.flag)plindx <- paste(plindx,pldat[,'method'])
    uplindx <- unique(plindx)

    # Select number of panels
    Npanel <- length(uplindx)
    NpanelSet <- max(4,min(length(uplindx),maxpanel))
    Nr <- ceiling(sqrt(NpanelSet))
    Nc <- ceiling(NpanelSet/Nr)
    par(mfrow=c(Nr,Nc),mar=c(2,2,1,1)+0.1,mgp=c(0,0.5,0),oma=c(1.2,1.2,0,0),
        las=1)
    par(cex=1)
    for(i in 1:Npanel){
      subpldat <- pldat[plindx==uplindx[i],,drop=F]
      x <- subpldat[,ifelse(type=='con','Lbin','Yr')]
      plot(x,subpldat[,'Obsmn'],pch='-',
           xlim=if(length(x)>1)range(x) else c(x-0.5,x+0.5),
           ylim=range(subpldat[,c('ObsloAdj','ObshiAdj','Expmn')]),
           xlab='',ylab='')
      segments(x,subpldat[,'Obslo'],x,subpldat[,'Obshi'],lwd=3)
      arrows(x,subpldat[,'ObsloAdj'],x,subpldat[,'ObshiAdj'],lwd=1,
             length=0.03, angle=90, code=3)
      points(x,subpldat[,'Obsmn'],pch=21,bg='grey')
      ord <- order(x)
      if(length(x)>1)lines(x[ord],subpldat[ord,'Expmn'])
      else lines(c(x-0.5,x+0.5),rep(subpldat[,'Expmn'],2))
      # Lines
      fl <- fit$FleetNames[subpldat[1,'Fleet']]
      yr <- paste(subpldat[1,'Yr'])
      lab <- if(type=='con')ifelse(Nfleet>1,paste(yr,fl),yr) else fl
      if(gender.flag)lab <-
        paste(lab,ifelse(subpldat[1,'pick.gender']==0,'comb','sex'))
      if(method.flag)lab <- paste(lab,'meth',subpldat[1,'method'])
      mtext(lab,side=3,at=mean(x))
    }
    mtext(paste('Mean',ifelse(is.in(type,c('len','size')),'length','age')),
          side=2,las=0,outer=TRUE)
    mtext(ifelse(type=='con','Length','Year'),side=1,outer=TRUE)
  }
  tmp <- matrix(sample(pldat[,'Std.res'],1000*nrow(pldat),repl=T),nrow(pldat))
  confint <- as.vector(quantile(apply(tmp,2,function(x)1/var(x,na.rm=T)),
                                c(0.025,0.975)))
  Output <- c(w=Nmult,lo=confint[1],hi=confint[2])
  Outs <- paste("Francis Weights - ",type,":",fit$FleetNames[fleet],":",
                round(Nmult,5),round(confint[1],5),round(confint[2],5))
  print(Outs)
  return(Output)
}

