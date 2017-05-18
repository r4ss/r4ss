#' Apply Francis composition weighting method TA1.8 for conditional age-at-length fits
#'
#' Uses an extension of method TA1.8 (described in Appendix A of Francis, 2011)
#' to do stage-2 weighting of conditional age at length composition data from a
#' Stock Synthesis model.
#'
#' The function outputs a mutiplier, \emph{w},
#' (with bootstrap 95\% confidence intervals) so that
#' \emph{N2i} = \emph{w} x \emph{N1i},
#' where \emph{N1i} and \emph{N2i} are the stage-1 and stage-2 multinomial
#' sample sizes for the \emph{i}th composition. Optionally makes a plot
#' of observed and expected mean ages, with two alternative
#' sets of confidence limits - based on \emph{N1i} (thin lines) and \emph{N2i}
#' (thick lines) - for the observed values.
#'
#' This function formerly reported two versions of w differ according to whether
#' the calculated mean ages are
#' indexed by year (version A) or by year and length bin (version B).
#' However, research by Punt (2015) found Version A to perform better and
#' version B is no longer recommended and is only reported if requested by the user.
#' 
#' CAUTIONARY/EXPLANATORY NOTE. The large number of options available in SS makes it
#' very difficult to be sure that what this function does is appropriate for all
#' combinations of options. The following notes (for version A) might help anyone
#' wanting to check or correct the code.
#' \enumerate{
#'   \item The code first removes un-needed rows
#' from database condbase.
#'   \item The remaining rows of the database are grouped
#' (indexed by vector indx) and relevant statistics (e.g., observed and expected
#' mean age), and ancillary data, are calculated for each group (these are stored
#' in pldat - one row per group).
#'   \item If the data are to be plotted they are further
#' grouped by fleet, with one panel of the plot per fleet.
#'   \item A single multiplier, \emph{w}, is calculated to apply to all the
#' selected data.
#' }
#'
#' @param fit Stock Synthesis output as read by r4SS function SS_output
#' @param fleet vector of one or more fleet numbers whose data are to
#' be analysed simultaneously (the output N multiplier applies
#' to all fleets combined)
#' @param fleetnames Vector of alternative fleet names to draw from for
#' plot titles and captions. It should have length equal to the number
#' of fleets in the model, not the number of fleets considered in this function.
#' @param part vector of one or more partition values; analysis is restricted
#' to composition data with one of these partition values.
#' Default is to include all partition values (0, 1, 2).
#' @param seas string indicating how to treat data from multiple seasons
#' 'comb' - combine seasonal data for each year and plot against Yr
#' 'sep' - treat seasons separately, plotting against Yr.S
#' If is.null(seas) it is assumed that there is only one season in
#' the selected data (a warning is output if this is not true) and
#' option 'comb' is used.
#' @param plotit if TRUE, make an illustrative plot like one or more
#' panels of Fig. 4 in Francis (2011).
#' @param printit if TRUE, print results to R console.
#' @param maxpanel maximum number of panels within a plot
#' @param FullDiagOut Print full diagnostics?
#' @param ShowVersionB Report the Version B value in addition to the default?
#' @author Chris Francis, Andre Punt, Ian Taylor
#' @export
#' @seealso \code{\link{SSMethod.TA1.8}}
#' @references Francis, R.I.C.C. (2011). Data weighting in statistical
#' fisheries stock assessment models. Can. J. Fish. Aquat. Sci. 68: 1124-1138.
#' 
#' Punt, A.E. (2015). Some insights into data weighting in integrated stock assessments.
#' Fish. Res. \url{http://dx.doi.org/10.1016/j.fishres.2015.12.006}
#'
SSMethod.Cond.TA1.8 <-
  function(fit, fleet, part=0:2, seas=NULL,
           plotit=TRUE, printit=TRUE, maxpanel=1000, FullDiagOut=FALSE,
           ShowVersionB=FALSE, fleetnames=NULL)
{
  # Check the type is correct and the pick.sex is correct
  is.in <- function (x, y)!is.na(match(x, y))

  # replace default fleetnames with user input if requested
  if(is.null(fleetnames)){
    # use fleetnames in the model
    fleetnames <- fit$FleetNames
  }else{
    # if custom names input, check length
    if(length(fleetnames) != fit$nfleets){
      stop('fleetnames needs to be NULL or have length = nfleets = ', fit$nfleets)
    }
  }
  
  # Select the type of datbase
  dbase <- fit[["condbase"]]
  sel <- is.in(dbase$Fleet,fleet) & is.in(dbase$Part,part)
  if(sum(sel)==0) return()

  dbase <- dbase[sel,]
  if(is.null(seas)){
    seas <- 'comb'
    if(length(unique(dbase$Seas))>1)
      cat('Warning: combining data from multiple seasons\n')
  }

  indx <- paste(dbase$Fleet,dbase$Yr,if(seas=='sep')dbase$Seas else '')
  uindx <- unique(indx)
  if(length(uindx)==1){
    # presumably the method is meaningless of there's only 1 point,
    # but it's good to be able to have the function play through
    cat('Warning: only one point to plot\n')
    return()
  }

  pldat <- matrix(0,length(uindx),12,
                  dimnames=list(uindx,
                      c('Obsmn','Obslo','Obshi','semn','Expmn','Std.res','ObsloAdj',
                        'ObshiAdj','Total','Fleet','Yr',"EffN")))
  pldat <- cbind(pldat,Lbin=0)

  # Find the wieghting factor for this combination of factors
  AllRes <- NULL
  for(i in 1:length(uindx)){  # each row of pldat is an individual comp
    subdbase <- dbase[indx==uindx[i],]

    Lbins <- unique(subdbase$Lbin_lo)
    Intermediate <- matrix(0,length(Lbins),5,
                           dimnames=list(Lbins,c('Obsmn','Varn','Expmn','N',"Resid")))
    for (j in 1:length(Lbins)){
      ILbin <- Lbins[j]
      subsubdbase <- subdbase[subdbase$Lbin_lo==ILbin,]
      if (length(subsubdbase$Yr) > 0){
        xvar <- subsubdbase$Bin
        AbarNObs <- sum(subsubdbase$Obs*xvar)/sum(subsubdbase$Obs)
        AbarNPre <- sum(subsubdbase$Exp*xvar)/sum(subsubdbase$Exp)
        AbarVarn <- (sum(subsubdbase$Exp*xvar^2)/sum(subsubdbase$Exp)-AbarNPre^2)
        Intermediate[j,'Obsmn'] <- AbarNObs
        Intermediate[j,'Expmn'] <- AbarNPre
        Intermediate[j,'Varn'] <- AbarVarn
        Intermediate[j,'N'] <- mean(subsubdbase$N)
        Intermediate[j,'Resid'] <- (AbarNObs-AbarNPre)/sqrt(AbarVarn/mean(subsubdbase$N))
      }
    }
    Total <- sum(Intermediate[,'N'])
    Weights <- Intermediate[,'N']/Total

    AbarNObs <- 0
    AbarNPre <- 0
    AbarVarn <- 0
    for (j in 1:length(Lbins)){
      AbarNObs <- AbarNObs + as.double(Intermediate[j,'Obsmn'] * Weights[j])
      AbarNPre <- AbarNPre + as.double(Intermediate[j,'Expmn'] * Weights[j])
      AbarVarn <- AbarVarn + as.double(Weights[j]^2*Intermediate[j,'Varn'])/
        as.double(Intermediate[j,'N'])
    }
    AbarVarn <- sqrt(AbarVarn)

    pldat[i,'Obsmn'] <- AbarNObs
    pldat[i,'Expmn'] <- AbarNPre
    pldat[i,'semn'] <- AbarVarn
    pldat[i,'Obslo'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']
    pldat[i,'Obshi'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']
    pldat[i,'Std.res'] <- (pldat[i,'Obsmn']-pldat[i,'Expmn'])/pldat[i,'semn']
    pldat[i,'Fleet'] <- mean(subdbase$Fleet)
    pldat[i,'Total'] <- Total
    pldat[i,'Yr'] <- mean(if(seas=='comb')subdbase$Yr else subdbase$Yr.S)
    pldat[i,'EffN'] <- 1/var(Intermediate[,'Resid'] )
    AllRes <- c(AllRes,Intermediate[,'Resid'] )
  }
  Nmult <- 1/var(pldat[,'Std.res'],na.rm=TRUE)

  # Find the adjusted confidence intervals
  for(i in 1:length(uindx)){
    pldat[i,'ObsloAdj'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']/sqrt(Nmult)
    pldat[i,'ObshiAdj'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']/sqrt(Nmult)
  }
  if (FullDiagOut==TRUE) print(pldat)

  Nfleet <- length(unique(pldat[,'Fleet']))
  if(plotit){
    plindx <- paste(pldat[,'Fleet'])
    uplindx <- unique(plindx)

    # Select number of panels
    Npanel <- length(uplindx)
    ## Ian T. 9/25/14: changing from having at least 4 panels to no minimum
    #NpanelSet <- max(4,min(length(uplindx),maxpanel))
    NpanelSet <- min(length(uplindx),maxpanel)
    Nr <- ceiling(sqrt(NpanelSet))
    Nc <- ceiling(NpanelSet/Nr)
    par(mfrow=c(Nr,Nc),mar=c(2,2,1,1)+0.1,mgp=c(0,0.5,0),oma=c(1.2,1.2,0,0),
        las=1)
    par(cex=1)
    for(i in 1:Npanel){
      subpldat <- pldat[plindx==uplindx[i],,drop=FALSE]
      x <- subpldat[,'Yr']
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
      fl <- fleetnames[subpldat[1,'Fleet']]
      yr <- paste(subpldat[1,'Yr'])
      lab <- paste(fl)
      mtext(lab,side=3,at=mean(x))
    }
    mtext('Mean age',side=2,outer=TRUE,las=0)
    mtext('Year',side=1,outer=TRUE)
  }
  tmp <- matrix(sample(pldat[,'Std.res'],1000*nrow(pldat),replace=TRUE),nrow(pldat))
  confint <- as.vector(quantile(apply(tmp,2,function(x)1/var(x,na.rm=TRUE)),
                                c(0.025,0.975)))
  Output <- c(w=Nmult,lo=confint[1],hi=confint[2])

  Outs <- paste0("Francis CAA Weights: ",
                 fleetnames[fleet], ": ", round(Nmult,5),
                 " (", round(confint[1],5), "-", round(confint[2],5), ")")
  if(printit){
    print(Outs)
  }
  if(ShowVersionB){
    # Original Francis method (a.k.a. Francis-B)
    Nmult2 <- 1/var(AllRes,na.rm=TRUE)
    tmp <- matrix(sample(AllRes,1000*length(AllRes),replace=TRUE),length(AllRes))
    confint2 <- as.vector(quantile(apply(tmp,2,function(x)1/var(x,na.rm=TRUE)),
                                   c(0.025,0.975)))

    Outs <- paste0("Francis CAA Weights-Version B (not recommended): ",
                   fleetnames[fleet], ": ", round(Nmult2,5), 
                   " (", round(confint2[1],5), "-", round(confint2[2],5), ")")
    if(printit){
      print(Outs)
    }
  }
  return(Output)
}
