#' make table comparing quantities across models
#'
#' Creates a table comparing key quantities from multiple models, which is a
#' reduction of the full information in various parts of the list created using
#' the \code{SSsummarize} function.
#'
#'
#' @param summaryoutput list created by \code{SSsummarize}
#' @param models optional subset of the models described in
#' \code{summaryoutput}.  Either "all" or a vector of numbers indicating
#' columns in summary tables.
#' @param likenames Labels for likelihood values to include, should match
#' substring of labels in \code{summaryoutput$likelihoods}.
#' @param names Labels for parameters or derived quantities to include, should
#' match substring of labels in \code{summaryoutput$pars} or
#' \code{summaryoutput$quants}.
#' @param digits Optional vector of the number of decimal digits to use in
#' reporting each quantity.
#' @param modelnames optional vector of labels to use as column names. Default
#' is 'model1','model2',etc.
#' @param csv write resulting table to CSV file?
#' @param csvdir directory for optional CSV file
#' @param csvfile filename for CSV file
#' @param verbose report progress to R GUI?
#' @param mcmc summarize MCMC output in table?
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SSsummarize}}, \code{\link{SSplotComparisons}},
#' \code{\link{SS_output}}
SStableComparisons <-  function(summaryoutput,
                                models="all",
                                likenames=c("TOTAL",
                                  "Survey",
                                  "Length_comp",
                                  "Age_comp",
                                  "priors",
                                  "Size_at_age"),
                                names=c("R0",
                                  "steep",
                                  "NatM",
                                  "L_at_Amax",
                                  "VonBert_K",
                                  "SPB_Virg",
                                  "Bratio_2015",
                                  "SPRratio_2014"),
                                digits=NULL,
                                modelnames="default",
                                csv=FALSE,
                                csvdir="workingdirectory",
                                csvfile="parameter_comparison_table.csv",
                                verbose=TRUE,
                                mcmc=FALSE){
  if(verbose) cat("running SStableComparisons\n")

  # get stuff from summary output
  n           <- summaryoutput$n
  nsexes      <- summaryoutput$nsexes
  pars        <- summaryoutput$pars
  quants      <- summaryoutput$quants
  likelihoods <- summaryoutput$likelihoods
  npars       <- summaryoutput$npars
  indices     <- summaryoutput$indices

  if(models[1]=="all") models <- 1:n
  ncols <- length(models)
  nsexes <- nsexes[models]

  if(modelnames[1]=="default") modelnames <- paste("model",1:ncols,sep="")
  tab <- as.data.frame(matrix(NA,nrow=0,ncol=ncols+1))

  if(!mcmc) {
    if(!is.null(likenames)){
      likenames <- paste(likenames,"_like",sep="")
      likelihoods$Label <- paste(likelihoods$Label,"_like",sep="")
      names <- c(likenames, names)
    }
    nnames <- length(names)

    bigtable <- rbind(likelihoods[,c(n+1,models)],
                      pars[,c(n+1,models)],
                      quants[,c(n+1,models)])
    # loop over big list of names to get values
    for(iname in 1:nnames){
      name <- names[iname]
      if(!is.null(digits)) digit <- digits[iname]
      if(verbose) cat("name=",name,": ",sep="")
      if(name=="BLANK"){
        if(verbose) cat("added a blank row to the table\n")
        # add to table
        tab <- rbind(tab, " ")
      }else{
        # get values
        vals <- bigtable[grep(name, bigtable$Label),]
        #      cat("labels found:\n",bigtable$Label[grep(name, bigtable$Label)],"\n")
        # fix scale on a few things
        if(name %in% c("SR_LN(R0)","SR_LN.R0.","SR_R0","R0")){
          vals[-1] <- round(exp(vals[-1])/1e6,6)
          vals[1] <- "R0_billions"
        }
        if(substring(name,1,4)=="Recr" & length(grep("like",name))==0) {
          vals[1,-1] <- round(vals[1,-1]/1e6,6)
          vals[1,1] <-paste(vals[1,1],"billions",sep="_")
        }
        if(substring(name,1,3)%in%c("SPB","SSB") | substring(name,1,8)=="TotYield") {
          vals[1,-1] <- round(vals[1,-1]/1e3,3)
          vals[1,1] <-paste(vals[1,1],"thousand_mt",sep="_")
        }
        ## if(name=="SPB_Virg"){
        ##   vals[1,-1] <- as.numeric(vals[1,-1])/1e3
        ##   vals[1,1] <- "SB0_thousand_mt"
        ## }
        if(((length(grep("SPB",name))>0  | length(grep("SSB",name))>0) & any(nsexes==1))){
          cat("dividing name by 2 for single-sex models:",(1:ncols)[nsexes==1],"\n")
          for(i in (1:ncols)[nsexes==1]) vals[1+i] <- vals[1+i]/2
        }

        if(name %in% c("Q","Q_calc")){
          Calc_Q <- aggregate(Calc_Q ~ name+FleetNum,data=indices,FUN=mean)
          cat("\n")
          fleetvec <- sort(as.numeric(unique(Calc_Q$FleetNum)))
          vals <- data.frame(matrix(NA,nrow=length(fleetvec),ncol=ncol(bigtable)))
          names(vals) <- names(bigtable)
          for(ifleet in 1:length(fleetvec)){
            f <- fleetvec[ifleet]
            vals[ifleet,1] <- paste("Q_calc_mean_fleet_",f,sep="")
            vals[ifleet,-1] <- Calc_Q$Calc_Q[Calc_Q$FleetNum==f]
          }
        }
        if(verbose) cat("added ",nrow(vals)," row",ifelse(nrow(vals)!=1,"s",""),"\n",sep="")
        if(!is.null(digits)){
          if(verbose) cat("rounded to",digit,"digits\n")
          vals[,-1] <- round(vals[,-1],digit)
        }
        # add to table
        tab <- rbind(tab, vals)

      } # end if not blank
    } # end loop over names
  } # end if not mcmc

  if(mcmc) {
    nnames <- length(names)
    for(iname in 1:nnames){
      name <- names[iname]
      if(!is.null(digits)) digit <- digits[iname]
      if(verbose) cat("name=",name,": ",sep="")
      if(name=="BLANK"){
        if(verbose) cat("added a blank row to the table\n")
        # add to table
        tab <- rbind(tab, " ")
      }else{

        vals <- as.data.frame(matrix(NA,ncol=ncols+1,nrow=1))
        vals[1] <- name
        for(imodel in models) {   ###loop over models and create a vector of medians to put into tab
          mcmcTable <- summaryoutput$mcmc[[imodel]]
          # get values
          tmp <- mcmcTable[,grep(name, names(mcmcTable))]  #for future functionality grabbing more than one column
          #        cat("labels found: ",names(mcmcTable)[grep(name, names(mcmcTable))],"\n")
          if(!is.null(dim(tmp))){
            if(ncol(tmp)>0)
              stop("This only works with a single column from the mcmc. Use a specific name")
          }
          if(!is.null(dim(tmp)) && ncol(tmp)==0){
            vals[1,imodel+1] <- NA
          }else{
            vals[1,imodel+1] <- median(tmp)  #First element is label
          }
        }
        # fix scale on a few things
        if(name %in% c("SR_LN(R0)","SR_LN.R0.","SR_R0","R0")){
          vals[1,-1] <- round(exp(vals[1,-1])/1e6,6)
          vals[1,1] <- "R0_billions"
        }
        if(substring(name,1,4)=="Recr") {
          vals[1,-1] <- round(vals[1,-1]/1e6,6)
          vals[1,1] <-paste(vals[1,1],"billions",sep="_")
        }
        if(substring(name,1,3)%in%c("SPB","SSB") | substring(name,1,8)=="TotYield") {
          vals[1,-1] <- round(vals[1,-1]/1e3,3)
          vals[1,1] <-paste(vals[1,1],"thousand_mt",sep="_")
        }
        ## if(name=="SPB_Virg"){
        ##   vals[1,-1] <- as.numeric(vals[1,-1])/1e3
        ##   vals[1,1] <- "SB0_thousand_mt"
        ## }
        if(((length(grep("SPB",name))>0  | length(grep("SSB",name))>0) & any(nsexes==1))){
          cat("dividing name by 2 for single-sex models:",(1:ncols)[nsexes==1],"\n")
          for(i in (1:ncols)[nsexes==1]) vals[1,1+i] <- vals[1,1+i]/2
          print(vals)
        }
        if(!is.null(digits)){
          if(verbose) cat("rounded to",digit,"digits\n")
          vals[,-1] <- round(vals[,-1],digit)
        }

        if(verbose) cat("added an mcmc row\n")
        # add to table
        tab <- rbind(tab, vals)

      } # end if not blank
    } # end loop over names
  } # end if mcmc

  names(tab) <- c("Label",modelnames)
  rownames(tab) <- 1:nrow(tab)

  if(csv){
    if(csvdir=="workingdirectory") csvdir <- getwd()
    fullpath <- paste(csvdir,csvfile,sep="/")
    cat("writing table to:\n  ",fullpath,"\n")
    write.csv(tab,fullpath,row.names=FALSE)
  }
  return(tab)
}
