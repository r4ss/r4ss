#' read control file
#' 
#' read Stock Synthesis control file into list object in R
#' 
#' This function is not fully implemented. The logic to figure out all the
#' details of a Stock Synthesis control file is very complex, so this function
#' may be completed in a way that is not totally consistent with the other
#' similar files. Or it may never be completed at all. The functions
#' \code{\link{SS_changepars}} and \code{\link{SS_parlines}} offer alternatives
#' for working with SS control files.
#' 
#' @param file Filename either with full path or relative to working directory.
#' @seealso \code{\link{SS_changepars}}, \code{\link{SS_parlines}},
#' \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_readdat}}, \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}},
#' \code{\link{SS_writectl}}
#' @author Ian Taylor
#' @export
#' @keywords data
SS_readctl <- function(file){
  cat("Warning!\n",
      "  SS_readctl is not fully implemented. The logic to figure out\n",
      "  all the details of a Stock Synthesis control file is very complex,\n",
      "  so this function may be completed in a way that is not totally\n",
      "  consistent with the other similar files. Or it may never be\n",
      "  completed at all. The functions 'SS_changepars' and 'SS_parlines'\n",
      "  offer alternatives for working with SS control files.\n")

  ctl <- readLines(file,warn=FALSE)

  if(strsplit(ctl[2]," ")[[1]][1]=="Start_time:") ctl <- ctl[-(1:2)]
  allnums <- NULL
  for(i in 1:length(ctl)){
      mysplit <- strsplit(ctl[i],split="[[:blank:]]+")[[1]]
      mysplit <- mysplit[mysplit!=""]
      nums <- suppressWarnings(as.numeric(mysplit))
      if(sum(is.na(nums)) > 0) maxcol <- min((1:length(nums))[is.na(nums)])-1
      else maxcol <- length(nums)
      if(maxcol > 0){
          nums <- nums[1:maxcol]
          allnums <- c(allnums, nums)
      }
  }
  i <- 1
  ctllist <- list()

  ctllist$sourcefile <- file
  ctllist$type <- "Stock_Synthesis_control_file"
  ctllist$SSversion <- "SSv3.10b_or_later"

  # model dimensions
  ctllist$N_Growth_Patterns <- N_Growth_Patterns <- allnums[i]; i <- i+1
  ctllist$N_Morphs_Within_GrowthPattern <- N_Morphs_Within_GrowthPattern <- allnums[i]; i <- i+1

  if(N_Growth_Patterns!=1 | N_Morphs_Within_GrowthPattern!=1){
    stop("Error! SS_readctl doesn't yet handle fancy models with growth patterns or morphs")
  }

  ctllist$zzz <- zzz <- allnums[i]; i <- i+1
  ctllist$Nblock_Patterns <- Nblock_Patterns <- allnums[i]; i <- i+1
  ctllist$fracfemale <- fracfemale <- allnums[i]; i <- i+1
  ctllist$natM_type <- natM_type <- allnums[i]; i <- i+1
  ctllist$GrowthModel <- GrowthModel <- allnums[i]; i <- i+1
  ctllist$Growth_Age_for_L1 <- Growth_Age_for_L1 <- allnums[i]; i <- i+1
  ctllist$Growth_Age_for_L2 <- Growth_Age_for_L2 <- allnums[i]; i <- i+1
  ctllist$SD_add_to_LAA <- SD_add_to_LAA <- allnums[i]; i <- i+1
  ctllist$CV_Growth_Pattern <- CV_Growth_Pattern <- allnums[i]; i <- i+1
  ctllist$maturity_option <- maturity_option <- allnums[i]; i <- i+1
  ctllist$First_Mature_Age <- First_Mature_Age <- allnums[i]; i <- i+1
  ctllist$fecundity_option <- fecundity_option <- allnums[i]; i <- i+1
  ctllist$hermaphroditism_option <- hermaphroditism_option <- allnums[i]; i <- i+1
  ctllist$parameter_offset_approach <- parameter_offset_approach <- allnums[i]; i <- i+1
  ctllist$adjust_method <- adjust_method <- allnums[i]; i <- i+1


  # note: needs some logic in here to determine number of rows
  Ncols <- 14
  Nrows <- 24 # temporary placeholder for Ian Taylor's model
  MGparms <- data.frame(matrix(
    allnums[i:(i+Nrows*Ncols-1)],nrow=Nrows,ncol=Ncols,byrow=TRUE))
  i <- i+Nrows*Ncols
  names(MGparms) <- c("LO","HI","INIT","PRIOR","PR_type","SD","PHASE","env_var","use_dev","dev_minyr","dev_maxyr","dev_stddev","Block","Block_Fxn")
  ctllist$MGparms <- MGparms

  N_effects <- 10 # need better logic to determine this
  ctllist$seasonal_effects <- allnums[i:(i+N_effects-1)]; i <- i+N_effects

  ctllist$SR_function <- SR_function <- allnums[i]; i <- i+1
  Ncols <- 7
  Nrows <- 6
  SRparms <- data.frame(matrix(
    allnums[i:(i+Nrows*Ncols-1)],nrow=Nrows,ncol=Ncols,byrow=TRUE))
  i <- i+Nrows*Ncols
  names(MGparms) <- c("LO","HI","INIT","PRIOR","PR_type","SD","PHASE")
  ctllist$MGparms <- MGparms

  ctllist$SR_evn_link <- SR_evn_link <- allnums[i]; i <- i+1
  ctllist$SR_evn_target <- SR_evn_target <- allnums[i]; i <- i+1
  ctllist$do_recdev <- do_recdev <- allnums[i]; i <- i+1
  ctllist$fyr_main_recdevs <- fyr_main_recdevs <- allnums[i]; i <- i+1
  ctllist$lyr_main_recdevs <- lyr_main_recdevs <- allnums[i]; i <- i+1
  ctllist$recdev_phase <- recdev_phase <- allnums[i]; i <- i+1

  # a bunch more to come...this is a work in progress

  # all done
  return(ctllist)
}
