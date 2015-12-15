#' Write length.fit file to be used by the MFCL length-comp viewer.
#' 
#' Writes files in the format used by the MFCL length-composition viewer.
#' Inspired by Simon Hoyle's demonstration. Still needs work.
#' 
#' 
#' @param replist List created by \code{SS_output}
#' @param outfile Name of file to create.
#' @param compfile SS output file with composition data info.
#' @param dir Directory where stuff happens. Defaults to directory where model
#' was run.
#' @param overwrite Overwrite existing file?
#' @param verbose More verbose info on progress of the function?
#' @author Ian Taylor
#' @export
#' @references \url{http://www.multifan-cl.org/},
#' \url{http://www.spc.int/OceanFish/en/ofpsection/sam/research/272-mfcl-viewer}
#' @keywords data manip
SS_write_length.fit <- function(replist=NULL,
                               outfile="length.fit",
                               compfile="CompReport.sso",
                               dir="default",
                               overwrite=FALSE,
                               verbose=TRUE){
  if(verbose) cat("This is a not very generalized function to write files of SS length comps\n",
                  "that can be read by the MULTIFAN-CL Viewer created by SPC.\n")
  
  on.exit({if(sink.number()>0) sink()})
  if(is.null(replist)) stop("The input 'replist' should refer to an R object created by the function 'SS_output'.")
  if(dir=="default") dir <- replist$inputs$dir

  setwd(dir)
  
  # read composition output since some info was filtered in the SS_output function
  if(!file.exists(compfile)) stop("Missing ",compfile,". Change the compfile input or rerun model to get the file.\n",sep="")

  comphead <- readLines(con=compfile,n=30)
  compskip <- grep("Composition_Database",comphead)
  col.names <- 1:30
  rawcompdbase <- read.table(file=compfile, col.names=col.names, fill=TRUE, colClasses="character", skip=compskip, nrows=-1)
  names(rawcompdbase) <- rawcompdbase[1,]
  names(rawcompdbase)[names(rawcompdbase)=="Used?"] <- "Used"
  compdbase <- rawcompdbase[2:(nrow(rawcompdbase)-4),] # subtract header line and last 4 lines
  ALK.f <- replist$ALK[,,1] # length-at-age matrix (for females only at this point)
  ALK.f <- t(ALK.f[nrow(ALK.f):1,])

  # get some quantities from replist
  lbins <- replist$lbins # data length bins
  nlbins <- replist$nlbins
  lbinspop <- replist$lbinspop # population length bins
  nlbinspop <- replist$nlbinspop
  nfleets <- replist$nfleets # fleets
  nages <- replist$accuage+1 # ages

  # process mapping of population length bins to data length bins
  len_len <- matrix(0,nlbinspop,nlbins)
  len_len[which(lbinspop < lbins[2]),1] <- 1
  for(icol in 2:(nlbins-1)){
    len_len[which(lbinspop >= lbins[icol] & lbinspop < lbins[icol+1]),icol] <- 1
  }
  len_len[which(lbinspop >= lbins[nlbins]),nlbins] <- 1
  
  # calculate number of compositions vectors for each fleet
  rowsbyfleet <- rep(0,nfleets)
  for(ifleet in 1:nfleets){
    rowsbyfleet[ifleet] <- sum(compdbase$Fleet==ifleet &
                               compdbase$Gender==1 &
                               compdbase$Kind=="LEN" &
                               compdbase$Bin=="")
  }
  if(file.exists(outfile)){
    if(!overwrite){
      cat("File exists and input 'overwrite'=FALSE:",outfile,"\n")
      return()
    }else{
      file.remove(outfile)
    }
  }
  cat("writing to file: ",dir,"/",outfile,"\n",sep="")

  printwarning <- FALSE
  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  options(width=5000,max.print=9999999)
  
  zz <- file(outfile, open="at")
  sink(zz)
 
  cat(" 0 0 20 0 0 0 0 0 0 0\n")
  cat(nlbins, lbins[1], diff(lbins[1:2]),"\n") # (no. length bins, "nbins") (lower length of 1st bin) (width of bins)
  cat(nfleets+1,"\n")               # (no. fisheries + 1)
  cat(rowsbyfleet,nfleets,"\n")     # (no. samples in fishery 1) ... (no. samples in last fishery) (no. fisheries)
  cat(nages,"\n")       # (no. ages) In SS this is accumulator age + 1 for age 0 (no age 0 in MFCL)
  for(ifleet in 1:nfleets){
  #for(ifleet in 1){
    cat("# fishery",ifleet,"\n")
    lendbase.f <- replist$lendbase[replist$lendbase$Fleet==ifleet,]
    yrs <- sort(unique(lendbase.f$Yr))
    for(y in yrs){
      lendbase.fy <- lendbase.f[lendbase.f$Yr==y,]
      seasons <- sort(unique(lendbase.fy$Seas))
      for(s in seasons){
        lendbase.fys <- lendbase.fy[lendbase.fy$Seas==s,]
        month <- round(12*replist$seasfracs)[s]
        week <- 1
        cat(y, month, week, "\n")                                        # (year from start) (month) (week)
        # subset for females only at this point
        lendbase.fys <- lendbase.fy[lendbase.fy$Seas==s & lendbase.fy$Gender==1,]
        # for some reason, the mean length in the next line needs to get divided by 2
        cat(0.5*replist$endgrowth$Len_Mid[replist$endgrowth$Gender==1],"\n") # (scaled mean length at age 1) . . . . (scaled mean length at age nages)
        cat("1\n")                                                       # (sum of observed frequencies in next record)
        lenobs <- 0*lbins
        lenexp <- 0*lbins
        for(irow in 1:nrow(lendbase.fys)){
          lbin <- lendbase.fys$Bin[irow]
          ibin <- which(lbins==lbin)
          lenobs[ibin] <- lendbase.fys$Obs[irow]
          lenexp[ibin] <- lendbase.fys$Exp[irow]
        }
        # renormalizing
        if(sum(lenexp)!=1 | sum(lenobs)!=1) printwarning <- TRUE
        lenobs <- lenobs/sum(lenobs)
        lenexp <- lenexp/sum(lenexp)
        
        cat(lenobs,"\n") # observed length comp
        cat(lenexp,"\n") # expected length comp
        #cat("\n") # blank line
        a <- replist$fleet_area[ifleet]

        #####################
        # to calculate contributions to expected numbers at length for each age
        # requires the numbers at age for the right year, the matrix of length-at-age
        # and the age- and length-based selectivities
        #####################
        
        # numbers at age for fleet, year, and seas
        natage.fys <- replist$natage[replist$natage$Yr==y &
                                    replist$natage$Seas==s &
                                    replist$natage$Morph==1 & # females only at this point
                                    replist$natage$Area==a &
                                    replist$natage$"Beg/Mid"=="M", -(1:11)]
        # selectivity at age for fleet
        agesel.fys <- replist$ageselex[replist$ageselex$fleet==ifleet &
                                       replist$ageselex$gender==1 & # females only at this point
                                       replist$ageselex$factor=="Asel",]
        # subset for representative year and remove non-data columns
        agesel.fys <- agesel.fys[agesel.fys$year==max(agesel.fys$year[agesel.fys$year<=y]),-(1:7)]

        # selectivity at length for fleet
        lensel.fys <- replist$sizeselex[replist$sizeselex$Fleet==ifleet &
                                        replist$sizeselex$gender==1 & # females only at this point
                                        replist$sizeselex$Factor=="Lsel",]
        # subset for representative year and remove non-data columns
        lensel.fys <- lensel.fys[lensel.fys$year==max(lensel.fys$year[lensel.fys$year<=y]),-(1:5)]

        # selected numbers at age (from age-based selectivity only)
        natage.fys.sel <- as.numeric(agesel.fys)*as.numeric(natage.fys)
        natage.fys.sel <- natage.fys.sel/sum(natage.fys.sel) # renormalized

        # reformulate quantities from above as identical-sized matrices
        lensel.fys.matrix <- matrix(as.numeric(lensel.fys), nrow=nages, ncol=nlbinspop, byrow=TRUE)
        natage.fys.matrix <- matrix(as.numeric(natage.fys.sel), nrow=nages, ncol=nlbinspop, byrow=FALSE)

        # matrix of length comp by population length bin (for each cohort)
        natlen.fys.matrix <- ALK.f * natage.fys.matrix

        # apply length-based selectivity
        natagelen.fys.sel <- lensel.fys.matrix * natlen.fys.matrix
        natagelen.fys.sel <- natagelen.fys.sel/sum(natagelen.fys.sel) # renormalized

        #for debugging
        if(FALSE){
          sink()
          close(zz)
          return(list(ALK.f=ALK.f,
                      agesel.fys=agesel.fys,
                      lensel.fys.matrix=lensel.fys.matrix,
                      natage.fys.matrix=natage.fys.matrix,
                      natagelen.fys.sel=natagelen.fys.sel))
        }
        
        # convert to matrix of length comp by data length bin, round to 4 digits, make into data.frame
        natagelen.fys.sel <- as.data.frame(round(natagelen.fys.sel %*% len_len,4))
        
        rownames(natagelen.fys.sel) <- 1:nrow(natagelen.fys.sel)
        names(natagelen.fys.sel) <- 1:ncol(natagelen.fys.sel)

        ### remove scientific notation, give empty rownames and columns
        names(natagelen.fys.sel)[1] <- paste("#_",names(natagelen.fys.sel)[1],sep="")
        print(natagelen.fys.sel, row.names=FALSE, strip.white=TRUE)
        #format(natagelen.fys.sel, scientific=FALSE)
        cat("\n") # blank line
        
      } # end loop over seasons
    } # end loop over years
  } # end loop over fleets

  # fishery totals
  cat("# fishery totals\n")
  natagelen.fys.sel <- 0*natagelen.fys.sel
  for(ifleet in 1:nfleets){
    cat(ifleet, 1, 1, "\n")
    cat(rep(-1,nages),"\n")
    cat("1\n")
    lenobs <- 0*lbins
    lenexp <- 0*lbins
    cat(lenobs,"\n") # observed length comp
    cat(lenexp,"\n") # expected length comp
    print(natagelen.fys.sel, row.names=FALSE, strip.white=TRUE)
  }

  options(width=oldwidth,max.print=oldmax.print)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
  if(printwarning) cat("note: female length comps were normalized, masking differences in obs. & exp. sex-ratios\n")
} # end function
