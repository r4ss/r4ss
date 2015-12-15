##' Convert a matrix of natural mortality values into
##' inputs for Stock Synthesis
##'
##' Inspired by Valerio Bartolino and North Sea herring
##'
##' @param  mat             a matrix of natural mortality by year and age, starting with age 0
##' @param  startyr         the first year of the natural mortality values (no missing years)
##' @param  outfile         optional file to which the results will be written
##' @param  overwrite       if 'outfile' is provided and exists, option to overwrite or not
##' @param  yrs.in.columns  an indicator of whether the matrix has years in columns or rows
##' @return Prints inputs with option to write to chosen file
##' @author Ian Taylor
##' @export
SSmakeMmatrix <- function(mat, startyr, outfile=NULL, 
                          overwrite=FALSE, yrs.in.columns=TRUE){
  # A function for converting a matrix of natural mortality values
  # into inputs for Stock Synthesis
  #
  # Inspired by Valerio Bartolino and North Sea herring
  #
  # inputs are:
  #   mat             a matrix of natural mortality by year and age, starting with age 0
  #   startyr         the first year of the natural mortality values (no missing years)
  #   outfile         optional file to which the results will be written
  #   overwrite       if 'outfile' is provided and exists, option to overwrite or not
  #   yrs.in.columns  an indicator of whether the matrix has years in columns or rows

  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})

  # check for existing file
  if(!is.null(outfile) && file.exists(outfile)){
    if(!overwrite){
      cat("File exists and input 'overwrite'=FALSE:",outfile,"\n")
      return()
    }else{
      file.remove(outfile)
    }
  }

  printdf <- function(dataframe){
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    print(dataframe, row.names=FALSE, strip.white=TRUE)
  }
  
  # temporarily change the number of characters per line for printing to R console
  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  options(width=5000,max.print=9999999)

  # open file connection if requested
  if(!is.null(outfile)){
    cat("opening connection to",outfile,"\n")
    zz <- file(outfile, open="at")
    sink(zz)
  }

  if(!yrs.in.columns) mat <- t(mat) # transpose as needed
  nyrs <- ncol(mat)                 # number of years
  yrs <- startyr + 1:nyrs - 1       # vector of years
  maxage <- nrow(mat) - 1           # maximum age (assuming first age=0)
  ages <- 0:maxage                  # vector of ages

  cat("#### Calculating inputs to Stock Synthesis for a matrix of natural mortality values",
      paste("\n#### over the range of ages:",min(ages),"to",maxage,"\n\n"))
  
  Msetup <- c("# three lines to paste near top of control file:\n",
              "1 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate\n",
              paste(maxage+1, "# N_natMparms for segmented approach (required when using natM_type=1)\n"),
              paste(paste(ages,collapse=" "), "# NatM_breakages (required when using natM_type=1)\n"))

  cat(Msetup)

  # create data frame of parameter lines
  HI <- ceiling(max(mat)*10)/10
  Mparams <- data.frame(LO = 0,           # lower bound
                        HI = HI,          # upper bound
                        INIT = mat[,1],   # initial values are first column of matrix
                        
                        # the prior stuff doesn't matter for fixed parameters
                        PRIOR = 0.2,      # prior values
                        PR_type = -1,     # uniform prior
                        SD = 1,           # standard deviation of the prior
                        PHASE = -99,      # parameter phase
                        env_var = -(ages+1), # index of environmental variable

                        # remaining options are not used
                        use_dev   = 0,  # 
                        dev_minyr = 0,  # 
                        dev_maxyr = 0,  #
                        dev_stddev= 0,  #
                        Block     = 0,  #
                        Block_Fxn = 0)  #

  # add some comments
  Mparams$comment <- paste("# M parameter for age",ages)
  Mparams$comment[maxage+1] <- paste(Mparams$comment[maxage+1],"+",sep="")

  cat("\n# stuff to paste into the first block of parameter lines\n")
  printdf(Mparams)

  # create data frame of environmental link parameters
  Mlinks <- data.frame(LO = 0,       # lower bound
                       HI = 2,       # upper bound
                       INIT = 1,     # initial values are first column of matrix
                       
                       # the prior stuff doesn't matter for fixed parameters
                       PRIOR = 1,    # prior values
                       PR_type = -1, # uniform prior
                       SD = 1,       # standard deviation of the prior
                       PHASE = -99,  # parameter phase
                       comment = paste("# M env. link parameter for age",ages),
                       stringsAsFactors = FALSE)

  # modify final comment to make clear as a plus group
  Mlinks$comment[maxage+1] <- paste(Mlinks$comment[maxage+1],"+",sep="")

  cat("\n# stuff to paste below the line labeled 'CohortGrowDev'\n")
  cat("1 #_custom mortality/growth environmental setup\n")
  printdf(Mlinks)
  
  # create a data frame of environmental variables
  Menv <- NULL
  for(a in ages){
    index <- a+1
    Mvals <- as.numeric(mat[index,])     # get row for each age
    Mscaled <- Mvals - Mvals[1] # rescale to subtract M for first year
    temp <- data.frame(Yr=yrs,
                       Variable=index,
                       Value=Mscaled,
                       comment=paste("# Env. index for time-varying M at age",a))
    if(a==maxage) temp$comment <- paste(temp$comment,"+",sep="")
    Menv <- rbind(Menv, temp) # paste into data.frame
  }

  cat("\n# Environmental variables to paste into the bottom of the data file\n")

  cat(paste(maxage+1, "# N environmental variables\n"))
  cat(paste(nrow(Menv), "# N environmental observations\n"))
  
  printdf(Menv)

  # restore things to how they were
  options(width=oldwidth,max.print=oldmax.print)
  if(!is.null(outfile)){
    sink()
    close(zz)
  }
  if(!is.null(outfile)) cat("file written to",outfile,"\n")
}
