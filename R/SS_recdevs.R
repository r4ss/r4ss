#' Insert a vector of recruitment deviations into the control file.
#'
#' A function to insert a vector of recruitment deviations into the control
#' file for simulation studies. This function was written in 2010, long before
#' the functions to read and write the input files were created. An alternative
#' approach would be to read the control file, add the recdevs, and then write
#' it again, but this function still works so there's no immediate need to
#' streamline that alternative approach.
#'
#' @param fyr First year of the recdev vector.
#' @param lyr Last year of the recdev vector.
#' @param ctl Either NULL to read anew or an already read control file.
#' Default=NULL.
#' @param recdevs Either NULL to generate anew or an already generated vector
#' of recdevs. Default=NULL.
#' @param rescale Should the recdevs be rescaled to have mean = 0 and std.
#' deviation = sigmaR? Default=TRUE.
#' @param scaleyrs Vector of years over which rescaling (if chosen) should
#' occur.
#' @template dir
#' @param ctlfile Name of control file to modify.  Default="control.ss_new".
#' @param newctlfile Name of new file to output modified control file.
#' Default="control_modified.ss".
#' @template verbose
#' @param writectl Write new file? Default=TRUE.
#' @param returnctl Return contents ctl file as an object in the R workspace.
#' Default=FALSE.
#' @param newmaxbias Replace the maximum bias adjustment fraction with any
#' non-NULL value. Default=NULL.
#' @author Ian Taylor
#' @export
SS_recdevs <-
  function(fyr, lyr, ctl = NULL, recdevs = NULL,
           rescale = TRUE, scaleyrs = NULL,
           dir = getwd(),
           ctlfile = "control.ss_new",
           newctlfile = "control_modified.ss",
           verbose = TRUE, writectl = TRUE, returnctl = FALSE,
           newmaxbias = NULL) {
    # Determine working directory on start and return upon exit
    current_wd <- getwd()
    on.exit(setwd(current_wd))
    setwd(dir)

    # define a general function for reading values from control file
    readfun <- function(string, maxlen = Inf) {
      line1 <- grep(string, ctl)
      if (length(line1) < 1) stop("no line contains the phrase, '", string, "'", sep = "")
      if (length(line1) > 1) stop("more than one line contains the phrase, '", string, "'", sep = "")

      # split parameter line at hash mark
      splitline <- strsplit(ctl[line1], "#")[[1]]
      # split along all blank spaces, including tabs
      vecstrings <- strsplit(splitline[1], "[[:blank:]]+")[[1]]
      # convert to numeric
      vec <- as.numeric(vecstrings[vecstrings != ""])
      # check for length
      if (length(vec) > maxlen) {
        stop(paste("this line has more than ", maxlen, " value", c("s", "")[1 + (maxlen == 1)], ": ", ctl[line1], sep = ""))
      }
      return(vec)
    } # end readfun

    # read control file if ctl is not supplied
    if (is.null(ctl)) ctl <- readLines(ctlfile)

    # get sigma R
    sigmaR <- readfun("SR_sigmaR")[3]

    # make sure model includes recdevs and get some information
    do_recdev <- readfun("do_recdev", maxlen = 1)
    if (do_recdev == 0) {
      stop("do_recdev should be set to 1 or 2")
    }
    yrs <- fyr:lyr
    Nrecdevs <- lyr - fyr + 1
    phase <- readfun("recdev phase", maxlen = 1)
    advanced <- readfun("advanced options", maxlen = 1)
    if (advanced != 1) {
      stop("advanced options must be turned on in control file")
    }
    if (phase > 0) {
      newphase <- -abs(phase)
      if (verbose) {
        message("Changing recdev phase to negative: ", newphase)
      }
      ctl[grep("recdev phase", ctl)] <- paste(newphase, "#_recdev phase")
    }

    # turn on read_recdevs
    key1 <- grep("read_recdevs", ctl)
    ctl[key1] <- paste(Nrecdevs, "#_read_recdevs")

    # check for keyword at start of following section
    key2 <- grep("Fishing Mortality info", ctl)
    if (length(key2) == 0) {
      warning(
        "The phrase 'Fishing Mortality info' does not occur after the\n",
        "recdev section; Format of control file may be messy."
      )
    } else {
      key2 == key2[1]
    }

    # generate new recdevs
    if (!is.null(recdevs)) {
      if (length(recdevs) != Nrecdevs) {
        stop(paste("input 'recdevs' has length=", length(recdevs), " but Nrecdevs=lyr-fyr+1=", Nrecdevs, sep = ""))
      } else {
        newdevs <- recdevs
      }
    } else {
      newdevs <- rnorm(n = Nrecdevs)
    }
    if (rescale) {
      if (is.null(scaleyrs)) {
        scaleyrs <- yrs %in% yrs
      } else {
        scaleyrs <- yrs %in% scaleyrs
      }
      if (verbose) {
        message(
          "Rescaling recdevs vector so yrs ", min(yrs[scaleyrs]), ":",
          max(yrs[scaleyrs]), " have mean 0 and std. dev. = sigmaR = ", sigmaR
        )
      }
      newdevs <- sigmaR * (newdevs - mean(newdevs[scaleyrs])) / sd(newdevs[scaleyrs])
    }
    # build new recdev section
    newsection <- c(
      "#_end of advanced SR options",
      "#",
      "# read specified recr devs",
      "#_Yr Input_value"
    )
    # newsection <-c(newsection, rep("",(key2-key1-1)-length(newsection))) # preserve length of file

    for (i in 1:Nrecdevs) {
      newsection[4 + i] <-
        paste((fyr:lyr)[i], c("  ", " ")[1 + (newdevs[i] < 0)], newdevs[i], " #_stochastic_recdev_with_sigmaR=", sigmaR, sep = "")
    }

    ctl <- c(ctl[1:key1], newsection, ctl[key2:length(ctl)])
    # ctl[(key1+1):(key2-1)] <- newsection

    # if maxbias is input, then replace
    if (!is.null(newmaxbias)) ctl[grep("max_bias", ctl)] <- paste(newmaxbias, "#_max_bias_adj_in_MPD")

    # write and/or return the modified control file
    if (writectl) {
      writeLines(ctl, newctlfile)
      if (verbose) {
        message("Wrote new file: ", newctlfile)
      }
    }
    # reset working directory
    setwd(current_wd)
    if (returnctl) {
      return(ctl)
    }
  } # end function
