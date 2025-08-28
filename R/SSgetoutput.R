#' Get output from multiple Stock Synthesis models.
#'
#' Apply the function [SS_output()] multiple times and save output as
#' individual objects or a list of lists.
#'
#'
#' @param keyvec A vector of strings that are appended to the output files from
#' each model if models are all in one directory. Default=NULL.
#' @param dirvec A vector of directories (full path or relative to working
#' directory) in which model output is located. Default=NULL.
#' @param getcovar Choice to read or not read covar.sso output (saves time and
#' memory). Default=TRUE.
#' @param getcomp Choice to read or not read CompReport.sso output (saves time
#' and memory). Default=TRUE.
#' @param forecast Choice to read or not read forecast quantities.
#' Default=FALSE.
#' @template verbose
#' @param listlists Deprecated argument that wasn't working.
#' @param underscore Add an underscore '_' between any file names and any keys
#' in keyvec. Default=FALSE.
#' @param save.lists Save each list of parsed output as a .Rdata file (with default
#' filenaming convention based on iteration and date stamp.
#' @inheritParams SS_output
#' @author Ian Taylor
#' @export
#' @seealso [SS_output()]
#' @family model comparison functions
#' @examples
#' # contrived example where the same model is read twice
#' mydir <- file.path(
#'   path.package("r4ss"),
#'   file.path("extdata", "simple_small")
#' )
#' models <- SSgetoutput(
#'   dirvec = c(mydir, mydir),
#'   modelnames = c("simple_small", "simple_small_again")
#' )
SSgetoutput <-
  function(
      keyvec = NULL,
      dirvec = NULL,
      getcovar = TRUE,
      getcomp = TRUE,
      forecast = TRUE,
      verbose = TRUE,
      listlists = lifecycle::deprecated(),
      underscore = FALSE,
      save.lists = FALSE,
      SpawnOutputLabel = "Spawning output",
      modelnames = NULL) {
    if (lifecycle::is_present(listlists)) {
      lifecycle::deprecate_stop(
        when = "1.52.1",
        what = "SSgetoutput(listlists)"
      )
    }

    if (verbose) {
      if (!is.null(keyvec)) {
        message("length(keyvec) as input to SSgetoutput: ", length(keyvec))
      }
      if (!is.null(dirvec)) {
        message("length(dirvec) as input to SSgetoutput: ", length(dirvec))
      }
    }

    # change inputs so that keyvec and dirvec have matching lengths or keyvec=NULL
    biglist <- list()
    n1 <- length(keyvec)
    n2 <- length(dirvec)
    if (n1 > 1 & n2 > 1 & n1 != n2) {
      message("inputs 'keyvec' and 'dirvec' have unmatched lengths > 1")
    } else {
      n <- max(1, n1, n2) # n=1 or n=length of either optional input vector
    }

    # apply optional input of modelnames
    if (!is.null(modelnames)) {
      if (length(modelnames) == n) {
        objectnames <- modelnames
      } else {
        cli::cli_abort(
          "length of modelnames input ({length(modelnames)}) does not match number of models ({n})"
        )
      }
    }

    # if keyvec is length 1, recycled to length n
    if (n1 == 1) {
      keyvec <- rep(keyvec, n)
    }
    # default model names if not provided by user
    if (is.null(modelnames)) {
      objectnames <- paste("replist", keyvec, sep = "")
      if (n1 == 0) {
        # if keyvec is NULL, use sequential numbering
        objectnames <- paste("replist", 1:n, sep = "")
      }
    }

    # working directory is defult if dirvec not provided
    if (n2 == 0) {
      dirvec <- getwd()
    }
    if (length(dirvec) == 1) {
      dirvec <- rep(dirvec, n)
    }
    dirvec <- paste(dirvec, "/", sep = "")

    # loop over directories or key strings
    for (i in 1:n) {
      key <- keyvec[i]
      mydir <- dirvec[i]
      if (is.null(key)) {
        key2 <- NULL
      } else {
        key2 <- ifelse(underscore, paste("_", key, sep = ""), key)
      }
      newobject <- objectnames[i]

      if (verbose & !is.null(key)) {
        message("getting files with key =", key)
      }

      repFileName <- paste("Report", key2, ".sso", sep = "")
      covarname <- paste("covar", key2, ".sso", sep = "")
      warnFileName <- paste("warning", key2, ".sso", sep = "")
      if (getcomp) {
        compFileName <- paste("CompReport", key2, ".sso", sep = "")
        NoCompOK <- FALSE
      } else {
        compFileName <- NULL
        NoCompOK <- TRUE
      }

      # mycovar = TRUE/FALSE based on presence of file and user input
      mycovar <- file.exists(file.path(mydir, covarname)) & getcovar

      fullfile <- paste(mydir, repFileName, sep = "")
      if (verbose) {
        message("reading output from ", fullfile)
      }
      repfilesize <- file.info(fullfile)[["size"]]

      output <- NA
      if (!is.na(repfilesize) && repfilesize > 0) {
        # if there's a non-empty file
        output <- SS_output(
          dir = mydir,
          repfile = repFileName,
          covarfile = covarname,
          compfile = compFileName,
          NoCompOK = NoCompOK,
          warnfile = warnFileName,
          printstats = FALSE,
          covar = mycovar,
          forecast = forecast,
          verbose = FALSE,
          SpawnOutputLabel = SpawnOutputLabel
        )
        if (is.null(output)) {
          # for some reason covarfile exists, but is old so SS_output rejects
          message("output==NULL so trying again with covar=FALSE")
          output <- SS_output(
            dir = mydir,
            repfile = repFileName,
            covarfile = covarname,
            compfile = compFileName,
            NoCompOK = NoCompOK,
            printstats = FALSE,
            covar = FALSE,
            forecast = forecast,
            verbose = FALSE,
            SpawnOutputLabel = SpawnOutputLabel
          )
        }
        output[["key"]] <- as.character(key)
      } else {
        message("!repfile doesn't exists or is empty")
      }
      if (verbose) {
        message("added element '", newobject, "' to list")
      }
      biglist[[newobject]] <- output

      # IGT (Aug 2025): not clear if the option to save Rdata files is useful
      if (save.lists) {
        biglist.file <- paste(
          "biglist",
          i,
          "_",
          format(Sys.time(), "%d-%b-%Y_%H.%M"),
          ".Rdata",
          sep = ""
        )
        save(biglist, file = biglist.file)
      }
    } # end loop over models

    return(invisible(biglist))
  }
