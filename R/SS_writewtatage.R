#' Write weight-at-age file
#'
#' Write Stock Synthesis weight-at-age file from R object that was probably
#' created using [SS_readwtatage()]
#'
#' @param mylist Object created by [SS_readwtatage()].
#' @template dir
#' @param file Filename for new weight-at-age file, which
#' will be appended to `dir` to create a full file path.
#' Default="wtatage.ss".
#' @template overwrite
#' @template verbose
#' @param warn Deprecated.
#' @author Kelli F. Johnson
#' @export
#' @seealso [SS_readwtatage()]
#'
SS_writewtatage <- function(
  mylist,
  dir = NULL,
  file = "wtatage.ss",
  overwrite = FALSE,
  verbose = TRUE,
  warn = lifecycle::deprecated()
) {
  if (verbose) {
    message("running SS_writewtatage\n")
  }

  if (lifecycle::is_present(warn)) {
    lifecycle::deprecate_warn(
      when = "1.45.0",
      what = "SS_writewtatage(warn)"
    )
  }
  # Prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  on.exit({
    if (sink.number() > 0) sink()
  })

  if (is.null(dir)) {
    dir <- getwd()
  } # set to working directory if no input provided
  if (grepl("/$", dir)) {
    outfile <- paste0(dir, file) # bc trailing backslash
  } else {
    outfile <- file.path(dir, file)
  }
  if (file.exists(outfile)) {
    if (!overwrite) {
      stop("file exists:", outfile, "\n  set overwrite=TRUE to replace\n")
    } else {
      file.remove(outfile)
    }
  } else {
    if (verbose) message("writing new file:", outfile, "\n")
  }

  # record current max characters per line and then expand in case of long lines
  oldwidth <- options()[["width"]]
  options(width = 1000)

  if (verbose) {
    message("opening connection to ", outfile, "\n")
  }
  zz <- file(outfile, open = "at")
  on.exit(close(zz))
  sink(zz, split = verbose)

  writeLines(paste(NCOL(mylist) - 7, "# maxage"))
  writeLines(
    "# if Yr is negative, then fill remaining years for that Seas, growpattern, Bio_Pattern, Fleet"
  )
  writeLines(
    "# if season is negative, then fill remaining fleets for that Seas, Bio_Pattern, Sex, Fleet"
  )
  writeLines("# will fill through forecast years, so be careful")
  writeLines("# fleet 0 contains begin season pop WT")
  writeLines("# fleet -1 contains mid season pop WT")
  writeLines("# fleet -2 contains maturity*fecundity")

  # Check for terminal line in data frame
  mylist <- mylist[
    order(mylist[["year"]], mylist[["fleet"]], mylist[["seas"]]),
  ]
  if (any(mylist[["year"]] < -9998)) {
    mylist <- mylist[
      c(
        which(mylist[["year"]] >= -9998),
        which(mylist[["year"]] < -9998)
      ),
    ]
  } else {
    mylist <- rbind(mylist, mylist[1, ])
    mylist[NROW(mylist), "year"] <- -9999
  }
  colnames(mylist)[1] <- paste0("#", colnames(mylist)[1])
  print.data.frame(
    mylist,
    row.names = FALSE,
    strip.white = TRUE,
    max = dim(mylist)[1] * dim(mylist)[2]
  )

  # restore printing width to whatever the user had before
  options(width = oldwidth)
  sink()
  if (verbose) message("file written to", outfile, "\n")
}
