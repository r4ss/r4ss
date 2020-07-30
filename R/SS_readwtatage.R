#' Read weight-at-age data file
#' 
#' Read in a weight-at-age data file into a data frame in R.
#' 
#' @param file A relative or full path to the weight-at-age file.
#' The default values is \code{wtatage.ss}.
#' @template verbose
#' @export
#' @return Returns a data frame with a variable number of columns based on the
#' number of ages that are included in the file. Though, the first columns
#' will always be Yr, Seas, Sex, Bio_Pattern, BirthSeas, and Fleet.
#' The seventh column will be age zero.
#' The last or next to last column will be the maximum age included
#' in the weight-at-age data. For SS version 3.30 and greater, the last column
#' will be a column of comments.
#' @author Kelli F. Johnson, Ian G. Taylor
#' 
SS_readwtatage <- function(file = "wtatage.ss", verbose=TRUE) {
  if(!file.exists(file) | file.info(file)$size==0) {
    if(verbose){
      message("Skipping weight-at-age file. File missing or empty: ", file)
    }
    return(NULL)
  }

  # read full file
  wtatagelines <- read.table(file,
                             header = FALSE,
                             comment.char = "#",
                             blank.lines.skip = TRUE,
                             stringsAsFactors = FALSE,
                             strip.white = TRUE,
                             fill = TRUE)
  # check for NA is first 2 rows of 2nd column as indicator of version
  # prior to 3.30 where old models had additional number of lines input
  skip <- 1
  if (all(is.na(wtatagelines[1:2,2])) & !is.na(wtatagelines[3,2])) {
    # number of lines input is not actually used in this function
    # but included here just to clarify what that first row contains
    Nlines <- wtatagelines[1,1] 
    skip <- 2
  }
  # get max age on 1st line for 3.30, 2nd line for 3.24
  accuage <- wtatagelines[skip, 1]
  # remainder of table skipping first 1 or 2 lines
  wtatage <- wtatagelines[-(1:skip),]
  # problems with header so simply manually replacing column names
  wtatage_names <- c("Yr", "Seas", "Sex", "Bio_Pattern", "BirthSeas", "Fleet",
                     0:accuage)
  # new comment line in 3.30
  if(ncol(wtatage)==length(wtatage_names)+1){
    wtatage_names <- c(wtatage_names, "comment")
  }
  names(wtatage) <- wtatage_names
  # Remove terminator line
  wtatage <- wtatage[wtatage$Yr > -9998, ]
  return(wtatage)
}
