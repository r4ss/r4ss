# Outline of function
#
# 1. Accepts same argument as for gdata::write.fwf
# 2. for each column vector
#    convert to vector of string conditional to if
#    a. numeric
#       apply signif
#       if an element x is  is.na(x) is TRUE
#         replace it with "NA"
#
#    b. charactor
#      leave as it is
# 3. Fill blank to make fixed width
#'
#' Function to write formatted table similar to
#' table written by gdata::write.fwf from data.frame or matrix
#' This function doesnot accept columns or logical with factor
#'
#' @param x data.frame or matrix the object to be written
#' @param file either a character string naming a file or a
#' connection open for writing. "" indicates output to the console.
#' @param append logical, append to existing data in `file`
#' @param quote logical, quote data in output
#' @param na character, the string to use for missing values i.e. `NA` in
#' the output
#' @param sep character, separator between columns in output
#' @param rownames logical, print row names
#' @param colnames logical, print column names
#' @param rowCol character, rownames column name
#' @param justify character, alignment of character columns; see
#' [format()]
#' @param width numeric, width of the columns in the output
#' @param eol the character(s) to print at the end of each line (row).  For
#' example, 'eol="\\r\\n"' will produce Windows' line endings on a Unix-alike OS,
#' and 'eol="\\r"' will produce files as expected by Mac OS Excel 2004.
#' @param qmethod a character string specifying how to deal with embedded
#' double quote characters when quoting strings.  Must be one of '"escape"'
#' (default), in which case the quote character is escaped in C style by a
#' backslash, or '"double"', in which case it is doubled.  You can specify just
#' the initial letter.
#' @param digits Used for signif
#' @param checkNA logical if TRUE, function will stop when NA is found
#' @param checkInfty logical if TRUE, function will stop when Infinity is found
#' @param checkError logical if TRUE both, set checkNA and checkInftr TRUE
#' @author Yukio Takeuchi
#' @export
#'
write_fwf4 <- function(x,
                       file = "",
                       append = FALSE,
                       quote = FALSE,
                       sep = " ",
                       na = "NA",
                       rownames = FALSE,
                       colnames = TRUE,
                       rowCol = NULL,
                       justify = "left",
                       width = NULL,
                       eol = "\n",
                       qmethod = c("escape", "double"),
                       digits = 6,
                       checkNA = TRUE,
                       checkInfty = TRUE,
                       checkError = TRUE) {
  # If input is a matrix, convert it to a data.frame
  if (is.matrix(x)) x <- as.data.frame(x)
  # if checkError is TRUE, make checkNA and checkInfty TRUE,
  # regarless of their inputs
  if (checkError) {
    checkNA <- TRUE
    checkInfty <- TRUE
  }

  nCol <- ncol(x)
  if (colnames) {
    colnames <-
      if (!is.null(colnames(x)) && length(colnames(x)) == nCol) {
        paste0(colnames(x), collapse = sep)
      } else {
        paste0("V", seq_len(nCol), collapse = sep)
      }
  }
  # Process by column
  for (i in seq_len(nCol)) {
    vect0 <- x[, i]
    if (!is.null(width) && length(width) != nCol) {
      if (length(width) == 1) {
        width1 <- width
      }
      if (length(width) != 1 & length(width) != nCol) {
        stop("length(width) must be 1 or ncol(x)")
      }
      width1 <- width[i]
    }
    #
    if (is.numeric(vect0)) {
      isNA <- is.na(vect0)
      if (any(isNA)) {
        warning(
          "Found NA in data.frame. Correspong inputs id\n",
          x[isNA, ]
        )
        # if (checkNA)
        #  stop("Found NA in data.frame")
        stopifnot("Found NA in data.frame" = checkNA == FALSE)
      }
      isInfty <- is.infinite(vect0)
      if (any(isInfty)) {
        warning(
          "Found Infinity in data.frame. Correspong inputs id\n",
          x[isInfty, ]
        )
        # if (checkInfty)
        #  stop("Found Infinity in data.frame")
        stopifnot("Found Infinity in data.frame" = checkInfty == FALSE)
      }
      vect1 <- vect0
      vect1[!isNA] <- signif(vect0[!isNA], digits = digits)
      vect1[isNA] <- na
      vect_char <- paste(vect1)
      if (is.null(width) || width1 < max(nchar(vect_char))) {
        width1 <- max(nchar(vect_char))
      }
      blank0 <- paste0(rep(" ", width1), collapse = "")
      vect_out <-
        paste0(substring(blank0, 1, width1 - nchar(vect_char)), vect_char)
    } else if (is.character(vect0)) {
      # if vect0 is not numeric
      vect_char <- paste(vect0)
      if (is.null(width) || width1 < max(nchar(vect_char))) {
        width1 <- max(nchar(vect_char))
      }
      blank0 <- paste0(rep(" ", width1), collapse = "")
      vect_out <-
        if (justify == "left") {
          paste0(vect_char, substring(blank0, 1, width1 - nchar(vect_char)))
        } else {
          paste0(substring(blank0, 1, width1 - nchar(vect_char)), vect_char)
        }
    } else {
      stop("x is neither numeric or character")
    }

    y <-
      if (i == 1) {
        vect_out
      } else {
        paste(y, vect_out, sep = sep)
      }
  }
  write.table(
    x = y,
    file = file,
    append = append,
    quote = quote,
    sep = "",
    eol = eol,
    na = "",
    row.names = FALSE,
    col.names = colnames,
    qmethod = qmethod
  )
  return(invisible(y))
}

#---------------------------------------------------------------
#' Utility function to test if x is "numerically" integer wrt machine epsilon
#'  taken from exaple section of help of is.integer
#' @param x value to check if it is "integer"
#' @param tol tolerace
#' @export
#'
is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
