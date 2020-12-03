#' Write object in fixed width format
#' 
#' \code{write.fwf} writes object in *f*ixed *w*idth *f*ormat.
#' 
#' Note: This function is copied from the gdata package version 2.18.0 on CRAN
#' under the GPL-2 license.
#' The package source is currently at
#' https://cran.r-project.org/web/packages/gdata/index.html
#' but the package is scheduled to be archived due to errors associated with
#' it (which don't seem related to the write.fwf function).
#' The documentation contained in the write.fwf.Rd file in the gdata source
#' was converted to these roxygen2 comments using the Rd2roxygen package
#' https://yihui.org/rd2roxygen.
#'
#' Original documentation from gdata version of the function:
#' #' While *F*ixed *w*idth *f*ormat is no longer widely used, it remains common
#' in some disciplines.
#' 
#' Output is similar to \code{print(x)} or \code{format(x)}. Formatting is done
#' completely by \code{\link{format}} on a column basis. Columns in the output
#' are by default separated with a space i.e. empty column with a width of one
#' character, but that can be changed with \code{sep} argument as passed to
#' \code{\link{write.table}} via \dots{}.
#' 
#' As mentioned formatting is done completely by \code{\link{format}}.
#' Arguments can be passed to \code{format} via \code{\dots{}} to further
#' modify the output. However, note that the returned \code{formatInfo} might
#' not properly account for this, since \code{\link{format.info}} (which is
#' used to collect information about formatting) lacks the arguments of
#' \code{\link{format}}.
#' 
#' \code{quote} can be used to quote fields in the output. Since all columns of
#' \code{x} are converted to character (via \code{\link{format}}) during the
#' output, all columns will be quoted! If quotes are used,
#' \code{\link{read.table}} can be easily used to read the data back into .
#' Check examples. Do read the details about \code{quoteInfo} argument.
#' 
#' Use only *true* character, i.e., avoid use of tabs, i.e., "\\t", or similar
#' separators via argument \code{sep}. Width of the separator is taken as the
#' number of characters evaluated via \code{\link{nchar}(sep)}.
#' 
#' Use argument \code{na} to convert missing/unknown values. Only single value
#' can be specified. Use \code{gdata::NAToUnknown} prior to export if you need
#' greater flexibility.
#' 
#' If \code{rowCol} is not \code{NULL} and \code{rownames=TRUE}, rownames will
#' also have column name with \code{rowCol} value. This is mainly for
#' flexibility with tools outside . Note that (at least in 2.4.0) it is not
#' "easy" to import data back to with \code{\link{read.fwf}} if you also export
#' rownames. This is the reason, that default is \code{rownames=FALSE}.
#' 
#' Information about format of output will be returned if
#' \code{formatInfo=TRUE}. Returned value is described in value section. This
#' information is gathered by \code{\link{format.info}} and care was taken to
#' handle numeric properly. If output contains rownames, values account for
#' this. Additionally, if \code{rowCol} is not \code{NULL} returned values
#' contain also information about format of rownames.
#' 
#' If \code{quote=TRUE}, the output is of course wider due to quotes. Return
#' value (with \code{formatInfo=TRUE}) can account for this in two ways;
#' controlled with argument \code{quoteInfo}. However, note that there is no
#' way to properly read the data back to if \code{quote=TRUE & quoteInfo=FALSE}
#' arguments were used for export. \code{quoteInfo} applies only when
#' \code{quote=TRUE}. Assume that there is a file with quoted data as shown
#' bellow (column numbers in first three lines are only for demonstration of
#' the values in the output).
#' 
#' \preformatted{ 123456789 12345678 # for position 123 1234567 123456 # for
#' width with quoteInfo=TRUE 1 12345 1234 # for width with quoteInfo=FALSE "a"
#' "hsgdh" " 9" " " " bb" " 123" }
#' 
#' With \code{quoteInfo=TRUE} \code{write.fwf} will return
#' 
#' \preformatted{ colname position width V1 1 3 V2 5 7 V3 13 6 }
#' 
#' or (with \code{quoteInfo=FALSE})
#' 
#' \preformatted{ colname position width V1 2 1 V2 6 5 V3 14 4 }
#' 
#' Argument \code{width} can be used to increase the width of the columns in
#' the output. This argument is passed to the width argument of
#' \code{\link{format}} function. Values in \code{width} are recycled if there
#' is less values than the number of columns. If the specified width is to
#' short in comparison to the "width" of the data in particular column, error
#' is issued.
#' 
#' @param x data.frame or matrix, the object to be written
#' @param file character, name of file or connection, look in
#' \code{\link{write.table}} for more
#' @param append logical, append to existing data in \code{file}
#' @param quote logical, quote data in output
#' @param na character, the string to use for missing values i.e. \code{NA} in
#' the output
#' @param sep character, separator between columns in output
#' @param rownames logical, print row names
#' @param colnames logical, print column names
#' @param rowCol character, rownames column name
#' @param justify character, alignment of character columns; see
#' \code{\link{format}}
#' @param formatInfo logical, return information on number of levels, widths
#' and format
#' @param quoteInfo logical, should \code{formatInfo} account for quotes
#' @param width numeric, width of the columns in the output
#' @param eol the character(s) to print at the end of each line (row).  For
#' example, 'eol="\\r\\n"' will produce Windows' line endings on a Unix-alike OS,
#' and 'eol="\\r"' will produce files as expected by Mac OS Excel 2004.
#' @param qmethod a character string specifying how to deal with embedded
#' double quote characters when quoting strings.  Must be one of '"escape"'
#' (default), in which case the quote character is escaped in C style by a
#' backslash, or '"double"', in which case it is doubled.  You can specify just
#' the initial letter.
#' @param scientific logical, if TRUE, allow numeric values to be formatted
#' using scientific notation.
#' @param \dots further arguments to \code{\link{format.info}} and
#' \code{\link{format}}
#' @return
#' 
#' Besides its effect to write/export data \code{write.fwf} can provide
#' information on format and width. A data.frame is returned with the following
#' columns: \item{colname}{name of the column} \item{nlevels}{number of unique
#' values (unused levels of factors are dropped), 0 for numeric column}
#' \item{position}{starting column number in the output} \item{width}{width of
#' the column} \item{digits}{number of digits after the decimal point}
#' \item{exp}{width of exponent in exponential representation; 0 means there is
#' no exponential representation, while 1 represents exponent of length one
#' i.e. \code{1e+6} and 2 \code{1e+06} or \code{1e+16}}
#' @author Gregor Gorjanc
#' @keywords print file
#' @examples
#' 
#' \dontrun{
#'   #### Note: example below is from the version of write.fwf in the gdata
#'   ####       package but failed when running after moving to r4ss
#' 
#'   ## Some data
#'   num <- round(c(733070.345678, 1214213.78765456, 553823.798765678,
#'                  1085022.8876545678,  571063.88765456, 606718.3876545678,
#'                  1053686.6, 971024.187656, 631193.398765456, 879431.1),
#'                digits=3)
#' 
#'   testData <- data.frame(num1=c(1:10, NA),
#'                          num2=c(NA, seq(from=1, to=5.5, by=0.5)),
#'                          num3=c(NA, num),
#'                          int1=c(as.integer(1:4), NA, as.integer(4:9)),
#'                          fac1=factor(c(NA, letters[1:9], "hjh")),
#'                          fac2=factor(c(letters[6:15], NA)),
#'                          cha1=c(letters[17:26], NA),
#'                          cha2=c(NA, "longer", letters[25:17]),
#'                          stringsAsFactors=FALSE)
#'   levels(testData[["fac1"]]) <- c(levels(testData[["fac1"]]), "unusedLevel")
#'   testData[["Date"]] <- as.Date("1900-1-1")
#'   testData[["Date"]][2] <- NA
#'   testData[["POSIXt"]] <- as.POSIXct(strptime("1900-1-1 01:01:01",
#'                                          format="%Y-%m-%d %H:%M:%S"))
#'   testData[["POSIXt"]][5] <- NA
#' 
#'   ## Default
#'   write.fwf(x=testData)
#' 
#'   ## NA should be -
#'   write.fwf(x=testData, na="-")
#'   ## NA should be -NA-
#'   write.fwf(x=testData, na="-NA-")
#' 
#'   ## Some other separator than space
#'   write.fwf(x=testData[, 1:4], sep="-mySep-")
#' 
#'   ## Force wider columns
#'   write.fwf(x=testData[, 1:5], width=20)
#' 
#'   ## Show effect of 'scienfic' option
#'   testData[["num3"]] <- testData[["num3"]] * 1e8
#'   write.fwf(testData, scientific=TRUE)
#'   write.fwf(testData, scientific=FALSE)
#'   testData[["num3"]] <- testData[["num3"]] / 1e8
#' 
#'   ## Write to file and report format and fixed width information
#'   file <- tempfile()
#'   formatInfo <- write.fwf(x=testData, file=file, formatInfo=TRUE)
#'   formatInfo
#' 
#'   ## Read exported data back to R (note +1 due to separator)
#'   ## ... without header
#'   read.fwf(file=file, widths=formatInfo[["width"]] + 1, header=FALSE, skip=1,
#'            strip.white=TRUE)
#'   
#'   ## ... with header - via postimport modfication
#'   tmp <- read.fwf(file=file, widths=formatInfo[["width"]] + 1, skip=1,
#'                   strip.white=TRUE)
#'   colnames(tmp) <- read.table(file=file, nrow=1, as.is=TRUE)
#'   tmp
#' 
#'   ## ... with header - persuading read.fwf to accept header properly
#'   ## (thanks to Marc Schwartz)
#'   read.fwf(file=file, widths=formatInfo[["width"]] + 1, strip.white=TRUE,
#'            skip=1, col.names=read.table(file=file, nrow=1, as.is=TRUE))
#' 
#'   ## ... with header - with the use of quotes
#'   write.fwf(x=testData, file=file, quote=TRUE)
#'   read.table(file=file, header=TRUE, strip.white=TRUE)
#' 
#'   ## Tidy up
#'   unlink(file)
#' }

write.fwf <- function(x,
                      file="",
                      append=FALSE,
                      quote=FALSE,
                      sep=" ",
                      na="",
                      rownames=FALSE,
                      colnames=TRUE,
                      rowCol=NULL,
                      justify="left",
                      formatInfo=FALSE,
                      quoteInfo=TRUE,
                      width=NULL,
                      eol="\n",
                      qmethod=c("escape", "double"),
                      scientific=TRUE,
                      ...)
{
  ## --- Setup ---

  dapply <- function(x, FUN, ..., simplify=TRUE)
      {
          if(is.data.frame(x))
              return(sapply(x, FUN, ..., simplify=simplify))
          else if(is.matrix(x))
              return(apply(x, 2, FUN, ...))
          else
              stop("x must be a data.frame or a matrix")
      }

  if(!(is.data.frame(x) || is.matrix(x)))
    stop("'x' must be a data.frame or matrix")
  if(length(na) > 1)
    stop("only single value can be defined for 'na'")

  if(!scientific)
      {
          option.scipen <- getOption("scipen")
          on.exit( function() options("scipen"=option.scipen) )
          options("scipen"=100)
      }


  if(rownames) {
    x <- as.data.frame(x)
    x <- cbind(rownames(x), x)
    rowColVal <- ifelse(!is.null(rowCol), rowCol, "row")
    colnames(x)[1] <- rowColVal
  }
  colnamesMy <- colnames(x)
  if(length(colnamesMy)==0)
      colnamesMy <- paste( "V", 1:ncol(x), sep="")

  nRow <- nrow(x)
  nCol <- length(colnamesMy)

  widthNULL <- is.null(width)
  if(!widthNULL && length(width) != nCol) {
    warning("recycling 'width'")
    widthOld <- width
    width <- integer(length=nCol)
    width[] <- widthOld
  }

  ## --- Format info ---

  retFormat <- data.frame(colname=colnamesMy,
                          nlevels=0,
                          position=0,
                          width=0,
                          digits=0,
                          exp=0,
                          stringsAsFactors=FALSE)

  ## Which columns are numeric like
  isNum <- dapply(x, is.numeric)
  ## is.numeric picks also Date and POSIXt
  isNum <- isNum & !(dapply(x, inherits, what="Date") |
                     dapply(x, inherits, what="POSIXt"))

  ## Which columns are factors --> convert them to character
  isFac <- dapply(x, is.factor)
  if(any(isFac))
      ## This conditional is necessary because if x is a matrix, even if
      ## all(isFAC==FALSE), this assignment will coerce it to mode
      ## character.  This isn't a problem for dataframes.
      x[, isFac] <- sapply(x[, isFac, drop=FALSE], as.character)

  ## Collect information about how format() will format columns.
  ## We need to get this info now, since format will turn all columns to character
  tmp <- dapply(x, format.info, ..., simplify=FALSE)
  if(is.matrix(x)) tmp <- as.data.frame(tmp)
  tmp1 <- sapply(tmp, length)
  tmp <- t(as.data.frame(tmp))
  retFormat[["width"]] <- tmp[, 1]
  ## Collect other details for numeric columns
  if(any(isNum)) {
    ## Numeric columns with digits
    test <- tmp1 > 1
    if(any(test)) {
      retFormat[test, c("digits", "exp")] <- tmp[test, c(2, 3)]
      ## Numeric columns with scientific notation
      test2 <- tmp[test, 3] > 0
      if(any(test2)) ## adding +1; see ?format.info
        retFormat[test, ][test2, "exp"] <- retFormat[test, ][test2, "exp"] + 1
    }
  }

  ## --- Format ---

  ## store original object in 'y'
  y <- x

  ## Formatting (to character)
  for(i in 1:nCol) {
    if(widthNULL) {
      tmp <- NULL
    } else {
      tmp <- width[i]
    }
    ## Due to na.encode bug in format() in 2.7.1; na.encode=TRUE should
    ## return NA values and not "NA", but even then we rely on the
    ## following test to "fiddle" with the value in 'na' argument since -
    ## NA should not increase the width of column with width 1, while wider
    ## value for 'na' should increase the width
    test <- is.na(y[, i])
    ## Make a copy to make sure we get character after first format() - Date class caused problems
    x2 <- character(length=nRow)
    ## Add formatted values
    x2[!test] <- format(y[!test, i], justify=justify, width=tmp, ...)
    ## Add 'na' value
    x2[test] <- na
    ## Replace the original
    x[, i] <- x2
    ## Collect width (again)
    tmp2 <- format.info(x2, ...)[1]
    ## Reformat if 'na' value change the width of the column
    if(tmp2 != retFormat[i, "width"]) {
      retFormat[i, "width"] <- tmp2
      ## ifelse() makes sure that numeric columns are justified to right
      x[, i] <- format(x[, i], justify=ifelse(isNum[i], "right", justify),
                       width=tmp, ...)
    }
    ## Reformat 'na' value if it is narrower than the width of the column
    if(nchar(na) < retFormat[i, "width"]) {
      x[test, i] <- format(na, justify=ifelse(isNum[i], "right", justify),
                           width=retFormat[i, "width"], ...)
    }
  }

  ## Number of levels for "non-numeric"" columns
  if(any(!isNum)) {
    retFormat[!isNum, "nlevels"] <- dapply(x[, !isNum, drop=FALSE],
                                           function(z) length(unique(z)))
  }

  ## Check that width was not to small
  if(!widthNULL) {
    test <- retFormat[["width"]] > width
    if(any(test)) {
      tmpCol <- paste(colnamesMy[test], collapse=", ")
      tmpWidth <- paste(width[test], collapse=", ")
      tmpNeed <- paste(retFormat[["width"]][test], collapse=", ")
      stop(paste("'width' (", tmpWidth, ") was too small for columns: ",
                 tmpCol, "\n 'width' should be at least (", tmpNeed, ")",
                 sep=""))
    }
  }

  ## --- Write ---

  if(colnames) {
    if(rownames && is.null(rowCol)) colnamesMy <- colnamesMy[-1]
    write.table(t(as.matrix(colnamesMy)),
                file=file,
                append=append,
                quote=quote,
                sep=sep,
                eol=eol,
                na=na,
                row.names=FALSE,
                col.names=FALSE,
                qmethod=qmethod)
 }

  write.table(x=x,
              file=file,
              append=(colnames || append),
              quote=quote,
              sep=sep,
              eol=eol,
              na=na,
              row.names=FALSE,
              col.names=FALSE,
              qmethod=qmethod)

  ## --- Return format and fixed width information ---

  if(formatInfo) {
    ## be carefull with these ifelse constructs
    retFormat[["position"]][1] <- ifelse(quote, ifelse(quoteInfo, 1, 2), 1)
    if(ifelse(quote, quoteInfo, FALSE)) retFormat[["width"]] <- retFormat[["width"]] + 2
    N <- nrow(retFormat)
    if(N > 1) {
      for(i in 2:N) {
        retFormat[["position"]][i] <- retFormat[["position"]][i - 1] +
          retFormat[["width"]][i - 1] + nchar(x=sep, type="chars") +
            ifelse(quote, ifelse(quoteInfo, 0, 1), 0)
      }
    }
    if(rownames && is.null(rowCol)) {
      retFormat <- retFormat[-1,]
      rownames(retFormat) <- 1:(N-1)
    }
    return(retFormat)
  }
}

###------------------------------------------------------------------------
### write.fwf.R ends here
