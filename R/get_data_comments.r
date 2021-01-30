#' Function to obtain comments lines starting from "#C"
#' in datfile, ctlfile, starter.ss, forecast.ss etc
#' This will identify 1st numeric data in dat (vector of string)
#' Then this function finds lines starting "#C" from lines
#' above 1st numeric data
#' @param dat vector of strings usually outputs of readLines(*)
#'            * is filename of datfile, ctlfile etc
#' @param defaultDataComments vector of strings
#'            If this function finds lines containg one of elements
#'            of defaultDataComments, those lines will be ignored
#'
#
get_data_comments <-
  function (dat,
              defaultDataComments =
                c(
                "^#C file created using the SS_writectl function in the R package r4ss",
                "^#C file write time:"
                )
  ){
    # Remove left and right trailing spaces of each line
    dat <- sapply(dat, "trimws")
    names(dat) <- NULL
    if (length(defaultDataComments) > 0) {
      delLns <-
        sapply(defaultDataComments, "grep", x = dat)
      delLns <- unique(delLns)
      dat <- dat[-delLns]
    }

    # Regular expression hopefully covering most of numerics
    # https://qiita.com/BlueSilverCat/items/f35f9b03169d0f70818b (in Japanese)
     regexpNumeric <-
      '^[+-]?(?:\\d+\\.?\\d*|\\.\\d+)(?:(?:[eE][+-]?\\d+)|(?:\\*10\\^[+-]?\\d+))?'
    # Find the 1st line containing numeric data
    firstNumericLine <- grep(dat, pattern = regexpNumeric)[1]
    res <-
      grep(x = dat[seq_len(firstNumericLine - 1)],
        pattern = "^#C", value = TRUE)
    if(length(res) == 0)res <- NULL
    return(res)
  } 