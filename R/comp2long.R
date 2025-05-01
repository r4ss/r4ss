#' Convert composition data to long format
#'
#' Convert data frame containing length or generalized size composition data
#' from wide to long format.
#'
#' @param x A data frame containing length or generalized size composition data,
#'        or a list containing such a data frame.
#' @param measure An optional string indicating the type of measurement, e.g.,
#'        \code{"weight"}.
#' @param zero Whether zero frequencies should be included in the output.
#'
#' @details
#' If \code{x} is not a data frame, the function will look for data frames
#' called \code{x[["lencomp"]]} or \code{x[["dat"]][["lencomp"]]}.
#'
#' @return
#' Data frame containing the columns
#' \preformatted{year  month  fleet  sex  part  Nsamp  length  freq}
#' for length composition data, or
#' \preformatted{method  year  month  fleet  sex  part  Nsamp  size  freq}
#' for generalized size composition data.
#'
#' The second to last column can be named explicitly via the measure argument.
#'
#' @note
#' In r4ss list objects, length composition data are stored as \code{lencomp},
#' while generalized size composition data are stored as
#' \code{sizefreq_data_list[[i]]}.
#'
#' @seealso
#' \code{\link{SS_read}} and \code{\link{SS_readdat}} read composition data from
#' files into r4ss list objects.
#'
#' @examples
#' # Read composition data from example model
#' path <- system.file("extdata", "simple_small", package = "r4ss")
#' inputs <- SS_read(path)
#' dat <- SS_readdat(file.path(path, "data.ss"), verbose = FALSE)
#' x <- dat[["lencomp"]]
#'
#' # Main argument can be a list or data frame
#' head(comp2long(inputs))
#' head(comp2long(dat))
#' head(comp2long(x))
#'
#' # Rename second to last column
#' head(comp2long(x, measure = "weight"))
#'
#' # Shrink output by omitting zero frequencies
#' nrow(comp2long(x))
#' nrow(comp2long(x, zero = FALSE))
#'
#' @importFrom utils read.table type.convert
#'
#' @export

comp2long <- function(x, measure = NULL, zero = TRUE) {
  # Look for data frame
  if (!is.data.frame(x)) {
    if (is.list(x) && is.data.frame(x[["lencomp"]])) {
      x <- x[["lencomp"]]
    } else if (is.list(x) && is.data.frame(x[["dat"]][["lencomp"]])) {
      x <- x[["dat"]][["lencomp"]]
    } else {
      stop("composition data not found")
    }
  }

  # Check column names
  cols <- c("method", "year", "month", "fleet", "sex", "part", "Nsamp")
  if (!all(cols[-1] %in% names(x))) {
    stop("'x' must contain '", paste(cols[-1], collapse = "', '"), "'")
  }

  # Distinguish between length comps and generalized size comps
  if (any(names(x) == "method")) {
    method <- TRUE
    if (is.null(measure)) {
      measure <- "size"
    }
  } else {
    method <- FALSE
    x <- data.frame(method = 1, x)
    if (is.null(measure)) {
      measure <- "length"
    }
  }

  # Store variables as a combined string
  x[["Nsamp"]] <- format(x[["Nsamp"]], trim = TRUE, digits = 12)
  rowlab <- apply(x[cols], 1, paste, collapse = "|")

  # Prepare composition data
  x <- x[!names(x) %in% cols]
  names(x) <- gsub("[a-z]", "", names(x))
  x <- as.matrix(x)
  row.names(x) <- rowlab

  # Convert composition data to long format
  comp <- as.data.frame(as.table(x), stringsAsFactors = FALSE)
  vars <- read.table(text = comp[["Var1"]], sep = "|", col.names = cols)
  comp[["Var1"]] <- NULL
  comp[["Var2"]] <- type.convert(comp[["Var2"]], as.is = TRUE)
  names(comp) <- c(measure, "freq")

  # Combine vars and comp
  out <- data.frame(vars, comp)
  if (!method) {
    out[["method"]] <- NULL
  }
  if (!zero) {
    out <- out[out[["freq"]] > 0, ]
  }

  out
}
