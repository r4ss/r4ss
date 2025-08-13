#' Convert composition data to long format
#'
#' Convert age, length, or generalized size composition data from wide to long
#' format.
#'
#' @param x A data frame containing age, length, generalized size composition
#'        data, or a list containing such a data frame.
#' @param ... Passed to \code{age2long} or \code{size2long}.
#' @param expand Whether to repeat recurring entries in the resulting data
#'        frame, so that the \code{freq} column is \code{1} for every entry.
#' @param zero Whether zero frequencies should be included in the output.
#' @param measure An optional string indicating the type of measurement, e.g.,
#'        \code{"weight"}.
#'
#' @details
#' The \code{age2long} function converts age compositions and \code{size2long}
#' converts length or generalized size compositions. Alternatively, the wrapper
#' function \code{comp2long} converts any composition data and will call either
#' \code{age2long} or \code{size2long}, depending on the data type of \code{x}.
#'
#' If \code{x} is not a data frame, the function will look for a data frame
#' inside the list: first \code{lencomp}, then \code{agecomp}, and then
#' \code{sizefreq_data_list[[1]]}.
#'
#' The \code{expand = TRUE} option is mainly useful for accessing conditional
#' age-at-length, e.g., to produce a data frame that has the same number of rows
#' as the number of otoliths.
#'
#' @return
#' Data frame containing the data in long format.
#'
#' @note
#' In r4ss input data objects, age composition data are stored as
#' \code{agecomp}, length composition data as \code{lencomp}, and generalized
#' size composition data as \code{sizefreq_data_list[[i]]}.
#'
#' @author Arni Magnusson
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
#' y <- dat[["agecomp"]]
#'
#' # Main argument can be a list or data frame
#' head(comp2long(inputs))
#' head(comp2long(dat))
#' head(comp2long(x))
#' head(comp2long(y))
#'
#' # Rename second to last column
#' head(comp2long(x, measure = "weight"))
#'
#' # Shrink output by omitting zero frequencies
#' nrow(comp2long(x))
#' nrow(comp2long(x, zero = FALSE))
#' nrow(comp2long(y))
#' nrow(comp2long(y, zero = FALSE))
#'
#' # Expand output by repeating recurring entries
#' nrow(comp2long(y, expand = TRUE))
#'
#' # Aggregate by sex
#' aggregate(freq ~ sex, comp2long(x), sum)
#' aggregate(freq ~ sex, comp2long(y), sum)
#'
#' @importFrom utils read.table type.convert
#'
#' @export

comp2long <- function(x, ...) {
  # Look for data frame
  if (!is.data.frame(x) && is.list(x)) {
    if (is.data.frame(x[["lencomp"]])) {
      x <- x[["lencomp"]]
    } else if (is.data.frame(x[["dat"]][["lencomp"]])) {
      x <- x[["dat"]][["lencomp"]]
    } else if (is.data.frame(x[["agecomp"]])) {
      x <- x[["agecomp"]]
    } else if (is.data.frame(x[["dat"]][["agecomp"]])) {
      x <- x[["dat"]][["agecomp"]]
    } else if (is.data.frame(x[["sizefreq_data_list"]][[1]])) {
      x <- x[["sizefreq_data_list"]][[1]]
    } else if (is.data.frame(x[["dat"]][["sizefreq_data_list"]][[1]])) {
      x <- x[["dat"]][["sizefreq_data_list"]][[1]]
    }
  }
  if (!is.data.frame(x)) {
    stop("composition data not found")
  }

  # Call age2long() or size2long()
  if ("ageerr" %in% names(x)) {
    age2long(x, ...)
  } else {
    size2long(x, ...)
  }
}

#' @rdname comp2long
#'
#' @export

age2long <- function(x, expand = FALSE, zero = TRUE) {
  # Look for data frame
  if (!is.data.frame(x) && is.list(x)) {
    if (is.data.frame(x[["agecomp"]])) {
      x <- x[["agecomp"]]
    } else if (is.data.frame(x[["dat"]][["agecomp"]])) {
      x <- x[["dat"]][["agecomp"]]
    }
  }
  if (!is.data.frame(x)) {
    stop("age composition data not found")
  }

  # Check column names
  cols <- c(
    "year",
    "month",
    "fleet",
    "sex",
    "part",
    "ageerr",
    "Lbin_lo",
    "Lbin_hi",
    "Nsamp"
  )
  if (!all(cols[-1] %in% names(x))) {
    stop("'x' must contain ", paste(cols[-1], collapse = ", "))
  }

  # Simplify column names
  names(x)[-seq(cols)] <- gsub("[a-z]", "", names(x)[-seq(cols)])

  # Shuffle data frame if two sexes
  if (all(x[["sex"]] %in% c(0, 3))) {
    ncomp <- (ncol(x) - length(cols)) / 2
    f <- x[seq(length(cols) + 1, length = ncomp)]
    f <- cbind(x[cols], f)
    f[["sex"]] <- "f"
    m <- x[seq(length(cols) + ncomp + 1, length = ncomp)]
    m <- cbind(x[cols], m)
    m[["sex"]] <- "m"
    x <- rbind(f, m)
  }

  # Store variables as a combined string
  x[["Nsamp"]] <- format(x[["Nsamp"]], digits = 12)
  rowlab <- apply(x[cols], 1, paste, collapse = "|")

  # Prepare composition data
  x <- x[!names(x) %in% cols]
  x <- as.matrix(x)
  row.names(x) <- rowlab

  # Convert composition data to long format
  comp <- as.data.frame(as.table(x), stringsAsFactors = FALSE)
  vars <- read.table(text = comp[["Var1"]], sep = "|", col.names = cols)
  comp[["Var1"]] <- NULL
  comp[["Var2"]] <- type.convert(comp[["Var2"]], as.is = TRUE)
  names(comp) <- c("age", "freq")

  # Combine vars and comp
  out <- data.frame(vars, comp)
  if (!zero) {
    out <- out[out[["freq"]] > 0, ]
  }
  out <- out[
    order(
      out[["fleet"]],
      out[["part"]],
      out[["sex"]],
      out[["year"]],
      out[["month"]],
      out[["age"]],
      out[["Lbin_lo"]]
    ),
  ]
  if (expand) {
    if (!is.integer(out[["freq"]])) {
      stop("expand = TRUE requires composition frequencies to be integers")
    }
    out <- out[rep(seq_len(nrow(out)), out[["freq"]]), ]
    out[["freq"]] <- 1L
  }
  row.names(out) <- NULL

  out
}

#' @rdname comp2long
#'
#' @export

size2long <- function(x, measure = NULL, zero = TRUE) {
  # Look for data frame
  if (!is.data.frame(x) && is.list(x)) {
    if (is.data.frame(x[["lencomp"]])) {
      x <- x[["lencomp"]]
    } else if (is.data.frame(x[["dat"]][["lencomp"]])) {
      x <- x[["dat"]][["lencomp"]]
    } else if (is.data.frame(x[["sizefreq_data_list"]][[1]])) {
      x <- x[["sizefreq_data_list"]][[1]]
    } else if (is.data.frame(x[["dat"]][["sizefreq_data_list"]][[1]])) {
      x <- x[["dat"]][["sizefreq_data_list"]][[1]]
    }
  }
  if (!is.data.frame(x)) {
    stop("size composition data not found")
  }

  # Check column names
  cols <- c("method", "year", "month", "fleet", "sex", "part", "Nsamp")
  if (!all(cols[-1] %in% names(x))) {
    stop("'x' must contain ", paste(cols[-1], collapse = ", "))
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

  # Simplify column names
  names(x)[-seq(cols)] <- gsub("[a-z]", "", names(x)[-seq(cols)])

  # Shuffle data frame if two sexes
  if (all(x[["sex"]] %in% c(0, 3))) {
    ncomp <- (ncol(x) - length(cols)) / 2
    f <- x[seq(length(cols) + 1, length = ncomp)]
    f <- cbind(x[cols], f)
    f[["sex"]] <- "f"
    m <- x[seq(length(cols) + ncomp + 1, length = ncomp)]
    m <- cbind(x[cols], m)
    m[["sex"]] <- "m"
    x <- rbind(f, m)
  }

  # Store variables as a combined string
  x[["Nsamp"]] <- format(x[["Nsamp"]], digits = 12)
  rowlab <- apply(x[cols], 1, paste, collapse = "|")

  # Prepare composition data
  x <- x[!names(x) %in% cols]
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
  out <- out[
    order(
      out[["fleet"]],
      out[["part"]],
      out[["sex"]],
      out[["year"]],
      out[["month"]],
      out[[measure]]
    ),
  ]
  row.names(out) <- NULL

  out
}
