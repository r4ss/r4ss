#' Get version information for Stock Synthesis
#'
#' Get information about the version of Stock Synthesis that is
#' typically located in the first few lines of an input or output file.
#' The format of the way Stock Synthesis prints the version has
#' changed over time and
#' some information will only be present for most recent versions or
#' from the `Report.sso` file
#' because input files typically store less information about the version.
#'
#' @details # Example character strings
#' Below are some examples of character strings that were used by
#' Stock Synthesis to document the version of the code base that was used.
#' In more recent versions, this information is available in
#' `Report.sso` and `*.ss_new files`.
#' * Report files
#'   * 2.00 -
#'     `Code_version_:__2.00f;_06/20/07;_Stock_Synthesis_2_by_Richard_Methot_(NOAA);_using_Otter_Research_ADMB_7.0.1`
#'   * 3.24 -
#'     `#V3.24U` \\
#'     `SS-V3.24U-safe;_08/29/2014;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.2_Win64`
#'   * 3.03 -
#'     `SS-V3.03a-safe;_05/11/09;_Stock_Synthesis_by_Richard_Methot_(NOAA);_using_ADMB_7.0.1`
#'   * 3.30.13 -
#'     `#V3.30.13-safe;_2019_03_09;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.0`
#'   * 3.30.16 -
#'     `#V3.30.16.02;_2020_09_21;_safe;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.2`
#' * control (i.e., .ctl) and data (i.e., .dat) files
#'   * 3.2[-0-9] - A numeric value sandwiched between
#'     pound V and a lower-case letter that represents the minor version,
#'     e.g., `#V3.21e`
#'   * 3.30.13 - same as for the Report.sso file, e.g.,
#'     `#V3.30.13-safe;_2019_03_09;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.0`
#'
#' @template file
#' @param type A vector of character strings specifying what you want returned,
#'   where including multiple options is okay and will lead to
#'   returning a list with more than one entries.
#'   Options include the following character strings:
#'   * `"character"` is a character string of the short numeric version,
#'     e.g., `"3.30"`, that can be passed to [as.numeric()] without error,
#'   * `"numeric"` is a float with up to two decimal places,
#'   * `"long"` is a character string of the semantic version,
#'     e.g., `"3.24u"` or `"3.30.13"`,
#'   * `"full"` is the full character string found in `file`.
#' @template verbose
#' @param text A character string returned from `get_version_raw()`.
#'   You could also create your own string or read in the line yourself.
#' @param allow A vector of character strings specifying the acceptable
#'   version numbers, which is useful when code does not support all versions.
#'   The default is to error on any version other than 3.24 and 3.30.
#'   Note that `c(3.24, 3.30)` is not the same as `c("3.24", "3.30")` and
#'   only the latter with character values that include two decimal places
#'   should be used.
#'   Including `""` in the vector will
#'   stop the search for a viable version after looking in `file`.
#'   Otherwise, if a viable version is not found in `file`, then
#'   `get_version_search()` will attempt to find
#'   `control.ss_new` or `data.ss_new` in the same directory as `file`
#'   and search for a viable version within these files.
#'   A [stop()] message warns the user if
#'   the version number is not currently supported will be bypassed.
#' @author Kelli F. Johnson
#' @return
#' `get_version()` returns a named list with the length equal to
#' `length(type)`.
#' `""` is returned
#' for every type if the file is not found,
#' the version number is not printed in `file`, or
#' `get_version_parse()` was not able to extract a given type.
#' @seealso
#' * [SS_output()]
#' * [SS_readctl()]
#' * [SS_readdat()]
#'
#' @examples
#' # Get version 3.30.13 from stored files in {r4ss}
#' get_version(
#'   file = system.file("extdata", "simple_3.30.13", "Report.sso",
#'     package = "r4ss"
#'   )
#' )
get_version <- function(file,
                        type = c("character", "numeric", "long", "full"),
                        verbose = FALSE) {
  type <- match.arg(
    type,
    several.ok = TRUE
  )
  rawcontent <- get_version_raw(file)
  out <- get_version_parse(rawcontent)[type]
  if (verbose) {
    message(glue::glue('Version {out[[1]]} was found in {file}.'))
  }
  out
}

#' @rdname get_version
#' @export
get_version_raw <- function(file) {
  # if file not found
  if (!file.exists(file)) {
    return("")
  }
  # for a good file
  # #V is for 3.30 
  version <- file %>%
    scan(what = character(), nlines = 50, quiet = TRUE) %>%
    grep("#V[0-9]|Stock_Synthesis", ., value = TRUE, ignore.case = FALSE)
  out <- if (length(version) > 1) {
    grep("Stock_Synthesis", version, value = TRUE)[1]
  } else {
    ifelse(length(version) == 0, "", version)
  }
  out
}

#' @rdname get_version
#' @export
get_version_parse <- function(text) {
  nohead <- gsub("^\\s*#V|^\\s*SS-V", "", text)
  list(
    character = substring(nohead, 1, 4),
    numeric = as.numeric(substring(nohead, 1, 4)),
    long = gsub(
      pattern = "(^3)(\\.[0-9]{2}){1,}([a-zA-Z]){0,1}(.+)",
      replacement = "\\1\\2\\3",
      x = nohead
    ),
    full = text
  )
}

#' @rdname get_version
#' @description
#' Search files in a directory for the Stock Synthesis version number
#' in the numeric form, i.e., a float with up to two decimal places.
#' First, search the text in `file` and then search the text in the
#' control and data `ss_new` files found in
#' the same directory as `file` or
#' a subdirectory called `ssnew`.
#' Sometimes, these `ss_new` files have
#' more consistent formatting than the input files making
#' it easier to use standardized code to extract the short version number and
#' the control and data input files do not necessarily have standard names so
#' it is easier to search for `control.ss_new` and `data.ss_new` rather than
#' reading in `starter.ss` to determine what the file names should be.
#' If the model has not been ran yet and the `ss_new` files do not exist,
#' then just `file` will be searched.
#' @export
get_version_search <- function(file,
                               allow = c("3.24", "3.30"),
                               verbose = FALSE) {
  # find other potential files in directory
  potentialfiles <- dir(
    path = file.path(normalizePath(dirname(file)), c("", "ssnew")),
    pattern = "^[cd].+ss_new",
    full.names = TRUE
  )

  # Find the version in file or in potentialfiles
  version <- get_version(
    file = file,
    type = "character",
    verbose = verbose
  )
  if (!version %in% allow) {
    for (infile in potentialfiles) {
      version <- version(
        get_version_raw(infile),
        type = "numeric",
        verbose = verbose
      )
      if (version != "") break
    }
  }

  # See if version found is in the allowed vector
  if (!version %in% allow) {
    stop(call. = FALSE, glue::glue('
      Version {version} of Stock Synthesis is not currently supported by {{r4ss}}.
      Supported versions for this function include
      {glue::glue_collapse(glue::backtick(allow), sep = ", ", last = " and ")}.
      Updating to the newest version of Stock Synthesis is recommended.
    '))
  }

  # Return a numeric version always
  as.numeric(version)
}




shit <- function(text) {
    # warn if SS version used to create rep file is too old or too new for this code
    # note: SS_versionCode is new with V3.20
    # perhaps in the future we will use it to replace SS_versionshort throughout r4ss?
    SS_versionCode <- text[grep("#V", text)]
    SS_version <- text[grep("Stock_Synthesis", text)]
    SS_version <- SS_version[substring(SS_version, 1, 2) != "#C"] # remove any version numbering in the comments
    SS_version <- SS_version[1]
    if (substring(SS_version, 1, 2) == "#V") {
      SS_version <- substring(SS_version, 3)
    }
    if (substring(SS_version, 1, 4) == "3.30") {
      SS_versionshort <- "3.30"
      SS_versionNumeric <- as.numeric(SS_versionshort)
    } else {
      # typically something like "SS-V3.24"
      SS_versionshort <- toupper(substr(SS_version, 1, 8))
      SS_versionNumeric <- as.numeric(substring(SS_versionshort, 5))
    }
  return(c(SS_versionCode, SS_version, SS_versionshort, SS_versionNumeric))
}
# shit(get_version_raw("inst/extdata/simple_3.30.13/Report.sso"))
# shit(get_version_raw("inst/extdata/simple_3.24/Report.sso"))

#  get_version("inst/extdata/simple_3.24/Report.sso")
#  get_version("inst/extdata/simple_3.30.13/Report.sso")
#  get_version("inst/extdata/simple_3.30.13/Report.sso",type="character")
#  get_version("inst/extdata/simple_3.30.13/Report.sso",verbose = T,type="character")
