#' Replace dollar sign operator in a file
#'
#' The dollar sign is convienent to write, but allows for partial matching,
#' which we don't often want. This function takes a file and changes all dollar
#' signs to double brackets with names in quotations instead. Note that this
#' function will incorrectly convert text enclosed in backticks that includes a dollar sign.
#' Note also that if a dollar sign is within a text
#' string enclosed with quotation marks, it will also not  convert correctly
#' (for example, "See test$name" would become
#' "See test\[\["name"\]\]", which is not parsable R code due to 2 sets of quotation
#' marks.) Luckily, this last issue is easily discoverable by attempting to load or source
#' the function because an error will be produced.
#' @template file
#' @param out_file The name or path of a new file to write to. This is by
#'  default the same as the original file. Set to NULL to avoid writing a new
#'  file.
#' @param allow_recursive Should dollar sign references of list in list be
#'  replaced? If this is FALSE, then only the first reference will be replaced.
#'  For example, `F$first$second` would become `F[["first"]]$second` when
#'  allow_recursive is FALSE, but would become `F[["first"]][["second"]]` if TRUE.
#' @param max_loops How many times should dollar signs be looked for
#'  recursively, if allow_recursive is TRUE? Defaults to 5. This is meant to
#'  prevent an infinite loop in case the function does not work properly.
#' @return A character vector of the modified text. As a side effect, produces
#'  a text file (name specified in out_file) written out from R using writeLines().
#' @author Kathryn Doering
#' @examples
#' test_text <- c(
#'   "x$my_name <- y$test",
#'   "x[['my_name']]",
#'   "no_replace_here <- 44",
#'   "x$names<-new_assign;x$`22`",
#'   "x$names <- new_assign; x$`22`",
#'   "x$`$$weirdcharacters`<-222",
#'   "x$`nameinbacktick`",
#'   "x$mylist$my_col$YetAnotherCol",
#'   "x$mylist$my_col$`1_somename`",
#'   "x$`bad$name` <- 55",
#'   "x$`other_$badname`"
#' )
#' writeLines(test_text, "test_rm_dollar_sign.txt")
#' new_text <- r4ss:::rm_dollar_sign(
#'   file = "test_rm_dollar_sign.txt",
#'   out_file = NULL
#' )
#' new_text
#' file.remove("test_rm_dollar_sign.txt")
rm_dollar_sign <- function(file,
                           out_file = file,
                           allow_recursive = TRUE,
                           max_loops = 5) {
  # read in the file
  lines <- readLines(file)
  # warn if there are any names like test$`text$moretext`
  difficult_lines <-
    grep(
      pattern = "([[:alnum:]]|\\.|\\_)\\$`[[:alnum:]]+[[:print:]]*\\$([[:alnum:]]|\\.|\\_)*`",
      lines
    )
  if (length(difficult_lines) > 0) {
    difficult_txt <- lines[difficult_lines]
    warning(
      "The following lines may not have convert correctly because of names",
      " in back ticks containing dollar signs.\n",
      paste0(paste0("Line ", difficult_lines, " ", difficult_txt), collapse = "\n")
    )
  }
  # get rid of names in back ticks first:
  mod_lines <- gsub(
    pattern = "([[:alnum:]]|\\.|\\_)\\$`([[:print:]]+)`",
    replacement = "\\1\\[\\[\"\\2\"\\]\\]",
    lines
  )
  # all others not in back ticks
  pattern_no_backtick <-
    "([[:alnum:]]|\\.|\\_|\\])\\$([[:alnum:]]+)(([[:alnum:]]|\\.|\\_)*)(\\s|[[:punct:]]|$)"
  replace_no_backtick <- "\\1\\[\\[\"\\2\\3\"\\]\\]\\5"
  mod_lines <- gsub(
    pattern = pattern_no_backtick,
    replacement = replace_no_backtick,
    mod_lines
  )
  if (allow_recursive) {
    # get rid of $ when there are lists in lists.
    ind <- 1
    while (length(grep(pattern_no_backtick, x = mod_lines)) > 0 &
      ind <= max_loops) {
      ind <- ind + 1
      mod_lines <- gsub(
        pattern = pattern_no_backtick,
        replacement = replace_no_backtick,
        mod_lines
      )
    }
    if (length(grep(pattern_no_backtick, x = mod_lines)) > 0) {
      warning(
        "max_loops was set too low to replace all instances of dollar ",
        "sign references."
      )
    }
  } else {
    if (length(grep(pattern_no_backtick, x = mod_lines)) > 0) {
      warning(
        "There are lists in lists, but allow_recursive = FALE, so not all",
        "dollar sign operators were converted."
      )
    }
  }
  if (!is.null(out_file)) {
    writeLines(mod_lines, out_file)
  }
  mod_lines
}
