#' Summarize the configuration of the SS output
#'
#' @param file_csv A file path to the csv file.
#' @param caption Text you want in the caption.
#' @param caption_extra Additional text to add after the default
#' caption.
#' @param sens_group Optional description of the group of sensitivities
#' which will be used to change "alternative models" in the caption
#' (if present) to "alternative models related to \[sens_group\]"
#' @param dir Directory where the table should go (relative to "doc")
#' @param format "latex" or "html"
#' @param pretty Logical controlling whether names get cleaned up using
#' internal `prettynames()` function or not. Setting `pretty = FALSE`
#' will use `utils::read.csv(..., check.names = FALSE)`.
#' @export
#' @examples
#' \dontrun{
#' table_sens("tables/sens_table_s_bio_rec.csv")
#' }
#'
table_sens <- function(file_csv,
                       caption = "Differences in negative log-likelihood, estimates of key parameters, and estimates of derived quantities between the base model and several alternative models (columns). See main text for details on each sensitivity analysis. Red values indicate negative log-likelihoods that were lower (fit better to that component) than the base model.",
                       caption_extra = "",
                       sens_group = NULL,
                       dir = file.path("..", "tables"),
                       format = "latex",
                       pretty = TRUE) {
  # Revise caption if sens_group is provided
  if (!is.null(sens_group)) {
    caption <- gsub(
      pattern = "alternative models",
      replacement = paste(
        "alternative models related to",
        sens_group
      ),
      x = caption
    )
    caption <- gsub(
      pattern = "Differences",
      replacement = paste(
        stringr::str_replace(sens_group, "^\\w{1}", toupper), # capitalize first word
        "sensitivity analyses: differences"
      ),
      x = caption
    )
  }
  # add any additional text to caption
  caption <- paste(caption, caption_extra)

  # Make a new label that doesn't depend on area
  # Expecting sens_table_[a-z]_.+.csv
  label <- gsub(
    "sens_table_[a-z]", "sens-table",
    gsub(
      "_", "-",
      gsub("\\.[a-z]{3}$", "", basename(file_csv))
    )
  )

  data <- utils::read.csv(file_csv, check.names = pretty) %>%
    dplyr::mutate(Label = gsub("\\s+\\(.+\\)|likelihood", "", Label)) %>%
    dplyr::mutate(Label = gsub("(OTAL)", "\\L\\1", Label, perl = TRUE)) %>%
    dplyr::mutate(Label = gsub("Survey", "Indices", Label))

  prettynames <- function(x) {
    if (format == "latex") {
      x <- gsub("_", "", x)
    }
    x <- gsub("fixed[\\s.=]gear", "FG", x)
    x <- gsub("Base\\.model", "Base", x)
    x <- gsub("shareM", "share \\$M\\$", x)
    x <- gsub("(^[Mh]|_[Mh])", "\\$\\1\\$", x)
    x <- gsub("sigmaR", "\\$\\\\sigma_R\\$", x)
    x <- gsub("([0-9]{4})[.-]+([0-9]{4})", "(\\1-\\2)", x)
    x <- gsub("([0-9\\.]+$)", " = \\1", x, perl = TRUE)
    x <- gsub("([0-9])\\$", "\\1 \\$", x, perl = TRUE)
    x <- gsub("$M$0.3h", "$M$ = 0.3, $h$", x, fixed = TRUE) # ugly solution
    x <- gsub("seloffset", "sel. offset", x, fixed = TRUE) # ugly solution
    x <- gsub("offsetM", "offset, $M$", x, fixed = TRUE) # ugly solution
    x <- gsub("sexselscaledescend", "sex sel. scale and descend ", x, fixed = TRUE) # ugly solution
    x <- gsub("^no", "no ", x)
    x <- gsub("indices|index", "", x)
    x <- gsub("female", "fem. ", x)
    if (format == "html") {
      x <- gsub("_", " ", x)
      x <- gsub("\\$", "", x)
      x <- gsub("\\`", "", x)
      x <- gsub("\\", "", x, fixed = TRUE)
      x <- gsub("sigma R", "sigmaR", x)
      x <- gsub("sel offset", "sel. offset", x)
      x <- gsub("offset M", "offset, M", x)
    }
    return(x)
  }
  # clean up column names if requested
  if (pretty) {
    colnames(data) <- prettynames(colnames(data))
  }
  conditional_color <- function(x, n, nmax) {
    kableExtra::cell_spec(x,
      color = ifelse(x >= 0, "black", "red")
    )
  }
  tt <- kableExtra::kbl(
    data %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::mutate_if(is.numeric, conditional_color),
    booktabs = TRUE, longtable = TRUE,
    format = format, escape = FALSE,
    digits = 3,
    caption = caption,
    label = label
  )
  # decrease column width for tables with lots of columns
  if (NCOL(data) <= 7) {
    tt <- tt %>%
      kableExtra::column_spec(3:NCOL(data), width = "5em")
  }
  if (NCOL(data) > 7) {
    tt <- tt %>%
      kableExtra::column_spec(3:NCOL(data), width = "4.3em")
  }

  if (any(grepl("Total", data[, 1]))) {
    tt <- tt %>%
      kableExtra::pack_rows("Diff. in likelihood from base model", 1, 8) %>%
      kableExtra::pack_rows("Estimates of key parameters", 9, 12) %>%
      kableExtra::pack_rows("Estimates of derived quantities", 13, 21)
  } else {
    switch <- grep("age 3", data[, 1])[1]
    tt <- tt %>%
      kableExtra::pack_rows("Estimates of key parameters", 1, switch - 1) %>%
      kableExtra::pack_rows("Estimates of derived quantities", switch, NROW(data))
  }
  return(tt)
}
