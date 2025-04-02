#' Multiple tables of parameters (split to avoid long tables)
#'
#' @param output A list from [r4ss::SS_output()].
#' @param rows Which rows to include from the `parameters` table created by
#' [r4ss::SS_output()]. NULL will cause all rows to be included.
#' @param rows_per_table Number of rows to include in each table.
#' @param caption A character string for the caption. The caption will be
#' appended to include the parameter number in each table
#' (e.g. "Table 1 of 3 showing parameters 1-25.").
#' @param label A character string with the label for the table.
#' No underscores allowed. The label will be appended the table number if the
#' table is split (e.g. "table-pars-base" will be changed to
#' "table-pars-base-1")
#'
#' @seealso [table_pars()]
#' @author Ian G. Taylor
#' @export

table_pars_split <- function(output,
                             rows = NULL,
                             rows_per_table = 40,
                             caption = "Parameter estimates, estimation phase, parameter bounds, estimation status, estimated standard deviation (SD), prior information [distribution(mean, SD)] used in the base model.",
                             label = "table-pars-base") {
  # get parameters from model
  pars <- output[["parameters"]]
  # default is all rows from the table
  if (is.null(rows)) {
    rows <- seq_len(nrow(pars))
  }
  # subset table
  pars <- pars[rows, ]
  # count parameters and number of tables
  npars <- nrow(pars)
  ntables <- ceiling(npars / rows_per_table)

  # loop over tables
  for (itable in 1:ntables) {
    # get range of parameters to include in a table
    which_pars <- 1:rows_per_table + rows_per_table * (itable - 1)

    # avoid values outside the range
    which_pars <- intersect(which_pars, 1:npars)

    # adjust caption and label
    caption_appended <- paste0(
      caption, " Table ", itable, " of ", ntables,
      " showing parameters ", paste(range(which_pars), collapse = "-"), "."
    )
    label_appended <- paste0(label, "-", itable)

    # create the table and print it
    print(table_pars(output,
      rows = which_pars,
      caption = caption_appended,
      label = label_appended
    ))
    cat("\n")
  }
}

#' Table of parameters
#'
#' @param output A list from [r4ss::SS_output()].
#' @param rows Which rows to include from the `parameters` table created by
#' [r4ss::SS_output()]. NULL will cause all rows to be included.
#' @param caption A character string for the caption.
#' @param label A character string with the label for the table.
#' No underscores allowed.
#'
#' @seealso [table_pars_split()]
#' @author Kelli F. Johnson
#' @export

table_pars <- function(output,
                       rows = NULL,
                       caption = "Parameter estimates, estimation phase, parameter bounds, estimation status, estimated standard deviation (SD), prior information [distribution(mean, SD)] used in the base model.",
                       label = "table-pars-base") {
  # Find sigma R
  sigmar <- output[["parameters"]] |>
    dplyr::filter(grepl("sigma", ignore.case = TRUE, Label)) |>
    dplyr::pull(Value)
  sigmar <- sprintf("%2.2f", sigmar)
  # to do - Could format the names

  # default is all rows from the table
  if (is.null(rows)) {
    rows <- seq_along(output[["parameters"]][["Value"]])
  }
  # make the table
  output[["parameters"]] |>
    dplyr::slice(rows) |>
    dplyr::select(Label, Value, Phase, Min, Max, Pr_type, Prior, Parm_StDev, Pr_SD, Status) |>
    dplyr::mutate(
      Value = signif_string(Value, 3),
      Bounds = paste0("(", signif_string(Min, 2), ", ", signif_string(Max, 2), ")"),
      Status = dplyr::case_when(
        is.na(Status) ~ "fixed",
        Status == "act" ~ "dev",
        Status == "OK" ~ "ok",
        TRUE ~ Status
      ),
      SD = ifelse(is.na(Parm_StDev), "0", signif_string(Parm_StDev, 3)),
      pv = sprintf("%2.3f", Prior),
      psd = sprintf("%2.3f", Pr_SD),
      Prior = dplyr::case_when(
        Pr_type == "Log_Norm" ~ paste0("lognormal(", sprintf("%2.3f", exp(Prior)), ", ", psd, ")"),
        Pr_type == "Normal" ~ paste0("normal(", sprintf("%2.3f", Prior), ", ", psd, ")"),
        Pr_type == "No_prior" ~ "none",
        Pr_type == "Full_Beta" ~ paste0("beta(", pv, ", ", psd, ")"),
        Pr_type == "dev" ~ paste0("normal(0.00, ", sigmar, ")"),
        TRUE ~ Pr_type
      )
    ) |>
    dplyr::select(Label, Value, Phase, Bounds, Status, SD, Prior) 
}

#' Convert a numeric vector to character with chosen significant digits
#'
#' @details The `signif()` function applies the same number of decimal
#' to all values in a vector, but parameter tables often include very
#' different scales, like Wtlen_1_Fem_GP_1 = 2e-06 and
#' Wtlen_2_Fem_GP_1 = 3, so this applies `signif()` separately to each
#' value and then converts to a string.
#'
#' @param x Vector of numeric values
#' @param digits Number of significant digits
#' @author Ian G. Taylor
#' @export

signif_string <- function(x, digits = 3) {
  lapply(x, FUN = signif, digits = digits) |>
    as.character()
}
