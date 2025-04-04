#' Table of parameters
#'
#' @inheritParams table_exec_summary
#' @param rows Which rows to include from the `parameters` table created by
#' [r4ss::SS_output()]. NULL will cause all rows to be included.
#' @param caption A character string for the caption.
#'
#' @family table functions
#' @author Kelli F. Johnson
#' @export

table_pars <- function(replist,
                       dir = NULL,
                       rows = NULL,
                       caption = "Parameter estimates, estimation phase, parameter bounds, estimation status, estimated standard deviation (SD), prior information [distribution(mean, SD)] used in the base model.",
                       verbose = TRUE) {
  # check the inputs
  check_replist(replist)
  # create the rda_dir
  rda_dir <- file.path(
    ifelse(
      is.null(dir),
      yes = replist[["inputs"]][["dir"]],
      no = dir
    ),
    "tables"
  )
  dir.create(rda_dir, showWarnings = FALSE)
  check_dir(dir = rda_dir)

  # Find sigma R
  sigmar <- replist[["parameters"]] |>
    dplyr::filter(grepl("sigmaR", ignore.case = TRUE, Label)) |>
    dplyr::pull(Value)
  sigmar <- sprintf("%2.2f", sigmar)
  # to do - Could format the names

  # Find sigmasel
  sigmasel <- replist[["parameters"]] |>
    dplyr::filter(grepl("sigmasel", ignore.case = TRUE, Label)) |>
    dplyr::pull(Value)
  if (length(sigmasel) == 0) {
    sigmasel <- NA
  }
  if (length(sigmasel) > 1) {
    sigmasel <- NA
    cli::cli_alert_warning("More than one value for sigmasel found in the parameters table. Using NA for the prior on the 2DAR parameters.")
  }
  if (length(sigmasel) == 1) {
    sigmasel <- sprintf("%2.2f", sigmasel)
  }


  # default is all rows from the table
  if (is.null(rows)) {
    rows <- seq_along(replist[["parameters"]][["Value"]])
  }

  # make the table
  table <- replist[["parameters"]] |>
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
        Pr_type == "dev" & !grepl("ARDEV", Label) ~ paste0("normal(0.00, ", sigmar, ")"),
        Pr_type == "dev" & grepl("ARDEV", Label) ~ paste0("normal(0.00, ", sigmasel, ")"),
        TRUE ~ Pr_type
      )
    ) |>
    dplyr::select(Label, Value, Phase, Bounds, Status, SD, Prior)

  # add the table to a list along with caption
  table_pars <- list(
    cap = caption,
    table = table
  )
  # write the table to an rda file
  if (verbose) {
    cli::cli_alert_info("writing table to {file.path(rda_dir, 'table_pars.rda')}")
  }
  save(table_pars, file = file.path(rda_dir, "table_pars.rda"))

  return(invisible(table_pars))
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
