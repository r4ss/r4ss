#' Table of estimated time series
#'
#' Adapted from Lingcod 2021:
#' https://github.com/pfmc-assessments/lingcod/blob/main/R/table_ts.R
#'
#' @inheritParams table_exec_summary
#' @param caption A character string for the caption.
#' @family table functions
#' @export
#' @author Kelli F. Johnson, Ian G. Taylor
#' @examples
#' \dontrun{
#' table_ts(model)
#' }
table_ts <- function(
    replist,
    dir = NULL,
    caption = "Time series of population estimates for the base model.",
    verbose = TRUE) {
  # check the input
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
  check_dir(dir = rda_dir, verbose = verbose)

  table <- replist[["annual_time_series"]] %>%
    # dplyr::filter(!is.na(Deplete)) %>% # removes start year of model
    dplyr::filter(Era %in% c("VIRG", "TIME")) %>%
    dplyr::select(
      year, # 1
      SSB, # 2
      Depletion_std, # 3
      Bio_Smry_an, # 4
      dead_catch_B_an, # 5
      recruits, # 6
      SPR_std, # 7
      tot_exploit
    )
  names(table) <- c(
    "Year",
    replist[["SpawnOutputLabel"]],
    "Fraction unfished",
    paste0("Age-", replist[["summary_age"]], "+ biomass (t)"),
    "Dead catch (t)",
    "Age-0 recruits (1000s)",
    replist[["SPRratioLabel"]],
    "Exploitation rate"
  )

  # add the table to a list along with caption
  table_ts <- list(
    cap = caption,
    table = table
  )
  # write the table to an rda file
  if (verbose) {
    cli::cli_alert_info("writing table to {file.path(rda_dir, 'table_ts.rda')}")
  }
  save(table_ts, file = file.path(rda_dir, "table_ts.rda"))
  return(invisible(table_ts))
}
