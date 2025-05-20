#' Table of length and age comp Francis weights
#' (doesn't yet include other data types)
#'
#' @inheritParams table_exec_summary
#' @param caption A character string for the caption.
#' @param caption_CAAL Additional text added to the caption for models with conditional age at length data.
#' @family table functions
#' @author Kelli F. Johnson, Ian G. Taylor
#' @export
#' @return A list containing the table and caption.
#' @examples
#' \dontrun{
#' # Load the model output
#' output <- r4ss::SS_output()
#' # Create the table
#' table_compweight(output)
#' }
#'
table_compweight <- function(replist,
                             dir = NULL,
                             caption = paste(
                               "Data weightings applied to compositions",
                               "according to the `Francis` method. `Obs.` refers to the number of unique",
                               "composition vectors included in the likelihood. `N input` and `N adj.`",
                               "refer to the sample sizes of those vectors before and after being adjusted",
                               "by the the weights."
                             ),
                             caption_CAAL = "`CAAL` is conditional age-at-length data.",
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

  # figure out which fleets have conditional age at length data
  CAAL_fleets <- replist[["condbase"]][["Fleet"]] |> unique()
  Age_fleets <- replist[["agedbase"]][["Fleet"]] |> unique()
  CAAL_fleets <- setdiff(CAAL_fleets, Age_fleets)
  if (length(CAAL_fleets) > 0) {
    caption <- paste(caption, caption_CAAL)
  }

  # gather parts for table
  Age_Comp_Fit_Summary <- replist[["Age_Comp_Fit_Summary"]]
  if (!is.null(Age_Comp_Fit_Summary)) {
    CAAL_Comp_Fit_Summary <- replist[["Age_Comp_Fit_Summary"]] |> dplyr::filter(Fleet %in% CAAL_fleets)
    Age_Comp_Fit_Summary <- replist[["Age_Comp_Fit_Summary"]] |> dplyr::filter(Fleet %in% Age_fleets)
  } else {
    CAAL_Comp_Fit_Summary <- NULL
    Age_Comp_Fit_Summary <- NULL
  }

  table <- dplyr::bind_rows(
    .id = "Type",
    Length = replist[["Length_Comp_Fit_Summary"]],
    Age = Age_Comp_Fit_Summary,
    CAAL = CAAL_Comp_Fit_Summary
  )
  if (nrow(table) == 0) {
    if (verbose) {
      cli::cli_alert_warning("No composition data found in the model output.")
    }
    return(invisible(NULL))
  }

  # clean up table
  table <- table |>
    # dplyr::mutate(Fleet = get_fleet(col = "label_long")[match(Fleet_name, get_fleet(col = "fleet"))]) |>
    dplyr::mutate(Fleet = replist[["FleetNames"]][Fleet]) |>
    dplyr::mutate("Sum N adj." = mean_Nsamp_adj * Npos) |>
    dplyr::select(
      Type,
      Fleet,
      "Francis" = Curr_Var_Adj,
      "Obs." = Npos,
      "Mean N input" = mean_Nsamp_in,
      "Mean N adj." = mean_Nsamp_adj,
      "Sum N adj."
    ) |>
    dplyr::mutate(Francis = round(Francis, 3)) |> # round to 3 places
    dplyr::mutate_at(5:7, ~ round(.x, 1)) # round to 1 places

  # add the table to a list along with caption
  table_compweight <- list(
    cap = caption,
    table = table
  )
  # write the table to an rda file
  if (verbose) {
    cli::cli_alert_info("writing table to {file.path(rda_dir, 'table_compweight.rda')}")
  }
  save(table_compweight, file = file.path(rda_dir, "table_compweight.rda"))

  return(invisible(table_compweight))
}
