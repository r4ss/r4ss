#' Summarize the estimated parameters based on an SS3 control file
#'
#' Requires either
#'
#' @inheritParams table_exec_summary
#' @param inputs An options list of inputs from the SS3 model created by `SS_read()`.
#' If NULL, then the function will run `SS_read(replist[["inputs"]][["dir"]])` to get the inputs.
#'
#' @family table functions
#' @export
#' @examples
#' \dontrun{
#' # Load the model output
#' output <- r4ss::SS_output()
#' # Create the table
#' table_parcounts <- table_parcounts(output)
#' # filter for types with at least one estimated parameter
#' table_parcounts[["table"]] |> dplyr::filter(Count > 0)
#' }
#' @author Ian G. Taylor

table_parcounts <- function(
  replist,
  inputs = NULL,
  dir = NULL,
  caption = "Estimated parameters in the model.",
  verbose = TRUE
) {
  # check inputs
  check_replist(replist)

  inputs <- SS_read(replist[["inputs"]][["dir"]], verbose = FALSE)
  ctl <- inputs[["ctl"]]

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

  get_labs <- function(table) {
    if (!is.null(table)) {
      table |>
        dplyr::filter(PHASE > 0) |>
        rownames()
    } else {
      NULL
    }
  }
  # get parameter labels from model output
  parlabs <- replist[["parameters"]] |>
    dplyr::filter(!is.na(Active_Cnt)) |>
    dplyr::pull(Label)

  # get parameter labels from control file
  MGparms_labs <- get_labs(ctl[["MG_parms"]])
  MGparms_tv_labs <- get_labs(ctl[["MG_parms_tv"]])

  SRparms_labs <- get_labs(ctl[["SR_parms"]])
  SRparms_tv_labs <- get_labs(ctl[["SR_parms_tv"]])

  Qparms_labs <- get_labs(ctl[["Q_parms"]])
  Qparms_tv_labs <- get_labs(ctl[["Q_parms_tv"]])

  size_selex_parms_labs <- get_labs(ctl[["size_selex_parms"]])
  size_selex_parms_tv_labs <- get_labs(ctl[["size_selex_parms_tv"]])

  age_selex_parms_labs <- get_labs(ctl[["age_selex_parms"]])
  age_selex_parms_tv_labs <- get_labs(ctl[["age_selex_parms_tv"]])

  data <- data.frame(
    Type = c(
      "Natural Mortality (M)",
      "M time-variation",
      "Growth mean",
      "Growth variability",
      "Growth time-variation",
      "Stock-recruit",
      "Stock-recruit variation",
      "Rec. dev. time series",
      "Rec. dev. initial age",
      "Rec. dev. forecast",
      "Index",
      "Index time-variation",
      "Size selectivity",
      "Size selectivity time-variation",
      "Retention",
      "Retention time-variation",
      "Age selectivity",
      "Age selectivity time-variation"
    ),
    Count = c(
      # M
      sum(grepl("^NatM", MGparms_labs)),
      # M time-variation
      sum(grepl("^NatM", MGparms_tv_labs)),
      # Growth mean
      sum(grepl("^L_at_", MGparms_labs)) +
        sum(grepl("_K_", MGparms_labs)),
      # Growth variability
      sum(grepl("^CV_", MGparms_labs)) +
        sum(grepl("^SD_", MGparms_labs)),
      # Growth time-variation
      sum(!grepl("^NatM", MGparms_tv_labs)),
      # Recruitment stock-recruit
      length(SRparms_labs),
      # Recruitment stock-recruit variation
      length(SRparms_tv_labs),
      # Recruitment deviations time series
      sum(grepl("^Early_RecrDev_", parlabs)) +
        sum(grepl("^Main_RecrDev_", parlabs)) +
        sum(grepl("^Late_RecrDev_", parlabs)),
      # Recruitment initial age
      sum(grepl("^Early_InitAge_", parlabs)),
      # Recruitment forecast
      sum(grepl("^ForeRecr_", parlabs)),

      # Index
      length(Qparms_labs),
      # Index time-variation
      length(Qparms_tv_labs),

      # Size selectivity
      sum(!grepl("_PRet_", size_selex_parms_labs)),
      # Size selectivity time-variation"
      sum(!grepl("_PRet_", size_selex_parms_tv_labs)),

      # Retention
      sum(grepl("_PRet_", size_selex_parms_labs)),
      # Retention time-variation"
      sum(grepl("_PRet_", size_selex_parms_tv_labs)),

      # Age selectivity
      length(age_selex_parms_labs),
      # Age selectivity time-variation"
      length(age_selex_parms_tv_labs)
    )
  )

  count1 <- sum(data[["Count"]])
  count2 <- length(parlabs)

  if (count1 != count2) {
    warnings(
      "Parameter count is off by ",
      count2 - count1,
      "\nCount in table: ",
      count1,
      "\nCount in model: ",
      count2
    )
  }

  table <- rbind(
    data.frame(
      Type = "Total",
      Count = sum(data[["Count"]])
    ),
    data
  )

  # add the table to a list along with caption
  table_parcounts <- list(
    cap = caption,
    table = data
  )
  # write the table to an rda file
  if (verbose) {
    cli::cli_alert_info(
      "writing table to {file.path(rda_dir, 'table_parcounts.rda')}"
    )
  }
  save(table_parcounts, file = file.path(rda_dir, "table_parcounts.rda"))

  return(invisible(table_parcounts))
}
