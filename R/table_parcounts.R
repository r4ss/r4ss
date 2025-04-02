#' Summarize the estimated parameters based on an SS3 control file
#'
#' @param output Model output produced by `r4ss::SS_output()`
#' @param dir A directory which contains a control file, if different
#' from output$inputs$dir
#' @param ctlfile The filename for the control file
#' @export
#' @author Ian G. Taylor

table_parcounts <- function(
    output,
    dir = output$inputs$dir) {
  # check inputs
  if (is.null(output) || !is.list(output) || !"nfleets" %in% names(output)) {
    cli::cli_abort(
      "The input 'output' should be a list created by 'r4ss::SS_output()'."
    )
  }

  files <- SS_read(dir)
  ctl <- files[["ctl"]]

  get_labs <- function(table) {
    if (!is.null(table)) {
      table %>%
        dplyr::filter(PHASE > 0) %>%
        rownames()
    } else {
      NULL
    }
  }
  # get parameter labels from model output
  parlabs <- output$parameters %>%
    dplyr::filter(!is.na(Active_Cnt)) %>%
    dplyr::pull(Label)

  # get parameter labels from control file
  MGparms_labs <- get_labs(ctl$MG_parms)
  MGparms_tv_labs <- get_labs(ctl$MG_parms_tv)

  SRparms_labs <- get_labs(ctl$SR_parms)
  SRparms_tv_labs <- get_labs(ctl$SR_parms_tv)

  Qparms_labs <- get_labs(ctl$Q_parms)
  Qparms_tv_labs <- get_labs(ctl$Q_parms_tv)

  size_selex_parms_labs <- get_labs(ctl$size_selex_parms)
  size_selex_parms_tv_labs <- get_labs(ctl$size_selex_parms_tv)

  age_selex_parms_labs <- get_labs(ctl$age_selex_parms)
  age_selex_parms_tv_labs <- get_labs(ctl$age_selex_parms_tv)

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

  count1 <- sum(data$Count)
  count2 <- length(parlabs)

  if (count1 != count2) {
    warnings(
      "Parameter count is off by ", count2 - count1,
      "\nCount in table: ", count1,
      "\nCount in model: ", count2
    )
  }

  data <- rbind(
    data.frame(
      Type = "Total",
      Count = sum(data$Count)
    ),
    data
  )

  return(data)
}
