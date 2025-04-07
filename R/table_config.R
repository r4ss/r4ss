#' Create a table to summarize the configuration of the SS3 model
#'
#' @inheritParams table_exec_summary
#' @return Returns invisibly and optionally saves an .rda files containing
#' a list of table and caption
#' @family table functions
#' @export
#' @author Kelli F. Johnson, Ian G. Taylor
#'
#' @examples
#' \dontrun{
#' # Load the model output
#' output <- r4ss::SS_output()
#' # Create the table
#' table_config(output)
#'
#' # compare configuration for multiple models
#' config1 <- table_config(output1)
#' config2 <- table_config(output2)
#' table_compare <- data.frame(
#'   new_model = config1[["table"]],
#'   old_model = config2[["table"]][["Configuration"]]
#' )
#' names(table_compare) <- c("Section", "New model", "Old model")
#' }
table_config <- function(replist, dir = NULL, verbose = TRUE) {
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
  check_dir(dir = rda_dir)

  table <- data.frame(
    Section = c(
      "Maximum age",
      "Sexes",
      "Population bins",
      "Summary biomass (mt) age",
      "Number of areas",
      "Number of seasons",
      "Number of growth patterns",
      "Start year",
      "End year",
      "Data length bins",
      "Data age bins"
    ),
    Configuration = c(
      replist[["accuage"]],
      ifelse(replist[["nsexes"]] == 2, "Females, males", "Sexes combined"),
      sprintf(
        "%i-%i cm by %i cm bins",
        replist[["lbinspop"]][1],
        dplyr::last(replist[["lbinspop"]]),
        replist[["lbinspop"]][2] - replist[["lbinspop"]][1]
      ),
      sprintf("%i+", replist[["summary_age"]]),
      replist[["nareas"]],
      replist[["nseasons"]],
      ifelse(!is.null(replist[["ngpatterns"]]), replist[["ngpatterns"]], NA),
      replist[["startyr"]],
      replist[["endyr"]],
      sprintf(
        "%i-%i cm by %i cm bins",
        replist[["lbins"]][1],
        dplyr::last(replist[["lbins"]]),
        replist[["lbins"]][2] - replist[["lbins"]][1]
      ),
      sprintf(
        "%i-%i by %i year",
        replist[["agebins"]][1],
        dplyr::last(replist[["agebins"]]),
        replist[["agebins"]][2] - replist[["agebins"]][1]
      )
    )
  )

  # add the table to a list along with caption
  table_config <- list(
    cap = "Specifications and structure of the model.",
    table = table
  )
  # write to rda file
  if (verbose) {
    cli::cli_alert_info("writing table to {file.path(rda_dir, 'table_config.rda')}")
  }
  save(table_config, file = file.path(rda_dir, "table_config.rda"))

  return(invisible(table_config))
}
