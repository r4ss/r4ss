#' Run all the tables functions
#'
#' Not necessarily a good idea but helpful for testing
#' 
#' @inheritParams table_exec_summary
#' @export 
#' @family table functions
#' @author Ian G. Taylor
#'
table_all <- function(
    replist,
    dir = NULL,
    fleetnames = NULL,
    selexyr = NULL,
    verbose = TRUE) {
  # run all table functions
  table_compweight <- table_compweight(replist = replist, dir = dir, verbose = verbose)
  table_config <- table_config(replist = replist, dir = dir, verbose = verbose)
  table_exec_summary <- table_exec_summary(replist = replist, dir = dir, verbose = verbose)
  table_pars <- table_pars(replist = replist, dir = dir, verbose = verbose)
  table_parcounts <- table_parcounts(replist = replist, dir = dir, verbose = verbose)
  table_ts <- table_ts(replist = replist, dir = dir, verbose = verbose)
  if (!replist$wtatage_switch) {
    table_biology <- table_biology(replist = replist, dir = dir, fleetnames, selexyr, verbose = verbose)
  }
}
