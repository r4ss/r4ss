#' Extract the `sigma_R_info` table from a data frame of recruitment parameters
#'
#' Extract the `sigma_R_info` table that is available in the object returned from
#' [SS_output]. Typically, this table is determined from MLE results, but this
#' function also offers the capability to calculate this info from the table of
#' MCMC results as well.
#'
#' @param data A data frame of recruitment parameters or MCMC results, both of
#' which are available in the list returned from [SS_output].
#' @param sigma_R_in The input value used for sigma_R, this is also available in
#' the list output by [SS_output] and is used to scale the results.
#' @importFrom magrittr %>%
#'
#' @export
#' @return A data frame is returned with three rows and 10 columns. Rows provide
#' summary statistics for a given group of recruitments, where the
#' rows are additive in their inclusiveness, i.e., the data frames used to calculate
#' rows two and three include all data used in the calculations that led to the
#' results displayed in the first row. Columns include the following variables:
#' * `period`: Categories that specify which recruitments were included in the
#' subset used to calculate the summary.
#' * `N_devs`: The number of recruitment estimates used in the summary.
#' * `SD_of_devs`: The standard deviation (sd) of the recruitment deviations.
#' * `Var_of_devs`: The variance (var) of the recruitment deviations.
#' * `mean_SE`: The mean of the estimated standard error (se) of the recruitment
#' deviations, or more precisely, the mean of the `Parm_StDev` column of your data.
#' * `mean_SEsquared`: The mean of the squared estimates of standard error of the
#' recruitment deviations, i.e., `mean(Parm_StDev^2)`.
#' * `sqrt_sum_of_components`: The square root of the sum of the deviation and the
#' standard error.
#' * `SD_of_devs_over_sigma_R`: The scaled version of `SD_of_devs`.
#' * `sqrt_sum_over_sigma_R`: The scaled version of `sqrt_sum_of_components`.
#' * `alternative_sigma_R`: The suggested value for a new input sigma_R, which is
#' just a repeat of the sum of components column.
#' If the input data frame is `NULL`, then `NULL` is returned.
#' @examples
#' # Read in simple model results
#' base <- SS_output(system.file("extdata", "simple_3.30.13", package = "r4ss"),
#'   verbose = FALSE, printstats = FALSE)
#' test <- extract_sigma_R_info(base[["recruitpars"]], base[["sigma_R_in"]])
#' testthat::expect_equivalent(base[["sigma_R_info"]], test)
#'
extract_sigma_R_info <- function(data, sigma_R_in) {

  if (is.null(data)) return(NULL)

  #### If MCMC then get data in correct form
  if (!"Value" %in% colnames(data)) {
    minyear <- min(as.numeric(gsub("SSB_", "",
      grep("SSB_[0-9]+", value = TRUE, colnames(data)))))
    data <- data %>%
      dplyr::select(dplyr::matches("InitAge|RecrDev|ForeRec")) %>%
      tidyr::gather(key = "rows", value = "value", tidyr::everything()) %>%
      dplyr::group_by(.data[["rows"]]) %>%
      dplyr::summarise(.groups = "keep",
        Value = mean(.data[["value"]]),
        Parm_StDev = stats::sd(.data[["value"]]),
      ) %>%
      dplyr::mutate(
        type = gsub("_[0-9]+", "", .data[["rows"]]),
        number = as.numeric(gsub("[a-zA-z_]+_", "", .data[["rows"]]))
      ) %>%
      dplyr::mutate(min = minyear - .data[["number"]]) %>%
      dplyr::mutate(
        Yr = dplyr::case_when(
          !grepl("InitAge", .data[["rows"]]) ~ .data[["number"]],
          TRUE ~ .data[["min"]])
      ) %>% tibble::column_to_rownames(var = "rows") %>%
      dplyr::select(.data[["Value"]]:.data[["type"]], .data[["Yr"]])
  }

  # Create three data frames in a tibble each is an increasingly bigger subset
  mysubset <- function(grepval, thed = data, thedcol = "type") {
    thed[grep(grepval, thed[, thedcol]), ]
  }
  mycalcs <- function(thed) {
    out <- thed %>% dplyr::summarise(
      N_devs = dplyr::n(),
      SD_of_devs = sd(.data[["Value"]]),
      Var_of_devs = sd(.data[["Value"]])^2,
      mean_SE = mean(.data[["Parm_StDev"]]),
      mean_SEsquared = mean(.data[["Parm_StDev"]]^2)
      )
    return(as.data.frame(out))
  }
  out <- tibble::tibble(subsets = lapply(FUN = mysubset, c(
    "Main" = "Main",
    "Early+Main" = "Early|Main",
    "Early+Main+Late" = "Early|Main|Late"))) %>%
  dplyr::mutate(
    yes = lapply(.data[["subsets"]], mycalcs),
    period = names(.data[["subsets"]])) %>%
  tidyr::unnest(.data[["yes"]]) %>%
  dplyr::select(.data[["period"]], .data[["N_devs"]]:.data[["mean_SEsquared"]]) %>%
  dplyr::mutate(
    sqrt_sum_of_components = sqrt(.data[["Var_of_devs"]] + .data[["mean_SEsquared"]]),
    SD_of_devs_over_sigma_R = .data[["SD_of_devs"]] / sigma_R_in,
    sqrt_sum_over_sigma_R = .data[["sqrt_sum_of_components"]] / sigma_R_in,
    alternative_sigma_R = sigma_R_in * .data[["sqrt_sum_over_sigma_R"]]
    )
  return(as.data.frame(out))
}
