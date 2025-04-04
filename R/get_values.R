#' Function to pull values and calculate confidence intervals from model output
#' called from `r4ss::table_exec_summary()`.
#'
#' @template replist
#' @param label The parameter name to calculate confidence intervals for. The name
#'  is based on the names in the `replist` object.
#' @param yrs Vector of years to calculate confidence intervals for.
#' @param ci_value To calculate confidence intervals, the desired interval must
#'   be specified. The default is 0.95.
#' @param single Calculate the confidence interval for a single year or parameter.
#'   The default is FALSE.
#'
#' @return data frame with point estimate and confidence interval low and high values
#' @author Chantel R. Wetzel, Kelli F. Johnson, Ian G. Taylor
#' @export
#'
get_values <- function(replist, label, yrs, ci_value, single = FALSE) {
  dat <- replist[["derived_quants"]]
  if (label == "Main_RecrDev" || label == "Late_RecrDev" || label == "ForeRecr") {
    dat <- replist[["parameters"]]
  }

  if (!single) {
    value <- dat[grep(label, dat[["Label"]]), ]
    value <- value[value[["Label"]] >= paste0(label, "_", yrs[1]) &
      value[["Label"]] <= paste0(label, "_", max(yrs)), ]
    dq <- value[["Value"]]
    ind <- names(value) %in% c("StdDev", "Parm_StDev")
    sd <- value[, ind]
  }

  if (single) {
    value <- dat[grep(label, dat[["Label"]])[1], ]
    dq <- value[["Value"]]
    sd <- value[["StdDev"]]
  }

  if (label == "Recr" || label == "Recr_virgin") {
    low <- exp(log(dq) - qnorm(1 - (1 - ci_value) / 2) * sqrt(log(1 + (sd / dq) * (sd / dq))))
    high <- exp(log(dq) + qnorm(1 - (1 - ci_value) / 2) * sqrt(log(1 + (sd / dq) * (sd / dq))))
  }
  if (label != "Recr" && label != "Recr_virgin") {
    low <- dq - qnorm(1 - (1 - ci_value) / 2) * sd
    high <- dq + qnorm(1 - (1 - ci_value) / 2) * sd
  }

  if (!single) {
    if (length(dq) > 0) {
      return(data.frame(yrs, dq, low, high))
    } else {
      return(NULL)
    }
  }
  if (single) {
    return(data.frame(dq, low, high))
  }
}
