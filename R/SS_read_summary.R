#' read ss_summary file
#'
#' read Stock Synthesis ss_summary.sso file into list object in R
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @return Output will be a list with four elements, \code{header},
#' \code{likelihoods}, \code{parameters}, and \code{derived_quants}.
#' Each is a data frame with rownames indicating the quantity shown in each row.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_output}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_readdat}}, \code{\link{SS_readstarter}}
#' @examples
#'
#'   \dontrun{
#'     summary <- SS_read_summary(file='c:/mymodel/ss_summary.sso')
#'   }
#'


SS_read_summary <- function(file="ss_summary.sso"){

  # check to make sure file is present and non-empty
  if(is.na(file.info(file)$size) || file.info(file)$size == 0){
    warning("file is missing or empty: ", file)
    return(NULL)
  }
  # read all lines as strings
  all_lines <- readLines(file)

  # starting line of different sections
  like_start <- grep("#_LIKELIHOOD", all_lines)
  param_start <- grep("#_PARAMETERS", all_lines)
  derived_quants_start <- grep("#_Derived_Quantities", all_lines)

  # read header
  header <- all_lines[1:(like_start-1)]
  
  # read likelihood section
  df <- all_lines[(like_start+2):(param_start - 1)] ## subset all lines for this section
  df <- strsplit(df, "[[:blank:]]+") ## Split by whitespace and collapse (+)
  df <- as.list(df)                  ## Must be a list for the next operation
  df <- do.call("rbind", df)         ## Make it into a dataframe
  df <- as.data.frame(df[, 2], stringsAsFactors = FALSE, row.names=df[, 1])
  df[, 1] <- as.numeric(df[, 1])     ## convert to numeric
  names(df) <- c("logL*Lambda")
  likelihoods <- df                  ## rename

  # read parameter section
  df <- all_lines[(param_start+2):(derived_quants_start - 1)]
  df <- df[-grep("^#", df)]
  df <- strsplit(df, "[[:blank:]]+") ## Split by whitespace and collapse (+)
  df <- as.list(df)                  ## Must be a list for the next operation
  df <- do.call("rbind", df)         ## Make it into a dataframe
  df <- as.data.frame(df[, -1], stringsAsFactors = FALSE, row.names=df[, 1])
  df[, c(1,2,4)] <- lapply(df[, c(1,2,4)], as.numeric)
  names(df) <- c("Value", "SE", "Active?", "Range")
  parameters <- df

  # read derived quantities section
  df <- all_lines[(derived_quants_start+2):length(all_lines)]
  df <- df[-grep("^#", df)]
  df <- strsplit(df, "[[:blank:]]+") ## Split by whitespace and collapse (+)
  df <- as.list(df)                  ## Must be a list for the next operation
  df <- do.call("rbind", df)         ## Make it into a dataframe
  df <- as.data.frame(df[, -1], stringsAsFactors = FALSE, row.names=df[, 1])
  df[, 1:2] <- lapply(df[, 1:2], as.numeric)
  names(df) <- c("Value", "SE")
  derived_quants <- df

  # return stuff
  return(list(header         = header,
              likelihoods    = likelihoods,
              parameters     = parameters,
              derived_quants = derived_quants))
}
