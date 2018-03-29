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


  read_summary_section <- function(start, end, ncol, nonnumeric=NULL, names){
    # function to read one section at a time

    # start is starting line of section
    # end in ending line of section
    # ncol is number of columns after the label column
    # nonnumeric is the column number (after label) of any non-numeric column
    # names is the names of all columns after the label

    if(start >= end){
      # if section is empty
      return(NULL)
    }

    df <- all_lines[start:end]         ## Subset all lines for this section
    comment_lines <- grep("^#", df)    ## find and remove any subsection headers
    if(length(comment_lines) > 0){
      df <- df[-comment_lines]
    }
    df <- strsplit(df, "[[:blank:]]+") ## Split by whitespace and collapse (+)
    df <- as.list(df)                  ## Must be a list for the next operation
    df <- do.call("rbind", df)         ## Make it into a dataframe
    df <- as.data.frame(df[, -1], stringsAsFactors = FALSE, row.names=df[, 1])
    # convert columns to numeric
    if(ncol > 1){
      # which columns after label are numeric (shifted by 1 for label column)
      numeric.cols <- which(!(1:ncol) %in% nonnumeric)
      df[, numeric.cols] <- lapply(df[, numeric.cols], as.numeric)
    }else{
      df[, 1] <- as.numeric(df[, 1])   ## convert single column to numeric
    }
    # name the columns
    names(df) <- names
    return(df)
  }

  # read all lines as strings
  all_lines <- readLines(file)

  # find starting line of different sections
  like_start           <- grep("#_LIKELIHOOD", all_lines)
  param_start          <- grep("#_PARAMETERS", all_lines)
  derived_quants_start <- grep("#_Derived_Quantities", all_lines)
  survey_start         <- grep("#_survey_stdev", all_lines) # section optional
  biomass_start        <- grep("#_Biomass", all_lines)

  # read header
  header <- all_lines[1:(like_start-1)]

  # read likelihood section
  likelihoods <- read_summary_section(start = like_start+2,
                                      end = param_start - 1,
                                      ncol = 1,
                                      names="logL*Lambda")

  # read parameter section
  parameters <- read_summary_section(start = param_start+2,
                                     end = derived_quants_start - 1,
                                     ncol = 4,
                                     nonnumeric = 3,
                                     names=c("Value", "SE", "Active?", "Range"))

  # read derived quantities section
  derived_quants <- read_summary_section(start = derived_quants_start+2,
                                         end = survey_start - 1,
                                         ncol = 2,
                                         names=c("Value", "SE"))

  # read survey section (if present)
  survey <- read_summary_section(start = survey_start+1,
                                 end = biomass_start-1,
                                 ncol = 6,
                                 nonnumeric = c(3,5),
                                 names=c("Value", "SE", "XX", "Exp", "XX", "Q"))
  # remove extra column from survey data frame
  survey <- survey[names(survey) != "XX"]
  
  # read biomass section
  biomass <- read_summary_section(start = biomass_start+2,
                                  end = length(all_lines),
                                  ncol = 2,
                                  names=c("Value", "SE"))

  # return stuff
  return(list(header         = header,
              likelihoods    = likelihoods,
              parameters     = parameters,
              derived_quants = derived_quants,
              survey         = survey,
              biomass        = biomass))
}
