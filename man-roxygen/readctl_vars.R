
#' @template verbose
#' @param use_datlist LOGICAL. If TRUE, use datlist to derive parameters which 
#'  can not be determined from control file. Defaults to TRUE.
#' @param datlist list or character. If list, should be a list produced from
#'   [SS_writedat()]. If character, should be the file name of an SS data file.
#' @param nseas number of seasons in the model. This information is not
#'  explicitly available in control file and used only if `use_datlist = FALSE`.
#' @param N_areas number of spatial areas in the model. Default = 1. This information is not
#'  explicitly available in control file and used only if if `use_datlist = FALSE`.
#' @param Nages oldest age in the model. This information is also not
#'  explicitly available in control file and used only if `use_datlist = FALSE`.
#' @param Nsexes number of sexes in the model. This information is also not
#'  explicitly available in control file and used only if `use_datlist = FALSE`.
#' @param Npopbins number of population bins in the model. This information is also not
#'  explicitly available in control file and this information is only required if length based
#'  maturity vector is directly supplied (Maturity option of 6).  and used only 
#'  if `use_datlist = FALSE`.
#' @param Do_AgeKey Flag to indicate if 7 additional ageing error parameters to
#'  be read set 1 (but in fact any non zero numeric in R) or TRUE to enable to
#'  read them 0 or FALSE to disable them. This information is not 
#'  explicitly available in control file and used only if `use_datlist = FALSE`.
#' @param N_tag_groups number of tag release group. Default =NA. This information
#'  is not explicitly available control file  and used only if 
#'  `use_datlist = FALSE`. This information is only required if custom tag 
#'  parameters is enabled (TG_custom=1)

