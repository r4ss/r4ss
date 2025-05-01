library(r4ss)
library(dplyr)

#' @title Toggle off data
#' @description This function toggles off data in a Stock Synthesis model by removing 
#' the specified data types, fleets, or years.
#' 
#' @params base_model_dir character string of the base model directory
#' @params new_model_dir character string of the new model directory to put the 
#' modified model
#' @params data_type list of types of data to remove all or only certain fleets 
#' or only certain years data_type = c("catch", "CPUE", "lencomp", "agecomp", "MeanSize_at_Age_obs")
#' @params fleet_or_index list of fleets or indices to remove data
#' @params only_these_years list of years to remove data for only those years from the 
#' data type you have specified
#' 
#' @export
#' @author Elizabeth F. Perl
#' 
data_type <- c("CPUE")
fleet_or_index <- c(9,10,11,12)
only_these_years <- c(2020,2021,2022,2023)

toggle_off_data <- function(
    base_model_dir,
    new_model_dir,
    data_type = c("CPUE"),
    fleet_or_index = c(9,10,11,12),
    only_these_years = c(2020,2021,2022,2023)
    )
{ 
  if(is.null(data_type) & is.null(fleet_or_index) & is.null(only_these_years)){
    stop("Error: No data type, fleet or index, or years specified to remove. Stopping the function.")
  }
    
  if (!dir.exists(base_model_dir)) {
    stop(sprintf("Warning: The directory '%s' does not exist. Stopping the function.", base_model_dir))
  }
  
  if(!dir.exists(new_model_dir)){
    
    message(sprintf("Creating new model directory: '%s'", new_model_dir))
    dir.create(new_model_dir)
    copy_SS_inputs(dir.old = base_model_dir, 
                   dir.new = new_model_dir)
  }
  
  ##### Removing catches ##### -------------------------------------------------
  if("catch" %in% type){
    catch_chr <- paste0("(", fleet_or_index, ")$")
    not_in_indices <- setdiff(fleet_or_index, inputs$dat$CPUE$index)
    catch_und <- paste0(not_in_indices, "_")
    catch_chr2 <- paste0("(", not_in_indices, ")$")
  
    if(is.null(only_these_years)){
      years_to_match <- unique(inputs$dat$catch$year)
      
      # QUESTION: If catch selectivity is mirrored elsewhere, and the catch, length , 
      # and age comp data for that fleet that you are mirroring are removed what do you do 
      # to that selectivity, do you fix it?
      
      # QUESTION: If you have time-varying selectivity, do you need to remove it if any
      # particular data is removed?
      
    } else{
      years_to_match <- only_these_years
    }
    
    # If no fleet_or_index, remove all catch
    if(is.null(fleet_or_index)){
      fleet_or_index <- unique(inputs$dat$catch$fleet)
    }
    
    inputs$dat$catch <- inputs$dat$catch |>
      mutate(year = case_when(index %in% fleet_or_index & year %in% years_to_match ~ -1*abs(year), 
                              TRUE ~ year))
    
    # QUESTION: You do have to remove length and age comps for a catch fleet if that 
    # catch fleet is removed, right?
    
    # Set length and age comp data to negative year if catch is removed and 
    # doesn't have associated catch or index data
    inputs$dat$lencomp <- inputs$dat$lencomp |>
      mutate(year = case_when(fleet %in% not_in_indices & year %in% years_to_match ~ -1*abs(year), 
                              TRUE ~ year))
    inputs$dat$agecomp <- inputs$dat$agecomp |>
      mutate(year = case_when(fleet %in% not_in_indices & year %in% years_to_match ~ -1*abs(year), 
                              TRUE ~ year))
    if(!is.null(inputs$dat$MeanSize_at_Age_obs)){
      inputs$dat$MeanSize_at_Age_obs <- inputs$dat$MeanSize_at_Age_obs |>
        mutate(year = case_when(fleet %in% not_in_indices & year %in% years_to_match ~ -1*abs(year), 
                                TRUE ~ year))
    }
    
  }
  ##### Removing indices ##### ---------------------------------------------------
  # Creates variations of vectors to remove that are seen in fleets and row names of data and parameters
  if("CPUE" %in% type){
    indices_chr <- paste0("(", fleet_or_index, ")$")
    not_in_catch <- setdiff(fleet_or_index, inputs$dat$catch$fleet)
    indices_und <- paste0(not_in_catch, "_")
    indices_chr2 <- paste0("(", not_in_catch, ")$")
    indices_chr2_und <- paste0("(", not_in_catch, ")$")
    
    
    # If no years in only_these_years, then remove all years
    if(is.null(only_these_years)){
      years_to_match <- unique(inputs$dat$CPUE$year)
    } else{
      years_to_match <- only_these_years
    }
    
    # If no fleet_or_index, remove all indices
    if(is.null(fleet_or_index)){
      fleet_or_index <- unique(inputs$dat$CPUE$index)
    }
    
    # Set indices to negative year to remove
    inputs$dat$CPUE <- inputs$dat$CPUE |>
      mutate(year = case_when(index %in% fleet_or_index & year %in% years_to_match ~ -1*abs(year), 
                              TRUE ~ year))
    
    # Set length and age comp data to negative year if index is removed and 
    # doesn't have associated catch data
    inputs$dat$lencomp <- inputs$dat$lencomp |>
      mutate(year = case_when(fleet %in% not_in_catch & year %in% years_to_match ~ -1*abs(year), 
                              TRUE ~ year))
    inputs$dat$agecomp <- inputs$dat$agecomp |>
      mutate(year = case_when(fleet %in% not_in_catch & year %in% years_to_match ~ -1*abs(year), 
                               TRUE ~ year))
    if(!is.null(inputs$dat$MeanSize_at_Age_obs)){
      inputs$dat$MeanSize_at_Age_obs <- inputs$dat$MeanSize_at_Age_obs |>
        mutate(year = case_when(fleet %in% not_in_catch & year %in% years_to_match ~ -1*abs(year), 
                                TRUE ~ year))
    }
    
    ## Remove Q, and T-V params if indices are fully removed
    if(is.null(only_these_years)){
      # Remove catchability params if indices are removed
      inputs$ctl$Q_options <- inputs$ctl$Q_options |>
        filter(! fleet %in% indices_to_remove)
      inputs$ctl$Q_parms <- inputs$ctl$Q_parms[!grepl(paste(indices_chr, collapse = "|"), rownames(inputs$ctl$Q_parms)),]
      inputs$ctl$Q_parms_tv[grepl(paste(indices_chr2_und, collapse = "|"), rownames(inputs$ctl$Q_parms_tv)),] <- NULL
    }
  }
  ##### Removing length comps ##### --------------------------------------------
  if("lencomp" %in% type){
    if(is.null(only_these_years)){
      years_to_match <- unique(inputs$dat$lencomp$year)

      # If lencomp belongs to an index, need to fix selex params
      if(!fleet_or_index %in% unique(inputs$dat$catch$fleet)){
        
        inputs$ctl$size_selex_parms <- inputs$ctl$size_selex_parms |>
          mutate(PHASE = case_when(grepl(paste(not_in_catch, collapse = "|"), rownames(inputs$ctl$size_selex_parms)) ~ abs(PHASE)*-1,
                                  TRUE ~ PHASE))
      }
    } else{
      years_to_match <- only_these_years
    }
    
    # If no fleet_or_index, remove all lencomp
    if(is.null(fleet_or_index)){
      fleet_or_index <- unique(inputs$dat$lencomp$fleet)
    }
    
    inputs$dat$lencomp <- inputs$dat$lencomp |>
      mutate(year = case_when(fleet %in% fleet_or_index & year %in% years_to_match ~ -1*abs(year), 
                              TRUE ~ year))
  }
  
  ##### Removing age comps ##### -----------------------------------------------
  if("agecomp" %in% type){
    if(is.null(only_these_years)){
      years_to_match <- unique(inputs$dat$lencomp$year)
      
      # If lencomp belongs to an index, need to fix selex params
      if(!is.null(inputs$ctl$age_selex_parms)){
        if(!fleet_or_index %in% unique(inputs$dat$catch$fleet)){
          inputs$ctl$age_selex_parms <- inputs$ctl$age_selex_parms |>
            mutate(PHASE = case_when(grepl(paste(not_in_catch, collapse = "|"), rownames(inputs$ctl$age_selex_parms)) ~ abs(PHASE)*-1,
                                     TRUE ~ PHASE))
        }
      }
    } else{
      years_to_match <- only_these_years
    }
    
    # If no fleet_or_index, remove all lencomp
    if(is.null(fleet_or_index)){
      fleet_or_index <- unique(inputs$dat$agecomp$fleet)
    }
    
    inputs$dat$agecomp <- inputs$dat$lencomp |>
      mutate(year = case_when(fleet %in% fleet_or_index & year %in% years_to_match ~ -1*abs(year), 
                              TRUE ~ year))
  }
  
  ##### Fix endyr & block year end years if removing years from all data ##### ------------------------
  all_endyrs <- c(max(inputs$dat$catch$year), max(inputs$dat$CPUE$year),
                  max(inputs$dat$lencomp$year), max(inputs$dat$agecomp$year))
  if(max(all_endyrs) < inputs$dat$endyr){
    inputs$dat$endyr <- max(all_endyrs)
  }
  
  inputs$ctl$Block_Design <- lapply(inputs$ctl$Block_Design, function(inner_list) {
    if (inner_list[2] > inputs$dat$endyr) {
      inner_list[2] <- inputs$dat$endyr
    }
    return(inner_list)
  })
  
  
  # QUESTION: Will the recdev years need to be changed if certain years are removed?
}

