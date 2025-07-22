# Creating a function that generates a filtered dataset

#' @title filter_by_tc
#' @description Function takes argguments of df, tc, matches and returns a
#' counterfactual dataframe to be used by the hurdle function
#' @param df counterfactual_test_data
#' @param tc tropical cyclone name
#' @param matches municipality matches NOT NEEDED
#' @return counterfactual_data_list return

filter_by_tc  <- function(df, tc){
  
  library(dplyr)
  
  # get unique municipality codes
  mun_code  <- unique(df$Mun_Code)
  
  # filter df by tc and get hazard characheristics
  filtered_data  <- df %>%
    filter(typhoon == tc) %>% # keep the minimum distance from the filter
    mutate(
      wind_blue_ss = wind_max * blue_ss_frac,
      wind_yellow_ss = wind_max * yellow_ss_frac,
      wind_orange_ss = wind_max * orange_ss_frac,
      wind_red_ss = wind_max * red_ss_frac,
      rain_blue_ss = wind_max * blue_ls_frac,
      rain_yellow_ss = wind_max * yellow_ls_frac,
      rain_orange_ss = wind_max * orange_ls_frac,
      rain_red_ss = wind_max * red_ls_frac
    ) 
  
  # which municipalities are not in the filtered data?
  missing_mun  <- setdiff(mun_code, filtered_data$Mun_Code)
  
  # debugging
  cat("number of missing municipalities:", sep = " ", length(missing_mun))
  
  # IF WE ARE TO GO THIS ROUTE THEN WE HAVE TO
  # SET distance, wind and rain to zero...
  # Check if there are any missing municipalities
  # if (length(missing_mun) > 0) {
  #   # Get the characteristics of the missing mun codes
  #   #remaining_mun <- df %>%
  #   #    filter(Mun_Code %in% missing_mun) %>%
  #   #    select(-typhoon, -rain_total, -wind_max, -track_min_dist)
  #   
  #   # Assign the hazard characteristics from the counterfactual data
  #  # remaining_mun <- df %>%
  #    filter(Mun_Code %in% missing_mun) %>% # after filtering Mun_Code has duplicates how do we remove duplicates?
  #     distinct(Mun_Code, .keep_all = TRUE) %>%  # Keeps the first occurrence of each Mun_Code
  #     mutate(wind_blue_ss = wind_max * blue_ss_frac,
  #            wind_yellow_ss = wind_max * yellow_ss_frac,
  #            wind_orange_ss = wind_max * orange_ss_frac,
  #            wind_red_ss = wind_max * red_ss_frac,
  #            rain_blue_ss = wind_max * blue_ls_frac,
  #            rain_yellow_ss = wind_max * yellow_ls_frac,
  #            rain_orange_ss = wind_max * orange_ls_frac,
  #            rain_red_ss = wind_max * red_ls_frac #,
  #            #damage_perc = 0, # set damage variable to zero or "Damage_below_10"
  #            #damage_binary = 0,
  #            #damage_binary_2 = "Damage_below_10"
  #     ) 
  #   # debugging
  #   cat("number of columns in remaining_mun", sep = " ", ncol(remaining_mun))
  #   
  #   cat("\n number of columns in filtered data", sep = " ", ncol(filtered_data))
  #   
  #   # Add the remaining municipalities back into the counterfactual data
  #   filtered_data <- rbind(filtered_data, remaining_mun)
    
    
  #}
  
  # column clean-up
  filtered_data  <- filtered_data %>%
    select(#-typhoon, 
           -Unnamed..0,
           -X10.Digit.Code,
           -Correspondence.Code,
           -vulnerable_groups,
           -pantawid_benef,
           -rain_max6h,
           -rain_max24h,
           -poverty_pct,
           -housing_units,
           -Income.Class,
           -Population.2020.Census.,
           -poverty_pct,
           #-damage_perc
    )
  
  # df should have all the 1478 municipalities
  
  return(filtered_data) # returns a dataframe (maybe list for more experiments)
}