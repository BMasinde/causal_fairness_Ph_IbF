# Creating a function that generates a counterfactual dataset

#' @title counterfactual_gen
#' @description Function takes argguments of df, tc, matches and returns a
#' counterfactual dataframe to be used by the hurdle function
#' @param df counterfactual_test_data
#' @param tc tropical cyclone name
#' @param matches municipality matches NOT NEEDED
#' @return counterfactual_data_list return

counterfactual_gen  <- function(df, tc, storm_surge, landslide,){
  
  # get unique municipality codes
  mun_code  <- unique(df$Mun_Code)
  
  # filter df by tc and get hazard characheristics
  counterfactual_data  <- df %>%
    filter(typhoon == tc) %>% # keep the minimum distance from the filter
    mutate(track_min_dist = min(track_min_dist, na.rm = TRUE),
           rain_total = rain_total[which.min(track_min_dist)],
           wind_max = wind_max[which.min(track_min_dist)],
           wind_blue_ss = wind_max * storm_surge,
           wind_yellow_ss = wind_max * storm_surge,
           wind_orange_ss = wind_max * storm_surge,
           wind_red_ss = wind_max * storm_surge,
           rain_blue_ls = rain_total * landslide,
           rain_yellow_ls = rain_total * landslide,
           rain_orange_ls = rain_total * landslide,
           rain_red_ls = rain_total * landslide,
           blue_ss_frac = storm_surge,
           yellow_ss_frac = storm_surge,
           orange_ss_frac = storm_surge,
           red_ss_frac = storm_surge,
           blue_ls_frac = landslide,
           yellow_ls_frac = landslide,
           orange_ls_frac= landslide,
           red_ls_frac = landslide
    ) %>%
    select(-typhoon)
  
  # which municipalities are not in the filtered data?
  missing_mun  <- setdiff(mun_code, counterfactual_data$Mun_Code)
  
  # debugging
  #cat("number of missing municipalities:", sep = " ", length(missing_mun))
  
  # Check if there are any missing municipalities
  if (length(missing_mun) > 0) {
    # Get the characteristics of the missing mun codes
    #remaining_mun <- df %>%
    #    filter(Mun_Code %in% missing_mun) %>%
    #    select(-typhoon, -rain_total, -wind_max, -track_min_dist)
    
    # Assign the hazard characteristics from the counterfactual data
    remaining_mun <- df %>%
      filter(Mun_Code %in% missing_mun) %>% # after filtering Mun_Code has duplicates how do we remove duplicates?
      distinct(Mun_Code, .keep_all = TRUE) %>%  # Keeps the first occurrence of each Mun_Code
      mutate(track_min_dist = unique(counterfactual_data$track_min_dist),
             rain_total = unique(counterfactual_data$rain_total),
             wind_max = unique(counterfactual_data$wind_max),
             wind_blue_ss = wind_max * storm_surge,
             wind_yellow_ss = wind_max * storm_surge,
             wind_orange_ss = wind_max * storm_surge,
             wind_red_ss = wind_max * storm_surge,
             rain_blue_ls = rain_total * landslide,
             rain_yellow_ls = rain_total * landslide,
             rain_orange_ls = rain_total * landslide,
             rain_red_ls = rain_total * landslide,
             blue_ss_frac = storm_surge,
             yellow_ss_frac = storm_surge,
             orange_ss_frac = storm_surge,
             red_ss_frac = storm_surge,
             blue_ls_frac = landslide,
             yellow_ls_frac = landslide,
             orange_ls_frac= landslide,
             red_ls_frac = landslide
      ) %>%
      select(-typhoon)
    # debugging
    cat("number of columns in remaining_mun", sep = " ", ncol(remaining_mun))
    
    cat("\n number of columns in counterfactual data", sep = " ", ncol(counterfactual_data))
    
    # Add the remaining municipalities back into the counterfactual data
    counterfactual_data <- rbind(counterfactual_data, remaining_mun)
  }
  
  # df should have all the 1478 municipalities
  
  return(counterfactual_data) # returns a dataframe (maybe list for more experiments)
}
