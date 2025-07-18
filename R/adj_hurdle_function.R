# HURDLE METHOD FUNCTION
#' Title: Predict the building damage % from TCs
#'
#' Function takes the test data & trained models and returns predicted building damages.
#'
#' @param df A dataframe for prediction (can be the test set for testing hurdle method)
#' @param class_model The trained model for classification
#' @param scm_models_base A list of the SCM models for the base regression
#' @param scm_models_high A list of SCM models for the high-impact regression
#'
#'

adj_hurdle_function <- function(df, scm_models_base, scm_models_high, threshold) {
  
  #set.seed(0000)
  base_col_models_list <- list(
    wind_max = scm_models_base[["base_wind_model"]],
    rain_total = scm_models_base[["base_rain_model"]],
    roof_strong_wall_strong = scm_models_base[["base_roof_strong_wall_strong_model"]],
    roof_strong_wall_light = scm_models_base[["base_roof_strong_wall_light_model"]],
    roof_strong_wall_salv = scm_models_base[["base_roof_strong_wall_salv_model"]],
    roof_light_wall_strong = scm_models_base[["base_roof_light_wall_strong_model"]],
    roof_light_wall_light = scm_models_base[["base_roof_light_wall_light_model"]],
    roof_light_wall_salv = scm_models_base[["base_roof_light_wall_salv_model"]],
    roof_salv_wall_strong = scm_models_base[["base_roof_salv_wall_strong_model"]],
    roof_salv_wall_light = scm_models_base[["base_roof_salv_wall_light_model"]],
    roof_salv_wall_salv = scm_models_base[["base_roof_salv_wall_salv_model"]]
  )
  
  ## common predictions btw class & base regression
  df1 <-  df %>%
    mutate(across(names(base_col_models_list), ~ predict(base_col_models_list[[cur_column()]],
                                                         newdata = df), .names = "{.col}_pred"))
  
  # predict for interaction terms
  # Define wind and rain interaction variables
  wind_fractions <- c("blue_ss_frac", "yellow_ss_frac", "orange_ss_frac", "red_ss_frac")
  rain_fractions <- c("blue_ls_frac", "yellow_ls_frac", "orange_ls_frac", "red_ls_frac")
  
  for (col in wind_fractions) {
    new_col_name <- paste0("wind_", col)
    df1[[new_col_name]] <- df1[[col]] * df1 [["wind_max_pred"]]
  }
  
  # Multiply rain fractions by rain_total_pred
  for (col in rain_fractions) {
    new_col_name <- paste0("rain_", col)
    df1[[new_col_name]] <- df1[[col]] * df1[["rain_total_pred"]]
  }
  
  # WHY DO WE NEED THE OUTCOME VARIABLE HERE FACTOR COLUMNS
  # factors cleaning for classification task
  # df$damage_binary_2 <- factor(df$damage_binary,
  #                                      levels = c("0", "1"),  # Your current levels
  #                                      labels = c("Damage_below_10", "Damage_above_10"))  # New valid labels
  
  ## Step 1: Predict the class label (whether the damage will exceed the threshold)
  ## class_model should return predicted classes and not probs.
  ## class_model expects variables "wind_max_pred" and "rain_total_pred" in dataframe df
  ## type = "prob" for custom threshold specification
  
  prob_pred <- predict(scm_models_base$base_clas_full_model, df1, type = "prob")[,2]  # Probability of class 1
  ## assigning final class based on threshold
  class_pred <- ifelse(prob_pred > threshold, 1, 0) # low threhold of 0.35 can be changed to 0.65/0.75
  
  class_pred  <- factor(class_pred, levels = c("0", "1"),  # Your current levels
                        labels = c("Damage_below_10", "Damage_above_10"))  # New valid labels
  
  ## Step 2: Predict the base damage percentage using the base regression model (for low impact cases)
  ## base expects variables "wind_max_pred" and "rain_total_pred" in dataframe df
  ## should return the predicted damage percentages
  base_pred <- predict(scm_models_base$base_reg_model, df1)
  
  ## Step 3: Predict the high-impact damage percentage using the high-impact
  ### SCM models (for high impact cases)
  ## wind and rainfall predictions are based on high impact data (damage >= 10)
  
  trunc_col_models_list <- list(
    wind_max = scm_models_high[["trunc_wind_model"]],
    rain_total = scm_models_high[["trunc_rain_model"]],
    roof_strong_wall_strong = scm_models_high[["trunc_roof_strong_wall_strong_model"]],
    roof_strong_wall_light = scm_models_high[["trunc_roof_strong_wall_light_model"]],
    roof_strong_wall_salv = scm_models_high[["trunc_roof_strong_wall_salv_model"]],
    roof_light_wall_strong = scm_models_high[["trunc_roof_light_wall_strong_model"]],
    roof_light_wall_light = scm_models_high[["trunc_roof_light_wall_light_model"]],
    roof_light_wall_salv = scm_models_high[["trunc_roof_light_wall_salv_model"]],
    roof_salv_wall_strong = scm_models_high[["trunc_roof_salv_wall_strong_model"]],
    roof_salv_wall_light = scm_models_high[["trunc_roof_salv_wall_light_model"]],
    roof_salv_wall_salv = scm_models_high[["trunc_roof_salv_wall_salv_model"]]
  )
  # add the predictions of wind and rainfall to the dataframe df
  df2 <- df %>%
    mutate(across(names(trunc_col_models_list), ~ predict(trunc_col_models_list[[cur_column()]],
                                                          newdata = df), .names = "{.col}_pred"))
  # compute interaction terms based on the predictions
  for (col in wind_fractions) {
    new_col_name <- paste0("wind_", col)
    df2[[new_col_name]] <- df2[[col]] * df2[["wind_max_pred"]]
  }
  
  # Multiply rain fractions by rain_total_pred
  for (col in rain_fractions) {
    new_col_name <- paste0("rain_", col)
    df2[[new_col_name]] <- df2[[col]] * df2[["rain_total_pred"]]
  }
  
  high_pred <- predict(scm_models_high$trunc_reg_model, df2)
  
  # Step 4: Apply the hurdle method logic
  predicted_damage <- ifelse(class_pred == "Damage_above_10", high_pred, base_pred)
  
  # Return the predicted damage
  return(predicted_damage)
}