# HURDLE METHOD FUNCTION
#' Title: Predict the building damage % from TCs
#'
#' Function takes the test data & trained models and returns predicted building damages.
#'
#' @param df A dataframe for prediction (can be the test set for testing hurdle method)
#' @param ass_clas_model The trained model for classification
#' @param ass_base_model A list of the SCM models for the base regression
#' @param ass_trunc_model A list of SCM models for the high-impact regression
#'
#'

ass_hurdle_function <- function(df, ass_clas_model, ass_base_model, ass_trunc_model, threshold) {
  
  # factors cleaning for classification task
  #df$damage_binary_2 <- factor(df$damage_binary,
   #                            levels = c("0", "1"),  # Your current levels
  #                             labels = c("Damage_below_10", "Damage_above_10"))  # New valid labels
  
  ## Step 1: Predict the class label (whether the damage will exceed the threshold)
  ## class_model should return predicted classes and not probs.
  ## type = "prob" for custom threshold specification
  prob_pred <- predict(ass_clas_model, df, type = "prob")[,2]  # Probability of class 1
  ## assigning final class based on threshold
  class_pred <- ifelse(prob_pred > threshold, 1, 0) # low threhold of 0.35 can be changed to 0.65/0.75
  
  class_pred  <- factor(class_pred, levels = c("0", "1"),  # Your current levels
                        labels = c("Damage_below_10", "Damage_above_10"))  # New valid labels
  
  ## Step 2: Predict the base damage percentage using the base regression model (for low impact cases)
  ## should return the predicted damage percentages
  base_pred <- predict(ass_base_model, df)
  
  ## Step 3: Predict the high-impact damage percentage using the high-impact
  
  high_pred <- predict(ass_trunc_model, df)
  
  # Step 4: Apply the hurdle method logic
  predicted_damage <- ifelse(class_pred == "Damage_above_10", high_pred, base_pred)
  
  # Return the predicted damage
  return(predicted_damage)
}
