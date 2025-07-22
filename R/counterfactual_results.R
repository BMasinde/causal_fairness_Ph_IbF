# Creating a function: Computes median, means and generates counterfactual boxplots
#'   @param cf_data Counterfactual dataset
#'   @param cluster_levels cluster levels

counterfactual_results <- function(cf_data, cluster_levels) {
  
  # list outputs:
  plots_list  <- list()
  means_list  <- list()
  median_list  <- list()
  
  # number of clusters
  n_cluster <- length(cluster_levels)
  
  for (i in 1:n_cluster) {
    # Get the current entry
    cat("currently evaluating cluster:", cluster_levels[i], sep = " ")
    
    # data by cluster
    cluster_i_data <- melor_2015 %>%
      filter(Cluster == cluster_levels[i])
    
    
    
    # Create boxplot
    p <- ggplot(cluster_i_data, aes(x = island_groups, y = damage_preds, fill = island_groups)) +
      geom_boxplot() +
      labs(title = paste("Predicted Damage Distribution - List Entry", i),
           x = "Island Region",
           y = "Predicted Damage") +
      theme_minimal()
    
    # Save the plot in the list
    plots_list[[i]] <- p
    
    # Calculate the mean of damage_preds for each island_groups
    mean_values <- cluster_i_data %>%
      group_by(island_groups) %>%
      summarise(mean_damage = mean(damage_preds, na.rm = TRUE))
    
    # Save the means in the list
    means_list[[i]] <- mean_values
    
    # Calculate median of the damage_preds for each island groups
    median_values  <- cluster_i_data %>%
      group_by(island_groups) %>%
      summarise(median_damage = median(damage_preds, na.rm = TRUE))
    
    # save the medians in the list
    median_list[[i]]  <- median_values
    
  }
  
  return(list(
    "plots" = plots_list,
    "averages" =  means_list,
    "median" =  median_list
  ))
  
}