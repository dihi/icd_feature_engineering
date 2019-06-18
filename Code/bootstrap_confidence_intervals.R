#!/usr/bin/env Rscript
# Bootstrap AUC and F1 measures

suppress_all <- function(x) {
  suppressWarnings(suppressMessages(x))
}

suppress_all(library(glmnet))
suppress_all(library(optparse))
suppress_all(library(MLmetrics))

option_list <- list(
  make_option(c("-o", "--output_dir"), action =  "store", default = "./Output/",
              help = "The directory where the output gets stored"),
  make_option(c("-n", "--num_boots"), action = "store", default = 5000,
              help = "Number of bootstrap samples when determining confidence intervals"),
  make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
              help = "Whether to print progress")
)

parser <- OptionParser(usage = "%prog [options]", option_list = option_list)
arguments <- parse_args(parser, positional_arguments = 0)

bootstrap_measures <- function(results, num_boots, debug = FALSE) {
  # Generates bootstrapped AUC and pr measures
  # Params
  # ------
  # results (data.frame): results dataframe produced by model run experiments
  # num_boots (numeric): number of bootstrap samples to perform
  #
  # Returns
  # -------
  # bootstrapped_df (data.frame): a data.frame with the sample size and any calculated percentiles
  

  result_df <- results[[4]]
  class(result_df) <- "data.frame"
  
  bootstrapped_df <- data.frame(sample_size = numeric(),
                                measure = character(),
                                lower_bound = numeric(),
                                upper_bound = numeric())
  
  for (sample_size in unique(result_df[["sample_size"]])){
    if (debug == TRUE){
      print(paste0("sample_size: ", sample_size))
    }
    subset_df <- result_df[result_df['sample_size'] == sample_size, c("pred_death", "death_in_year")]
    
    auc_bootstrapped <- numeric(num_boots)
    pr_bootstrapped <- numeric(num_boots)
    
    for (n in seq_along(1:num_boots)){
      if (debug == TRUE){
        if (n %% 100 == 0){
          print(n)
        }
      }
      # Generate sample indices
      sample_indices <- sample(1:nrow(subset_df), size = nrow(subset_df), replace = TRUE)
      temp_predictions <- subset_df$pred_death[sample_indices]
      temp_label <- subset_df$death_in_year[sample_indices]
      auc_bootstrapped[n] <- auc(temp_label, temp_predictions)
      pr_bootstrapped[n] <- PRAUC(temp_predictions, temp_label)
    }
    
    # Calculate percentiles
    auc_percentiles <- quantile(auc_bootstrapped, c(0.025, 0.975))
    pr_percentiles <- quantile(pr_bootstrapped, c(0.025, 0.975))
    
    bootstrapped_df <- rbind(bootstrapped_df, data.frame(
                                                sample_size = sample_size,
                                                measure = "AUC",
                                                lower_bound = auc_percentiles[1],
                                                upper_bound = auc_percentiles[2]
    ))
    bootstrapped_df <- rbind(bootstrapped_df, data.frame(
                                                sample_size = sample_size,
                                                measure = "pr",
                                                lower_bound = pr_percentiles[1],
                                                upper_bound = pr_percentiles[2]
    ))
    
  }
  
  return(bootstrapped_df)
}


# ====================
# Run bootstrap and store results

files_to_process <- list.files(arguments$options$output_dir)

for (f in files_to_process) {
  if (grepl(".rds", f) == TRUE & grepl("results", f) == TRUE) {
    if (arguments$options$verbose == TRUE) {
      print(paste0("Now processing: ", f))
      temp_results <- readRDS(paste0(arguments$options$output_dir, f))
      bootstrapped_results <- bootstrap_measures(temp_results, num_boots = arguments$options$num_boots)
      if (arguments$options$verbose == TRUE) {
        print(paste0("Writing results for: ", f))
      }
      saveRDS(bootstrapped_results, paste0(arguments$options$output_dir, gsub("results.rds", "bootstrap.rds", f)))
    }
  }
}
