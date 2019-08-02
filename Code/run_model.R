#!/usr/bin/env Rscript
# Run Models
# A generic script to run all of the models
# Command-line arguments will be used to specify which models to run
library(checkpoint)
checkpoint("2019-05-01", checkpointLocation = '/root/', scanForPackages = FALSE)
suppress_all <- function(x){
  suppressWarnings(suppressMessages(x))
}


suppress_all(library(optparse))
suppress_all(library(data.table))
suppress_all(library(glmnet))
suppress_all(library(xgboost))
suppress_all(library(ROCR))
suppress_all(library(MLmetrics))

# Command-line Arguments
option_list <- list(
  make_option(c("-g", "--grouping"), action = "store", default = "ahrq",
              help = "The grouping method used for icd codes. One of 'ahrq', 'ccs', 'trunc', or 'raw'"),
  make_option(c("-o", "--output_dir"), action =  "store", default = "./Output/",
              help = "The directory where the output gets stored"),
  make_option(c("-m", "--model_type"), action = "store", default = "l1",
              help = "The model type to be run. One of 'l1', 'l2', 'rf', or 'xgb'")
)

# Example run:
# Rscript run_model.R -g ahrq -m xgb 

parser <- OptionParser(usage = "%prog [options]", option_list = option_list)
arguments <- parse_args(parser, positional_arguments = 0)

# Create output directory
dir.create(arguments$options$output_dir, showWarnings = FALSE)

# Split sizes to test
split <- c(5000, 10000, 20000, 35000)

# Model Run and Evaluation Code ==============================
# Penalized Logistic Regression
train_penalized_logistic <- function(model_matrix_list, train_indices, test_indices, alpha){
  # alpha here represents the call to glmnet. Alpha = 0 turns the regularization to the l-2 norm
  # alpha = 1 (default) refers to the LASSO-like penalty (l1 norm)
  
  # Initialize data.frames to store results 
  auc <- data.frame(auc_death = numeric())
  pr <- data.frame(pr_death = numeric())
  predictions <- data.frame(pred_death = numeric(), death_in_year = numeric(), sample_size = numeric())
  
  # cannot create empty data frame for time_elapsed in the same way because we are trying to rbind proc_time objects
  time_elapsed <- data.frame(user = 0, system = 0, elapsed = 0)

  for(i in split){
    # Set indices for training based on split size
    subset_indices <- train_indices[1:i]
    set.seed(1)
    
    # Fit model and get timing information
    time_elapsed <- rbind(time_elapsed, system.time(model_death <- cv.glmnet(y = model_matrix_list[[2]][subset_indices], 
                                                                             x = model_matrix_list[[1]][subset_indices, ], 
                                                                             family = "binomial", 
                                                                             maxit = 10000, 
                                                                             alpha = alpha,
                                                                             thresh = 1E-6, 
                                                                             type.logistic = "modified.Newton")))
    # prediction on the holdout set
    pred_death <- predict(model_death, model_matrix_list[[1]][test_indices,], type = "response")
    
    # add predictions to the stored results
    predictions_death <- cbind(pred_death = pred_death[,1], death_in_year = model_matrix_list[[2]][test_indices], sample_size = i)
    predictions <- rbind(predictions, predictions_death)
    
    # calculate auc and store results
    auc_death <- auc(model_matrix_list[[2]][test_indices], pred_death)
    auc <- rbind(auc, data.frame(auc_death = auc_death))
    
    # calculate pr and store results
    pr_death <- PRAUC(pred_death[,1], model_matrix_list[[2]][test_indices])
    pr <- rbind(pr, data.frame(pr_death = pr_death))
  }
  # remove the dummy 0,0,0 row in time_elapsed, created just to maintain col header
  time_elapsed <- time_elapsed[-1, ]
  return(list(auc, pr, time_elapsed, predictions))
}

rf_calc <- function(model_matrix_list, train_indices, test_indices){
  
  auc_death_final <- data.frame(auc_death = numeric())
  time_elapsed <- data.frame(user = 0, system = 0, elapsed = 0)
  pr <- data.frame(pr_death = numeric())
  predictions <- data.frame(pred_death = numeric(), death_in_year = numeric(), sample_size = numeric())

  # Parameters to tune
  params <- expand.grid(eta =  1,
                        max_depth = c(4,5,6),
                        gamma = 1,
                        min_child_weight = c(3,5),
                        colsample_bynode= c(0.5, 0.8),
                        subsample = c(0.5, 0.8),
                        num_parallel_tree = 500)
  
  for(i in split){
    
    # dataframe to hold aucs for ALL combinations of parameters for a given split
    auc_death_all <- data.frame(auc = numeric())
    subset <- train_indices[1:i]
    
    time_elapsed <- 
      rbind(time_elapsed, 
            system.time(for(j in 1:nrow(params)){
              set.seed(1)
              model_death <- xgboost(data = model_matrix_list[[1]][subset,], 
                                     label = model_matrix_list[[2]][subset], 
                                     params = params[j,], 
                                     objective = "binary:logistic", 
                                     nrounds = 1,
                                     verbose = 0)
              
              pred_death <- predict(model_death, model_matrix_list[[1]][test_indices,], type="response")
              auc_death <- auc(model_matrix_list[[2]][test_indices], pred_death)
              auc_death_all <- rbind(auc_death_all, data.frame(death = auc_death))
            }))
    # find index of max auc
    auc_greatest_death <- which.max(auc_death_all$death)
    
    model_death <- xgboost(data = model_matrix_list[[1]][subset,],
                           label = model_matrix_list[[2]][subset], 
                           params = params[auc_greatest_death,],
                           objective = "binary:logistic",
                           nrounds = 1,
                           verbose = 0)
    
    # bind max auc value to final auc dataframe
    pred_death <- predict(model_death, model_matrix_list[[1]][test_indices,], type="response")
    auc_death <- auc(model_matrix_list[[2]][test_indices], pred_death)
    auc_death_final <- rbind(auc_death_final, data.frame(auc_death = auc_death_all$death[auc_greatest_death]))
    
    predictions_death <- cbind(pred_death = pred_death, death_in_year = model_matrix_list[[2]][test_indices], sample_size = i)
    predictions <- rbind(predictions, predictions_death)
    # calculate pr score
    pr_death <- PRAUC(pred_death, model_matrix_list[[2]][test_indices])
    
    # add to pr dataframe
    pr <- rbind(pr, data.frame(pr_death = pr_death))
  }
  # first row of time_elapsed is dummy
  time_elapsed <- time_elapsed[-1, ]
  return(list(auc_death_final, pr, time_elapsed, predictions))  
}

xgb_calc <- function(model_matrix_list, train_indices, test_indices){
  # dataframe to hold auc from best combination of parameters for each split
  auc_death_final <- data.frame(auc_death = numeric())
  time_elapsed <- data.frame(user = 0, system = 0, elapsed = 0)
  pr <- data.frame(pr_death = numeric())
  predictions <- data.frame(pred_death = numeric(), death_in_year = numeric(), sample_size = numeric())

  
  # create a parameter grid (3 combos right now)
  params <- expand.grid(eta =  seq(0.1,0.3,0.1),
                        max_depth = 6,
                        nrounds = 100,
                        gamma = 1,
                        min_child_weight = 5,
                        colsample_bytree = seq(0.5, 1, 0.25),
                        subsample = seq(0.5, 1, 0.25))
  
  for(i in split){
    
    # dataframe to hold aucs for ALL combinations of parameters for a given split
    auc_death_all <- data.frame(auc = numeric())
    subset <- train_indices[1:i]
    
    time_elapsed <- 
      rbind(time_elapsed, 
            system.time(for(j in 1:nrow(params)){
              set.seed(1)
              model_death <- xgboost(data = model_matrix_list[[1]][subset,], 
                                     label = model_matrix_list[[2]][subset], 
                                     params = params[j,], 
                                     objective = "binary:logistic", 
                                     nrounds = 150,
                                     verbose = 0)
              
              pred_death <- predict(model_death, model_matrix_list[[1]][test_indices,], type="response")
              auc_death <- auc(model_matrix_list[[2]][test_indices], pred_death)
              auc_death_all <- rbind(auc_death_all, data.frame(death = auc_death))
            }))
    # find index of max auc
    auc_greatest_death <- which.max(auc_death_all$death)
    
    model_death <- xgboost(data = model_matrix_list[[1]][subset,],
                           label = model_matrix_list[[2]][subset], 
                           params = params[auc_greatest_death,],
                           objective = "binary:logistic",
                           nrounds = 150,
                           verbose = 0)
    
    # bind max auc value to final auc dataframe
    pred_death <- predict(model_death, model_matrix_list[[1]][test_indices,], type="response")
    auc_death <- auc(model_matrix_list[[2]][test_indices], pred_death)
    auc_death_final <- rbind(auc_death_final, data.frame(auc_death = auc_death_all$death[auc_greatest_death]))
    
    predictions_death <- cbind(pred_death = pred_death, death_in_year = model_matrix_list[[2]][test_indices], sample_size = i)
    predictions <- rbind(predictions, predictions_death)
    
    # calculate pr score
    pr_death <- PRAUC(pred_death, model_matrix_list[[2]][test_indices])
    
    # add to pr dataframe
    pr <- rbind(pr, data.frame(pr_death = pr_death))
  }
  # first row of time_elapsed is dummy
  time_elapsed <- time_elapsed[-1, ]
  return(list(auc_death_final, pr, time_elapsed, predictions))  
}


# ==================================
# Prep experiment based on args that are passed
if (arguments$options$model_type == "l1"){
  run_model <- train_penalized_logistic
  alpha <- 1
} else if (arguments$options$model_type == "l2"){
  run_model <- train_penalized_logistic
  alpha <- 0
} else if (arguments$options$model_type == "rf"){
  run_model <- rf_calc
} else if (arguments$options$model_type == "xgb"){
  run_model <- xgb_calc
}


# ==================================
# Run Models and store results

print(paste0("Running ", arguments$options$model_type, " model with Total codes grouped by ", arguments$options$grouping))
total_matrix_list <- readRDS(paste0("./Data/Processed/", arguments$options$grouping, "_total_matrix.rds"))

# Train and Test split
set.seed(1)
random_indices <- sample(1:total_matrix_list[[1]]@Dim[1])
train_indices <- random_indices[1:round(0.8*length(random_indices))]
test_indices <- random_indices[(round(0.8*length(random_indices)+1):length(random_indices))]

if (exists("alpha")){
  total_results <- run_model(total_matrix_list, train_indices, test_indices, alpha)
} else {
  total_results <- run_model(total_matrix_list, train_indices, test_indices)
}
saveRDS(total_results, paste0(arguments$options$output_dir, arguments$options$model_type, "_", arguments$options$grouping, "_total_results.rds"))

remove(total_matrix_list)

print(paste0("Running ", arguments$options$model_type, " model with Binary codes grouped by ", arguments$options$grouping))
binary_matrix_list <- readRDS(paste0("../Data/Processed/", arguments$options$grouping, "_binary_matrix.rds"))

if (exists("alpha")){
  binary_results <- run_model(binary_matrix_list, train_indices, test_indices, alpha)
} else {
  binary_results <- run_model(binary_matrix_list, train_indices, test_indices)
}

saveRDS(binary_results, paste0(arguments$options$output_dir, arguments$options$model_type, "_", arguments$options$grouping, "_binary_results.rds"))


