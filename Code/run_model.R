#!/usr/bin/env Rscript
# Run Models
# A generic script to run all of the models
# Command-line arguments will be used to specify which models to run

suppress_all <- function(x){
  suppressWarnings(suppressMessages(x))
}


suppress_all(library(optparse))
suppress_all(library(data.table))
suppress_all(library(lubridate))
suppress_all(library(glmnet))
suppress_all(library(xgboost))
suppress_all(library(ROCR))
suppress_all(library(MLmetrics))

# Command-line Arguments
option_list <- list(
  make_option(c("-g", "--grouping"), action = "store", default = "ahrq",
              help = "The grouping method used for icd codes. One of 'ahrq', 'ccs', 'trunc', or 'raw'"),
  make_option(c("-o", "--output_dir"), action =  "store", default = "../Output/",
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
# l1-penalized logistic regression
calc_l1 <- function(model_matrix, train_indices, test_indices){
  # Initialize data.frame to store results 
  auc <- data.frame(auc_death = numeric())
  f1 <- data.frame(f1_death = numeric())
  predictions <- data.frame(pred_death = numeric(), feat_death_in_year = numeric(), sample_size = numeric())
  
  # cannot create empty data frame for time_elapsed in the same way because we are trying to rbind proc_time objects
  time_elapsed <- data.frame(user = 0, system = 0, elapsed = 0)
  names(model_matrix) <- gsub('\\W', '_', names(model_matrix)) # Prevents naming errors
  names(model_matrix) <- sapply(names(model_matrix), function(x) paste0('feat_', x)) # Prevents error in coercing to sprase matrix
  
  # Remove intercept
  x_matrix <- sparse.model.matrix(feat_death_in_year ~., data = model_matrix)[,-1]
  
  for(i in split){
    # Set indices for training based on split size
    subset_indices <- train_indices[1:i]
    set.seed(1)
    
    # Fit model and get timing information
    time_elapsed <- rbind(time_elapsed, system.time(model_death <- cv.glmnet(y = model_matrix$feat_death_in_year[subset_indices], 
                                                                                         x = x_matrix[subset_indices, ], 
                                                                                         family = "binomial", 
                                                                                         maxit = 10000)))
    # prediction on the holdout set
    pred_death <- predict(model_death, x_matrix[test_indices,], type = "response")
    
    # add predictions to the stored results
    predictions_death <- cbind(pred_death = pred_death[,1], model_matrix[test_indices, "feat_death_in_year"], sample_size = i)
    predictions <- rbind(predictions, predictions_death)
    
    # calculate auc and store results
    auc_death <- auc(model_matrix$feat_death_in_year[test_indices], pred_death)
    auc <- rbind(auc, data.frame(auc_death = auc_death))
    
    # calculate F1 and store results
    f1_death <- F1_Score(model_matrix$feat_death_in_year[test_indices], round(pred_death[,1], digits = 0))
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # remove the dummy 0,0,0 row in time_elapsed, created just to maintain col header
  time_elapsed <- time_elapsed[-1, ]
  return(list(auc, f1, time_elapsed, predictions))
}

# l2-penalized logistic regression

calc_l2 <- function(model_matrix, train_indices, test_indices){
  
  # Initialize data.frame to store results 
  auc <- data.frame(auc_death = numeric())
  f1 <- data.frame(f1_death = numeric())
  predictions <- data.frame(pred_death = numeric(), feat_death_in_year = numeric(), sample_size = numeric())
  
  # cannot create empty data frame for time_elapsed in the same way because we are trying to rbind proc_time objects
  time_elapsed <- data.frame(user = 0, system = 0, elapsed = 0)
  names(model_matrix) <- gsub('\\W', '_', names(model_matrix)) # Prevents naming errors
  names(model_matrix) <- sapply(names(model_matrix), function(x) paste0('feat_', x)) # Prevents error in coercing to sprase matrix
  
  # Remove intercept
  x_matrix <- sparse.model.matrix(feat_death_in_year ~., data = model_matrix)[,-1]
  
  for(i in split){
    # Set indices for training based on split size
    subset_indices <- train_indices[1:i]
    set.seed(1)
    
    # Fit model and get timing information
    time_elapsed <- rbind(time_elapsed, system.time(model_death <- cv.glmnet(y = model_matrix$feat_death_in_year[subset_indices], 
                                                                             x = x_matrix[subset_indices, ],
                                                                             alpha = 0,
                                                                             family = "binomial", 
                                                                             maxit = 10000)))
    # prediction on the holdout set
    pred_death <- predict(model_death, x_matrix[test_indices,], type = "response")
    
    # add predictions to the stored results
    predictions_death <- cbind(pred_death = pred_death[,1], model_matrix[test_indices, "feat_death_in_year"], sample_size = i)
    predictions <- rbind(predictions, predictions_death)
    
    # calculate auc and store results
    auc_death <- auc(model_matrix$feat_death_in_year[test_indices], pred_death)
    auc <- rbind(auc, data.frame(auc_death = auc_death))
    
    # calculate F1 and store results
    f1_death <- F1_Score(model_matrix$feat_death_in_year[test_indices], round(pred_death[,1], digits = 0))
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # remove the dummy 0,0,0 row in time_elapsed, created just to maintain col header
  time_elapsed <- time_elapsed[-1, ]
  return(list(auc, f1, time_elapsed, predictions))
}

rf_calc <- function(model_matrix, train_indices, test_indices){
  
  names(model_matrix) = gsub('[^a-zA-Z0-9]+', '_', names(model_matrix))
  model_matrix$GENDER <- as.factor(model_matrix$GENDER)

  # dataframe to hold auc from best combination of parameters for each split
  auc_death_final <- data.frame(auc_death = numeric())
  time_elapsed <- data.frame(user = 0, system = 0, elapsed = 0)
  f1 <- data.frame(f1_death = numeric())
  predictions <- data.frame(pred_death = numeric(), feat_death_in_year = numeric(), sample_size = numeric())
  
  names(model_matrix) <- sapply(names(model_matrix), function(x) paste0('feat_', x))
  
  x_matrix_death <- sparse.model.matrix(feat_death_in_year~., model_matrix[, -1])
  
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
              model_death <- xgboost(data = x_matrix_death[subset,], 
                                     label = model_matrix$feat_death_in_year[subset], 
                                     params = params[j,], 
                                     objective = "binary:logistic", 
                                     nrounds = 1)
              
              pred_death <- predict(model_death, x_matrix_death[test_indices,], type="response")
              auc_death <- auc(model_matrix$feat_death_in_year[test_indices], pred_death)
              auc_death_all <- rbind(auc_death_all, data.frame(death = auc_death))
            }))
    # find index of max auc
    auc_greatest_death <- which.max(auc_death_all$death)
    
    model_death <- xgboost(data = x_matrix_death[subset,],
                           label = model_matrix$feat_death_in_year[subset], 
                           params = params[auc_greatest_death,],
                           objective = "binary:logistic",
                           nrounds = 1)
    
    # bind max auc value to final auc dataframe
    pred_death <- predict(model_death, x_matrix_death[test_indices,], type="response")
    auc_death <- auc(model_matrix$feat_death_in_year[test_indices], pred_death)
    auc_death_final <- rbind(auc_death_final, data.frame(auc_death = auc_death_all$death[auc_greatest_death]))
    
    predictions_death <- cbind(pred_death = pred_death, model_matrix[test_indices, "feat_death_in_year"], sample_size = i)
    predictions <- rbind(predictions, predictions_death)
    # calculate F1 score
    f1_death <- F1_Score(model_matrix$feat_death_in_year[test_indices], round(pred_death, digits = 0))
    
    # add to F1 dataframe
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # first row of time_elapsed is dummy
  time_elapsed <- time_elapsed[-1, ]
  return(list(auc_death_final, f1, time_elapsed, predictions))  
}

xgb_calc <- function(model_matrix, train_indices, test_indices){
  names(model_matrix) = gsub('\\W', '_', names(model_matrix))
  model_matrix$GENDER <- as.factor(model_matrix$GENDER)
  
  # dataframe to hold auc from best combination of parameters for each split
  auc_death_final <- data.frame(auc_death = numeric())
  time_elapsed <- data.frame(user = 0, system = 0, elapsed = 0)
  f1 <- data.frame(f1_death = numeric())
  predictions <- data.frame(pred_death = numeric(), xdeath_in_year = numeric(), sample_size = numeric())
  
  names(model_matrix) <- sapply(names(model_matrix), function(x) paste0('feat_', x))
  
  x_matrix_death <- sparse.model.matrix(feat_death_in_year~., model_matrix[, -1])
  
  # factors to dummy vars using model.matrix, take out 'DIED'
  # model.matrix creates an intercept column, need to remove
  
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
              model_death <- xgboost(data = x_matrix_death[subset,], 
                                     label = model_matrix$feat_death_in_year[subset], 
                                     params = params[j,], 
                                     objective = "binary:logistic", 
                                     nrounds = 150)
              
              pred_death <- predict(model_death, x_matrix_death[test_indices,], type="response")
              auc_death <- auc(model_matrix$feat_death_in_year[test_indices], pred_death)
              auc_death_all <- rbind(auc_death_all, data.frame(death = auc_death))
            }))
    # find index of max auc
    auc_greatest_death <- which.max(auc_death_all$death)
    
    model_death <- xgboost(data = x_matrix_death[subset,],
                           label = model_matrix$feat_death_in_year[subset], 
                           params = params[auc_greatest_death,],
                           objective = "binary:logistic",
                           nrounds = 150)
    
    # bind max auc value to final auc dataframe
    pred_death <- predict(model_death, x_matrix_death[test_indices,], type="response")
    auc_death <- auc(model_matrix$feat_death_in_year[test_indices], pred_death)
    auc_death_final <- rbind(auc_death_final, data.frame(auc_death = auc_death_all$death[auc_greatest_death]))
    
    predictions_death <- cbind(pred_death = pred_death, model_matrix[test_indices, "feat_death_in_year"], sample_size = i)
    predictions <- rbind(predictions, predictions_death)
    
    # calculate F1 score
    f1_death <- F1_Score(model_matrix$feat_death_in_year[test_indices], round(pred_death, digits = 0))
    
    # add to F1 dataframe
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # first row of time_elapsed is dummy
  time_elapsed <- time_elapsed[-1, ]
  return(list(auc_death_final, f1, time_elapsed, predictions))  
}


# ==================================
# Prep experiment based on args that are passed
if (arguments$options$model_type == "l1"){
  run_model <- calc_l1
} else if (arguments$options$model_type == "l2"){
  run_model <- calc_l2
} else if (arguments$options$model_type == "rf"){
  run_model <- rf_calc
} else if (arguments$options$model_type == "xgb"){
  run_model <- xgb_calc
}


# ==================================
# Run Models and store results

print(paste0("Running ", arguments$options$model_type, " model with Total codes grouped by ", arguments$options$grouping))
total_matrix <- readRDS(paste0("../Data/Final/", arguments$options$grouping, "_total_with_death.rds"))
total_matrix <- total_matrix[, -c(1)]

# Train and Test split
set.seed(1)
random_indices <- sample(1:nrow(total_matrix))
train_indices <- random_indices[1:round(0.8*length(random_indices))]
test_indices <- random_indices[(round(0.8*length(random_indices)+1):length(random_indices))]

total_results <- run_model(total_matrix, train_indices, test_indices)
saveRDS(total_results, paste0(arguments$options$output_dir, arguments$options$model_type, "_", arguments$options$grouping, "_total_results.rds"))

print(paste0("Running ", arguments$options$model_type, " model with Binary codes grouped by ", arguments$options$grouping))
binary_matrix <- readRDS(paste0("../Data/Final/", arguments$options$grouping, "_binary_with_death.rds"))
binary_matrix <- binary_matrix[, -c(1)]


binary_results <- run_model(binary_matrix, train_indices, test_indices)
saveRDS(binary_results, paste0(arguments$options$output_dir, arguments$options$model_type, "_", arguments$options$grouping, "_binary_results.rds"))



