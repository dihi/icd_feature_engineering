#!/usr/bin/env Rscript
# Run Models
# A generic script to run all of the models
# Command-line arguments will be used to specify which models to run

library(optparse)

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


library(data.table)
library(lubridate)
library(glmnet)
library(xgboost)
library(randomForest)
library(ROCR)
library(MLmetrics)


# Create output directory
dir.create(arguments$options$output_dir, showWarnings = FALSE)

# Split sizes to test
split <- c(5000, 10000, 20000, 35000)

# Model Run and Evaluation Code ==============================
# l1-penalized logistic regression
calc_l1 <- function(model_matrix, train_indices, test_indices){
  auc <- data.frame(auc_death = numeric())
  f1 <- data.frame(f1_death = numeric())
  # cannot create empty data frame for time_elapsed in the same way because we are trying to rbind proc_time objects
  time_elapsed_death <- data.frame(user = 0, system = 0, elapsed = 0)
  names(model_matrix) <- gsub('\\W', '_', names(model_matrix)) # Prevents naming errors
  names(model_matrix) <- sapply(names(model_matrix), function(x) paste0('feat_', x)) # Prevents error in coercing to sprase matrix
  
  # Remove intercept
  x_matrix <- sparse.model.matrix(feat_death_in_year ~., data = model_matrix)[,-1]
  
  for(i in split){
    # build model
    subset_indices <- train_indices[1:i]
    set.seed(1)
    time_elapsed_death <- rbind(time_elapsed_death, system.time(model_death <- cv.glmnet(y = model_matrix$feat_death_in_year[subset_indices], x = x_matrix[subset_indices, ], family = "binomial", maxit = 10000)))
    # prediction on the holdout
    pred_death <- predict(model_death, x_matrix[test_indices,], type = "response")
    predictions <- cbind(pred_death = pred_death[,1], model_matrix[test_indices, "feat_death_in_year"])
    # calculate auc
    auc_death <- auc(model_matrix$feat_death_in_year[test_indices], pred_death)
    # add to auc dataframe
    auc <- rbind(auc, data.frame(auc_death = auc_death))
    # calculate F1 score
    f1_death <- F1_Score(model_matrix$feat_death_in_year[test_indices], round(pred_death[,1], digits = 0))
    # add to F1 dataframe
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # remove the dummy 0,0,0 row in time_elapsed, created just to maintain col header
  time_elapsed_death <- time_elapsed_death[-1, ]
  return(list(auc, f1, time_elapsed_death, predictions))
}

# l2-penalized logistic regression

calc_l2 <- function(model_matrix, train_indices, test_indices){
  auc <- data.frame(auc_death = numeric())
  f1 <- data.frame(f1_death = numeric())
  # cannot create empty data frame for time_elapsed in the same way because we are trying to rbind proc_time objects
  time_elapsed_death <- data.frame(user = 0, system = 0, elapsed = 0)
  names(model_matrix) <- gsub('\\W', '_', names(model_matrix)) # Prevents naming errors
  names(model_matrix) <- sapply(names(model_matrix), function(x) paste0('feat_', x)) # Prevents error in coercing to sprase matrix
  
  # Remove intercept
  x_matrix <- sparse.model.matrix(feat_death_in_year ~., data = model_matrix)[,-1]
  
  for(i in split){
    # build model
    subset_indices <- train_indices[1:i]
    set.seed(1)
    time_elapsed_death <- rbind(time_elapsed_death, system.time(model_death <- cv.glmnet(y = model_matrix$feat_death_in_year[subset_indices], x = x_matrix[subset_indices, ], alpha = 0, family = "binomial", maxit = 10000)))
    # prediction on the holdout
    pred_death <- predict(model_death, x_matrix[test_indices,], type = "response")
    predictions <- cbind(pred_death = pred_death[,1], model_matrix[test_indices, "feat_death_in_year"])
    # calculate auc
    auc_death <- auc(model_matrix$feat_death_in_year[test_indices], pred_death)
    # add to auc dataframe
    auc <- rbind(auc, data.frame(auc_death = auc_death))
    # calculate F1 score
    f1_death <- F1_Score(model_matrix$feat_death_in_year[test_indices], round(pred_death[,1], digits = 0))
    # add to F1 dataframe
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # remove the dummy 0,0,0 row in time_elapsed, created just to maintain col header
  time_elapsed_death <- time_elapsed_death[-1, ]
  return(list(auc, f1, time_elapsed_death, predictions))
}
rf_calc <- function(model_matrix, train_indices, test_indices){
  auc <- data.frame(auc_death = numeric())
  f1 <- data.frame(f1_death = numeric())
  time_elapsed_death <- data.frame(user = 0, system = 0, elapsed = 0)
  model_matrix[, c("GENDER", "death_in_year") := lapply(.SD, as.factor), .SDcols = c("GENDER", "death_in_year")]
  
  for(i in split){
    size <- train_indices[1:i]
    
    # randomForest::tuneRF to tune hyperparameters, specifically mtry
    # mtry =  the number of variables considered as candidate splitting variables at each split
    # tuneRF: mtry default for classification is sqrt(numFeatures); then default/stepFactor in each direction and only keeps going if OOB (Out of Bag) error improves by 0.05
    set.seed(1)
    time_elapsed_death <- 
      rbind(time_elapsed_death, 
            system.time(tuneRF_model_death <- 
                          tuneRF(subset(model_matrix[size,], select = -c(death_in_year)), 
                                 model_matrix[size, ][["death_in_year"]], 
                                 ntreeTry = 50, 
                                 stepFactor = 2, 
                                 improve = 0.05, 
                                 doBest = TRUE,
                                 plot = FALSE)))
    pred_death <- predict(tuneRF_model_death, model_matrix[test_indices, ], type = "prob")
    auc_death <- auc(as.numeric(as.character(model_matrix[test_indices, ][["death_in_year"]])), pred_death[,2])
    
    predictions <- cbind.data.frame(pred_death = pred_death[,2], real_death = model_matrix[test_indices, "death_in_year"])
    
    auc <- rbind(auc, data.frame(auc_death = auc_death))
    
    # calculate F1 score
    f1_death <- F1_Score(model_matrix[test_indices, ][["death_in_year"]], round(pred_death[,2], digits = 0))
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # remove the dummy 0,0,0 row in time_elapsed, created just to maintain col header
  time_elapsed_death <- time_elapsed_death[-1, ]
  return(list(auc, f1, time_elapsed_death, predictions))
}


xgb_calc <- function(model_matrix, train_indices, test_indices){
  names(model_matrix) = gsub('\\W', '_', names(model_matrix))
  model_matrix$GENDER <- as.factor(model_matrix$GENDER)
  
  # dataframe to hold auc from best combination of parameters for each split
  auc_death_final <- data.frame(auc_death = numeric())
  time_elapsed_death <- data.frame(user = 0, system = 0, elapsed = 0)
  f1 <- data.frame(f1_death = numeric())
  
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
    
    time_elapsed_death <- 
      rbind(time_elapsed_death, 
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
    
    predictions <- cbind(pred_death = pred_death, model_matrix[test_indices, "feat_death_in_year"])
    
    # calculate F1 score
    f1_death <- F1_Score(model_matrix$feat_death_in_year[test_indices], round(pred_death, digits = 0))
    
    # add to F1 dataframe
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # first row of time_elapsed is dummy
  time_elapsed_death <- time_elapsed_death[-1, ]
  return(list(auc_death_final, f1, time_elapsed_death, predictions))  
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
saveRDS(total_results, paste0(arguments$output_dir, arguments$options$model_type, "_", arguments$options$grouping, "_total_results.rds"))

print(paste0("Running ", arguments$options$model_type, " model with Binary codes grouped by ", arguments$options$grouping))
binary_matrix <- readRDS(paste0("../Data/Final/", arguments$options$grouping, "_binary_with_death.rds"))
binary_matrix <- binary_matrix[, -c(1)]


binary_results <- run_model(binary_matrix, train_indices, test_indices)
saveRDS(binary_results, paste0(arguments$options$output_dir, arguments$options$model_type, "_", arguments$options$grouping, "_binary_results.rds"))



#

