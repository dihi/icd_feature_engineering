## ICD MIMIC - rf model
## Date Created: 3/11/2019
## Author: Aman Kansal

library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(stringr)
library(tidyr)
library(profvis)
library(glmnet)
library(randomForest)
library(ROCR)
library(MLmetrics)

# read in data
ahrq_total_with_death <- readRDS("../Data/Final/ahrq_total_with_death.rds")
ahrq_total_with_death <- ahrq_total_with_death[, -c(1)]
ahrq_binary_with_death <- readRDS("../Data/Final/ahrq_binary_with_death.rds")
ahrq_binary_with_death <- ahrq_binary_with_death[, -c(1)]
ccs_total_with_death <- readRDS("../Data/Final/ccs_total_with_death.rds")
ccs_total_with_death <- ccs_total_with_death[, -c(1)]
ccs_binary_with_death <- readRDS("../Data/Final/ccs_binary_with_death.rds")
ccs_binary_with_death <- ccs_binary_with_death[, -c(1)]
trunc_total_with_death <- readRDS("../Data/Final/trunc_total_with_death.rds")
trunc_total_with_death <- trunc_total_with_death[, -c(1)]
trunc_binary_with_death <- readRDS("../Data/Final/trunc_binary_with_death.rds")
trunc_binary_with_death <- trunc_binary_with_death[, -c(1)]
raw_total_with_death <- readRDS("../Data/Final/raw_total_with_death.rds")
raw_total_with_death <- raw_total_with_death[, -c(1)]
raw_binary_with_death <- readRDS("../Data/Final/raw_binary_with_death.rds")
raw_binary_with_death <- raw_binary_with_death[, -c(1)]

## RUN LOG REG MODEL

# train and test split
set.seed(1)
random_indices <- sample(1:nrow(ahrq_total_with_death))
train_indices <- random_indices[1:round(0.8*length(random_indices))]
test_indices <- random_indices[(round(0.8*length(random_indices)+1):length(random_indices))]
# split vector
split <- c(5000, 10000, 20000, 35000)
# empty vector to store auc for 4 subset levels, both for complication and death

rf_calc <- function(death_matrix){
  names(death_matrix) = gsub('[^a-zA-Z0-9]+', '_', names(death_matrix))
  death_matrix$GENDER <- as.factor(death_matrix$GENDER)
  
  # dataframe to hold auc from best combination of parameters for each split
  auc_death_final <- data.frame(auc_death = numeric())
  time_elapsed_death <- data.frame(user = 0, system = 0, elapsed = 0)
  f1 <- data.frame(f1_death = numeric())
  predictions <- data.frame(pred_death = numeric(), xdeath_in_year = numeric(), sample_size = numeric())
  
  names(death_matrix) <- sapply(names(death_matrix), function(x) paste0('x', x))
  
  x_matrix_death <- sparse.model.matrix(xdeath_in_year~., death_matrix[, -1])
  
  # factors to dummy vars using model.matrix, take out 'DIED'
  # model.matrix creates an intercept column, need to remove
  
  # create a parameter grid (3 combos right now)
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
    
    time_elapsed_death <- 
      rbind(time_elapsed_death, 
            system.time(for(j in 1:nrow(params)){
              set.seed(1)
              model_death <- xgboost(data = x_matrix_death[subset,], 
                                     label = death_matrix$xdeath_in_year[subset], 
                                     params = params[j,], 
                                     objective = "binary:logistic", 
                                     nrounds = 1)
              
              pred_death <- predict(model_death, x_matrix_death[test_indices,], type="response")
              auc_death <- auc(death_matrix$xdeath_in_year[test_indices], pred_death)
              auc_death_all <- rbind(auc_death_all, data.frame(death = auc_death))
            }))
    # find index of max auc
    auc_greatest_death <- which.max(auc_death_all$death)
    
    model_death <- xgboost(data = x_matrix_death[subset,],
                           label = death_matrix$xdeath_in_year[subset], 
                           params = params[auc_greatest_death,],
                           objective = "binary:logistic",
                           nrounds = 1)
    
    # bind max auc value to final auc dataframe
    pred_death <- predict(model_death, x_matrix_death[test_indices,], type="response")
    auc_death <- auc(death_matrix$xdeath_in_year[test_indices], pred_death)
    auc_death_final <- rbind(auc_death_final, data.frame(auc_death = auc_death_all$death[auc_greatest_death]))
    
    predictions_death <- cbind(pred_death = pred_death, death_matrix[test_indices, "xdeath_in_year"], sample_size = i)
    predictions <- rbind(predictions, predictions_death)
    # calculate F1 score
    f1_death <- F1_Score(death_matrix$xdeath_in_year[test_indices], round(pred_death, digits = 0))
    
    # add to F1 dataframe
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # first row of time_elapsed is dummy
  time_elapsed_death <- time_elapsed_death[-1, ]
  return(list(auc_death_final, f1, time_elapsed_death, predictions))  
}

# param_list <- list(
#   "colsample_bynode" = 0.8,
#   "learning_rate" = 1,
#   "max_depth" = 5, 
#   "num_parallel_tree" = 500,
#   "objective" = 'binary:logistic',
#   "subsample" = 0.8
# )
# 
# ahrq_total_with_death[, c("GENDER", "death_in_year") := lapply(.SD, as.factor), .SDcols = c("GENDER", "death_in_year")]
# 
# x_matrix_death <- sparse.model.matrix(death_in_year~., ahrq_total_with_death)[, -1]
# 
# 
# example_model <- xgboost(data = x_matrix_death[size,], 
#                         label = ahrq_total_with_death[size, ][["death_in_year"]], 
#                         params = param_list,
#                         nrounds = 1)




# apply function to final datasets
rf_ahrq_total_results <- rf_calc(ahrq_total_with_death)
saveRDS(rf_ahrq_total_results, "../Output/rf_ahrq_total_results.rds")
rf_ahrq_binary_results <- rf_calc(ahrq_binary_with_death)
saveRDS(rf_ahrq_binary_results, "../Output/rf_ahrq_binary_results.rds")
rf_ccs_total_results <- rf_calc(ccs_total_with_death)
saveRDS(rf_ccs_total_results, "../Output/rf_ccs_total_results.rds")
rf_ccs_binary_results <- rf_calc(ccs_binary_with_death)
saveRDS(rf_ccs_binary_results, "../Output/rf_ccs_binary_results.rds")
rf_trunc_total_results <- rf_calc(trunc_total_with_death)
saveRDS(rf_trunc_total_results, "../Output/rf_trunc_total_results.rds")
rf_trunc_binary_results <- rf_calc(trunc_binary_with_death)
saveRDS(rf_trunc_binary_results, "../Output/rf_trunc_binary_results.rds")
rf_raw_total_results <- rf_calc(raw_total_with_death)
saveRDS(rf_raw_total_results, "../Output/rf_raw_total_results.rds")
rf_raw_binary_results <- rf_calc(raw_binary_with_death)
saveRDS(rf_raw_binary_results, "../Output/rf_raw_binary_results.rds")