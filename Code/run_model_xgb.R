## ICD MIMIC - xgb model
## Date Created: 3/11/2019
## Author: Aman Kansal

library(xgboost)
library(dplyr)
library(caret)
library(pROC)
library(dplyr)
library(data.table)
library(Matrix)
library(ROCR)
library(MLmetrics)
library(profvis)

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
split <- c(5000, 10000, 20000, 40000)
# empty vector to store auc for 4 subset levels, both for complication and death

xgb_calc <- function(death_matrix){
  names(death_matrix) = gsub('[^a-zA-Z0-9]+', '_', names(death_matrix))
  death_matrix$GENDER <- as.factor(death_matrix$GENDER)
  
  # dataframe to hold auc from best combination of parameters for each split
  auc_death_final <- data.frame(auc_death = numeric())
  time_elapsed_death <- data.frame(user = 0, system = 0, elapsed = 0)
  f1 <- data.frame(f1_death = numeric())
  
  names(death_matrix) <- sapply(names(death_matrix), function(x) paste0('x', x))
  
  x_matrix_death <- sparse.model.matrix(xdeath_in_year~., death_matrix[, -1])
  
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
    
    for(j in 1:nrow(params)){
      set.seed(1)
      model_death <- xgboost(data = x_matrix_death[subset,], 
                             label = death_matrix$xdeath_in_year[subset], 
                             params = params[j,], 
                             objective = "binary:logistic", 
                             nrounds = 150)
      
      pred_death <- predict(model_death, x_matrix_death[test_indices,], type="response")
      auc_death <- auc(death_matrix$xdeath_in_year[test_indices], pred_death)
      auc_death_all <- rbind(auc_death_all, data.frame(death = auc_death))
    }
    # find index of max auc
    auc_greatest_death <- which.max(auc_death_all$death)
    
    # calculate time using parameters that generated max auc
    time_elapsed_death <- 
      rbind(time_elapsed_death, 
            system.time(model_death <- 
                          xgboost(data = x_matrix_death[subset,], 
                                  label = death_matrix$xdeath_in_year[subset], 
                                  params = params[auc_greatest_death,], 
                                  objective = "binary:logistic", 
                                  nrounds = 150)))
    
    # bind max auc value to final auc dataframe
    pred_death <- predict(model_death, x_matrix_death[test_indices,], type="response")
    auc_death <- auc(death_matrix$xdeath_in_year[test_indices], pred_death)
    auc_death_final <- rbind(auc_death_final, data.frame(auc_death = auc_death_all$death[auc_greatest_death]))
    
    predictions <- cbind(pred_death = pred_death, death_matrix[test_indices, "xdeath_in_year"])
    
    # calculate F1 score
    f1_death <- F1_Score(death_matrix$xdeath_in_year[test_indices], round(pred_death, digits = 0))
    
    # add to F1 dataframe
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # first row of time_elapsed is dummy
  time_elapsed_death <- time_elapsed_death[-1, ]
  return(list(auc_death_final, f1, time_elapsed_death, predictions))  
}

# apply function to final datasets
xgb_ahrq_total_results <- xgb_calc(ahrq_total_with_death)
saveRDS(xgb_ahrq_total_results, "../Output/xgb_ahrq_total_results.rds")
xgb_ahrq_binary_results <- xgb_calc(ahrq_binary_with_death)
saveRDS(xgb_ahrq_binary_results, "../Output/xgb_ahrq_binary_results.rds")
xgb_ccs_total_results <- xgb_calc(ccs_total_with_death)
saveRDS(xgb_ccs_total_results, "../Output/xgb_ccs_total_results.rds")
xgb_ccs_binary_results <- xgb_calc(ccs_binary_with_death)
saveRDS(xgb_ccs_binary_results, "../Output/xgb_ccs_binary_results.rds")
xgb_trunc_total_results <- xgb_calc(trunc_total_with_death)
saveRDS(xgb_trunc_total_results, "../Output/xgb_trunc_total_results.rds")
xgb_trunc_binary_results <- xgb_calc(trunc_binary_with_death)
saveRDS(xgb_trunc_binary_results, "../Output/xgb_trunc_binary_results.rds")
xgb_raw_total_results <- xgb_calc(raw_total_with_death)
saveRDS(xgb_raw_total_results, "../Output/xgb_raw_total_results.rds")
xgb_raw_binary_results <- xgb_calc(raw_binary_with_death)
saveRDS(xgb_raw_binary_results, "../Output/xgb_raw_binary_results.rds")
