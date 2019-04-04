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
split <- c(5000, 10000, 20000, 40000)
# empty vector to store auc for 4 subset levels, both for complication and death

rf_calc <- function(death_matrix){
  auc <- data.frame(auc_death = numeric())
  f1 <- data.frame(f1_death = numeric())
  time_elapsed_death <- data.frame(user = 0, system = 0, elapsed = 0)
  death_matrix[, c("GENDER", "death_in_year") := lapply(.SD, as.factor), .SDcols = c("GENDER", "death_in_year")]
  
  for(i in split){
    size <- train_indices[1:i]
    
    # randomForest::tuneRF to tune hyperparameters, specifically mtry
    # mtry =  the number of variables considered as candidate splitting variables at each split
    # tuneRF: mtry default for classification is sqrt(numFeatures); then default/stepFactor in each direction and only keeps going if OOB (Out of Bag) error improves by 0.05
    set.seed(1)
    time_elapsed_death <- 
      rbind(time_elapsed_death, 
            system.time(tuneRF_model_death <- 
                          tuneRF(subset(death_matrix[size,], select = -c(death_in_year)), 
                                 death_matrix[size, ][["death_in_year"]], 
                                 ntreeTry = 50, 
                                 stepFactor = 2, 
                                 improve = 0.05, 
                                 doBest = TRUE,
                                 plot = FALSE)))
    pred_death <- predict(tuneRF_model_death, death_matrix[test_indices, ], type = "prob")
    auc_death <- auc(as.numeric(as.character(death_matrix[test_indices, ][["death_in_year"]])), pred_death[,2])
    
    predictions <- cbind.data.frame(pred_death = pred_death[,2], real_death = death_matrix[test_indices, "death_in_year"])
    
    auc <- rbind(auc, data.frame(auc_death = auc_death))
    
    # calculate F1 score
    f1_death <- F1_Score(death_matrix[test_indices, ][["death_in_year"]], round(pred_death[,2], digits = 0))
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # remove the dummy 0,0,0 row in time_elapsed, created just to maintain col header
  time_elapsed_death <- time_elapsed_death[-1, ]
  return(list(auc, f1, time_elapsed_death, predictions))
}

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