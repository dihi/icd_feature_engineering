## ICD MIMIC Analysis
## Date Created: 1/4/2019
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

## RUN LOG REG MODEL

# train and test split
set.seed(1)
random_indices <- sample(1:nrow(ahrq_total_with_death))
train_indices <- random_indices[1:round(0.8*length(random_indices))]
test_indices <- random_indices[(round(0.8*length(random_indices)+1):length(random_indices))]
# split vector
split <- c(2000, 10000, 20000, 40000)
# empty vector to store auc for 4 subset levels, both for complication and death

l1_calc <- function(death_matrix){
  auc <- data.frame(auc_death = numeric())
  f1 <- data.frame(f1_death = numeric())
  # cannot create empty data frame for time_elapsed in the same way because we are trying to rbind proc_time objects
  time_elapsed_death <- data.frame(user = 0, system = 0, elapsed = 0)
  # factors to dummy vars using model.matrix, take out 'DIED'
  # model.matrix creates an intercept column, need to remove
  x_matrix_death <- model.matrix(death_in_year ~., data = death_matrix[, -1])
  
  for(i in split){
    # build model
    subset <- train_indices[1:i]
    set.seed(1)
    time_elapsed_death <- rbind(time_elapsed_death, system.time(model_death <- cv.glmnet(y = death_matrix$death_in_year[subset], x = x_matrix_death[subset, ], family = "binomial", maxit = 10000)))
    # prediction on the holdout
    pred_death <- predict(model_death, x_matrix_death[test_indices,], type = "response")
    # calculate auc
    auc_death <- auc(death_matrix$death_in_year[test_indices], pred_death)
    # add to auc dataframe
    auc <- rbind(auc, data.frame(auc_death = auc_death))
    # calculate F1 score
    f1_death <- F1_Score(death_matrix$death_in_year[test_indices], round(pred_death[,1], digits = 0))
    # add to F1 dataframe
    f1 <- rbind(f1, data.frame(f1_death = f1_death))
  }
  # remove the dummy 0,0,0 row in time_elapsed, created just to maintain col header
  time_elapsed_death <- time_elapsed_death[-1, ]
  return(list(auc, f1, time_elapsed_death))
}

# apply function to final datasets
l1_ahrq_total_results <- l1_calc(ahrq_total_with_death)
saveRDS(l1_ahrq_total_results, "l1_ahrq_total_results.rds")
l1_ccs_total_results <- l1_calc(ccs_total_with_death)
l1_trunc_total_results <- l1_calc(trunc_total_with_death)
