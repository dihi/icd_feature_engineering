## ICD MIMIC - l1 model
## Date Created: 1/4/2019
## Author: Aman Kansal

library(data.table)
library(lubridate)
library(glmnet)
library(ROCR)
library(MLmetrics)

# Command-line Arguments
args = commandArgs(trailingOnly=TRUE)

if (length(args) == 0) {
  stop("At least one argument must be supplied", call.=FALSE)
} else if(length(args) == 1) {
  args[2] <- "../Output/"
} 

dir.create(args[2], showWarnings = FALSE)

# Split sizes to test
split <- c(5000, 10000, 20000, 40000)

calc_l1 <- function(model_matrix, train_indices, test_indices){
  auc <- data.frame(auc_death = numeric())
  f1 <- data.frame(f1_death = numeric())
  # cannot create empty data frame for time_elapsed in the same way because we are trying to rbind proc_time objects
  time_elapsed_death <- data.frame(user = 0, system = 0, elapsed = 0)
  # model.matrix creates an intercept column, need to remove
  x_matrix <- model.matrix(death_in_year ~., data = model_matrix)[,-1]
  
  for(i in split){
    # build model
    subset_indices <- train_indices[1:i]
    set.seed(1)
    time_elapsed_death <- rbind(time_elapsed_death, system.time(model_death <- cv.glmnet(y = model_matrix$death_in_year[subset_indices], x = x_matrix[subset_indices, ], family = "binomial", maxit = 10000)))
    # prediction on the holdout
    pred_death <- predict(model_death, x_matrix[test_indices,], type = "response")
    predictions <- cbind(pred_death = pred_death[,1], death_matrix[test_indices, "death_in_year"])
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
  return(list(auc, f1, time_elapsed_death, predictions))
}

if (args[1] == "ahrq"){
  print("Running Total")
  ahrq_total_with_death <- readRDS("../Data/Final/ahrq_total_with_death.rds")
  ahrq_total_with_death <- ahrq_total_with_death[, -c(1)]

  # train and test split
  set.seed(1)
  random_indices <- sample(1:nrow(ahrq_total_with_death))
  train_indices <- random_indices[1:round(0.8*length(random_indices))]
  test_indices <- random_indices[(round(0.8*length(random_indices)+1):length(random_indices))]

  l1_ahrq_total_results <- calc_l1(ahrq_total_with_death, train_indices, test_indices)
  saveRDS(l1_ahrq_total_results, paste0(args[2], "l1_ahrq_total_results.rds"))
  rm(ahrq_total_with_death)

  print("Running Binary")
  ahrq_binary_with_death <- readRDS("../Data/Final/ahrq_binary_with_death.rds")
  ahrq_binary_with_death <- ahrq_binary_with_death[, -c(1)]

  l1_ahrq_binary_results <- calc_l1(ahrq_binary_with_death, train_indices, test_indices)
  saveRDS(l1_ahrq_binary_results, paste0(args[2], "l1_ahrq_binary_results.rds"))
  rm(ahrq_binary_with_death) # For consistency
}

if (args[1] == "ccs"){
  print("Running Total")
  ccs_total_with_death <- readRDS("../Data/Final/ccs_total_with_death.rds")
  ccs_total_with_death <- ccs_total_with_death[, -c(1)]

  # train and test split
  set.seed(1)
  random_indices <- sample(1:nrow(ccs_total_with_death))
  train_indices <- random_indices[1:round(0.8*length(random_indices))]
  test_indices <- random_indices[(round(0.8*length(random_indices)+1):length(random_indices))]

  l1_ccs_total_results <- calc_l1(ccs_total_with_death, train_indices, test_indices)
  saveRDS(l1_ccs_total_results, paste0(args[2], "l1_ccs_total_results.rds"))

  rm(ccs_total_with_death)

  print("Running Binary")
  ccs_binary_with_death <- readRDS("../Data/Final/ccs_binary_with_death.rds")
  ccs_binary_with_death <- ccs_binary_with_death[, -c(1)]
  l1_ccs_binary_results <- calc_l1(ccs_binary_with_death, train_indices, test_indices)
  saveRDS(l1_ccs_binary_results, paste0(args[2], "l1_ccs_binary_results.rds"))

  rm(ccs_binary_with_death)
}

if(args[1] == "trunc"){
  print("Running Total")

  trunc_total_with_death <- readRDS("../Data/Final/trunc_total_with_death.rds")
  trunc_total_with_death <- trunc_total_with_death[, -c(1)]
   # train and test split
  set.seed(1)
  random_indices <- sample(1:nrow(trunc_total_with_death))
  train_indices <- random_indices[1:round(0.8*length(random_indices))]
  test_indices <- random_indices[(round(0.8*length(random_indices)+1):length(random_indices))]

  l1_trunc_total_results <- calc_l1(trunc_total_with_death, train_indices, test_indices)
  saveRDS(l1_trunc_total_results, paste0(args[2], "l1_trunc_total_results.rds"))
  rm(trunc_total_with_death)

  print("Running Binary")
  trunc_binary_with_death <- readRDS("../Data/Final/trunc_binary_with_death.rds")
  trunc_binary_with_death <- trunc_binary_with_death[, -c(1)]

  l1_trunc_binary_results <- calc_l1(trunc_binary_with_death, train_indices, test_indices)
  saveRDS(l1_trunc_binary_results, paste0(args[2], "l1_trunc_binary_results.rds"))
}

if(args[1] == "raw"){

  print("Running Total")
  raw_total_with_death <- readRDS("../Data/Final/raw_total_with_death.rds")
  raw_total_with_death <- raw_total_with_death[, -c(1)]

   # train and test split
  set.seed(1)
  random_indices <- sample(1:nrow(raw_total_with_death))
  train_indices <- random_indices[1:round(0.8*length(random_indices))]
  test_indices <- random_indices[(round(0.8*length(random_indices)+1):length(random_indices))]

  l1_raw_total_results <- calc_l1(raw_total_with_death, train_indices, test_indices)
  saveRDS(l1_raw_total_results, paste0(args[2],"l1_raw_total_results.rds"))
  rm(raw_total_with_death)

  print("Running Binary")
  raw_binary_with_death <- readRDS("../Data/Final/raw_binary_with_death.rds")
  raw_binary_with_death <- raw_binary_with_death[, -c(1)]

  l1_raw_binary_results <- calc_l1(raw_binary_with_death, train_indices, test_indices)
  saveRDS(l1_raw_binary_results, paste0(args[2],"l1_raw_binary_results.rds"))

}


