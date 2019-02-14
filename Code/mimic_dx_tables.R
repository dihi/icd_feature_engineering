## ICD MIMIC Analysis
## Dx tables: using ONLY codes
## Date Created: 1/4/2019
## Author: Aman Kansal
## Updated: 02/14/2019
## Test merge conflict 12

library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(stringr)

# READ IN DATA

admissions <- fread("../Data/Raw/ADMISSIONS.csv")
patients <- fread("../Data/Raw/PATIENTS.csv")
codes <- fread("../Data/Raw/DIAGNOSES_ICD.csv")

codes <- codes[, c(3, 5)]

icd9_ahrq_cw <- fread("../Data/Crosswalk/icd9_ahrq.csv")
icd9_ccs_cw <- fread("../Data/Crosswalk/icd9_to_singleCCScategory.csv")

# CREATE MAP TO AHRQ, CCS, truncated

ahrq_map <- left_join(codes, icd9_ahrq_cw, by = c("ICD9_CODE" = "V1"))
ahrq_map <- as.data.table(ahrq_map[, c(1,3)])

ccs_map <- left_join(codes, icd9_ccs_cw, by = c("ICD9_CODE" = "ICD-9-CM CODE"))
ccs_map <- as.data.table(ccs_map[, c(1,3)])

trunc_map <- codes
trunc_map$truncated <- substr(trunc_map$ICD9_CODE, 1, 3)
trunc_map <- trunc_map[, c(1,3)]

# CREATE CODE MATRICES

create_matrix <- function(map, groups){
  # initialize matrix with all ahrq as 0 cols
  code_matrix <- admissions[, 2]
  code_matrix[, groups] <- 0
  # significantly decreased memory/time by using df[formula,,] to filter
  for(i in 1:nrow(code_matrix)){
    id <- code_matrix$HADM_ID[i]
    encounters <- map[HADM_ID == id,,] 
    encounters <- encounters[complete.cases(encounters),]
    if(nrow(encounters) != 0){
      for(j in 1:nrow(encounters)){
        name <- as.character(encounters[, 2][j])
        code_matrix[[name]][i] <- code_matrix[[name]][i] + 1
      }
    }
  }
  return(code_matrix)
}

ahrq_total_matrix <- create_matrix(ahrq_map, levels(as.factor(ahrq_map$i)))
ahrq_binary_matrix <- data.frame(apply(ahrq_total_matrix[, -c(1)], 2, function(x) as.numeric(x > 0)))
ahrq_binary_matrix <- cbind(ahrq_total_matrix[, 1], ahrq_binary_matrix)
saveRDS(ahrq_total_matrix, "ahrq_total_matrix.rds")
saveRDS(ahrq_binary_matrix, "ahrq_binary_matrix.rds")

ccs_total_matrix <- create_matrix(ccs_map, levels(as.factor(ccs_map$`CCS DIAGNOSIS CATEGORIES`)))
ccs_binary_matrix <- data.frame(apply(ccs_total_matrix[, -c(1)], 2, function(x) as.numeric(x>0)))
ccs_binary_matrix <- cbind(ccs_total_matrix[, 1], ccs_binary_matrix)
saveRDS(ccs_total_matrix, "ccs_total_matrix.rds")
saveRDS(ccs_binary_matrix, "ccs_binary_matrix.rds")

trunc_total_matrix <- create_matrix(trunc_map, levels(as.factor(trunc_map$truncated)))
trunc_binary_matrix <- data.frame(apply(trunc_total_matrix[, -c(1)], 2, function(x) as.numeric(x>0)))
trunc_binary_matrix <- cbind(trunc_total_matrix[, 1], trunc_binary_matrix)
saveRDS(trunc_total_matrix, "trunc_total_matrix.rds")
saveRDS(trunc_binary_matrix, "trunc_binary_matrix.rds")

# Add own comment