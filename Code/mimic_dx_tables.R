## ICD MIMIC Analysis
## Dx tables: using ONLY codes
## Date Created: 1/4/2019
## Author: Aman Kansal
## Modified: 03/14/19 - Michael Gao

## Usage:
## Rscript mimic_dx_tables.R {"ccs", "truncated", "ahrq", "raw"} {output_directory}


args = commandArgs(trailingOnly=TRUE)

print(args[1])
print(args[2])
if (length(args)==0) {
  stop("At least one argument must be supplied", call.=FALSE)
} else if(length(args)==1) {
  args[2] <- "../Data/Processed/"
}

dir.create(args[2])



library(dplyr)
library(data.table)

# READ IN DATA

admissions <- fread("../Data/Raw/ADMISSIONS.csv")
patients <- fread("../Data/Raw/PATIENTS.csv")
codes <- fread("../Data/Raw/DIAGNOSES_ICD.csv")
icd9_ahrq_cw <- fread("../Data/Crosswalk/icd9_ahrq.csv")
icd9_ccs_cw <- fread("../Data/Crosswalk/icd9_to_singleCCScategory.csv")

# Define Helper function

create_matrix <- function(map, groups){
  #' @param map A 2-column dataframe containing an ID along with values corresponding to those IDs
  #' @param groups A character vector containing all possible values -- will be converted to columns
  #' @return code_matrix A matrix where the rows are the unique IDs in map and each entry is the count of the number of codes
  
  # initialize matrix 
  code_matrix <- as.data.frame(admissions[, "HADM_ID"])
  code_matrix[, groups] <- 0
  
  for(i in 1:nrow(code_matrix)){
    id <- code_matrix$HADM_ID[i]
    encounters <- map[HADM_ID == id,] 
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

if(args[1] == "ahrq"){
  codes <- codes[, c("HADM_ID", "ICD9_CODE")]
  codes <- codes[ICD9_CODE != "",]
  ahrq_map <- left_join(codes, icd9_ahrq_cw, by = c("ICD9_CODE" = "V1"))
  ahrq_map <- as.data.table(ahrq_map[, c("HADM_ID", "i")]) # i corresponds to ahrq groups
  
  ahrq_total_matrix <- create_matrix(ahrq_map, levels(as.factor(ahrq_map$i)))
  ahrq_binary_matrix <- data.frame(apply(ahrq_total_matrix[, -c(1)], 2, function(x) as.numeric(x>0)))
  ahrq_binary_matrix <- cbind(ahrq_total_matrix[, 1], ahrq_binary_matrix)
  saveRDS(ahrq_total_matrix, paste0(args[2], "/ahrq_total_matrix.rds"))
  saveRDS(ahrq_binary_matrix, paste0(args[2], "/ahrq_binary_matrix.rds"))
  
} else if(args[1] == "ccs"){
  ccs_map <- left_join(codes, icd9_ccs_cw, by = c("ICD9_CODE" = "ICD-9-CM CODE"))
  ccs_map <- as.data.table(ccs_map[, c("HADM_ID", "CCS DIAGNOSIS CATEGORIES")])
  
  ccs_total_matrix <- create_matrix(ccs_map, levels(as.factor(ccs_map$`CCS DIAGNOSIS CATEGORIES`)))
  ccs_binary_matrix <- data.frame(apply(ccs_total_matrix[, -c(1)], 2, function(x) as.numeric(x>0)))
  ccs_binary_matrix <- cbind(ccs_total_matrix[, 1], ccs_binary_matrix)
  saveRDS(ccs_total_matrix, paste0(args[2], "/ccs_total_matrix.rds"))
  saveRDS(ccs_binary_matrix, paste0(args[2], "/ccs_binary_matrix.rds"))
  
} else if(args[1] == "truncated"){
  trunc_map <- codes
  trunc_map$truncated <- substr(trunc_map$ICD9_CODE, 1, 3)
  trunc_map <- trunc_map[, c("HADM_ID", "truncated")]
  
  
  trunc_total_matrix <- create_matrix(trunc_map, levels(as.factor(trunc_map$truncated)))
  trunc_binary_matrix <- data.frame(apply(trunc_total_matrix[, -c(1)], 2, function(x) as.numeric(x>0)))
  trunc_binary_matrix <- cbind(trunc_total_matrix[, 1], trunc_binary_matrix)
  saveRDS(trunc_total_matrix, paste0(args[2], "/trunc_total_matrix.rds"))
  saveRDS(trunc_binary_matrix, paste0(args[2], "/trunc_binary_matrix.rds"))
  
} else if(args[1] == "raw"){
  raw_total_matrix <- create_matrix(codes, levels(as.factor(codes$ICD9_CODE)))
  raw_binary_matrix <- data.frame(apply(raw_total_matrix[, -c(1)], 2, function(x) as.numeric(x>0)))
  raw_binary_matrix <- cbind(raw_total_matrix[, 1], raw_binary_matrix)
  saveRDS(raw_total_matrix, paste0(args[2], "/raw_total_matrix.rds"))
  saveRDS(raw_binary_matrix, paste0(args[2], "/raw_binary_matrix.rds"))
}




