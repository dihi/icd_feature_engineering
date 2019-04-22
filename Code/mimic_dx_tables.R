# ICD MIMIC Analysis
# Dx tables: using ONLY codes
# Date Created: 1/4/2019
# Author: Aman Kansal
# Modified: 03/14/19 - Michael Gao

# Usage:
# Rscript mimic_dx_tables.R {"ccs", "truncated", "ahrq", "raw"} {output_directory}
library(data.table)

# Parse Command Line Arguments
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("At least one argument must be supplied", call.=FALSE)
} else if(length(args)==1) {
  args[2] <- "../Data/Processed/"
}

dir.create(args[2], showWarnings = FALSE)

# Read in data
admissions <- fread("../Data/Raw/ADMISSIONS.csv")
codes <- fread("../Data/Raw/DIAGNOSES_ICD.csv")

# Define Helper function
# Takes in map, a 2-column DataFrame that contains an ID and the associated ICD grouping category
# group_name is the name of the column that has the ICD groups
# Returns a data.frame where the columns are the groups and each entity is the number of times 
# the code appears for a given ID
create_matrix <- function(map, group_name){
  res <- map[, count := .N, by = list(HADM_ID, map[[group_name]])]
  res <- dcast(res, HADM_ID ~ map[[group_name]], value.var = 'count')
  return(res)
}


if(args[1] == "ahrq"){
  # Read in crosswalk
  icd9_ahrq_cw <- fread("../Data/Crosswalk/icd9_ahrq.csv")
  
  # Create codes map
  codes <- codes[, c("HADM_ID", "ICD9_CODE")]
  codes <- codes[ICD9_CODE != "",]
  ahrq_map <- merge(codes, icd9_ahrq_cw, by.x = "ICD9_CODE", by.y = "V1", all.x = TRUE, allow.cartesian = TRUE)
  ahrq_map <- as.data.table(ahrq_map[, c("HADM_ID", "i")]) # i corresponds to ahrq groups
  ahrq_map <- ahrq_map[complete.cases(ahrq_map),]
  
  # Create matrices and save
  ahrq_total_matrix <- create_matrix(ahrq_map, 'i')
  ahrq_binary_matrix <- data.frame(apply(ahrq_total_matrix[, -c(1)], 2, function(x) as.numeric(x>0)))
  ahrq_binary_matrix <- cbind("HADM_ID" = ahrq_total_matrix[, 1], ahrq_binary_matrix)
  saveRDS(ahrq_total_matrix, paste0(args[2], "/ahrq_total_matrix.rds"))
  saveRDS(ahrq_binary_matrix, paste0(args[2], "/ahrq_binary_matrix.rds"))
  
} else if(args[1] == "ccs"){
  # Read in crosswalk
  icd9_ccs_cw <- fread("../Data/Crosswalk/icd9_to_singleCCScategory.csv")
  
  # Create Codes map
  ccs_map <- merge(codes, icd9_ccs_cw, by.x = "ICD9_CODE", by.y ="ICD-9-CM CODE", all.x = TRUE, allow.cartesian = TRUE)
  ccs_map <- as.data.table(ccs_map[, c("HADM_ID", "CCS DIAGNOSIS CATEGORIES")])
  ccs_map <- ccs_map[complete.cases(ccs_map),]
  
  # Create matrices and save
  ccs_total_matrix <- create_matrix(ccs_map, 'CCS DIAGNOSIS CATEGORIES')
  ccs_binary_matrix <- data.frame(apply(ccs_total_matrix[, -c(1)], 2, function(x) as.numeric(x>0)))
  ccs_binary_matrix <- cbind("HADM_ID" = ccs_total_matrix[, 1], ccs_binary_matrix)
  saveRDS(ccs_total_matrix, paste0(args[2], "/ccs_total_matrix.rds"))
  saveRDS(ccs_binary_matrix, paste0(args[2], "/ccs_binary_matrix.rds"))
  
} else if(args[1] == "truncated"){
  # Create map
  trunc_map <- codes
  trunc_map$truncated <- substr(trunc_map$ICD9_CODE, 1, 3)
  trunc_map <- trunc_map[, c("HADM_ID", "truncated")]
  trunc_map <- trunc_map[truncated != "", ]
  
  # Create matrices and save
  trunc_total_matrix <- create_matrix(trunc_map, 'truncated')
  trunc_binary_matrix <- data.frame(apply(trunc_total_matrix[, -c(1)], 2, function(x) as.numeric(x>0)))
  trunc_binary_matrix <- cbind("HADM_ID" = trunc_total_matrix[, 1], trunc_binary_matrix)
  saveRDS(trunc_total_matrix, paste0(args[2], "/trunc_total_matrix.rds"))
  saveRDS(trunc_binary_matrix, paste0(args[2], "/trunc_binary_matrix.rds"))
  
} else if(args[1] == "raw"){
  # Create map
  codes <- codes[, c("HADM_ID", "ICD9_CODE")]
  codes <- codes[ICD9_CODE != "",]
  
  # Create matrices and save
  raw_total_matrix <- create_matrix(codes, "ICD9_CODE")
  raw_binary_matrix <- data.frame(apply(raw_total_matrix[, -c(1)], 2, function(x) as.numeric(x>0)))
  raw_binary_matrix <- cbind("HADM_ID" = raw_total_matrix[, 1], raw_binary_matrix)
  saveRDS(raw_total_matrix, paste0(args[2], "/raw_total_matrix.rds"))
  saveRDS(raw_binary_matrix, paste0(args[2], "/raw_binary_matrix.rds"))
}




