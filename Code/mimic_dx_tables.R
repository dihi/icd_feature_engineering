# ICD MIMIC Analysis
# Dx tables: using ONLY codes
# Date Created: 1/4/2019
# Author: Aman Kansal
# Modified: 03/14/19 - Michael Gao

# Usage:
# Rscript mimic_dx_tables.R {"ccs", "truncated", "ahrq", "raw"} {output_directory}

Sys.setenv(TZ = "America/New_York")

suppress_all <- function(x){
  suppressWarnings(suppressMessages(x))
}

suppress_all(library(data.table))
suppress_all(library(Matrix))
suppress_all(library(lubridate))


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
patients <- fread("../Data/Raw/PATIENTS.csv")

# Admissions
admissions <- admissions[, c("SUBJECT_ID", "HADM_ID", "ADMITTIME")]
patients <- patients[, c("SUBJECT_ID", "GENDER", "DOB", "DOD")]

# create data.table of IDs with demographic features
all_encounters <- merge(admissions, patients, by = "SUBJECT_ID", all.x = TRUE)
all_encounters[, c("ADMITTIME", "DOB", "DOD") := lapply(.SD, ymd_hms), .SDcols = c("ADMITTIME", "DOB", "DOD")]
all_encounters[, AGE := year(ADMITTIME) - year(DOB)]
all_encounters[, death_in_year := ifelse((year(DOD) - year(ADMITTIME)) > 365, 0, 1)]
all_encounters$death_in_year[is.na(all_encounters$death_in_year)] <- 0
all_encounters <- all_encounters[, c("HADM_ID", "GENDER", "AGE", "death_in_year")]
all_encounters$GENDER <- ifelse(all_encounters$GENDER == "M", 1, 0)

all_encounters$GENDER <- as.integer(all_encounters$GENDER)
all_encounters$AGE <- as.integer(all_encounters$AGE)

remove(admissions)
remove(patients)
# Define Helper function
# Takes in map, a 2-column DataFrame that contains an ID and the associated ICD grouping category
# group_name is the name of the column that has the ICD groups
# Returns a data.frame where the columns are the groups and each entity is the number of times 
# the code appears for a given ID
create_matrix_total <- function(map, group_name) {
    res <- map[, count := .N, by = list(HADM_ID, map[[group_name]])]
    res <- dcast(res, HADM_ID ~ map[[group_name]], fun.aggregate = length, value.var = 'count')
    return(res)
}
  
create_matrix_binary <- function(map, group_name) {
    map$count <- 1L
    map <- unique(map)
    map <- dcast(map, HADM_ID ~ map[[group_name]], value.var = 'count', fill = 0)
    return(map)
}

create_model_matrix <- function(model_df){
  model_matrix <- vector(mode = "list", length = ncol(model_df))
  for(col in seq_along(names(model_df))){
    temp <- model_df[, names(model_df)[col], with = F][, RowIdx := .I]
    temp <- temp[temp[[1L]] != 0]
    temp <- sparseMatrix(
      x = temp[[1L]], 
      i = temp$RowIdx, 
      j = rep(1L, nrow(temp)), 
      dims = c(nrow(model_df), 1L)
    )
    model_matrix[[col]] <- temp
  }
  model_matrix <- do.call(cbind, model_matrix)
  return (model_matrix)
}


if (args[1] == "ahrq") {
  # Read in crosswalk
  icd9_ahrq_cw <- fread("../Data/Crosswalk/icd9_ahrq.csv")
  
  # Create codes map
  codes <- codes[, c("HADM_ID", "ICD9_CODE")]
  codes <- codes[ICD9_CODE != "",]
  ahrq_map <- merge(codes, icd9_ahrq_cw, by.x = "ICD9_CODE", by.y = "codes", all.x = TRUE, allow.cartesian = TRUE)
  ahrq_map <- as.data.table(ahrq_map[, c("HADM_ID", "names")]) # i corresponds to ahrq groups
  ahrq_map <- ahrq_map[complete.cases(ahrq_map),]
  
  # Create matrices and save
  ahrq_total_matrix <- create_matrix_total(ahrq_map, 'names')
  saveRDS(ahrq_total_matrix, paste0(args[2], "/ahrq_total_matrix.rds"))
  remove(ahrq_total_matrix)
  
  ahrq_binary_matrix <- create_matrix_binary(ahrq_map, 'names')
  saveRDS(ahrq_binary_matrix, paste0(args[2], "/ahrq_binary_matrix.rds"))
  
} else if(args[1] == "ccs") {
  # Read in crosswalk
  icd9_ccs_cw <- fread("../Data/Crosswalk/icd9_to_singleCCScategory.csv")
  
  # Create Codes map
  ccs_map <- merge(codes, icd9_ccs_cw, by.x = "ICD9_CODE", by.y ="ICD-9-CM CODE", all.x = TRUE, allow.cartesian = TRUE)
  ccs_map <- as.data.table(ccs_map[, c("HADM_ID", "CCS CATEGORY DESCRIPTION")])
  ccs_map <- ccs_map[complete.cases(ccs_map),]
  
  # Create matrices and save
  ccs_total_matrix <- create_matrix_total(ccs_map, 'CCS CATEGORY DESCRIPTION')
  saveRDS(ccs_total_matrix, paste0(args[2], "/ccs_total_matrix.rds"))
  remove(ccs_total_matrix)
  
  ccs_binary_matrix <- create_matrix_binary(ccs_map, 'CCS CATEGORY DESCRIPTION')
  saveRDS(ccs_binary_matrix, paste0(args[2], "/ccs_binary_matrix.rds"))
  
} else if(args[1] == "truncated") {
  # Create map
  codes$truncated <- substr(codes$ICD9_CODE, 1, 3)
  codes <- codes[, c("HADM_ID", "truncated")]
  codes <- codes[truncated != "", ]
  
  # Create matrices and save
  trunc_total_matrix <- create_matrix_total(codes, 'truncated')
  saveRDS(trunc_total_matrix, paste0(args[2], "/trunc_total_matrix.rds"))
  remove(trunc_total_matrix)
  
  trunc_binary_matrix <- create_matrix_binary(codes, 'truncated')
  saveRDS(trunc_binary_matrix, paste0(args[2], "/trunc_binary_matrix.rds"))
  
} else if(args[1] == "raw") {
  # Create map
  codes <- codes[, c("HADM_ID", "ICD9_CODE")]
  codes <- codes[ICD9_CODE != "",]
  
  #Create matrices and save
  raw_total_matrix <- create_matrix_total(codes, "ICD9_CODE")
  raw_total_matrix <- merge(all_encounters, raw_total_matrix, by = "HADM_ID")
  
  death_in_year <- raw_total_matrix$death_in_year
  
  raw_total_matrix <- raw_total_matrix[, -c("HADM_ID", "death_in_year")]
  raw_total_matrix <- create_model_matrix(raw_total_matrix)
  
  final_list <- list(raw_total_matrix, death_in_year)
  saveRDS(final_list, paste0(args[2], "/raw_total_matrix.rds"))
  remove(raw_total_matrix)
  remove(final_list)
  
  raw_binary_matrix <- create_matrix_binary(codes, "ICD9_CODE")
  raw_binary_matrix <- merge(all_encounters, raw_binary_matrix, by = "HADM_ID")
  
  death_in_year <- raw_binary_matrix$death_in_year
  
  raw_binary_matrix <- raw_binary_matrix[, -c("HADM_ID", "death_in_year")]
  raw_binary_matrix <- create_model_matrix(raw_binary_matrix)
  
  final_list <- list(raw_binary_matrix, death_in_year)
  saveRDS(final_list, paste0(args[2], "/raw_binary_matrix.rds"))
}


