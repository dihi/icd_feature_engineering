## ICD MIMIC Analysis
## Final Dataset: Admissions + codes
## Date Created: 1/4/2019
## Author: Aman Kansal
## Modified: 03/17/2019 - Michael Gao

## Rscript mimic_final_dataset.R {"ccs", "truncated", "ahrq", "raw"} {input_directory} {output_directory} 
## input_directory here refers to where the output of mimic_dx_tables.R has run

# READ IN DATA

suppress_all <- function(x) {
  suppressWarnings(suppressMessages(x))
}

suppress_all(library(data.table))
suppress_all(library(lubridate))

Sys.setenv(TZ = "America/New_York")

admissions <- fread("../Data/Raw/ADMISSIONS.csv")
patients <- fread("../Data/Raw/PATIENTS.csv")
codes <- fread("../Data/Raw/DIAGNOSES_ICD.csv")

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("At least one argument must be supplied", call.=FALSE)
} else if(length(args)==1) {
  args[2] <- "../Data/Processed/"
  args[3] <- "../Data/Final/"
} else if(length(args)==2) {
  args[3] <- "../Data/Final/"
}

dir.create(args[2], showWarnings = FALSE)
dir.create(args[3], showWarnings = FALSE)

admissions <- admissions[, c("SUBJECT_ID", "HADM_ID", "ADMITTIME")]
patients <- patients[, c("SUBJECT_ID", "GENDER", "DOB", "DOD")]

# create data.table of IDs with demographic features
all_encounters <- merge(admissions, patients, by = "SUBJECT_ID", all.x = TRUE)
all_encounters[, `:=`(ADMITTIME = gsub(" .*$", "", ADMITTIME), 
                      DOB = gsub(" .*$", "", DOB),
                      DOD = gsub(" .*$", "", DOD))]
all_encounters[, c("ADMITTIME", "DOB", "DOD") := lapply(.SD, ymd), .SDcols = c("ADMITTIME", "DOB", "DOD")]
all_encounters[, AGE := year(ADMITTIME) - year(DOB)]
all_encounters[, death_in_year := ifelse((DOD - ADMITTIME) > 365, 0, 1)]
all_encounters <- all_encounters[, c("HADM_ID", "GENDER", "AGE", "death_in_year")]

if(args[1] == "ahrq") {
  ahrq_total_matrix <- readRDS(paste0(args[2], "ahrq_total_matrix.rds"))
  ahrq_binary_matrix <- readRDS(paste0(args[2], "ahrq_binary_matrix.rds"))
  
  ahrq_total_with_death <- merge(all_encounters, ahrq_total_matrix, by = "HADM_ID")
  ahrq_total_with_death[is.na(ahrq_total_with_death)] <- 0
  saveRDS(ahrq_total_with_death, paste0(args[3], "ahrq_total_with_death.rds"))
  
  ahrq_binary_with_death <- merge(all_encounters, ahrq_binary_matrix, by = "HADM_ID")
  ahrq_binary_with_death[is.na(ahrq_binary_with_death)] <- 0
  saveRDS(ahrq_binary_with_death, paste0(args[3], "ahrq_binary_with_death.rds"))
  
} else if(args[1] == "ccs") {
  ccs_total_matrix <- readRDS(paste0(args[2], "ccs_total_matrix.rds"))
  ccs_binary_matrix <- readRDS(paste0(args[2], "ccs_binary_matrix.rds"))
  
  ccs_total_with_death <- merge(all_encounters, ccs_total_matrix, by = "HADM_ID")
  ccs_total_with_death[is.na(ccs_total_with_death)] <- 0
  saveRDS(ccs_total_with_death, paste0(args[3], "ccs_total_with_death.rds"))
  
  ccs_binary_with_death <- merge(all_encounters, ccs_binary_matrix, by = "HADM_ID")
  ccs_binary_with_death[is.na(ccs_binary_with_death)] <- 0
  saveRDS(ccs_binary_with_death, paste0(args[3], "ccs_binary_with_death.rds"))
  
} else if(args[1] == "truncated") {
  trunc_total_matrix <- readRDS(paste0(args[2], "trunc_total_matrix.rds"))
  
  trunc_total_with_death <- merge(all_encounters, trunc_total_matrix, by = "HADM_ID")
  trunc_total_with_death[is.na(trunc_total_with_death)] <- 0
  saveRDS(trunc_total_with_death, paste0(args[3], "trunc_total_with_death.rds"))
  remove(trunc_total_with_death)
  remove(trunc_total_matrix)
  
  trunc_binary_matrix <- readRDS(paste0(args[2], "trunc_binary_matrix.rds"))
  trunc_binary_with_death <- merge(all_encounters, trunc_binary_matrix, by = "HADM_ID")
  trunc_binary_with_death[is.na(trunc_binary_with_death)] <- 0
  saveRDS(trunc_binary_with_death, paste0(args[3], "trunc_binary_with_death.rds"))
  
} else if(args[1] == "raw") {
  raw_total_matrix <- readRDS(paste0(args[2], "raw_total_matrix.rds"))
  
  
  raw_total_with_death <- merge(all_encounters, raw_total_matrix, by = "HADM_ID")
  raw_total_with_death[is.na(raw_total_with_death)] <- 0
  saveRDS(raw_total_with_death, paste0(args[3], "raw_total_with_death.rds"))
  remove(raw_total_matrix)
  remove(raw_total_with_death)
  
  raw_binary_matrix <- readRDS(paste0(args[2], "raw_binary_matrix.rds"))
  raw_binary_with_death <- merge(all_encounters, raw_binary_matrix, by = "HADM_ID")
  raw_binary_with_death[is.na(raw_binary_with_death)] <- 0
  saveRDS(raw_binary_with_death, paste0(args[3], "raw_binary_with_death.rds"))
  
}


