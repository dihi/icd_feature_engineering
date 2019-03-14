## ICD MIMIC Analysis
## Final Dataset: Admissions + codes
## Date Created: 1/4/2019
## Author: Aman Kansal

library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(stringr)

# READ IN DATA

admissions <- fread("../Data/Raw/ADMISSIONS.csv")
patients <- fread("../Data/Raw/PATIENTS.csv")
codes <- fread("../Data/Raw/DIAGNOSES_ICD.csv")
ahrq_total_matrix <- readRDS("../Data/Processed/ahrq_total_matrix.rds")
ahrq_binary_matrix <- readRDS("../Data/Processed/ahrq_binary_matrix.rds")
ccs_total_matrix <- readRDS("../Data/Processed/ccs_total_matrix.rds")
ccs_binary_matrix <- readRDS("../Data/Processed/ccs_binary_matrix.rds")
trunc_total_matrix <- readRDS("../Data/Processed/trunc_total_matrix.rds")
trunc_binary_matrix <- readRDS("../Data/Processed/trunc_binary_matrix.rds")
raw_total_matrix <- readRDS("../Data/Processed/raw_total_matrix.rds")
raw_binary_matrix <- readRDS("../Data/Processed/raw_binary_matrix.rds")

admissions <- admissions[, c("SUBJECT_ID", "HADM_ID", "ADMITTIME")]
patients <- patients[, c("SUBJECT_ID", "GENDER", "DOB", "DOD")]

# create data.table of IDs with demographic features
all_encounters <- as.data.table(left_join(admissions, patients, by = "SUBJECT_ID"))
all_encounters[, `:=`(ADMITTIME = gsub(" .*$", "", ADMITTIME), 
                      DOB = gsub(" .*$", "", DOB),
                      DOD = gsub(" .*$", "", DOD))]
all_encounters[, c("ADMITTIME", "DOB", "DOD") := lapply(.SD, ymd), .SDcols = c("ADMITTIME", "DOB", "DOD")]
all_encounters[, AGE := year(ADMITTIME) - year(DOB)]
all_encounters[, death_in_year := ifelse((DOD - ADMITTIME) > 365, 0, 1)]
all_encounters <- all_encounters[, c("HADM_ID", "GENDER", "AGE", "death_in_year")]

# merge demographics with code count features
ahrq_total_with_death <- merge(all_encounters, ahrq_total_matrix, by = "HADM_ID")
ahrq_total_with_death[is.na(ahrq_total_with_death)] <- 0
saveRDS(ahrq_total_with_death, "../Data/Final/ahrq_total_with_death.rds")

ccs_total_with_death <- merge(all_encounters, ccs_total_matrix, by = "HADM_ID")
ccs_total_with_death[is.na(ccs_total_with_death)] <- 0
saveRDS(ccs_total_with_death, "../Data/Final/ccs_total_with_death.rds")

trunc_total_with_death <- merge(all_encounters, trunc_total_matrix, by = "HADM_ID")
trunc_total_with_death[is.na(trunc_total_with_death)] <- 0
saveRDS(trunc_total_with_death, "../Data/Final/trunc_total_with_death.rds")

raw_total_with_death <- merge(all_encounters, raw_total_matrix, by = "HADM_ID")
raw_total_with_death[is.na(raw_total_with_death)] <- 0
saveRDS(raw_total_with_death, "../Data/Final/raw_total_with_death.rds")
