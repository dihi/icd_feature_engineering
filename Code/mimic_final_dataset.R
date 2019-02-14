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
trunc_total_matrix <- readRDS("../Data/Processed/trunc_total_matrix.rds")

admissions <- admissions[, c(2:4)]
patients <- patients[, c(2:5)]

all_encounters <- left_join(admissions, patients, by = "SUBJECT_ID")
 
all_encounters$ADMITTIME <- gsub(" .*$", "", all_encounters$ADMITTIME)
all_encounters$ADMITTIME <- ymd(all_encounters$ADMITTIME)
all_encounters$DOB <- gsub(" .*$", "", all_encounters$DOB)
all_encounters$DOB <- ymd(all_encounters$DOB)
all_encounters$DOD <- gsub(" .*$", "", all_encounters$DOD)
all_encounters$DOD <- ymd(all_encounters$DOD)
all_encounters$AGE <- year(all_encounters$ADMITTIME) - year(all_encounters$DOB)
all_encounters$death_in_year <- ifelse((all_encounters$DOD - all_encounters$ADMITTIME) > 365, 0, 1)
all_encounters <- all_encounters[, c(2,4,7,8)]

ahrq_total_with_death <- merge(all_encounters, ahrq_total_matrix, by = "HADM_ID")
ahrq_total_with_death[is.na(ahrq_total_with_death)] <- 0
saveRDS(ahrq_total_with_death, "ahrq_total_with_death.rds")

ccs_total_with_death <- merge(all_encounters, ccs_total_matrix, by = "HADM_ID")
ccs_total_with_death[is.na(ccs_total_with_death)] <- 0
saveRDS(ccs_total_with_death, "ccs_total_with_death.rds")

trunc_total_with_death <- merge(all_encounters, trunc_total_matrix, by = "HADM_ID")
trunc_total_with_death[is.na(trunc_total_with_death)] <- 0
saveRDS(trunc_total_with_death, "trunc_total_with_death.rds")



