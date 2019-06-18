# Calculate non-AHRQ codes

library(data.table)

admissions <- fread("./Data/Raw/ADMISSIONS.csv")
codes <- fread("./Data/Raw/DIAGNOSES_ICD.csv")

codes <- merge(codes, admissions, by = "HADM_ID")
icd9_ahrq_cw <- fread("./Data/Crosswalk/icd9_ahrq.csv")

overlap <- length(intersect(unique(codes$ICD9_CODE), icd9_ahrq_cw$V1))
total <- length(unique(codes$ICD9_CODE))

print(paste0(overlap/total * 100, " percent of unique codes overlap with AHRQ-Elixhauser"))
print(paste0((total-overlap)/total * 100, " percent of codes drop out"))