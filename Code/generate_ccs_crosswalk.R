# The original version of the crosswalks can be downloaded from
# the following location:
# https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip
library(data.table)

icd9_ccs <- fread("../Data/Crosswalk/raw_ccs/$dxref 2015.csv", skip = 1) # Skip a row

# Fix names
names(icd9_ccs) <- sapply(names(icd9_ccs), function(x) gsub("'", "", x))
icd9_ccs <- as.data.frame(apply(icd9_ccs, MARGIN = c(1,2), function(x) gsub("'", '', x)), stringsAsFactors = FALSE)
icd9_ccs <- as.data.frame(apply(icd9_ccs, MARGIN = c(1,2), function(x) gsub("\\s", "", x)), stringsAsFactors = FALSE)
icd9_ccs <- icd9_ccs[, c("ICD-9-CM CODE", "CCS CATEGORY DESCRIPTION")]
icd9_ccs <- icd9_ccs[-c(1), ]

write.csv(icd9_ccs, "../Data/Crosswalk/icd9_to_singleCCSCategory.csv", row.names = FALSE)
