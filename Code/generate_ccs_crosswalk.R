# The original version of the crosswalks can be downloaded from
# the following location:
# https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip

# The column called CCS CATEGORY DESCRIPTION does not match between ICD-9 and ICD-10. 
# Although neither are perfect, the ICD-10 version is a bit better formatted
# Therefore, we can use those names to define categories.


icd9_ccs <- read.csv("../Data/Crosswalk/raw_ccs/$dxref 2015.csv", skip = 1, stringsAsFactors = FALSE, check.names = FALSE) # Skip a row

# Fix names
names(icd9_ccs) <- sapply(names(icd9_ccs), function(x) gsub("'", "", x))
icd9_ccs$`ICD-9-CM CODE` <- sapply(icd9_ccs$`ICD-9-CM CODE`, function(x) gsub("'", "", x))
icd9_ccs$`ICD-9-CM CODE` <- sapply(icd9_ccs$`ICD-9-CM CODE`, function(x) gsub("\\s", "", x))
icd9_ccs$`CCS CATEGORY DESCRIPTION` <- sapply(icd9_ccs$`CCS CATEGORY DESCRIPTION`, function(x) gsub("'", "", x))
icd9_ccs$`CCS CATEGORY` <- sapply(icd9_ccs$`CCS CATEGORY`, function(x) gsub("\\s", "", x))
icd9_ccs$`CCS CATEGORY` <- sapply(icd9_ccs$`CCS CATEGORY`, function(x) gsub("'", "", x))


# Read in ICD-10 Crosswalk
icd10_ccs <- read.csv("../Data/Crosswalk/raw_ccs/ccs_dx_icd10cm_2019_1.csv", stringsAsFactors = FALSE, check.names = FALSE)
names(icd10_ccs) <- sapply(names(icd10_ccs), function(x) gsub("'", "", x))
icd10_ccs$`ICD-10-CM CODE` <- sapply(icd10_ccs$`ICD-10-CM CODE`, function(x) gsub("'", "", x))
icd10_ccs$`ICD-10-CM CODE` <- sapply(icd10_ccs$`ICD-10-CM CODE`, function(x) gsub("\\s", "", x))
icd10_ccs$`CCS CATEGORY DESCRIPTION` <- sapply(icd10_ccs$`CCS CATEGORY DESCRIPTION`, function(x) gsub("'", "", x))
icd10_ccs$`CCS CATEGORY` <- sapply(icd10_ccs$`CCS CATEGORY`, function(x) gsub("\\s", "", x))
icd10_ccs$`CCS CATEGORY` <- sapply(icd10_ccs$`CCS CATEGORY`, function(x) gsub("'", "", x))


icd10_ccs <- icd10_ccs[, c("ICD-10-CM CODE", "CCS CATEGORY", "CCS CATEGORY DESCRIPTION")]

icd9_ccs <- merge(icd9_ccs, unique(icd10_ccs[, c("CCS CATEGORY", "CCS CATEGORY DESCRIPTION")]), by = "CCS CATEGORY")

icd9_ccs$`CCS CATEGORY DESCRIPTION` <- icd9_ccs$`CCS CATEGORY DESCRIPTION.y`
icd9_ccs <- icd9_ccs[, c("ICD-9-CM CODE", "CCS CATEGORY", "CCS CATEGORY DESCRIPTION")]
icd9_ccs <- icd9_ccs[-c(1), ]

write.csv(icd9_ccs, "../Data/Crosswalk/icd9_to_singleCCSCategory.csv", row.names = FALSE)
write.csv(icd10_ccs, "../Data/Crosswalk/icd10_to_singleCCSCategory.csv", row.names = FALSE)