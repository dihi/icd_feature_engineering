# ICD MIMIC Analysis
# Dx tables: using ONLY codes
# Date Created: 1/4/2019
# Author: Aman Kansal
# Modified: 03/14/19 - Michael Gao

# Typical run:
# ./mimic_dx_tables.R -g ccs 
# To see usage:
# ./mimic_dx_tables.R -h

Sys.setenv(TZ = "America/New_York")

suppress_all <- function(x){
  suppressWarnings(suppressMessages(x))
}

suppress_all(library(data.table))
suppress_all(library(Matrix))
suppress_all(library(lubridate))
suppress_all(library(optparse))



# Parse Command Line Arguments
option_list <- list(
  make_option(c("-g", "--grouping"), action = "store", default = "ahrq",
              help = "The grouping method used for icd codes. One of 'ahrq', 'ccs', 'trunc', or 'raw'"),
  make_option(c("-o", "--output_dir"), action =  "store", default = "..Data/Processed/",
              help = "The directory where the output matrices gets stored")
)

# Example run:
# Rscript run_model.R -g ahrq -m xgb 

parser <- OptionParser(usage = "%prog [options]", option_list = option_list)
arguments <- parse_args(parser, positional_arguments = 0)

# Create output directory
dir.create(arguments$options$output_dir, showWarnings = FALSE)


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


create_matrix <- function(map, group_name, type) {
  if (type == "total"){
    res <- map[, count := .N, by = list(HADM_ID, map[[group_name]])]
    res <- dcast(res, HADM_ID ~ map[[group_name]], fun.aggregate = length, value.var = 'count')
    return(res)
  } else if (type == "binary"){
    map$count <- 1L
    map <- unique(map)
    map <- dcast(map, HADM_ID ~ map[[group_name]], value.var = 'count', fill = 0)
    return(map)
  }
}
  
# The following function creates a sparse model matrix
# It does so in a way to minimize memory overhead to ensure
# that operations will run on commodity hardware
# From https://github.com/ben519/mltools/blob/master/R/sparsify.R

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


if (args$options$grouping == "ahrq") {
  # Read in crosswalk
  icd9_ahrq_cw <- fread("../Data/Crosswalk/icd9_ahrq.csv")
  
  # Create codes map
  codes <- codes[, c("HADM_ID", "ICD9_CODE")]
  codes <- codes[ICD9_CODE != "",]
  ahrq_map <- merge(codes, icd9_ahrq_cw, by.x = "ICD9_CODE", by.y = "codes", all.x = TRUE, allow.cartesian = TRUE)
  ahrq_map <- as.data.table(ahrq_map[, c("HADM_ID", "names")]) 
  ahrq_map <- ahrq_map[complete.cases(ahrq_map),]
  
  # Create matrices and save
  ahrq_total_matrix <- create_matrix(ahrq_map, 'names', type = 'total')
  ahrq_total_matrix <- merge(all_encounters, ahrq_total_matrix, by = "HADM_ID")
  
  death_in_year <- ahrq_total_matrix$death_in_year
  
  ahrq_total_matrix <- ahrq_total_matrix[, -c("HADM_ID", "death_in_year")]
  ahrq_total_matrix <- create_model_matrix(ahrq_total_matrix)
  
  saveRDS(list(ahrq_total_matrix, death_in_year), paste0(arguments$options$output_dir, "/ahrq_total_matrix.rds"))
  remove(ahrq_total_matrix)
  
  # Create Binary matrix

  ahrq_binary_matrix <- create_matrix(ahrq_map, 'names', type = 'binary')
  ahrq_binary_matrix <- merge(all_encounters, ahrq_binary_matrix, by = "HADM_ID")
  
  ahrq_binary_matrix <- ahrq_binary_matrix[, -c("HADM_ID", "death_in_year")]
  ahrq_binary_matrix <- create_model_matrix(ahrq_binary_matrix)
  
  saveRDS(list(ahrq_binary_matrix, death_in_year), paste0(arguments$options$output_dir, "/ahrq_binary_matrix.rds"))
  
} else if(args$options$grouping == "ccs") {
  # Read in crosswalk
  icd9_ccs_cw <- fread("../Data/Crosswalk/icd9_to_singleCCScategory.csv")
  icd9_ccs_cw <- icd9_ccs_cw[, c("ICD-9-CM CODE", "CCS CATEGORY DESCRIPTION")]
  # Create Codes map
  ccs_map <- merge(codes, icd9_ccs_cw, by.x = "ICD9_CODE", by.y ="ICD-9-CM CODE", all.x = TRUE, allow.cartesian = TRUE)
  ccs_map <- as.data.table(ccs_map[, c("HADM_ID", "CCS CATEGORY DESCRIPTION")])
  ccs_map <- ccs_map[complete.cases(ccs_map),]
  
  # Create matrices and save
  ccs_total_matrix <- create_matrix(ccs_map, 'CCS CATEGORY DESCRIPTION', type = "total")
  ccs_total_matrix <- merge(all_encounters, ccs_total_matrix, by = "HADM_ID")
  
  death_in_year <- ccs_total_matrix$death_in_year
  
  ccs_total_matrix <- ccs_total_matrix[, -c("HADM_ID", "death_in_year")]
  ccs_total_matrix <- create_model_matrix(ccs_total_matrix)
  
  saveRDS(list(ccs_total_matrix, death_in_year), paste0(arguments$options$output_dir, "/ccs_total_matrix.rds"))
  remove(ccs_total_matrix)
  
  ccs_binary_matrix <- create_matrix(ccs_map, 'CCS CATEGORY DESCRIPTION', type = "binary")
  ccs_binary_matrix <- merge(all_encounters, ccs_binary_matrix, by = "HADM_ID")
  
  ccs_binary_matrix <- ccs_binary_matrix[, -c("HADM_ID", "death_in_year")]
  ccs_binary_matrix <- create_model_matrix(ccs_binary_matrix)
  
  saveRDS(list(ccs_binary_matrix, death_in_year), paste0(arguments$options$output_dir, "/ccs_binary_matrix.rds"))
  
} else if(args$options$grouping == "truncated") {
  # Create map
  codes$truncated <- substr(codes$ICD9_CODE, 1, 3)
  codes <- codes[, c("HADM_ID", "truncated")]
  codes <- codes[truncated != "", ]
  
  # Create matrices and save
  trunc_total_matrix <- create_matrix(codes, 'truncated', type = "total")
  trunc_total_matrix <- merge(all_encounters, trunc_total_matrix, by = "HADM_ID")
  
  death_in_year <- trunc_total_matrix$death_in_year
  
  trunc_total_matrix <- trunc_total_matrix[, -c("HADM_ID", "death_in_year")]
  trunc_total_matrix <- create_model_matrix(trunc_total_matrix)
  
  saveRDS(list(trunc_total_matrix, death_in_year), paste0(arguments$options$output_dir, "/trunc_total_matrix.rds"))
  remove(trunc_total_matrix)
  
  trunc_binary_matrix <- create_matrix(codes, 'truncated', type = "binary")
  trunc_binary_matrix <- merge(all_encounters, trunc_binary_matrix, by = "HADM_ID")
  
  trunc_binary_matrix <- trunc_binary_matrix[, -c("HADM_ID", "death_in_year")]
  trunc_binary_matrix <- create_model_matrix(trunc_binary_matrix)
  
  saveRDS(list(trunc_binary_matrix, death_in_year), paste0(arguments$options$output_dir, "/trunc_binary_matrix.rds"))
  
} else if(args$options$grouping == "raw") {
  # Create map
  codes <- codes[, c("HADM_ID", "ICD9_CODE")]
  codes <- codes[ICD9_CODE != "",]
  
  #Create matrices and save
  raw_total_matrix <- create_matrix(codes, "ICD9_CODE", type = "total")
  raw_total_matrix <- merge(all_encounters, raw_total_matrix, by = "HADM_ID")
  
  death_in_year <- raw_total_matrix$death_in_year
  
  raw_total_matrix <- raw_total_matrix[, -c("HADM_ID", "death_in_year")]
  raw_total_matrix <- create_model_matrix(raw_total_matrix)
  
  saveRDS(list(raw_total_matrix, death_in_year), paste0(arguments$options$output_dir, "/raw_total_matrix.rds"))
  remove(raw_total_matrix)

  raw_binary_matrix <- create_matrix(codes, "ICD9_CODE", type = "binary")
  raw_binary_matrix <- merge(all_encounters, raw_binary_matrix, by = "HADM_ID")
  
  death_in_year <- raw_binary_matrix$death_in_year
  
  raw_binary_matrix <- raw_binary_matrix[, -c("HADM_ID", "death_in_year")]
  raw_binary_matrix <- create_model_matrix(raw_binary_matrix)
  
  saveRDS(list(raw_binary_matrix, death_in_year), paste0(arguments$options$output_dir, "/raw_binary_matrix.rds"))
}


