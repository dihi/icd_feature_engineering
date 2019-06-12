# Generates Sparsity Table
# For each of the datasets, look at the the proportion of zero elements

data_dir <- '../Data/Final/'
representation <- "binary"

result_df <- data.frame()
for (f in list.files(data_dir)){
  if (grepl(representation, f)){
    print(paste0("Reading in: ", f))
    mat <- readRDS(file.path(data_dir, f))
    # remove columns that are not binary
    mat <- mat[, !names(mat) %in% c('HADM_ID', 'GENDER', 'AGE', 'death_in_year')]
    sparsity <- sum(mat == 0)/(prod(dim(mat)))
    result_df <- rbind(result_df, data.frame(file = f, sparsity = sparsity))
    print("Computed sparsity")
  }
}

print(result_df)