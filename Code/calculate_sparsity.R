# Generates Sparsity Table
# For each of the datasets, look at the the proportion of zero elements
library(checkpoint)
checkpoint("2019-05-01", checkpointLocation = '/root/', scanForPackages = FALSE)
data_dir <- './Data/Processed/'
representation <- "binary"

result_df <- data.frame()
for (f in list.files(data_dir)){
  if (grepl(representation, f)){
    print(paste0("Reading in: ", f))
    mat <- readRDS(file.path(data_dir, f))
    # remove columns that are not binary
    sparsity <- 1 - (length(mat[[1]]@i) / (mat[[1]]@Dim[1] * mat[[1]]@Dim[2]))
    result_df <- rbind(result_df, data.frame(file = f, sparsity = sparsity))
    print("Computed sparsity")
  }
}

print(result_df)