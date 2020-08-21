library(icd)
library(checkpoint)
checkpoint("2019-05-01", checkpointLocation = '/root/', scanForPackages = FALSE)

codes <- c()
names <- c()

for (element in names(icd9_map_ahrq)){
  for (icd in icd9_map_ahrq[[element]]){
    codes <- c(codes, icd)
    names <- c(names, element)
  }
  
}

cw <- cbind(codes, names)
cw <- data.frame(cw, stringsAsFactors = FALSE)

write.csv(cw, "./Data/Crosswalk/icd9_ahrq.csv", row.names=FALSE)
