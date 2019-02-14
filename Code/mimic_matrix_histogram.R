## ICD MIMIC Analysis
## create frequency tables for code groupings
## Date Created: 2/10/2019
## Author: Aman Kansal

library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(stringr)

# read in data
ahrq_total_matrix <- readRDS("../Data/Processed/ahrq_total_matrix.rds")
pythia_ahrq_grouping_list <- read.csv("../Data/Final/pythia_ahrq_grouping_list.csv", header= T)

ccs_total_matrix <- readRDS("../Data/Processed/ccs_total_matrix.rds")
pythia_ccs_grouping_list <- read.csv("../Data/Final/pythia_ccs_grouping_list.csv", header= T)

trunc_total_matrix <- readRDS("../Data/Processed/trunc_total_matrix.rds")
pythia_trunc_grouping_list <- read.csv("../Data/Final/pythia_trunc_grouping_list.csv", header = T)

# transpose matrix and create frequency by proportion column
create_freq_plot <- function(code_matrix, grouping_list, table_title){
  transposed <- as.data.frame(t(code_matrix[,-1]))
  transposed <- cbind(grouping = rownames(transposed), transposed)
  rownames(transposed) <- NULL
  transposed <- as.data.table(transposed)
  transposed[, freq_1 := rowSums(transposed[, -1])]
  transposed[, freq_1 := freq_1/(sum(freq_1))]
  transposed <- merge(grouping_list, transposed, by = "grouping")
  setnames(transposed, old = c("freq", "freq_1"), new = c("pythia", "mimic"))
  
  freq_plot <- transposed %>%
    select(c("grouping", "pythia", "id", "mimic")) %>%
    melt(id = c("grouping", "id"), variable = "dataset") %>%
    arrange(id) %>%
    ggplot(aes(x = reorder(grouping, id), y = value, fill = dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + labs(x = "Grouping", y = "Frequency (proportion)", title = table_title)
  
  return(freq_plot)
}

ahrq_freq_plot <- create_freq_plot(ahrq_total_matrix, pythia_ahrq_grouping_list, "Frequency table of AHRQ groupings")
ccs_freq_plot <- create_freq_plot(ccs_total_matrix, pythia_ccs_grouping_list, "Frequency table of CCS groupings")
trunc_freq_plot <- create_freq_plot(trunc_total_matrix, pythia_trunc_grouping_list, "Frequency table of Truncated groupings")
