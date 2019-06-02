library(ggplot2)

results <- readRDS("../Output/l1_ccs_binary_results.rds")
bootstrap <- readRDS("../Output/l1_ccs_binary_bootstrap.rds")

auc_df <- cbind(results[[1]], bootstrap[bootstrap$measure == "AUC",])

g <- ggplot(data = auc_df, aes(x = sample_size, y = auc_death)) + 
  geom_point() + 
  ylim(0.5, 1) + 
  theme_classic() + 
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 500, position = position_dodge(0.9))
