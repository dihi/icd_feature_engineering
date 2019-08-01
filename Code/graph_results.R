#!/usr/bin/env Rscript

suppress_all <- function(x){
  suppressWarnings(suppressMessages(x))
}

suppress_all(library(ggplot2))
suppress_all(library(optparse))

# Command-line Arguments
option_list <- list(
  make_option(c("-o", "--output_dir"), action =  "store", default = "./Output/",
              help = "The directory where the output of model runs are stored"),
  make_option(c("-m", "--model_type"), action = "store", default = "l1",
              help = "The model results to plot. One of 'l1', 'l2', 'rf', or 'xgb'")
)


parser <- OptionParser(usage = "%prog [options]", option_list = option_list)
arguments <- parse_args(parser, positional_arguments = 0)

# Create output directory
dir.create(paste0(arguments$options$output_dir, "figures/"), showWarnings = FALSE)

new_output <- paste0(arguments$options$output_dir, "figures/")

results_list = list()
bootstrap_list = list()

for (f in list.files(arguments$options$output_dir)){
  if (grepl(arguments$options$model_type, f)){
    if (grepl("results", f)){
      results_list[[f]] <- readRDS(paste0(arguments$options$output_dir, f))
    }
    if (grepl("bootstrap", f)){
      bootstrap_list[[f]] <- readRDS(paste0(arguments$options$output_dir, f))
    }
  }
}


auc_df <- data.frame(grouping = character(), auc = numeric(), sample_size = numeric(), lower_bound = numeric(), upper_bound = numeric(), representation = character())
pr_df <- data.frame(grouping = character(), pr = numeric(), sample_size = numeric(), lower_bound = numeric(), upper_bound = numeric(), representation = character())
time_df <- data.frame(grouping = character(), time = numeric(), sample_size = numeric(), representation = character())

build_grouping_regex <- function(prefix){
  return (paste0("(?<=", prefix, "_)[A-Za-z0-9]+"))
}

grouping_regex <- build_grouping_regex(arguments$options$model_type)

for (i in names(results_list)){
  if (grepl("binary", i)){
    grouping <- regmatches(i, regexpr(grouping_regex, i, perl = TRUE))
    auc <- results_list[[i]][[1]]
    bootstrap_df <- bootstrap_list[[gsub("results", "bootstrap", i)]]
    auc_res <- cbind(grouping=grouping, auc = auc$auc_death, bootstrap_df[bootstrap_df$measure == "AUC", c("sample_size", "lower_bound", "upper_bound")], representation = "binary")
    auc_df <- rbind(auc_df, auc_res)
    
    pr <- results_list[[i]][[2]]
    pr_res <- cbind(grouping=grouping, pr = pr$pr_death, bootstrap_df[bootstrap_df$measure == "pr", c("sample_size", "lower_bound", "upper_bound")], representation = "binary")
    pr_df <- rbind(pr_df, pr_res)
    
    time <- results_list[[i]][[3]]$elapsed
    time_res <- cbind(grouping = grouping, time = time, sample_size = bootstrap_df[bootstrap_df$measure == "AUC", "sample_size"], representation = "binary")
    time_df <- rbind(time_df, time_res)
  }
  if (grepl("total", i)){
    grouping <- regmatches(i, regexpr(grouping_regex, i, perl = TRUE))
    auc <- results_list[[i]][[1]]
    bootstrap_df <- bootstrap_list[[gsub("results", "bootstrap", i)]]
    auc_res <- cbind(grouping=grouping, auc = auc$auc_death, bootstrap_df[bootstrap_df$measure == "AUC", c("sample_size", "lower_bound", "upper_bound")], representation = "total")
    auc_df <- rbind(auc_df, auc_res)
    
    pr <- results_list[[i]][[2]]
    pr_res <- cbind(grouping=grouping, pr = pr$pr_death, bootstrap_df[bootstrap_df$measure == "pr", c("sample_size", "lower_bound", "upper_bound")], representation = "total")
    pr_df <- rbind(pr_df, pr_res)
    
    time <- results_list[[i]][[3]]$elapsed
    time_res <- cbind(grouping = grouping, time = time, sample_size = bootstrap_df[bootstrap_df$measure == "pr", "sample_size"], representation = "total")
    time_df <- rbind(time_df, time_res)
    
  }
}


# set levels and unfactor
time_df$time <- as.numeric(as.character(time_df$time))
time_df$sample_size <- as.numeric(as.character(time_df$sample_size))
time_df$grouping <- factor(time_df$grouping, c("ahrq", "ccs", "trunc", "raw"))

# Set limits for plots
auc_ylim_lower <- min(auc_df$lower_bound) - sd(auc_df$auc)
auc_ylim_upper <- max(auc_df$upper_bound) + sd(auc_df$auc)

pr_ylim_lower <- min(pr_df$lower_bound) - sd(pr_df$pr)
pr_ylim_upper <- max(pr_df$upper_bound) + sd(pr_df$pr)

# make_name
make_title <- function(type){
  if (type == "l1"){
    return ("L1 Penalized Logistic Regression")
  } else if (type == "l2"){
    return ("L2 Penalized Logistic Regression")
  } else if (type == "rf"){
    return ("Random Forest")
  } else if (type == "xgb"){
    return ("Gradient Boosted Trees")
  }
}

title <- make_title(arguments$options$model_type)

# Plot 
auc_plot <- ggplot(data = auc_df, aes(x = sample_size, y = auc, color = grouping)) + 
  geom_point(size = .85, position = position_dodge(1000)) + 
  facet_grid(representation ~ ., scales = "fixed") + 
  ylim(auc_ylim_lower, auc_ylim_upper) + 
  theme_minimal() + 
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), position = position_dodge(1000), width = 1200,  size = .75, alpha = 1) +
  labs(title = paste0(title, " AUC"), x = "Sample Size", y = "AUROC") +
  theme(panel.background = element_rect(fill = NA, color = "black"))

pr_plot <- ggplot(data = pr_df, aes(x = sample_size, y = pr, color = grouping)) + 
  geom_point(size = .85, position = position_dodge(1000)) + 
  facet_grid(representation ~ ., scales = "fixed") + 
  ylim(pr_ylim_lower, pr_ylim_upper) + 
  theme_minimal() + 
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), position = position_dodge(1000), width = 1200,  size = .75, alpha = 1) +
  labs(title = paste0(title, " AUCPR"), x = "Sample Size", y = "AUCPR") +
  theme(panel.background = element_rect(fill = NA, color = "black"))


time_plot <- ggplot(data = time_df, aes(x = sample_size, y = time, fill= grouping)) + 
  geom_bar(stat = "identity", position = "dodge", width = 1200) +
  facet_grid(representation ~ ., scales = "fixed") + 
  theme_minimal() + 
  labs(title = paste0(title, ' Training Time'), x = "Sample Size", y = "Time (minutes)") +
  theme(panel.background = element_rect(fill = NA, color = "black"))


ggsave(paste0(new_output, arguments$options$model_type, "_auc.png"), auc_plot, scale = 1, height = 4.7, width = 7, units = "in")
ggsave(paste0(new_output, arguments$options$model_type, "_pr.png"), pr_plot, scale = 1, height = 4.7, width = 7, units = "in")
ggsave(paste0(new_output, arguments$options$model_type, "_time.png"), time_plot, scale = 1, height = 4.7, width = 7, units = "in")

