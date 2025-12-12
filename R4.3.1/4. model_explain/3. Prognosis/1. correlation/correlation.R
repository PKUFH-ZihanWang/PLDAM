########### ALL
sample_data <- read.csv(file.choose())

sample_data_subset <- sample_data %>%
  dplyr::select(all_of(vars_to_keep))


sample_data_subset_names <- colnames(sample_data_subset)
					
shp_names <- sub("_$", "", sample_data_subset_names)

shp_names <- gsub("^original_", "", shp_names)
shp_names <- gsub("^diagnostics_", "", shp_names)

normalize_names <- function(shp_names) {
  shp_names <- sub("_$", "", shp_names)
  organs <- c("trigone_of_bladder", "pelvic_cavity", "pelvic_fat",
              "ureter", "bladder", "rectum")

  for (org in organs) {
    idx_end <- grepl(paste0("_", org, "$"), shp_names) 
    idx_start <- grepl(paste0("^", org, "(_|$)"), shp_names)
    to_change <- idx_end & !idx_start
    
    if (any(to_change)) {
      shp_names[to_change] <- paste0(
        org, "_",
        sub(paste0("(_", org, "$)"), "", shp_names[to_change])
      )
    }
  }
  
  shp_names <- gsub("__+", "_", shp_names)
  shp_names <- sub("^_|_$", "", shp_names)
  
  return(shp_names)
}

new_names <- normalize_names(shp_names)
print(new_names)

colnames(sample_data_subset) <- new_names

library(dplyr)


library(dplyr)
library(ggplot2)
library(reshape2)

cont_vars <- sample_data_subset %>%
  select(where(is.numeric))
  
corr_matrix <- cor(cont_vars, use = "complete.obs", method = "spearman")

corr_matrix[upper.tri(corr_matrix)] <- NA

corr_melt <- melt(corr_matrix, varnames = c("Var1","Var2"), value.name = "Correlation", na.rm = TRUE)

ggplot(corr_melt, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 2) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "bold", size = 6),
    axis.text.y = element_text(face = "bold", size = 6),
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    axis.ticks.y.right = element_line(),  
    axis.text.y.right  = element_text(face = "bold", size = 6),
    axis.text.y.left   = element_blank(),
    legend.position = "left"             
  ) +
  labs(
    title = "Correlation Matrix of Continuous Variables",
    x = NULL, y = NULL
  ) +
  scale_y_discrete(position = "right")


level_numeric <- as.numeric(sample_data_subset$level)

corr_with_level <- sapply(cont_vars, function(x) cor.test(x, level_numeric, method="spearman")$estimate)
p_with_level <- sapply(cont_vars, function(x) cor.test(x, level_numeric, method="spearman")$p.value)

rho_threshold <- 0.3
p_threshold <- 0.05

high_corr_vars <- names(cont_vars)[
  (abs(corr_with_level) >= rho_threshold) & (p_with_level < p_threshold)
]

print("High-correlation variables with level:")
print(high_corr_vars)







level_numeric <- as.numeric(sample_data_subset$level)
cont_vars <- sample_data_subset %>% 
  select(where(is.numeric)) %>% 
  select(-level)   

corr_with_level <- sapply(cont_vars, 
                          function(x) cor.test(x, level_numeric, method="spearman")$estimate)

p_with_level <- sapply(cont_vars, 
                       function(x) cor.test(x, level_numeric, method="spearman")$p.value)

corr_df <- data.frame(
  Variable = names(corr_with_level),
  Rho = corr_with_level,
  P = p_with_level
)

corr_df$Significance <- ifelse(corr_df$P < 0.05, "Significant", "Not Significant")

ggplot(corr_df, aes(x = reorder(Variable, Rho), y = Rho, fill = Significance)) +
  geom_bar(stat = "identity", color="black") +
  coord_flip() +
  geom_hline(aes(yintercept = 0.1, linetype = "Weak threshold (|rho|=0.1)"), color="blue", size=1) +
  geom_hline(aes(yintercept = -0.1, linetype = "Weak threshold (|rho|=0.1)"), color="blue", size=1) +
  geom_hline(aes(yintercept = 0.3, linetype = "Moderate threshold (|rho|=0.3)"), color="red", size=1) +
  geom_hline(aes(yintercept = -0.3, linetype = "Moderate threshold (|rho|=0.3)"), color="red", size=1) +
  scale_fill_manual(values = c("Significant"="darkseagreen3", "Not Significant"="grey80")) +
  scale_linetype_manual(values = c("Weak threshold (|rho|=0.1)" = "dashed",
                                   "Moderate threshold (|rho|=0.3)" = "dashed")) +
  labs(title = "Correlation with Level",
       x = NULL, y = "Spearman's rho",
       fill = "P-value significance",
       linetype = "Correlation thresholds") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.text = element_text(size=8, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text  = element_text(size=9)
  )







##########################腺性膀胱炎

sample_data_subset01 <- subset(sample_data_subset, level %in% c(0, 1))
colnames(sample_data_subset01)[colnames(sample_data_subset01) == "level"] <- "cystitis_glandularis"

colnames(sample_data_subset01)

cont_vars <- sample_data_subset01 %>%
  select(where(is.numeric))
  
corr_matrix <- cor(cont_vars, use = "complete.obs", method = "spearman")

corr_matrix[upper.tri(corr_matrix)] <- NA


corr_melt <- melt(corr_matrix, varnames = c("Var1","Var2"), value.name = "Correlation", na.rm = TRUE)

ggplot(corr_melt, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 2) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "bold", size = 6),
    axis.text.y = element_text(face = "bold", size = 6),
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    axis.ticks.y.right = element_line(),   
    axis.text.y.right  = element_text(face = "bold", size = 6),
    axis.text.y.left   = element_blank(), 
    legend.position = "left"             
  ) +
  labs(
    title = "Correlation Matrix of Continuous Variables",
    x = NULL, y = NULL
  ) +
  scale_y_discrete(position = "right")

level_numeric <- as.numeric(sample_data_subset01$level)

corr_with_level <- sapply(cont_vars, function(x) cor.test(x, level_numeric, method="spearman")$estimate)
p_with_level <- sapply(cont_vars, function(x) cor.test(x, level_numeric, method="spearman")$p.value)

rho_threshold <- 0.3
p_threshold <- 0.05

high_corr_vars <- names(cont_vars)[
  (abs(corr_with_level) >= rho_threshold) & (p_with_level < p_threshold)
]

print("High-correlation variables with level:")
print(high_corr_vars)






level_numeric <- as.numeric(sample_data_subset01$level)
cont_vars <- sample_data_subset01 %>% 
  select(where(is.numeric)) %>% 
  select(-level)

corr_with_level <- sapply(cont_vars, 
                          function(x) cor.test(x, level_numeric, method="spearman")$estimate)

p_with_level <- sapply(cont_vars, 
                       function(x) cor.test(x, level_numeric, method="spearman")$p.value)

corr_df <- data.frame(
  Variable = names(corr_with_level),
  Rho = corr_with_level,
  P = p_with_level
)

corr_df$Significance <- ifelse(corr_df$P < 0.05, "Significant", "Not Significant")

ggplot(corr_df, aes(x = reorder(Variable, Rho), y = Rho, fill = Significance)) +
  geom_bar(stat = "identity", color="black") +
  coord_flip() +
  geom_hline(aes(yintercept = 0.1, linetype = "Weak threshold (|rho|=0.1)"), color="blue", size=1) +
  geom_hline(aes(yintercept = -0.1, linetype = "Weak threshold (|rho|=0.1)"), color="blue", size=1) +
  geom_hline(aes(yintercept = 0.3, linetype = "Moderate threshold (|rho|=0.3)"), color="red", size=1) +
  geom_hline(aes(yintercept = -0.3, linetype = "Moderate threshold (|rho|=0.3)"), color="red", size=1) +
  scale_fill_manual(values = c("Significant"="darkseagreen3", "Not Significant"="grey80")) +
  scale_linetype_manual(values = c("Weak threshold (|rho|=0.1)" = "dashed",
                                   "Moderate threshold (|rho|=0.3)" = "dashed")) +
  labs(title = "Correlation with Level",
       x = NULL, y = "Spearman's rho",
       fill = "P-value significance",
       linetype = "Correlation thresholds") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.text = element_text(size=8, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text  = element_text(size=9)
  )




##########################肾积水

sample_data_subset01 <- subset(sample_data_subset, level %in% c(0, 2))
colnames(sample_data_subset01)[colnames(sample_data_subset01) == "level"] <- "hydronephrosis"


cont_vars <- sample_data_subset01 %>%
  select(where(is.numeric))
  
corr_matrix <- cor(cont_vars, use = "complete.obs", method = "spearman")

corr_matrix[upper.tri(corr_matrix)] <- NA

corr_melt <- melt(corr_matrix, varnames = c("Var1","Var2"), value.name = "Correlation", na.rm = TRUE)

ggplot(corr_melt, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 2) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "bold", size = 6),
    axis.text.y = element_text(face = "bold", size = 6),
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    axis.ticks.y.right = element_line(), 
    axis.text.y.right  = element_text(face = "bold", size = 6),
    axis.text.y.left   = element_blank(), 
    legend.position = "left"              
  ) +
  labs(
    title = "Correlation Matrix of Continuous Variables",
    x = NULL, y = NULL
  ) +
  scale_y_discrete(position = "right")

level_numeric <- as.numeric(sample_data_subset01$hydronephrosis)

corr_with_level <- sapply(cont_vars, function(x) cor.test(x, level_numeric, method="spearman")$estimate)
p_with_level <- sapply(cont_vars, function(x) cor.test(x, level_numeric, method="spearman")$p.value)

rho_threshold <- 0.3
p_threshold <- 0.05

high_corr_vars <- names(cont_vars)[
  (abs(corr_with_level) >= rho_threshold) & (p_with_level < p_threshold)
]

print("High-correlation variables with level:")
print(high_corr_vars)




level_numeric <- as.numeric(sample_data_subset01$level)
cont_vars <- sample_data_subset01 %>% 
  select(where(is.numeric)) %>% 
  select(-level)

corr_with_level <- sapply(cont_vars, 
                          function(x) cor.test(x, level_numeric, method="spearman")$estimate)

p_with_level <- sapply(cont_vars, 
                       function(x) cor.test(x, level_numeric, method="spearman")$p.value)

corr_df <- data.frame(
  Variable = names(corr_with_level),
  Rho = corr_with_level,
  P = p_with_level
)

corr_df$Significance <- ifelse(corr_df$P < 0.05, "Significant", "Not Significant")

ggplot(corr_df, aes(x = reorder(Variable, Rho), y = Rho, fill = Significance)) +
  geom_bar(stat = "identity", color="black") +
  coord_flip() +
  geom_hline(aes(yintercept = 0.1, linetype = "Weak threshold (|rho|=0.1)"), color="blue", size=1) +
  geom_hline(aes(yintercept = -0.1, linetype = "Weak threshold (|rho|=0.1)"), color="blue", size=1) +
  geom_hline(aes(yintercept = 0.3, linetype = "Moderate threshold (|rho|=0.3)"), color="red", size=1) +
  geom_hline(aes(yintercept = -0.3, linetype = "Moderate threshold (|rho|=0.3)"), color="red", size=1) +
  scale_fill_manual(values = c("Significant"="darkseagreen3", "Not Significant"="grey80")) +
  scale_linetype_manual(values = c("Weak threshold (|rho|=0.1)" = "dashed",
                                   "Moderate threshold (|rho|=0.3)" = "dashed")) +
  labs(title = "Correlation with Level",
       x = NULL, y = "Spearman's rho",
       fill = "P-value significance",
       linetype = "Correlation thresholds") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.text = element_text(size=8, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text  = element_text(size=9)
  )




