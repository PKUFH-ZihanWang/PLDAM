library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggsci)
library(openxlsx)


data <- read.xlsx(file.choose())
plot_cluster_analysis <- function(data, feature_name, num_clusters = 3, cluster_colors = NULL) {
  
  set.seed(42)
  kmeans_result <- kmeans(data[[feature_name]], centers = num_clusters)
  
  data$cluster <- kmeans_result$cluster
  
  disease_rate_by_cluster <- data %>%
    group_by(cluster) %>%
    summarise(
      total_count = n(),                     
      disease_count = sum(diagnosis_result == 1),
      disease_rate = disease_count / total_count * 100 
    )
  
  if (is.null(cluster_colors)) {
    cluster_colors <- RColorBrewer::brewer.pal(num_clusters, "Set3")
  }
  
  p1 <- ggplot(data, aes(x = .data[[feature_name]], fill = factor(cluster))) +
    geom_density(alpha = 0.5) +  
    scale_fill_lancet() + 
    labs(x = feature_name, y = "Density") +
    theme_bw() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      panel.border = element_rect(color = "black", linewidth = 1)  
    )
  
  p2 <- ggplot(disease_rate_by_cluster, aes(x = factor(cluster), y = disease_rate, fill = factor(cluster))) +
    geom_bar(stat = "identity", alpha = 0.5) +  
    scale_fill_lancet() +  
    labs(x = "Cluster", y = "Prevalence (%)") +
    theme_bw() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      panel.border = element_rect(color = "black", linewidth = 1)  
    )
  
  grid.arrange(p1, p2, ncol = 2)
}


plot_cluster_analysis(data_subset, "bladder_aspect_ratio", num_clusters = 3)
plot_cluster_analysis(data_subset, "PL_RS", num_clusters = 3)
plot_cluster_analysis(data_subset, "bladder_volume", num_clusters = 3)
plot_cluster_analysis(data_subset, "bladder_surface_area", num_clusters = 3)
plot_cluster_analysis(data_subset, "pelvic_fat_volume", num_clusters = 3)
plot_cluster_analysis(data_subset_filtered, "pelvic_fat_to_volume_ratio", num_clusters = 3)
plot_cluster_analysis(data_subset_filtered, "max_ureter_diameter", num_clusters = 3)
plot_cluster_analysis(data_subset_filtered, "urter_surface_area", num_clusters = 3)
plot_cluster_analysis(data_subset_filtered, "ureter_volume", num_clusters = 3)
plot_cluster_analysis(data_subset_filtered, "max_rectum_diameter", num_clusters = 3)
