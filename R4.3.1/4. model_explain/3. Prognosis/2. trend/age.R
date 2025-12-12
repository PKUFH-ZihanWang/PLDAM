library(ggplot2)

plot_age_distribution <- function(data, level_value) {
  
  df <- subset(data, level == level_value)
  
  # Shapiro-Wilk 正态性检验
  shapiro_test <- shapiro.test(df$age)
  p_value <- round(shapiro_test$p.value, 2)
  
  ggplot(df, aes(x = age)) +
    geom_histogram(aes(y = ..density..), 
                   bins = 25, 
                   fill = "darkseagreen3", 
                   color = "black", 
                   alpha = 0.6) +
    geom_density(color = "red", size = 1) +
    geom_vline(aes(xintercept = mean(age, na.rm = TRUE)), 
               color = "blue", 
               linetype = "dashed", 
               size = 1) +
    labs(
      title = paste0("Age Distribution of Patients (Level ", level_value, ")"),
      subtitle = paste0("Shapiro-Wilk: p = ", p_value, 
                        ifelse(p_value > 0.05, " ... Normal distribution", " ... Not normal distribution")),
      x = "Age",
      y = "Density"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.line = element_line(color = "black"),     
      axis.ticks = element_line(color = "black"),    
      axis.text = element_text(color = "black")      
    )
}



plot_age_distribution(data_sample, 0)
plot_age_distribution(data_sample, 1)
plot_age_distribution(data_sample, 2)
plot_age_distribution(data_sample, 3)



plot_age_distribution <- function(data, 
                                  value_col, 
                                  group_col, 
                                  title = "Age Distribution by Level",
                                  bins = 30,
                                  save_path = NULL) {
  library(ggplot2)
  library(dplyr)
  
  # 提取数值和分组列
  df <- data %>%
    dplyr::select({{ value_col }}, {{ group_col }}) %>%
    rename(Value = {{ value_col }}, Group = {{ group_col }})
  
  # Shapiro–Wilk 正态性检验（整体）
  shapiro_result <- shapiro.test(df$Value)
  p_value <- round(shapiro_result$p.value, 3)
  normality_text <- ifelse(p_value > 0.05,
                           paste0("Shapiro–Wilk: p = ", p_value, " (Normal)"),
                           paste0("Shapiro–Wilk: p = ", p_value, " (Non-normal)"))
  
  # 整体均值
  mean_value <- mean(df$Value, na.rm = TRUE)
  
  # 绘图
  p <- ggplot(df, aes(x = Value, fill = Group)) +
    geom_histogram(aes(y = ..density..),
                   position = "stack", bins = bins,
                   color = "black", alpha = 0.75) +
    geom_density(aes(x = Value),
                 data = df, inherit.aes = FALSE,
                 color = "red", linewidth = 1.2) +
    geom_vline(xintercept = mean_value, color = "blue",
               linetype = "dashed", linewidth = 1) +
    annotate("text", x = mean_value + diff(range(df$Value)) * 0.03, 
             y = max(density(df$Value, na.rm = TRUE)$y) * 0.9,
             label = paste0("Mean = ", round(mean_value, 1)),
             color = "blue", hjust = 0, size = 4) +
    labs(title = title,
         subtitle = normality_text,
         x = deparse(substitute(value_col)),
         y = "Density", fill = deparse(substitute(group_col))) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
  
  # 保存 PDF
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 10, height = 5)
    message("✅ 图已保存到: ", save_path)
  }
  
  return(p)
}

lot_age_distribution(
  data = data_sample,
  value_col = Age,
  group_col = Level,
  title = "Age Distribution of Patients (All Levels)",
  bins = 30,
  save_path = "age_distribution_overall_density.pdf"
)