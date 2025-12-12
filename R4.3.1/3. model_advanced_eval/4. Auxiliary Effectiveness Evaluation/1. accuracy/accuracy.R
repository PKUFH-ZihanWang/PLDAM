library(fmsb)
library(dplyr)
library(ggplot2)



# 雷达图绘制函数
plot_radar_three_groups <- function(df, title_text, save_path,
                                    group_names = c("A", "B1", "B2"),
                                    colors_border = c("#0082A9", "#FF6633", "#009E73"),
                                    colors_in = c(scales::alpha("#0082A9", 0.3),
                                                  scales::alpha("#FF6633", 0.3),
                                                  scales::alpha("#009E73", 0.3))) {
  pdf(save_path, width = 8, height = 6)
  
  df_summary <- df %>%
    group_by(type) %>%
    summarise(across(c(Sensitivity, Specificity, PPV, NPV, F1, accuracy), mean, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-type) %>%
    as.data.frame()
	
	print(df_summary)
  
  radar_data <- rbind(
    rep(1, ncol(df_summary)),  # 最大值
    rep(0, ncol(df_summary)),  # 最小值
    df_summary
  )
  
  radarchart(
    radar_data,
    axistype = 1,
    pcol = colors_border,
    pfcol = colors_in,
    plwd = 2,
    plty = 1,
    cglcol = "grey",
    cglty = 1,
    cglwd = 0.8,
    axislabcol = "grey20",
    caxislabels = seq(0, 1, 0.25),
    vlcex = 1.0,
    title = paste("Radar Chart -", title_text)
  )
  
  legend("topright",
         legend = group_names,
         bty = "n",
         pch = 20,
         col = colors_border,
         text.col = "black",
         cex = 1.0,
         pt.cex = 2)
  
  dev.off()
}

# === 调用逻辑（和小提琴保持一致） ===
df_noexp <- data %>% filter(experience == "Inexperienced")
##plot_violin_three_groups(df_noexp, "Inexperienced", "C:/Users/ASUS/Desktop/abc/Inexperienced_violin.pdf")
plot_radar_three_groups(df_noexp, "Inexperienced", "C:/Users/ASUS/Desktop/abc/Inexperienced_radar.pdf")

df_lowexp <- data %>% filter(experience == "Less Experienced")
#plot_violin_three_groups(df_lowexp, "Less Experienced", "C:/Users/ASUS/Desktop/abc/Lowexp_violin.pdf")
plot_radar_three_groups(df_lowexp, "Less Experienced", "C:/Users/ASUS/Desktop/abc/Lowexp_radar.pdf")

df_midexp <- data %>% filter(experience == "Moderately Experienced")
#plot_violin_three_groups(df_midexp, "Moderately Experienced", "C:/Users/ASUS/Desktop/abc/Midexp_violin.pdf")
plot_radar_three_groups(df_midexp, "Moderately Experienced", "C:/Users/ASUS/Desktop/abc/Moderately Experienced_radar.pdf")

df_highexp <- data %>% filter(experience == "Highly Experienced")
#plot_violin_three_groups(df_highexp, "Highly Experienced", "C:/Users/ASUS/Desktop/abc/Highexp_violin.pdf")
plot_radar_three_groups(df_highexp, "Highly Experienced", "C:/Users/ASUS/Desktop/abc/Highly Experienced_radar.pdf")

df_lowlevel <- data %>% filter(level == "Junior Physician")
#plot_violin_three_groups(df_lowlevel, "Junior Physician", "C:/Users/ASUS/Desktop/abc/Lowlevel_violin.pdf")
plot_radar_three_groups(df_lowlevel, "Junior", "C:/Users/ASUS/Desktop/abc/Junior Physician_radar.pdf")

df_highlevel <- data %>% filter(level == "Senior Physician")
#plot_violin_three_groups(df_highlevel, "Senior Physician", "C:/Users/ASUS/Desktop/abc/Highlevel_violin.pdf")
plot_radar_three_groups(df_highlevel, "Senior", "C:/Users/ASUS/Desktop/abc/Senior Physician_radar.pdf")

df_urology <- data %>% filter(department == "Urology")
#plot_violin_three_groups(df_urology, "Urologist", "C:/Users/ASUS/Desktop/abc/Urology_violin.pdf")
plot_radar_three_groups(df_urology, "Urologist", "C:/Users/ASUS/Desktop/abc/Urologist_radar.pdf")

df_radio <- data %>% filter(department == "Radiology")
#plot_violin_three_groups(df_radio, "Radiologist", "C:/Users/ASUS/Desktop/abc/Radiology_violin.pdf")
plot_radar_three_groups(df_radio, "Radiologist", "C:/Users/ASUS/Desktop/abc/Radiologist_radar.pdf")

#plot_violin_three_groups(data, "All", "C:/Users/ASUS/Desktop/abc/Overall_violin.pdf")
plot_radar_three_groups(data, "All", "C:/Users/ASUS/Desktop/abc/Overall_radar.pdf")



library(ggplot2)
library(dplyr)
library(ggpubr)

# 三组小提琴散点图函数
plot_violin_three_groups <- function(df, title_text = "Accuracy", save_path = "violin_plot.pdf") {
  
  # 确保 type 是因子，顺序 A - B1 - B2
  df$type <- factor(df$type, levels = c("A", "B1", "B2"))
  
  # 找到 y 轴最大值
  y_max <- max(df$accuracy, na.rm = TRUE)
  
  # 配色：浅色用于violin，深色用于点
  violin_colors <- c("#0082A9", "#FF6633", "#009E73")   # 可自定义第三组
  point_colors  <- c("#0072B2", "#D55E00", "#009E73")   # 深色
  
  p <- ggplot(df, aes(x = type, y = accuracy, fill = type)) +
    geom_violin(trim = FALSE, alpha = 0.6, color = "black", width = 0.7, size = 0.6) +
    geom_jitter(aes(color = type), width = 0.15, size = 1.5, alpha = 0.7, stroke = 0.6) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.3, fatten = 1.6, color = "black") +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
    scale_fill_manual(values = violin_colors) +
    scale_color_manual(values = point_colors) +
    # 显著性标注：三组比较
    #stat_compare_means(
    #  method = "anova",
    #  label.y = y_max * 1.1
    #) +
    stat_compare_means(
      method = "t.test",
      comparisons = list(c("A", "B1"), c("A", "B2"), c("B1", "B2")),
      label.y = c(y_max * 1.15, y_max * 1.25, y_max * 1.35),
      symnum.args = list(
        cutpoints = c(0, 0.001, 0.01, 0.05, 1),
        symbols = c("<0.001", "<0.01", "<0.05", ">0.05")
      )
    ) +
    expand_limits(y = 0) +
    scale_y_continuous(limits = c(0, 1.3), breaks = seq(0, 1.3, 0.25)) +
    theme_classic(base_size = 14) +
    labs(title = title_text, y = "Accuracy", x = "") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none")
  
  # 保存 PDF
  ggsave(save_path, plot = p, width = 6, height = 6)
  
  return(p)
}



df_noexp <- data %>% filter(experience == "Inexperienced")
plot_violin_three_groups(df_noexp, "Inexperienced", "C:/Users/ASUS/Desktop/abc/Inexperienced.pdf")


df_lowexp <- data %>% filter(experience == "Less Experienced")
plot_violin_three_groups(df_lowexp, "Less Experienced", "C:/Users/ASUS/Desktop/abc/Less Experienced.pdf")


df_midexp <- data %>% filter(experience == "Moderately Experienced")
plot_violin_three_groups(df_midexp, "Moderately Experienced", "C:/Users/ASUS/Desktop/abc/Moderately Experienced.pdf")

df_highexp <- data %>% filter(experience == "Highly Experienced")
plot_violin_three_groups(df_highexp, "Highly Experienced", "C:/Users/ASUS/Desktop/abc/Highly Experienced.pdf")

df_lowlevel <- data %>% filter(level == "Junior Physician")
plot_violin_three_groups(df_lowlevel, "Junior", "C:/Users/ASUS/Desktop/abc/Junior Physician.pdf")

df_highlevel <- data %>% filter(level == "Senior Physician")
plot_violin_three_groups(df_highlevel, "Senior", "C:/Users/ASUS/Desktop/abc/Senior Physician.pdf")

plot_violin_three_groups <- function(df, title_text = "Accuracy", save_path = "violin_plot.pdf") {
  
  # 确保 type 是因子，顺序 A - B1 - B2
  df$type <- factor(df$type, levels = c("A", "B1", "B2"))
  
  # 找到 y 轴最大值
  y_max <- max(df$accuracy, na.rm = TRUE)
  
  # 配色：浅色用于violin，深色用于点
  violin_colors <- c("#0082A9", "#FF6633", "#009E73")   # 可自定义第三组
  point_colors  <- c("#0072B2", "#D55E00", "#009E73")   # 深色
  
  p <- ggplot(df, aes(x = type, y = accuracy, fill = type)) +
    geom_violin(trim = FALSE, alpha = 0.6, color = "black", width = 0.7, size = 0.6) +
    geom_jitter(aes(color = type), width = 0.15, size = 1.5, alpha = 0.7, stroke = 0.6) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.3, fatten = 1.6, color = "black") +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
    scale_fill_manual(values = violin_colors) +
    scale_color_manual(values = point_colors) +
    # 显著性标注：三组比较
    #stat_compare_means(
    #  method = "anova",
    #  label.y = y_max * 1.1
    #) +
    stat_compare_means(
      method = "t.test",
      comparisons = list(c("A", "B1"), c("A", "B2"), c("B1", "B2")),
      label.y = c(y_max * 1.3, y_max * 1.4, y_max * 1.5),
      symnum.args = list(
        cutpoints = c(0, 0.001, 0.01, 0.05, 1),
        symbols = c("<0.001", "<0.01", "<0.05", ">0.05")
      )
    ) +
    expand_limits(y = 0) +
    scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.25)) +
    theme_classic(base_size = 14) +
    labs(title = title_text, y = "Accuracy", x = "") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none")
  
  # 保存 PDF
  ggsave(save_path, plot = p, width = 6, height = 6)
  
  return(p)
}

df_urology <- data %>% filter(department == "Urology")
plot_violin_three_groups(df_urology, "Urologist", "C:/Users/ASUS/Desktop/abc/urology.pdf")

df_radio <- data %>% filter(department == "Radiology")
plot_violin_three_groups(df_radio, "Radiologist", "C:/Users/ASUS/Desktop/abc/radiology.pdf")

plot_violin_three_groups(data, "All", "C:/Users/ASUS/Desktop/abc/overall.pdf")