plot_time <- function(data, group_col, value_col,
                            colors = c("#0082A9", "#FF6633"),
                            comparisons = NULL,
                            y_label = "Time (s)",
                            title = NULL,
                            save_path = "violin_plot.pdf") {
  if (is.null(comparisons)) {
    grp <- unique(data[[group_col]])
    if (length(grp) > 1) {
      comparisons <- combn(grp, 2, simplify = FALSE)
    } else {
      comparisons <- NULL
    }
  }

  p <- ggplot(data, aes_string(x = group_col, y = value_col, color = group_col)) +
    geom_jitter(width = 0.15, size = 2.5, alpha = 0.8) +    
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") + 
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.15,
                 color = "black", size = 0.8) +               
    scale_color_manual(values = colors) +
    { if (!is.null(comparisons)) stat_compare_means(comparisons = comparisons,
                                                   method = "t.test",
                                                   label = "p.format",
                                                   size = 4,
                                                   hide.ns = TRUE) } +
    theme_classic(base_size = 14) +
    labs(y = y_label, x = NULL, title = title) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  if (!dir.exists(dirname(save_path))) dir.create(dirname(save_path), recursive = TRUE)
  ggsave(save_path, plot = p, width = 6, height = 6)
  return(p)
}


df_noexp <- data %>% filter(experience == "Inexperienced")
p1 <- plot_time(
  data = df_noexp,
  group_col = "type",
  value_col = "time",
  title = "",
  save_path = "./result/Inexperienced Time.pdf"
)
p1


df_lowexp <- data %>% filter(experience == "Less Experienced")
plot_time(
  data = df_lowexp,
  group_col = "type",
  value_col = "time",
  title = "",
  save_path = "./result/Less Experienced Time.pdf"
)



df_midexp <- data %>% filter(experience == "Moderately Experienced")
plot_time(
  data = df_midexp,
  group_col = "type",
  value_col = "time",
  title = "",
  save_path = "./result/Moderately Experienced Time.pdf"
)


df_highexp <- data %>% filter(experience == "Highly Experienced")
plot_time(
  data = df_highexp,
  group_col = "type",
  value_col = "time",
  title = "",
  save_path = "./result/Highly Experienced Time.pdf"
)


df_lowlevel <- data %>% filter(level == "Junior Physician")
plot_time(
  data = df_lowlevel,
  group_col = "type",
  value_col = "time",
  title = "",
  save_path = "./result/Junior Physician Time.pdf"
)


df_highlevel <- data %>% filter(level == "Senior Physician")
plot_time(
  data = df_highlevel,
  group_col = "type",
  value_col = "time",
  title = "",
  save_path = "./result/Senior Physician Time.pdf"
)



df_urology <- data %>% filter(department == "Urology")
plot_time(
  data = df_urology,
  group_col = "type",
  value_col = "time",
  title = "",
  save_path = "./result/Urology Time.pdf"
)

df_radio <- data %>% filter(department == "Radiology")
plot_time(
  data = df_radio,
  group_col = "type",
  value_col = "time",
  title = "",
  save_path = "./result/Radiology Time.pdf"
)


plot_time(
  data = data,
  group_col = "type",
  value_col = "time",
  title = "",
  save_path = "./result/Overall Time.pdf"
)
