library(ggplot2)
library(ggnewscale)
library(dplyr)

hot <- read.csv(file.choose())

plot_gbm_heatmap_v2 <- function(df, title = "GBM Probability Heatmap by Dataset") {
  # --- 数据预处理 ---
  df <- df %>%
    mutate(
      Group = factor(Group, levels = c("Patient", "Non-Patient")),
      Cohort = factor(Cohort, levels = unique(Cohort)),
      level = factor(level, levels = paste("PL", c("I", "II", "III", "IV")))
    ) %>%
    group_by(Cohort) %>%
    mutate(Sample_order = row_number()) %>%
    ungroup()
  
  # --- 分层数据准备 ---
  df_score <- df %>% mutate(y = 1)
  df_group <- df %>% mutate(y = 2)
  df_level <- df %>% mutate(y = 3)
  
  # --- 绘图 ---
  p <- ggplot() +
    # --- 第一层：Score ---
    geom_tile(data = df_score,
              aes(x = Sample_order, y = y, fill = Score),
              height = 0.9) +
    scale_fill_gradientn(
      colors = c("#0E76BD", "white", "#EE2623"),
      limits = c(min(df$Score, na.rm = TRUE),
                 max(df$Score, na.rm = TRUE)),
      name = "Predicted\nScore"
    ) +
    new_scale_fill() +
    
    # --- 第二层：Group ---
    geom_tile(data = df_group,
              aes(x = Sample_order, y = y, fill = Group),
              height = 0.25) +
    scale_fill_manual(
      values = c("Patient" = "#ED553E", "Non-Patient" = "#44BED4"),
      name = "Group"
    ) +
    new_scale_fill() +
    
    # --- 第三层：Level（病理分级） ---
    geom_tile(data = df_level,
              aes(x = Sample_order, y = y, fill = level),
              height = 0.25) +
    scale_fill_manual(
      values = c(
        "PL I" = "#c6dbef",
        "PL II" = "#9ecae1",
        "PL III" = "#6baed6",
        "PL IV" = "#2171b5"
      ),
      name = "Pathological\nLevel"
    ) +
    
    # --- 分面 & 坐标 ---
    facet_wrap(~ Cohort, ncol = 1, scales = "free_x") +
    labs(title = title) +
    scale_x_continuous(expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    
    # --- 主题优化 ---
    theme_minimal(base_size = 13) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      panel.spacing = unit(0.2, "lines"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 15)
    )
  
  print(p)
  invisible(p)
}

plot_gbm_heatmap_v2(hot, "")


library(ggplot2)
library(pROC)
library(dplyr)

# 确保 Group 是因子并转为二分类 (1 = 患者, 0 = 非患者)
hot <- hot %>%
  mutate(
    label = ifelse(Group == "Patient", 1, 0)
  )




library(ggplot2)
library(pROC)
library(dplyr)

# 绘制混淆矩阵和ROC曲线，并保存为PDF
save_confusion_and_roc <- function(data1,data2, 
                                   output_dir_confmat, 
                                   output_dir_roc, 
                                   dataset_name = "dataset") {
  # 创建文件夹（如果不存在）
  if (!dir.exists(output_dir_confmat)) dir.create(output_dir_confmat, recursive = TRUE)
  if (!dir.exists(output_dir_roc)) dir.create(output_dir_roc, recursive = TRUE)
  
  # 确保数据中包含必要列
  if (!all(c("Score", "label") %in% colnames(data1))) {
    stop("数据中必须包含列 'Score'（模型预测概率） 和 'label'（真实标签）")
  }
  
  # 确保数据中包含必要列
  if (!all(c("Score", "label") %in% colnames(data2))) {
    stop("数据中必须包含列 'Score'（模型预测概率） 和 'label'（真实标签）")
  }
  
  
  # 将label转换为factor
  # data <- data %>% mutate(label = as.factor(label))
  
  # -------------------
  # ROC曲线
  # -------------------
  roc_MRICT <- roc(response = data1$label, predictor = data1$Score, quiet = TRUE)
  roc_PLRS <- roc(response = data2$label, predictor = data2$Score, quiet = TRUE)
  
  ci_PLD  <- ci.auc(roc_MRICT)
  ci_PLRS <- ci.auc(roc_PLRS)

  coords_PLD <- coords(roc_MRICT, "best", ret = c("threshold","sensitivity","specificity"), best.method="youden")[1,]
  coords_PLRS  <- coords(roc_PLRS, "best", ret = c("threshold","sensitivity","specificity"), best.method="youden")
  
  roc_file <- file.path(output_dir_roc, dataset_name, "ROC.pdf")
  roc_dir <- dirname(roc_file)
  if (!dir.exists(roc_dir)) dir.create(roc_dir, recursive = TRUE)
  pdf(roc_file, width = 6, height = 6)  # 打开 PDF 设备
  
  plot(roc_MRICT, col = "red",lwd = 3, main = "",cex.main = 1.3, cex.axis = 1.1, cex.lab = 1.2)
  plot(roc_PLRS, col = "darkgreen", lwd = 3, add = TRUE)

  # --- legend ---
  legend("bottomright", legend = c(
	  sprintf("PL_ABH     AUC: %.2f (95%% CI: %.2f - %.2f)  Sens: %.2f  Spec: %.2f",
			  auc(roc_MRICT), ci_PLD[1], ci_PLD[3], coords_PLD["sensitivity"], coords_PLD["specificity"]),
	  sprintf("PL_Comp     AUC: %.2f (95%% CI: %.2f - %.2f)  Sens: %.2f  Spec: %.2f",
			  auc(roc_PLRS), ci_PLRS[1], ci_PLRS[3], coords_PLRS["sensitivity"], coords_PLRS["specificity"])
  ), col = c("red","darkgreen"), lwd = 3,  cex = 0.65)

  dev.off()  # 关闭 PDF 设备

  message("✅ ROC 曲线已保存为 PDF：", roc_file)

  
  # -------------------
  # 2️ 混淆矩阵
  # -------------------
  pred_MRICT_bin <- ifelse(data1$Score >= coords_PLD$threshold, 1, 0)
  pred_PLRS_bin <- ifelse(data2$Score >= coords_PLRS$threshold, 1, 0)
  cm_MRICT <- confusionMatrix(factor(pred_MRICT_bin), factor(data1$label))
  cm_PLRS  <- confusionMatrix(factor(pred_PLRS_bin),  factor(data2$label))
  library(grid)      # 👈 必须加
  library(gridExtra)

plot_cm <- function(cm, model_name) {
  # 提取 confusionMatrix 表格
  if ("table" %in% names(cm)) cm <- cm$table
  
  # 提取四个值
  TP <- cm[2,2]
  FN <- cm[1,2]
  FP <- cm[2,1]
  TN <- cm[1,1]
  print(cm)
  print(FP)
  
  # 构造绘图矩阵
  # y方向(Actual): 0 -> 1
  # x方向(Predicted): 1 -> 0
  cm_mat <- matrix(
    c(FP, TN,  # Actual = 0
      TP, FN), # Actual = 1
    nrow = 2, byrow = TRUE,
    dimnames = list(Actual = c("0", "1"), Predicted = c("1", "0"))
  )
  
  # 转换为数据框
  cm_data <- as.data.frame(as.table(cm_mat))
  colnames(cm_data) <- c("Actual", "Predicted", "Freq")
  
  # 标记反斜对角：TP + TN
  cm_data$Highlight <- FALSE
  cm_data$Highlight[cm_data$Actual == "0" & cm_data$Predicted == "0"] <- TRUE  # TN
  cm_data$Highlight[cm_data$Actual == "1" & cm_data$Predicted == "1"] <- TRUE  # TP
  
  # 绘图
  ggplot(cm_data, aes(x = Predicted, y = Actual, fill = Highlight)) +
    geom_tile(color = "grey50") +
    geom_text(aes(label = Freq), color = "black", size = 8) +
    scale_fill_manual(values = c("TRUE" = "darkseagreen3", "FALSE" = "white")) +
    ggtitle("") +
    xlab("Predicted") + ylab("Actual") +
    theme_minimal(base_size = 20) +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.text  = element_text(size = 16),
      axis.title = element_text(size = 18),
      legend.position = "none"
    )
}



  # --- 分别绘制 ---
  p1 <- plot_cm(cm_MRICT, "PL_ABH)")
  p2 <- plot_cm(cm_PLRS,  "PL_Comp")
  

  cm_file1 <- file.path(output_dir_confmat, dataset_name,paste0(dataset_name, "_ABH.pdf"))
  cm_file2 <- file.path(output_dir_confmat, dataset_name,paste0(dataset_name, "_Comp.pdf"))
  cm_dir <- dirname(cm_file1)
  if (!dir.exists(cm_dir)) dir.create(cm_dir, recursive = TRUE)
  
  ggsave(cm_file1, plot = p1, width = 6, height = 6)
  ggsave(cm_file2, plot = p2, width = 6, height = 6)
  
  message("✅ 图像已保存：")
  message("ROC曲线：", roc_file)
  message("混淆矩阵：", cm_file1)
  message("混淆矩阵：", cm_file2)
}



##### train
df_train <- hot %>% filter(Cohort == "Train")
df_train_controls <- df_train %>% filter(level == "Controls")
df_train_I <- df_train %>% filter(level == "PL I")
df_train_II <- df_train %>% filter(level == "PL II")
df_train_III <- df_train %>% filter(level == "PL III")
df_train_IV <- df_train %>% filter(level == "PL IV")

df_train_controls_I <- bind_rows(
  df_train_controls %>% mutate(dataset = "Controls"),
  df_train_I %>% mutate(dataset = "PL I")
)
df_train_controls_II <- bind_rows(
  df_train_controls %>% mutate(dataset = "Controls"),
  df_train_II %>% mutate(dataset = "PL II")
)
df_train_controls_II <- bind_rows(
  df_train_controls %>% mutate(dataset = "Controls"),
  df_train_III %>% mutate(dataset = "PL III")
)
df_train_controls_IV <- bind_rows(
  df_train_controls %>% mutate(dataset = "Controls"),
  df_train_IV %>% mutate(dataset = "PL IV")
)
save_confusion_and_roc(
  data1 = df_train_controls_IV,
  data2 = df_train_controls_II,
  output_dir_confmat = "results",
  output_dir_roc = "results",
  dataset_name = "train"
)



##### test
df_test <- hot %>% filter(Cohort == "Test")
df_test_controls <- df_test %>% filter(level == "Controls")
df_test_II <- df_test %>% 
  filter(level %in% c("PL IV", "PL III", "PL II"))
df_test_IV <- df_test %>% filter(level == "PL IV")


df_test_controls_II <- bind_rows(
  df_test_controls %>% mutate(dataset = "Controls"),
  df_test_II %>% mutate(dataset = "PL II–IV")
)

df_test_controls_IV <- bind_rows(
  df_test_controls %>% mutate(dataset = "Controls"),
  df_test_IV %>% mutate(dataset = "PL IV")
)


save_confusion_and_roc(
  data1 = df_test_controls_IV,
  data2 = df_test_controls_II,
  output_dir_confmat = "results",
  output_dir_roc = "results",
  dataset_name = "test"
)




##### Validation
df_validation <- hot %>% filter(Cohort == "Validation")
df_validation_controls <- df_validation %>% filter(level == "Controls")
df_validation_II <- df_validation %>% 
  filter(level %in% c("PL IV", "PL III", "PL II"))
df_validation_IV <- df_validation %>% filter(level == "PL IV")


df_validation_controls_II <- bind_rows(
  df_validation_controls %>% mutate(dataset = "Controls"),
  df_validation_II %>% mutate(dataset = "PL II–IV")
)

df_validation_controls_IV <- bind_rows(
  df_validation_controls %>% mutate(dataset = "Controls"),
  df_validation_IV %>% mutate(dataset = "PL IV")
)


save_confusion_and_roc(
  data1 = df_validation_controls_IV,
  data2 = df_validation_controls_II,
  output_dir_confmat = "results",
  output_dir_roc = "results",
  dataset_name = "validation"
)






##### External
df_external <- hot %>% filter(Cohort == "External")
df_external_controls <- df_external %>% filter(level == "Controls")
df_external_II <- df_external %>% 
  filter(level %in% c("PL IV", "PL III", "PL II"))
df_external_IV <- df_external %>% filter(level == "PL IV")


df_external_controls_II <- bind_rows(
  df_external_controls %>% mutate(dataset = "Controls"),
  df_external_II %>% mutate(dataset = "PL II–IV")
)

df_external_controls_IV <- bind_rows(
  df_external_controls %>% mutate(dataset = "Controls"),
  df_external_IV %>% mutate(dataset = "PL IV")
)


save_confusion_and_roc(
  data1 = df_external_controls_IV,
  data2 = df_external_controls_II,
  output_dir_confmat = "results",
  output_dir_roc = "results",
  dataset_name = "external"
)
