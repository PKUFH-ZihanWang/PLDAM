data_sample <- read.csv(file.choose())
pred_MRICT <- predict(gbm_model, newdata = early_model, type = "raw")
pred_PLRS <- ifelse(early_all$PL_RS >= 9, 1, 0)
pred_PLRS_prob <- early_all$PL_RS/13

table(early_all$PL_RS,early_all$diagnosis_result)


# --- ROC 曲线 ---
roc_MRICT <- roc(early_model$diagnosis_result, pred_MRICT)
roc_PLRS  <- roc(early_model$diagnosis_result, pred_PLRS_prob)


ci_PLD  <- ci.auc(roc_MRICT)
ci_PLRS <- ci.auc(roc_PLRS)


# --- Best threshold: Youden's J ---
coords_PLD <- coords(roc_MRICT, "best", ret = c("threshold","sensitivity","specificity"), best.method="youden")[1,]
coords_PLRS  <- coords(roc_PLRS, "best", ret = c("threshold","sensitivity","specificity"), best.method="youden")


# --- 绘制 ROC ---
plot(roc_MRICT, col = "red",lwd = 3, main = "ROC Comparison: PLRS vs PLDAM",cex.main = 1.3, cex.axis = 1.1, cex.lab = 1.2)
plot(roc_PLRS, col = "darkgreen", lwd = 3, add = TRUE)

# --- legend ---
legend("bottomright", legend = c(
  sprintf("PLDAM     AUC: %.2f (95%% CI: %.2f - %.2f)  Sens: %.2f  Spec: %.2f",
          auc(roc_MRICT), ci_PLD[1], ci_PLD[3], coords_PLD["sensitivity"], coords_PLD["specificity"]),
  sprintf("PL-RS     AUC: %.2f (95%% CI: %.2f - %.2f)  Sens: %.2f  Spec: %.2f",
          auc(roc_PLRS), ci_PLRS[1], ci_PLRS[3], coords_PLRS["sensitivity"], coords_PLRS["specificity"])
), col = c("red","darkgreen"), lwd = 3,  cex = 0.85)







true_labels <- early_model$diagnosis_result


pred_MRICT_bin <- ifelse(pred_MRICT >= 0.5, 1, 0)

pred_PLRS_bin <- ifelse(early_all$PL_RS >= 9, 1, 0)

# --- 混淆矩阵 ---
cm_MRICT <- confusionMatrix(factor(pred_MRICT_bin), factor(true_labels))
cm_PLRS  <- confusionMatrix(factor(pred_PLRS_bin),  factor(true_labels))

library(grid)     
library(gridExtra)

plot_cm <- function(cm, model_name) {
  cm_data <- as.data.frame(as.table(cm$table))
  colnames(cm_data) <- c("Actual", "Predicted", "Freq")
  
  cm_data$Correct <- cm_data$Actual == cm_data$Predicted
  
  ggplot(cm_data, aes(x = Predicted, y = Actual, fill = Correct)) +
    geom_tile(color = "grey50") +
    scale_fill_manual(values = c("TRUE" = "darkseagreen3", "FALSE" = "white")) +
    geom_text(aes(label = Freq), color = "black", size = 6) +
    ggtitle(model_name) +
    xlab("Predicted") + ylab("Actual") +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.text  = element_text(size = 16),
      axis.title = element_text(size = 18),
      legend.position = "none"
    )
}

# --- 分别绘制 ---
plot_cm(cm_MRICT, "PLDAM (MRI+CT)")
plot_cm(cm_PLRS,  "PL-RS (>=9 cutoff)")






############### 校准曲线

plot_calibration_v2 <- function(result, title) {
  colors <- c("red", "darkgreen")
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Mean Predicted Probability",
       ylab = "Observed Probability",
       main = title, cex.main = 1.3, cex.lab = 1.2, font.main = 2)

  abline(0, 1, lty = 2, col = "gray40", lwd = 2)
  
  legend_list <- vector("list", length(result))
  
  for (i in seq_along(result)) {
    model_result <- result[[i]]
    probs   <- model_result$probs
    actuals <- model_result$actuals

    if (max(probs, na.rm = TRUE) > 1) {
      probs <- probs / max(probs, na.rm = TRUE)
    }
    
    prob_bins <- cut(probs, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)
    calibration_data <- aggregate(actuals ~ prob_bins, FUN = mean)
    mean_predicted   <- aggregate(probs ~ prob_bins, FUN = mean)$probs
    
    smoothed <- loess(calibration_data$actuals ~ mean_predicted, span = 1)
    smoothed_values <- predict(smoothed, newdata = mean_predicted)
    
    color <- colors[(i - 1) %% length(colors) + 1]
    lines(mean_predicted, smoothed_values,
          col = color, lwd = 3, type = "o", pch = 19, cex = 0.7)
    
    legend_list[[i]] <- paste(model_result$model_name)
  }
  
  legend("bottomright", legend = unlist(legend_list),
         col = colors[1:length(result)], lwd = 3, pch = 19, cex = 0.8)
}

results_calib <- list(
  list(model_name="PLDAM", probs=pred_MRICT, actuals=true_labels),
  list(model_name="PL-RS score", probs=early_all$PL_RS, actuals=true_labels) # 原始分数也可以
)

plot_calibration_v2(results_calib, "Calibration Curves")






library(rmda)

dca_data <- data.frame(
  outcome   = early_model$diagnosis_result,
  PLDAM     = pred_MRICT,
  PL_RS     = pred_PLRS_prob 
)


pldam <- decision_curve(
  outcome ~ PLDAM,
  data = dca_data,
  family = binomial(link = 'logit'),
  thresholds = seq(0, 1, by = 0.01),
  confidence.intervals = 0.95,
  study.design = "cohort"
)
pl_rs <- decision_curve(
  outcome ~ PL_RS,
  data = dca_data,
  family = binomial(link = 'logit'),
  thresholds = seq(0, 1, by = 0.01),
  confidence.intervals = 0.95,
  study.design = "cohort"
)

par(cex.lab = 1.6,   
    cex.axis = 1.3,  
    cex.main = 1.6)  


rmda::plot_decision_curve(
  list(pldam, pl_rs),
  curve.names = c( "PLDAM", "PL_RS"),
  col = c("red", "darkgreen"),  
  lwd = 2, lty = 1,
  legend.position = "none",
  xlab = "Threshold Probability",
  ylab = "Net Benefit",
  confidence.intervals = FALSE,
  standardize = FALSE
)

legend("bottomright",
       legend = c("PLDAM", "PL_RS", "All", "None"),
       col = c( "red", "darkgreen", "black", "grey"),
       lwd = 2, lty = 1,
       cex = 0.6
)

