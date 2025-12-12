library(dplyr)
library(caret)
library(pROC)
library(grid)
library(gridExtra)
library(rmda)
library(openxlsx) 

# -----------------------------
# 1. Prepare single-modal dataset and compute derived features
# -----------------------------
CT <- read.csv("./data/CT_only.csv")
MRI <- read.csv("./data/MRI_only.csv")
subset_PLDAM <- read.xlsx("./data/multi.xlsx",sheet=1)
subset_CT_test <- read.xlsx("./data/multi.xlsx",sheet=2)
subset_MRI_test <- read.xlsx("./data/multi.xlsx",sheet=3)

subset_PLRS <- subset_PLDAM_PLRS %>%
  mutate(
    PL_RS = 0 +
      2 * (pelvic_fat_to_volume_ratio >= 0.35) +
      2 * (Bladder_AR >= 2) +
      1 * (bladder_surface_area >= 30000) +
      1 * (bladder_volume >= 250000) +
      1 * (ureter_volume >= 6000) +
      1 * (ureter_surface_area >= 5000) +
      2 * (max_rectum_diameter <= 41) +
      2 * (max_ureter_diameter >= 7) +
      1 * (pelvic_fat_volume >= 900000)
  )

# -----------------------------
# 2. Split CT and MRI datasets
# -----------------------------


feature_names <- setdiff(names(train_data_subset), "diagnosis_result")
CT <- CT[, c("diagnosis_result", feature_names)]
MRI <- MRI[, c("diagnosis_result", feature_names)]

set.seed(115)
trainIndex_CT <- createDataPartition(CT$diagnosis_result, p = 0.1, list = FALSE)
CT_train <- CT[trainIndex_CT, ]
CT_test  <- CT[-trainIndex_CT, ]

set.seed(116)
trainIndex_MRI <- createDataPartition(MRI$diagnosis_result, p = 0.1, list = FALSE)
MRI_train <- MRI[trainIndex_MRI, ]
MRI_test  <- MRI[-trainIndex_MRI, ]

# -----------------------------
# 3. GBM training
# -----------------------------
grid <- expand.grid(
  n.trees = seq(10, 100, by = 10),
  interaction.depth = 3,
  shrinkage = 0.1,
  n.minobsinnode = 3
)

set.seed(2)
gbm_CT <- train(
  diagnosis_result ~ .,
  data = CT_train,
  method = "gbm",
  trControl = train_control,
  tuneGrid = grid,
  verbose = FALSE
)

set.seed(136)
gbm_MRI <- train(
  diagnosis_result ~ .,
  data = MRI_train,
  method = "gbm",
  trControl = train_control,
  tuneGrid = grid,
  verbose = FALSE
)

# -----------------------------
# 4. Predictions and ROC analysis
# -----------------------------
pred_CT  <- predict(gbm_CT, newdata = subset_CT_test, type = "raw")
pred_MRI <- predict(gbm_MRI, newdata = subset_MRI_test, type = "raw")
pred_MRICT <- predict(gbm_model, newdata = subset_PLDAM, type = "raw")
pred_PLRS <- ifelse(subset_PLRS$PL_RS >= 9, 1, 0)
pred_PLRS_prob <- subset_PLRS$PL_RS / 13

roc_CT  <- roc(subset_CT_test$diagnosis_result, pred_CT)
roc_MRI <- roc(subset_MRI_test$diagnosis_result, pred_MRI)
roc_PLD  <- roc(subset_PLDAM$diagnosis_result, pred_MRICT)
roc_PLRS <- roc(subset_PLRS$diagnosis_result, pred_PLRS)

ci_CT   <- ci.auc(roc_CT)
ci_MRI  <- ci.auc(roc_MRI)
ci_PLD  <- ci.auc(roc_PLD)
ci_PLRS <- ci.auc(roc_PLRS)

coords_CT  <- coords(roc_CT, "best", ret = c("threshold","sensitivity","specificity"), best.method="youden")
coords_MRI <- coords(roc_MRI, "best", ret = c("threshold","sensitivity","specificity"), best.method="youden")
coords_PLD <- coords(roc_PLD, "best", ret = c("threshold","sensitivity","specificity"), best.method="youden")
coords_PLRS  <- coords(roc_PLRS, "best", ret = c("threshold","sensitivity","specificity"), best.method="youden")

# -----------------------------
# 5. Confusion matrices plotting
# -----------------------------
plot_cm <- function(cm, model_name) {
  if ("table" %in% names(cm)) cm <- cm$table
  cm <- t(cm)[, rev(colnames(cm))]
  cm_data <- as.data.frame(as.table(cm))
  colnames(cm_data) <- c("Actual", "Predicted", "Freq")
  cm_data$Correct <- cm_data$Actual == cm_data$Predicted
  
  p <- ggplot(cm_data, aes(x = Predicted, y = Actual, fill = Correct)) +
    geom_tile(color = "grey50") +
    scale_fill_manual(values = c("TRUE" = "darkseagreen3", "FALSE" = "white")) +
    geom_text(aes(label = Freq), color = "black", size = 8) +
    ggtitle(paste("Model:", model_name)) +
    xlab("Predicted") + ylab("Actual") +
    theme_minimal(base_size = 20) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.text  = element_text(size = 16),
      axis.title = element_text(size = 18),
      legend.position = "none"
    )
  return(p)
}

true_labels <- single$diagnosis_result
best_thresh_CT    <- coords(roc_CT,    "best", ret="threshold", best.method="youden")
best_thresh_MRI   <- coords(roc_MRI,   "best", ret="threshold", best.method="youden")
best_thresh_MRICT <- coords(roc_PLD,   "best", ret="threshold", best.method="youden")

pred_CT_bin    <- ifelse(pred_CT    >= best_thresh_CT$threshold,    1, 0)
pred_MRI_bin   <- ifelse(pred_MRI   >= best_thresh_MRI$threshold,   1, 0)
pred_MRICT_bin <- ifelse(pred_MRICT >= best_thresh_MRICT$threshold, 1, 0)
pred_PLRS_bin  <- ifelse(subset_single$PL_RS >= 9, 1, 0)

cm_CT    <- confusionMatrix(factor(pred_CT_bin),    factor(true_labels))
cm_MRI   <- confusionMatrix(factor(pred_MRI_bin),   factor(true_labels))
cm_MRICT <- confusionMatrix(factor(pred_MRICT_bin), factor(true_labels))
cm_PLRS  <- confusionMatrix(factor(pred_PLRS_bin),  factor(true_labels))

p1 <- plot_cm(cm_CT, "CT only")
p2 <- plot_cm(cm_MRI, "MRI only")
p3 <- plot_cm(cm_MRICT, "PLDAM (MRI+CT)")
p4 <- plot_cm(cm_PLRS, "PL-RS (>=9 cutoff)")

grid.arrange(p1, p2, p3, p4, ncol = 2,
             top = textGrob("Confusion Matrices", gp = gpar(fontsize = 18, fontface = "bold")))

# -----------------------------
# 6. Calibration curves
# -----------------------------
plot_calibration_multi <- function(results, title) {
  colors <- c("#1F77B4", "#FF7F0E", "red", "darkgreen")
  plot(NULL, xlim = c(0,1), ylim = c(0,1),
       xlab = "Predicted probability", ylab = "Observed probability",
       main = title, cex.main = 1.3, cex.lab = 1.2, font.main = 2)
  abline(0,1, lty=2, col="gray40", lwd=2)
  
  legend_labels <- vector("list", length(results))
  for (i in seq_along(results)) {
    model_result <- results[[i]]
    probs <- model_result$probs
    actuals <- model_result$actuals
    if (max(probs, na.rm = TRUE) > 1) probs <- probs / max(probs, na.rm = TRUE)
    
    qtls <- unique(quantile(probs, probs=seq(0,1,0.1), na.rm=TRUE))
    if (length(qtls)<3) next
    bins <- cut(probs, breaks=qtls, include.lowest=TRUE, labels=FALSE)
    mean_pred <- tapply(probs, bins, mean, na.rm=TRUE)
    mean_obs  <- tapply(actuals, bins, mean, na.rm=TRUE)
    n_bin     <- tapply(actuals, bins, length)
    
    se <- sqrt(mean_obs * (1 - mean_obs) / n_bin)
    lower <- pmax(mean_obs - 1.96*se, 0)
    upper <- pmin(mean_obs + 1.96*se, 1)
    
    arrows(mean_pred, lower, mean_pred, upper, length=0.05, angle=90, code=3, col=colors[i], lwd=1.5)
    points(mean_pred, mean_obs, col="black", bg=colors[i], pch=21, cex=1.5, lwd=1.2)
    smoothed <- loess(actuals ~ probs, span=0.75)
    lines(seq(0,1,0.01), predict(smoothed, newdata=seq(0,1,0.01)), col=colors[i], lwd=2.5)
    
    legend_labels[[i]] <- model_result$model_name
  }
  
  legend("bottomright", legend=unlist(legend_labels),
         col=colors, lwd=2.5, pch=16, pt.bg=colors, cex=0.9)
}

results_calib <- list(
  list(model_name="CT only", probs=pred_CT, actuals=true_labels),
  list(model_name="MRI only", probs=pred_MRI, actuals=true_labels),
  list(model_name="PLDAM (MRI+CT)", probs=pred_MRICT, actuals=true_labels),
  list(model_name="PL-RS score", probs=subset_single$PL_RS, actuals=true_labels)
)
plot_calibration_multi(results_calib, "Calibration Curves")

# -----------------------------
# 7. Decision curve analysis (DCA)
# -----------------------------
dca_data <- data.frame(
  outcome = single$diagnosis_result,
  CT_only = pred_CT,
  MRI_only = pred_MRI,
  PLDAM = pred_MRICT,
  PL_RS = pred_PLRS_prob
)

ct_only <- decision_curve(outcome ~ CT_only, data=dca_data, family=binomial(link='logit'), thresholds=seq(0,1,0.01), confidence.intervals=0.95, study.design="cohort")
mri_only <- decision_curve(outcome ~ MRI_only, data=dca_data, family=binomial(link='logit'), thresholds=seq(0,1,0.01), confidence.intervals=0.95, study.design="cohort")
pldam <- decision_curve(outcome ~ PLDAM, data=dca_data, family=binomial(link='logit'), thresholds=seq(0,1,0.01), confidence.intervals=0.95, study.design="cohort")
pl_rs <- decision_curve(outcome ~ PL_RS, data=dca_data, family=binomial(link='logit'), thresholds=seq(0,1,0.01), confidence.intervals=0.95, study.design="cohort")

par(cex.lab=1.6, cex.axis=1.3, cex.main=1.6)
plot_decision_curve(list(ct_only, mri_only, pldam, pl_rs),
                    curve.names=c("CT_only","MRI_only","PLDAM","PL_RS"),
                    col=c("#1F77B4","#FF7F0E","red","darkgreen"),
                    lwd=2, lty=1, legend.position="none",
                    xlab="Threshold Probability", ylab="Net Benefit",
                    confidence.intervals=FALSE, standardize=FALSE)

legend("bottomright",
       legend=c("CT_only","MRI_only","PLDAM","PL_RS","All","None"),
       col=c("#1F77B4","#FF7F0E","red","darkgreen","black","grey"),
       lwd=2, lty=1, cex=0.80)

# -----------------------------
# 8. Bootstrap metrics calculation
# -----------------------------
bootstrap_metrics <- function(probs, actuals, model_name, n_boot=2000, seed=123) {
  actuals <- factor(as.character(actuals), levels=c("0","1"))
  probs <- as.numeric(probs)
  auc_vec <- sens_vec <- spec_vec <- ppv_vec <- npv_vec <- f1_vec <- acc_vec <- thresh_vec <- numeric()
  n <- length(actuals)
  
  set.seed(seed)
  for (b in seq_len(n_boot)) {
    idx <- sample.int(n, replace=TRUE)
    probs_b <- probs[idx]
    actuals_b <- actuals[idx]
    if (length(unique(actuals_b))<2) next
    roc_obj <- try(roc(response=actuals_b, predictor=probs_b, quiet=TRUE, levels=c("0","1")), silent=TRUE)
    if (inherits(roc_obj,"try-error")) next
    
    youden <- roc_obj$sensitivities + roc_obj$specificities - 1
    threshold <- roc_obj$thresholds[which.max(youden)]
    pred_class <- factor(ifelse(probs_b >= threshold,"1","0"), levels=c("0","1"))
    if (length(unique(pred_class))<2) next
    cm <- confusionMatrix(pred_class, actuals_b, positive="1")
    
    auc_vec    <- c(auc_vec, as.numeric(auc(roc_obj)))
    sens_vec   <- c(sens_vec, cm$byClass["Sensitivity"])
    spec_vec   <- c(spec_vec, cm$byClass["Specificity"])
    ppv_vec    <- c(ppv_vec, cm$byClass["Pos Pred Value"])
    npv_vec    <- c(npv_vec, cm$byClass["Neg Pred Value"])
    f1_vec     <- c(f1_vec, cm$byClass["F1"])
    acc_vec    <- c(acc_vec, cm$overall["Accuracy"])
    thresh_vec <- c(thresh_vec, threshold)
  }
  
  data.frame(
    model_name = model_name,
    auc = sprintf("%.2f(%.2f,%.2f)", mean(auc_vec, na.rm=TRUE), quantile(auc_vec,0.025,na.rm=TRUE), quantile(auc_vec,0.975,na.rm=TRUE)),
    sensitivity = sprintf("%.2f(%.2f,%.2f)", mean(sens_vec, na.rm=TRUE), quantile(sens_vec,0.025,na.rm=TRUE), quantile(sens_vec,0.975,na.rm=TRUE)),
    specificity = sprintf("%.2f(%.2f,%.2f)", mean(spec_vec, na.rm=TRUE), quantile(spec_vec,0.025,na.rm=TRUE), quantile(spec_vec,0.975,na.rm=TRUE)),
    ppv = sprintf("%.2f(%.2f,%.2f)", mean(ppv_vec, na.rm=TRUE), quantile(ppv_vec,0.025,na.rm=TRUE), quantile(ppv_vec,0.975,na.rm=TRUE)),
    npv = sprintf("%.2f(%.2f,%.2f)", mean(npv_vec, na.rm=TRUE), quantile(npv_vec,0.025,na.rm=TRUE), quantile(npv_vec,0.975,na.rm=TRUE)),
    f1_score = sprintf("%.2f(%.2f,%.2f)", mean(f1_vec, na.rm=TRUE), quantile(f1_vec,0.025,na.rm=TRUE), quantile(f1_vec,0.975,na.rm=TRUE)),
    accuracy = sprintf("%.2f(%.2f,%.2f)", mean(acc_vec, na.rm=TRUE), quantile(acc_vec,0.025,na.rm=TRUE), quantile(acc_vec,0.975,na.rm=TRUE)),
    best_threshold = median(thresh_vec, na.rm=TRUE),
    stringsAsFactors = FALSE
  )
}

res_CT <- bootstrap_metrics(pred_CT, single$diagnosis_result, "CT_only", n_boot=2000)
res_MRI <- bootstrap_metrics(pred_MRI, single$diagnosis_result, "MRI_only", n_boot=2000)
res_PLDAM <- bootstrap_metrics(pred_MRICT, single$diagnosis_result, "PLDAM", n_boot=2000)
final_results <- do.call(rbind, list(res_CT, res_MRI, res_PLDAM))

write.csv(final_results, "calibration_metrics_bootstrap.csv", row.names=FALSE)
print(final_results)
