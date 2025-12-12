library(caret)
library(randomForest)
library(e1071)
library(rpart)
library(gbm)
library(xgboost)
library(naivebayes)
library(pROC)
library(ggsci)

mlp_data_train <- train_data_subset
mlp_data_train$diagnosis_result <- as.factor(mlp_data_train$diagnosis_result)
# Create a trainControl object for 10-fold CV
train_control <- trainControl(method = "cv", number = 10)

# 1. Random Forest with 10-fold CV
tune_grid <- expand.grid(mtry = 7)
rf_model <- train(diagnosis_result ~ ., 
                  data = train_data_subset, 
                  method = "rf",
				  tuneGrid = tune_grid, 
                  trControl = train_control, 
                  ntree = 20)

# 2. Logistic Regression with 10-fold CV
log_model <- train(diagnosis_result ~ ., 
                   data = train_data_subset, 
                   method = "glm", 
                   family = binomial,
                   trControl = train_control)

# 3. SVM with 10-fold CV
svm_model <- train(diagnosis_result ~ ., 
                   data = train_data_subset, 
                   method = "svmRadial", 
                   trControl = train_control)

# 4. K-Nearest Neighbors (KNN) with 10-fold CV
knn_model <- train(diagnosis_result ~ ., 
                   data = train_data_subset, 
                   method = "knn", 
                   trControl = train_control)

# 5. Naive Bayes with 10-fold CV
nb_model <- train(diagnosis_result ~ ., 
                  data = mlp_data_train, 
                  method = "naive_bayes", 
                  trControl = train_control)

# 6. Decision Tree with 10-fold CV
tree_model <- train(diagnosis_result ~ ., 
                    data = train_data_subset, 
                    method = "rpart", 
                    trControl = train_control)

# 7. Gradient Boosting Machine (GBM) with 10-fold CV
gbm_model <- train(diagnosis_result ~ ., 
                   data = train_data_subset, 
                   method = "gbm", 
                   trControl = train_control,
                   verbose = FALSE)

# 8. Extreme Gradient Boosting (XGBoost) with manual 10-fold CV
xgb_data <- xgb.DMatrix(data = as.matrix(train_data_subset[-49]), label = train_data_subset$diagnosis_result)
cv_results <- xgb.cv(
  data = xgb_data, 
  max_depth = 3, 
  eta = 0.1, 
  nrounds = 100, 
  objective = "binary:logistic", 
  nfold = 10, 
  verbose = 0,
  early_stopping_rounds = 10 # Optional: stop early if no improvement
)

# Check the best number of boosting rounds from cv_results
best_nrounds <- cv_results$best_iteration

# Train the final XGBoost model using the optimal number of rounds
xgb_model <- xgboost(
  data = xgb_data, 
  max_depth = 5, 
  eta = 0.1, 
  nrounds = 12, 
  objective = "binary:logistic", 
  verbose = 0
)


# 9. Multilayer Perceptron (MLP) with 10-fold CV
mlp_model <- train(
    diagnosis_result ~ .,
    data = train_data_subset,
    method = "mlp",
    preProcess = c("center", "scale"),
    trControl = train_control
)



# Define a unified evaluation function
evaluate_model <- function(model, data, is_prob, model_name) {
  print(model_name)
  
  if (is_prob) {
    probs <- predict(model, newdata = data, type = "raw")
  } else {
    if (class(model)[1] == "xgb.Booster") {
      probs <- predict(model, newdata = as.matrix(data[-49]))
    } else if (class(model)[1] == "svm") {
      probs <- attr(predict(model, newdata = data, probability = TRUE), "probabilities")[,2]
    } else if (model_name == "KNN") {
      probs <- predict(model, newdata = data, type = "raw")
    } else if(model_name == "NaiveBayes"){
      probs <- as.numeric(predict(model, newdata = data, type = "raw"))-1
    } else if(model_name == "DecisionTree"){
      probs <- predict(model, newdata = data)
    } else if(model_name == "MultilayerPerceptron"){
      probs <- as.numeric(predict(model, newdata = data))-1
    } else {
      probs <- predict(model, newdata = data, type = "raw")
    }
  }

  actuals <- data$diagnosis_result
  
  # ROC & AUC
  roc_result <- roc(actuals, probs)
  auc_value <- auc(roc_result)
  auc_ci <- ci(roc_result)

  #  (Youden's J Statistic)
  thresholds <- roc_result[["thresholds"]]
  sensitivities <- roc_result[["sensitivities"]]
  specificities <- roc_result[["specificities"]]
  
  youden_index <- sensitivities + specificities - 1
  best_threshold <- thresholds[which.max(youden_index)]
  
  preds <- ifelse(probs > best_threshold, 1, 0)
  
  cm <- confusionMatrix(factor(preds), factor(actuals))
  
  list(
    roc_result = roc_result,
    auc = auc_value,
    auc_ci = auc_ci,  # AUC 95% CI
    cm = cm,
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"],
    best_threshold = best_threshold,
    model_name = model_name,
    probs = probs,
    actuals = actuals
  )
}


# Evaluate on multiple datasets (train, test, validation, outer)
datasets <- list(train = train_data_subset, test = test_data_subset, validation = validation_data_subset, outer = outer_data_subset)
all_models <- list(
  RandomForest = rf_model,
  Logistic = log_model,
  SVM = svm_model,
  KNN = knn_model,
  NaiveBayes = nb_model,
  DecisionTree = tree_model,
  GBM = gbm_model,
  XGBoost = xgb_model,
  MultilayerPerceptron = mlp_model
)

results <- list()

for (data_name in names(datasets)) {
  dataset <- datasets[[data_name]]
  result_list <- lapply(names(all_models), function(m) {
    model <- all_models[[m]]
    is_prob <- m %in% c("RandomForest")
    evaluate_model(model, dataset, is_prob, m)
  })
  
  results[[data_name]] <- result_list
}


extract_auc_ci <- function(auc_ci_string) {
  auc_ci_values <- sub("95% CI: ([0-9.]+)-([0-9.]+)", "\\1,\\2", auc_ci_string)  
  auc_ci_lower <- as.numeric(auc_ci_values[1])
  auc_ci_upper <- as.numeric(auc_ci_values[3])
  
  if (is.na(auc_ci_lower) || is.na(auc_ci_upper)) {
    stop("Warning: Non-numeric value detected during conversion.")
  }
  
  return(list(lower = signif(auc_ci_lower, digits = 2), upper = signif(auc_ci_upper, digits = 2)))
}





#### ROC curves Function to plot ROC curves
plot_roc <- function(result, title) {
  colors <- c(
	  "#00468BFF", "#AD002AFF", "#42B540FF", "#0099B4FF", 
	  "#925E9FFF", "#FDAF91FF", "#ED0000FF", "#ADB6B6FF", "#1B1919FF"
	)

  plot(result[[1]]$roc_result, col = 1, main = title, lwd = 2, xlab = "1 - Specificity", ylab = "Sensitivity")
  legend_list <- vector("list", length(result))
  
  for (i in seq_along(result)) {
	color <- colors[(i - 1) %% length(colors) + 1]
    if (i > 1) {
      lines(result[[i]]$roc_result, col = color, lwd = 2)
    }
    auc_val <- round(result[[i]]$auc, 2)
    sensitivity <- round(result[[i]]$sensitivity, 2)
    specificity <- round(result[[i]]$specificity, 2)
	auc_ci <- result[[i]]$auc_ci
	auc_extra <- extract_auc_ci(auc_ci)
	upper <- auc_extra$upper
	lower <- auc_extra$lower
    legend_list[[i]] <- paste(result[[i]]$model_name, " - AUC:", auc_val,"(95% CI:", lower, "-", upper, ")"," Sens:", sensitivity, " Spec:", specificity)
  }
  
  legend("bottomright", legend = unlist(legend_list), col = colors[1:length(result)], lwd = 2, cex = 0.7)
}


# Plot ROC curves for train, test, and validation datasets
plot_roc(results$train, "Inner Training Data ROC Curves")
plot_roc(results$test, "Inner Testing Data ROC Curves")
plot_roc(results$validation, "Prospective Validation Data ROC Curves")
plot_roc(results$outer, "External Validation Data ROC Curves")



show_multiple_confusion_matrices <- function(result, dataset_name, save_dir = "confusion_matrices") {
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  
  plots <- lapply(result, function(r) {
    if (is.null(r$cm)) return(NULL)
    
    if ("table" %in% names(r$cm)) {
      cm <- r$cm$table
    } else {
      cm <- r$cm
    }
    
    cm <- t(cm)
    cm <- cm[, rev(colnames(cm))]
    

    cm_data <- as.data.frame(as.table(cm))
    colnames(cm_data) <- c("Actual", "Predicted", "Freq")
    cm_data$Correct <- cm_data$Actual == cm_data$Predicted
    
    # --- plot confusion matrix ---
    p <- ggplot(cm_data, aes(x = Predicted, y = Actual, fill = Correct)) +
      geom_tile(color = "grey50") +
      scale_fill_manual(values = c("TRUE" = "darkseagreen3", "FALSE" = "white")) +
      geom_text(aes(label = Freq), color = "black", size = 8) + 
      ggtitle(paste("Model:", r$model_name)) +
      xlab("Predicted") + ylab("Actual") +
      theme_minimal(base_size = 20) + 
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.text  = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.position = "none"
      )
    
    # save as PDF
    file_name <- file.path(save_dir, paste0(r$model_name, "_", dataset_name, ".pdf"))
    ggsave(file_name, plot = p, width = 6, height = 6) 
    return(p)
  })
  
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) stop("No valid confusion matrices found in result.")
  
  return(invisible(plots))
}



# Plot confusion matrix for train, test, and validation datasets
show_multiple_confusion_matrices(results$train, "Inner Training Data","Inner Training Data")
show_multiple_confusion_matrices(results$test, "Inner Validation Data","Inner Validation Data")
show_multiple_confusion_matrices(results$validation, "Prospective Validation Data", "Prospective Validation Data")
show_multiple_confusion_matrices(results$outer, "External Validation Data", "External Validation Data")




plot_calibration <- function(result, title) {
  colors <- c(
	  "#00468BFF", "#AD002AFF", "#42B540FF", "#0099B4FF", 
	  "#925E9FFF", "#FDAF91FF", "#ED0000FF", "#ADB6B6FF", "#1B1919FF"
	)

  # Plot layout
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "Mean Predicted Probability", ylab = "Observed Probability", main = title)
  abline(0, 1, lty = 2, col = "gray",lwd=3)  # Ideal calibration line (y = x)
  
  legend_list <- vector("list", length(result))
  
  for (i in seq_along(result)) {
    model_result <- result[[i]]
    
    # Get predicted probabilities and actuals
    probs <- model_result$probs
    actuals <- model_result$actuals
    
    # Discretize probabilities into finer bins for smoother curve
    prob_bins <- cut(probs, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)
    calibration_data <- aggregate(actuals ~ prob_bins, FUN = mean)
    mean_predicted <- aggregate(probs ~ prob_bins, FUN = mean)$probs
    
    # Apply loess smoothing for a smoother line
    smoothed <- loess(calibration_data$actuals ~ mean_predicted, span = 1)
    smoothed_values <- predict(smoothed, newdata = mean_predicted)
	
    color <- colors[(i - 1) %% length(colors) + 1]
    # Plot the smoothed calibration line for the current model
    lines(mean_predicted, smoothed_values, col = color, lwd = 3, type = "o", pch = 19,cex=0.7)
    
    legend_list[[i]] <- paste(model_result$model_name)
  }
  
  # Add legend without changing colors
  legend("bottomright", legend = unlist(legend_list), col = colors[1:length(result)], lwd = 3, pch = 19, cex = 0.6)
}
plot_calibration(results$train, "Inner Training Data Calibration Curves")
plot_calibration(results$test, "Inner Validation Data Calibration Curves")
plot_calibration(results$validation, "Prospective Validation Data Calibration Curves")
plot_calibration(results$outer, "External Validation Data Calibration Curves")



library(rmda)
library(ggsci)

plot_dca_curves <- function(result, title = NULL,
                           thresholds = seq(0, 1, by = 0.01)) {
  stopifnot(is.list(result), length(result) > 0)
  
  actuals <- result[[1]]$actuals
  dca_data <- data.frame(outcome = as.numeric(as.character(actuals)))
  
  model_names <- names(result)
  if (is.null(model_names) || any(model_names == "")) {
    model_names <- vapply(result, function(x) {
      if (!is.null(x$model_name)) x$model_name else "model"
    }, character(1))
    model_names <- make.unique(model_names)
  }
  
  for (i in seq_along(result)) {
    dca_data[[model_names[i]]] <- result[[i]]$probs
  }
  dca_data <- dca_data[complete.cases(dca_data), , drop = FALSE]
  
  dca_list <- lapply(model_names, function(m) {
    fml <- reformulate(termlabels = m, response = "outcome")
    rmda::decision_curve(
      fml,
      data = dca_data,
      family = binomial(link = "logit"),
      thresholds = thresholds,
      confidence.intervals = 0.95,
      study.design = "cohort"
    )
  })
  names(dca_list) <- model_names
  
  layout(matrix(c(1, 2), ncol = 2), widths = c(0.82, 0.18))
  
  colors <- c(
	  "#00468BFF", "#AD002AFF", "#42B540FF", "#0099B4FF", 
	  "#925E9FFF", "#FDAF91FF", "#ED0000FF", "#ADB6B6FF", "#1B1919FF"
	)

  par(mar = c(5, 4, 4, 1), cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.6)
  
  rmda::plot_decision_curve(
    dca_list,
    curve.names = model_names,
    col = colors,
    lwd = 3, lty = 1,
    legend.position = "none",
    xlab = "Threshold Probability",
    ylab = "Net Benefit",
    confidence.intervals = FALSE,
    standardize = FALSE
  )
  if (!is.null(title)) title(main = title, line = 2.5)
  
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("left",
         legend = c(model_names, "All", "None"),
         col = c(colors, "black", "grey"),
         lwd = 3,         
         lty = 1,      
         seg.len = 2,    
         cex = 1.1,        
         bty = "n",        
         x.intersp = 0.8,  
         y.intersp = 1.2,  
         text.width = 0.6)
}
plot_dca_curves(results$train, "Inner Training Data Decision Curves")
plot_dca_curves(results$test, "Inner Validation Data Decision Curves")
plot_dca_curves(results$validation, "Prospective Validation Data Decision Curves")
plot_dca_curves(results$outer, "Outer Validation Data Decision Curves")



library(fmsb)
library(stringr)
library(dplyr)
library(openxlsx)
library(fmsb)
library(stringr)
library(dplyr)

df <- read.xlsx(file.choose())


plot_radar_models <- function(df, title_text = "Model Performance Radar Chart") {
  library(fmsb)
  library(dplyr)
  library(stringr)

  numeric_cols <- names(df)[2:8]
  for (col in numeric_cols) {
    df[[col]] <- as.numeric(str_extract(df[[col]], "^[0-9\\.]+"))
  }

  df$GM <- sqrt(df$sensitivity * df$specificity)

  df[,-1] <- lapply(df[,-1], function(x) x * 100)

  radar_data <- df %>%
    select(model_name, auc, sensitivity, specificity, ppv, npv, f1_score, accuracy, GM)

  radar_plot <- rbind(rep(100, ncol(radar_data) - 1),
                      rep(0, ncol(radar_data) - 1),
                      radar_data[,-1])
  rownames(radar_plot) <- c("max", "min", radar_data$model_name)

  color_list <- c(
	  "#00468BFF", "#AD002AFF", "#42B540FF", "#0099B4FF", 
	  "#925E9FFF", "#FDAF91FF", "#ED0000FF", "#ADB6B6FF", "#1B1919FF"
	)

  radarchart(
    radar_plot,
    axistype = 1,
    pcol = color_list,
    pfcol = rep(NA, length(color_list)), 
    plwd = 3,
    plty = 1,
    cglcol = "grey",
    cglty = 1,
    cglwd = 1.2,
    axislabcol = "grey20",
    caxislabels = seq(0, 100, 25),
    vlcex = 0.9,
    title = title_text
  )

  legend("topright",
         legend = radar_data$model_name,
         bty = "n",
         pch = 20,
         col = color_list,
         text.col = "black",
         cex = 0.8,
         pt.cex = 2.5,
         inset = c(-0.25, 0)) 
}



plot_radar_models(df, title_text = "train")
df2 <- read.xlsx(file.choose())
plot_radar_models(df2, title_text = "test")
df3 <- read.xlsx(file.choose())
plot_radar_models(df3, title_text = "validate")
df4 <- read.xlsx(file.choose())
plot_radar_models(df4, title_text = "outer")











library(fmsb)
library(dplyr)
library(stringr)
library(openxlsx)

plot_radar_by_model <- function(df_list, dataset_names, 
                                title_prefix = "Model Performance",
                                save_dir = "radar_pdfs") {

  if (!dir.exists(save_dir)) dir.create(save_dir)

  for (i in seq_along(df_list)) {
    numeric_cols <- names(df_list[[i]])[2:8]
    for (col in numeric_cols) {
      df_list[[i]][[col]] <- as.numeric(str_extract(df_list[[i]][[col]], "^[0-9\\.]+"))
    }
    df_list[[i]]$GM <- sqrt(df_list[[i]]$sensitivity * df_list[[i]]$specificity)
    df_list[[i]][,-1] <- lapply(df_list[[i]][,-1], function(x) x * 100)
  }

  all_models <- df_list[[1]]$model_name

  color_list <- c("#4A90E2", "#7ED321", "#F5A623", "#B388EB")

  for (model in all_models) {
    radar_data <- lapply(df_list, function(df) {
      df %>%
        filter(model_name == model) %>%
        select(auc, sensitivity, specificity, ppv, npv, f1_score, accuracy, GM)
    }) %>%
      bind_rows()

    rownames(radar_data) <- dataset_names

    radar_plot <- rbind(rep(100, ncol(radar_data)),
                        rep(0, ncol(radar_data)),
                        radar_data)

    file_name <- paste0(save_dir, "/", gsub("[^A-Za-z0-9_]", "_", model), "_radar.pdf")

    pdf(file_name, width = 9, height = 7)

    radarchart(
      radar_plot,
      axistype = 1,
      pcol = color_list,
      pfcol = rep(NA, length(color_list)),  # 不填充
      plwd = 3,
      plty = 1,
      cglcol = "grey",
      cglty = 1,
      cglwd = 1.2,
      axislabcol = "grey20",
      caxislabels = seq(0, 100, 25),
      vlcex = 0.9,
      title = paste0(title_prefix, " - ", model),
	  xpd = TRUE
    )

    legend("topright",
           legend = dataset_names,
           bty = "n",
           pch = 20,
           col = color_list,
           text.col = "black",
           cex = 0.8,
           pt.cex = 2.5,
           inset = c(-0.25, 0),xpd=TRUE)

    dev.off()
  }

  message("✅ 所有模型雷达图已保存至: ", normalizePath(save_dir))
}


plot_radar_by_model(
  df_list = list(df, df2, df3, df4),
  dataset_names = c("Inner Training Data", "Inner Validation Data", "Prospective Validation Data", "External Validation Data"),
  title_prefix = "Model Performance",
  save_dir = "radar_pdfs"
)

