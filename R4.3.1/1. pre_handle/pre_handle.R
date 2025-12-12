library(openxlsx)
library(Boruta)
library(glmnet)
library(randomForest)
library(e1071)  
library(caret) 
library(pROC)  
library(keras) 
library(ggRandomForests)
library(gridExtra)
library(ggplot2)
library(nnet)
library(kknn)
library(xgboost)
library(gbm)
library(rpart)

set.seed(123)

#-----------------------------
# 1. 读取数据
#-----------------------------
data <- read.csv("./data/main.csv")
data_outer <- read.csv("./data/outer.csv")

norlmal <- read.csv("./data/norlmal.csv")
norlmal_outer <- read.csv("./data/norlmal_outer.csv")

#-----------------------------
# 2. 数据划分 train/validation/test
#-----------------------------

data$date <- as.Date(data$date)
cut_date <- as.Date("2023-12-31")
data_after <- data[data$date >= cut_date, ] 
data_before <- data[data$date < cut_date, ]
validation_data <- data_after
n_valid <- nrow(validation_data)
set.seed(123)
test_indices <- sample(1:nrow(data_before), size = n_valid, replace = FALSE)
test_data <- data_before[test_indices, ]
train_data <- data_before[-test_indices, ]


cat("Training:", nrow(train_data), "\n")
cat("Validation:", nrow(validation_data), "\n")
cat("Test:", nrow(test_data), "\n")

#-----------------------------
# 3. Boruta 特征选择
#-----------------------------
Boruta.gbsg <- Boruta(diagnosis_result ~ ., data = train_data, ntree = 70)
boruta_stats <- attStats(Boruta.gbsg)
par(mar = c(18, 4, 0.5, 2))
plot(Boruta.gbsg, las = 2, xlab = "", cex.axis = 0.6)
legend("topleft", legend = c("Accepted","Tentative", "Rejected"), fill = c("green", "yellow", "red"))

boruta_features <- rownames(boruta_stats[boruta_stats$decision == "Confirmed", ])

#-----------------------------
# 4. LASSO 特征选择
#-----------------------------
x <- as.matrix(train_data[, boruta_features])
y <- as.numeric(train_data$diagnosis_result)
x <- scale(x, center = TRUE, scale = TRUE)

mod_cv <- cv.glmnet(x = x, y = y, family = "gaussian", intercept = FALSE, alpha = 1)
best_lambda <- mod_cv$lambda.min
myCoefs <- coef(mod_cv, s = best_lambda)
lasso_features <- myCoefs@Dimnames[[1]][which(myCoefs != 0)]
lasso_features <- lasso_features[-1]  # 去掉截距
final_features <- intersect(boruta_features, lasso_features)

#-----------------------------
# 5. 构建训练/验证/测试子集
#-----------------------------
train_data_subset <- subset(train_data, select = c(final_features, 'diagnosis_result'))
validation_data_subset <- subset(validation_data, select = c(final_features, 'diagnosis_result'))
test_data_subset <- subset(test_data, select = c(final_features, 'diagnosis_result'))

colnames(data_outer) <- colnames(data)
outer_data_subset <- subset(data_outer, select = c(final_features, 'diagnosis_result'))

#-----------------------------
# 6. 处理 normal 数据
#-----------------------------
# 划分 normal 数据
total_rows_normal <- nrow(norlmal)
train_indices_normal <- sample(x = 1:total_rows_normal, size = 0.7 * total_rows_normal, replace = FALSE)
remaining_indices_normal <- setdiff(1:total_rows_normal, train_indices_normal)
validation_indices_normal <- sample(remaining_indices_normal, size = 0.5 * length(remaining_indices_normal), replace = FALSE)
test_indices_normal <- setdiff(remaining_indices_normal, validation_indices_normal)

train_normal <- norlmal[train_indices_normal, ]
validation_normal <- norlmal[validation_indices_normal, ]
test_normal <- norlmal[test_indices_normal, ]

# Boruta
Boruta.normal <- Boruta(diagnosis_result ~ ., data = train_normal, ntree = 70)
boruta_stats_normal <- attStats(Boruta.normal)
boruta_features_normal <- rownames(boruta_stats_normal[boruta_stats_normal$decision == "Confirmed", ])

# LASSO
x_normal <- as.matrix(train_normal[, boruta_features_normal])
y_normal <- as.numeric(train_normal$diagnosis_result)
x_normal <- scale(x_normal, center = TRUE, scale = TRUE)

mod_cv_normal <- cv.glmnet(x = x_normal, y = y_normal, family = "gaussian", intercept = FALSE, alpha = 1)
best_lambda_normal <- mod_cv_normal$lambda.min
myCoefs_normal <- coef(mod_cv_normal, s = best_lambda_normal)
lasso_features_normal <- myCoefs_normal@Dimnames[[1]][which(myCoefs_normal != 0)]
lasso_features_normal <- lasso_features_normal[-1]
final_features_normal <- intersect(boruta_features_normal, lasso_features_normal)

# 构建 normal 子集
train_normal_subset <- subset(train_normal, select = c(final_features_normal, 'diagnosis_result'))
validation_normal_subset <- subset(validation_normal, select = c(final_features_normal, 'diagnosis_result'))
test_normal_subset <- subset(test_normal, select = c(final_features_normal, 'diagnosis_result'))

colnames(norlmal_outer) <- colnames(norlmal)
outer_normal_subset <- subset(norlmal_outer, select = c(final_features_normal, 'diagnosis_result'))

#-----------------------------
# 7. 合并结果
#-----------------------------
# 注意这里要确保列名一致
common_features <- intersect(final_features, final_features_normal)
train_combined <- rbind(
    subset(train_data_subset, select = c(common_features, 'diagnosis_result')),
    subset(train_normal_subset, select = c(common_features, 'diagnosis_result'))
)
validation_combined <- rbind(
    subset(validation_data_subset, select = c(common_features, 'diagnosis_result')),
    subset(validation_normal_subset, select = c(common_features, 'diagnosis_result'))
)
test_combined <- rbind(
    subset(test_data_subset, select = c(common_features, 'diagnosis_result')),
    subset(test_normal_subset, select = c(common_features, 'diagnosis_result'))
)
outer_combined <- rbind(
    subset(outer_data_subset, select = c(common_features, 'diagnosis_result')),
    subset(outer_normal_subset, select = c(common_features, 'diagnosis_result'))
)