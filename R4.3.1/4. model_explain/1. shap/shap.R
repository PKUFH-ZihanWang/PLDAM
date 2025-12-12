RandomForest <- randomForest(diagnosis_result ~ ., data = train_data_subset)
importances <- importance(rf_model1)

importance_df <- data.frame(
  Feature = rownames(importances),
  Importance = importances[, 1]
)

importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]


importance_df$Feature <- sub("_$", "", importance_df$Feature)
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

new_names <- normalize_names(importance_df$Feature)
print(new_names)
importance_df$Feature <- new_names
importance_df$Feature <- gsub("_Mask_", "_mask_", importance_df$Feature)


library(ggplot2)
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", aes(fill = Importance), width = 0.7) +
  geom_line(aes(group = 1), color = "black", size = 1) +
  geom_point(color = "black", size = 2) +
  coord_flip() +
  labs(x = "Features", y = "Importance", title = "Feature Importance With Trend Line") +
  theme_minimal(base_size = 14) +
  scale_fill_gradient2(
    low = "white",
    mid = "blue",
    high = "#E41A1C",
    midpoint = median(importance_df$Importance)
  )

		 


library(fastshap)
library(DALEX)
library(iml)
library(fastshap)
library(magrittr)
library(tidyverse)
library(randomForest)
library(fastshap)
library(dplyr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fastshap)

shap <- fastshap::explain(
  rf_model1, 
  X = train_data_subset[, -49],
  nsim = 10,
  pred_wrapper = function(model, newdata) {
    predict(model, newdata = newdata, type = "class")
  }
)

shap_handle <- shap %>% 
  as.data.frame() %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = -id, names_to = "name", values_to = "shap")

data2 <- train_data_subset[,-49] %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = -id, names_to = "name", values_to = "value")

shap_scale <- shap_handle %>% 
  left_join(data2, by = c("id", "name")) %>% 
  group_by(name) %>% 
  mutate(value = (value - min(value)) / (max(value) - min(value))) %>% 
  sample_n(min(1200, n()))


shap_scale <- shap_scale %>% 
  mutate(name = ifelse(name %in% names(mapping), mapping[name], name))
  
shap_scale <- shap_scale %>% 
  mutate(name = sub("_$", "", name))

ggplot(data = shap_scale, aes(x = shap, y = name, color = value)) +
  geom_violin(scale = "width", adjust = 1, trim = TRUE) +
  geom_jitter(size = 1.5, height = 0.2, width = 0, alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red", 
                       breaks = c(0, 1), labels = c("Low", "High"), 
                       guide = guide_colorbar(barwidth = 1, barheight = 20), 
                       name = "Predictor Values") +
  labs(x = "SHAP values", y = "Selected Predictors", title = "SHAP Summary Plot") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(face = "italic", size = 10)
  )


  

ggplot(data = shap_scale, aes(x = shap, y = name, color = value)) +
  geom_jitter(size = 2, height = 0.1, width = 0) +
  scale_color_gradient(low = "#FFCC33", high = "#6600CC", 
                       breaks = c(0, 1), labels = c("Low", "High"), 
                       guide = guide_colorbar(barwidth = 2, barheight = 30), 
                       name = "Feature value", 
                       aesthetics = "color") +
  theme_bw()




shap_scale <- shap_handle %>% 
  left_join(data2, by = c("id","name")) %>% 
  group_by(name) %>% 
  mutate(value = (value - min(value)) / (max(value) - min(value)))
  
feature_name <- "original_shape_SurfaceArea_trigone_of_bladder_"  

shap_feature <- shap_scale %>%
  filter(name == feature_name)
ggplot(data = shap_feature, aes(x = value, y = shap, color = value)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "black", se = FALSE) + 
  scale_color_gradient(low = "blue", high = "red",
                       name = "Predictor Value", 
                       breaks = c(0, 1), labels = c("Low", "High"),
                       guide = guide_colorbar(barwidth = 1, barheight = 20)) +
  labs(x = "Feature Value", y = "SHAP Value", 
       title = paste("SHAP Values for", feature_name)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )



library(shapviz)
library(pROC)
library(shapviz)
library(stringr)
library(dplyr)
library(shapviz)
library(stringr)
library(dplyr)

shp <- shapviz(xgb_model,X_pred=as.matrix(train_data_subset[,-49]))
shp_names <- colnames(shp)

colnames(shp) <- shp_names

# 生成 waterfall plot
p <- sv_waterfall(shp)
p





