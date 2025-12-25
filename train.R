library(dplyr)
library(stringi)
library(tidyverse)
library(rvest)
library(jsonlite)
library(readr)
library(ggplot2)
library(fmsb)
library(xgboost)
library(caret)
library(SHAPforxgboost)
library(stats)
library(factoextra)


prepare_training_data <- function(df, train_pct = 0.8) {
  set.seed(123)
  
  df <- df %>%
    rowwise() %>%
    mutate(
      # Pokemon's type effectiveness
      First_Type_Max_Effectiveness = coalesce(max(c_across(starts_with("First_Type") & ends_with("Effectiveness")), na.rm = TRUE), First_Type1_Effectiveness),
      Second_Type_Max_Effectiveness = coalesce(max(c_across(starts_with("Second_Type") & ends_with("Effectiveness")), na.rm = TRUE), Second_Type1_Effectiveness),
      # Pokemon's move effectiveness
      First_Move_Max_Effectiveness = max(c_across(starts_with("First_Move") & ends_with("Effectiveness")), na.rm = TRUE),
      Second_Move_Max_Effectiveness = max(c_across(starts_with("Second_Move") & ends_with("Effectiveness")), na.rm = TRUE),
      # Move combination count
      First_Physical_Move_Count = sum(c_across(starts_with("First_Category_Move")) == "Physical", na.rm = TRUE),
      First_Special_Move_Count = sum(c_across(starts_with("First_Category_Move")) == "Special", na.rm = TRUE),
      First_Status_Move_Count = sum(c_across(starts_with("First_Category_Move")) == "Status", na.rm = TRUE),
      Second_Physical_Move_Count = sum(c_across(starts_with("Second_Category_Move")) == "Physical", na.rm = TRUE),
      Second_Special_Move_Count = sum(c_across(starts_with("Second_Category_Move")) == "Special", na.rm = TRUE),
      Second_Status_Move_Count = sum(c_across(starts_with("Second_Category_Move")) == "Status", na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      First_Outcome = ifelse(First_pokemon == Winner, "Winner", "Loser"),
      First_Outcome = ifelse(First_Outcome == "Winner", 1, 0),
      across(ends_with("Power"), ~ ifelse(.x == "" | .x == "None", 0, as.numeric(.x))),
      across(ends_with("Accuracy"), ~ ifelse(.x == "" | .x == "None", 100, as.numeric(.x))),
      across(where(is.character) | where(is.logical), ~ as.numeric(.))
    )%>%
    select(First_Type1, First_Type2, First_HP, First_Attack, First_Defense,
           First_Sp_Atk, First_Sp_Def, First_Speed, First_Is_Legendary,
           First_Type_Max_Effectiveness, First_Move_Max_Effectiveness,
           First_Physical_Move_Count, First_Special_Move_Count,
           First_Status_Move_Count,
           Second_Type1, Second_Type2, Second_HP, First_Attack, First_Defense,
           Second_Sp_Atk, Second_Sp_Def, Second_Speed, Second_Is_Legendary,
           Second_Type_Max_Effectiveness, Second_Move_Max_Effectiveness,
           Second_Physical_Move_Count, Second_Special_Move_Count,
           Second_Status_Move_Count, First_Outcome)

  features <- setdiff(names(df), "First_Outcome") # Exclude the target column
  train_matrix <- as.matrix(df[features])
  train_labels <- df$First_Outcome

  return(list(
    train_matrix = train_matrix,
    train_labels = train_labels
  ))
}


df <- readRDS("files/data/final_pokemon_data.rds")
training_data <- prepare_training_data(df)
train_matrix <- training_data$train_matrix
train_labels <- as.character(training_data$train_labels)

xgb_model <- xgboost(
  data = train_matrix,
  label = train_labels,
  max.depth = 6,        # Maximum depth of each tree
  eta = 0.1,            # Learning rate
  nrounds = 100,        # Number of boosting rounds
  objective = "binary:logistic", # Binary classification
  eval_metric = "error", # Evaluation metric
  scale_pos_weight = sum(train_labels == 0) / sum(train_labels == 1),
  verbose = 0           # Print progress
)

pred_probs <- predict(xgb_model, train_matrix)
# Convert probabilities to binary predictions (threshold = 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Calculate metrics
conf_matrix <- confusionMatrix(factor(pred_labels), factor(train_labels))
print(conf_matrix)

# Save model
if (!dir.exists("files/models")) {
  dir.create("files/models")
}
xgb.save(xgb_model, "files/models/pokemon_prediction_model.xgb")
