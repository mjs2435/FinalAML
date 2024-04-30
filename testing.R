library(shiny)
library(caret)
library(rpart) # For Decision Tree
library(randomForest) # For Random Forest
library(e1071) # For SVM
library(shinythemes)
library(shinycssloaders)
library(ggExtra)
library(data.table)
library(ggplot2)
library(dplyr)  # For data manipulation
library(recipes)
library(future)
library(shinycssloaders)
library(rsample)
library(tidymodels)
library(tidyverse)
library(SmartEDA)
library(gplots)


setwd("/Users/auroraweng/Desktop/STAT3106 AML/FinalAML/")
data <- read.csv("Credit_Card_Fraud_Detection/data/subset_application_data.csv", header = TRUE)
set.seed(50)
split <- initial_split(data, prop = 0.70, strata = "TARGET")   # 70%-30% split
train_data <- training(split)
test_data <- testing(split)
train_data$TARGET <- factor(train_data$TARGET, levels = c("0", "1"),labels = c("Positive", "Negative"))
test_data$TARGET <- factor(test_data$TARGET, levels = c("0", "1"),labels = c("Positive", "Negative"))
blueprint <- recipe(TARGET ~., data = train_data) %>%step_string2factor(all_nominal(), -all_outcomes())
blueprint <-blueprint %>% step_nzv(all_predictors())
blueprint <-blueprint %>% step_impute_knn(all_predictors())
blueprint <-blueprint %>% step_normalize(all_numeric_predictors())
blueprint <- blueprint %>% step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())
blueprint<- blueprint%>%step_dummy(all_nominal(), -all_outcomes())
blueprint_prep = prep(blueprint, training = train_data)
transformed_train=transformed_train<-(bake(blueprint_prep, new_data = train_data))
transformed_test=transformed_test<-(bake(blueprint_prep, new_data = test_data))

trainingControl = trainControl(method = "repeatedcv", number = 3, repeats = 5,classProbs = TRUE, summaryFunction = twoClassSummary)

tot_neg = sum(transformed_train$TARGET == 'Positive')
tot_pos = sum(transformed_train$TARGET == 'Negative')
TARGET = transformed_train$TARGET


hyper_grid <- expand.grid(mtry = c(13, 15, 17, 20, 22, 25), 
                          
                          splitrule = c("gini", "extratrees", "hellinger"), # 
                          
                          min.node.size = c(5, 7, 9)) # 
rf_fit <- train(TARGET ~ .,
                
                data = transformed_train, 
                
                method = "ranger", 
                
                trControl = trainingControl, 
                
                tuneGrid = hyper_grid,
                
                metric = "ROC",
                num.trees = 50
                )


ggplot(rf_fit)

plot(rf_fit, metric = "ROC", plotType = "level")
plot(rf_fit, metric = "Sens", plotType = "level")
plot(rf_fit, metric = "Spec", plotType = "level")
plot(rf_fit, metric = "ROCSD", plotType = "level")