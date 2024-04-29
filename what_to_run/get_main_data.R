library(ggplot2)

library(caret)

library(tidymodels)

library(tidyverse)

library(SmartEDA)

library(gplots)

library(randomForest)

#####################################################################################################################
############################################### UPDATE SETWD TO WHERE CSV IS ########################################
#####################################################################################################################


setwd("C:/Users/mjstr/Desktop/Git Repository Final Python Project/FinalAML")
cc_fraud = read.csv("application_data.csv", )

summary(cc_fraud)

location_categories = data.frame(matrix(ncol=3, nrow=12))
colnames(location_categories) = c("group", "target", "count")

categories = c("REG_REGION_NOT_LIVE_REGION", "REG_REGION_NOT_WORK_REGION", "LIVE_REGION_NOT_WORK_REGION", 
               "REG_CITY_NOT_LIVE_CITY", "REG_CITY_NOT_WORK_CITY", "LIVE_CITY_NOT_WORK_CITY")

i = 1
for (category in categories) {
  filtered_data = filter(cc_fraud, !!as.symbol(category) == 1)
  location_categories[i,] = c(category, "0", sum(filtered_data$TARGET == 0))
  location_categories[i+1,] = c(category, "1", sum(filtered_data$TARGET == 1))
  i = i + 2
}

location_categories$target = as.factor(location_categories$target)
location_categories$count = as.numeric(location_categories$count)



# Create quantile functions
quantile_25 = function(x){
  quantile_25 = quantile(x, na.rm = TRUE, 0.1)
}

quantile_75 = function(x){
  quantile_75 = quantile(x, na.rm = TRUE, 0.9)
}


## Feature Engineering ##

# Noticed that "Academic degree" is a rare value for feature NAME_EDUCATION_TYPE so lumping it with "Higher education"
cc_fraud$NAME_EDUCATION_TYPE[cc_fraud$NAME_EDUCATION_TYPE=="Academic degree"] = "Higher education"


# Converting "XNA" values to NA
cc_fraud[cc_fraud=="XNA"] = NA

# Converting "" values to NA
cc_fraud[cc_fraud==""] = NA

# Identifying how many NA values we have in each feature
num_nas = sort(colSums(is.na(cc_fraud)), decreasing = TRUE)

# Dropping features that are missing > ~10% of values
drop_features = names(which(num_nas > 30000))
cc_fraud = cc_fraud %>% select(-drop_features)

## Data splitting ##
set.seed(1)

# Removing loan ID 
cc_fraud = cc_fraud %>% select(-"SK_ID_CURR")

# 70%-30% split
split = initial_split(cc_fraud, prop = 0.70, strata = "TARGET")   

cc_train = training(split)

cc_test = testing(split)


blueprint = recipe(TARGET ~ ., data = cc_train) %>%
  
  step_string2factor(all_nominal_predictors()) %>%
  
  step_nzv(all_predictors()) %>%
  
  step_impute_knn(all_predictors()) %>%
  
  step_center(all_numeric_predictors()) %>%
  
  step_scale(all_numeric_predictors()) %>%
  
  step_other(NAME_TYPE_SUITE, NAME_INCOME_TYPE, NAME_HOUSING_TYPE, threshold = 0.01, other = "other") %>%
  
  step_dummy(all_nominal())

# Estimating blueprint parameters 
blueprint_prep = prep(blueprint, training = cc_train)

# Transforming data
transformed_train = bake(blueprint_prep, new_data = cc_train)

tot_neg = sum(transformed_train$TARGET == 'n')
tot_pos = sum(transformed_train$TARGET == 'p')

transformed_test = bake(blueprint_prep, new_data = cc_test)
transformed_train$TARGET[transformed_train$TARGET == 0] = 'n'
transformed_train$TARGET[transformed_train$TARGET == 1] = 'p'
transformed_train$TARGET = as.factor(transformed_train$TARGET)

transformed_test$TARGET[transformed_test$TARGET == 0] = 'n'
transformed_test$TARGET[transformed_test$TARGET == 1] = 'p'
transformed_test$TARGET = as.factor(transformed_test$TARGET)


