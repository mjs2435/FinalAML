library(ggplot2)

library(caret)

library(tidymodels)

library(tidyverse)

library(SmartEDA)

library(gplots)

## EDA ##

cc_fraud = read.csv("application_data.csv")

summary(cc_fraud)

# Loan credit amount density grouped by loan type
ggplot(data = cc_fraud, aes(x=AMT_CREDIT, group=NAME_CONTRACT_TYPE, fill=NAME_CONTRACT_TYPE)) +
  geom_density(adjust=1.5, alpha=.4) + scale_x_continuous(labels = scales::comma) +
  theme_bw() + labs(x="Loan Credit Amount ($)") + 
  guides(fill = guide_legend(title = "Loan Type")) 

# Plot of loan credit amount frequency vs. target
ggplot(data = cc_fraud, mapping = aes(x = AMT_CREDIT)) + 
  geom_freqpoly(mapping = aes(colour = factor(TARGET)), binwidth = 5000) +
  theme_bw() + scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(x="Loan Credit Amount ($)") + 
  guides(color = guide_legend(title = "Target")) 

# Plot of client income vs. gender
### could use na.omit to get a better chart
ggplot(data = cc_fraud[!cc_fraud$CODE_GENDER %in% c("XNA"),], mapping = aes(x = CODE_GENDER, y = AMT_INCOME_TOTAL)) +
  geom_boxplot() +
  labs(x="Gender", y="Client Income ($)") +
  coord_flip() +
  theme_bw() + scale_y_continuous(labels = scales::comma)

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

# Plot of region level address mismatches
ggplot(location_categories[1:6,], aes(x=group, y=count, fill=target)) +
  geom_bar(stat='identity', position='dodge') + theme_bw() + scale_fill_manual(values = c("darkgreen", "red")) +
  labs(x="Region level address mismatch") + scale_x_discrete(labels= c("Contact/work", "Permanent/contact", "Permanent/work"))

# Plot of city level address mismatches
ggplot(location_categories[7:12,], aes(x=group, y=count, fill=target)) +
  geom_bar(stat='identity', position='dodge') + theme_bw() + scale_fill_manual(values = c("darkgreen", "red")) + 
  labs(x="City level address mismatch") + scale_x_discrete(labels= c("Contact/work", "Permanent/contact", "Permanent/work"))

# Density plot of client education level and target
ggplot(data = cc_fraud,
       mapping = aes(x = NAME_EDUCATION_TYPE, fill = factor(TARGET))) + 
  geom_density(alpha = 0.7) + labs(x = "Level of highest education received by client", fill = "Target") + 
  scale_fill_manual(values = c("lightblue", "red")) + theme_bw() 

# NZV features
# DAYS_EMPLOYED, FLAG_MOBIL, FLAG_CONT_MOBILE, BASEMENTAREA_AVG, LANDAREA_AVG, LANDAREA_MODE, NONLIVINGAREA_MODE, 
# BASEMENTAREA_MEDI, LANDAREA_MEDI, NONLIVINGAREA_MEDI, FLAG_DOCUMENT_2, FLAG_DOCUMENT_4, FLAG_DOCUMENT_5, FLAG_DOCUMENT_7, 
# all the other FLAG_DOCUMENTs, AMT_REQ_CREDIT_BUREAU_HOUR, AMT_REQ_CREDIT_BUREAU_DAY, AMT_REQ_CREDIT_BUREAU_WEEK 
nzv = nearZeroVar(cc_fraud, saveMetrics = TRUE)
nzv

# Boxplot of credit amount of loan by # of children (red point = group mean)
credit_amt_children_plot = cc_fraud %>% 
  mutate(CNT_CHILDREN = factor(CNT_CHILDREN)) %>% 
  ggplot(aes(x = CNT_CHILDREN, y = AMT_CREDIT, group = CNT_CHILDREN, fill = CNT_CHILDREN)) +
  geom_boxplot(alpha = .7) + 
  stat_summary(fun.y = mean, color = "darkred", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = TRUE) +
  geom_jitter(width = .05, alpha = .4) +
  guides(fill = "none") +
  theme_bw() +
  labs(
    x = "Number of children",
    y = "Credit amount of loan ($)"
  ) + scale_y_continuous(labels = scales::comma)

credit_amt_children_plot

# Boxplot of credit amount of loan by client region rating (red point = group mean)
credit_amt_region_rate_plot = cc_fraud %>% 
  mutate(REGION_RATING_CLIENT = factor(REGION_RATING_CLIENT)) %>% 
  ggplot(aes(x = REGION_RATING_CLIENT, y = AMT_CREDIT, group = REGION_RATING_CLIENT, fill = REGION_RATING_CLIENT)) +
  geom_boxplot(alpha = .7) + 
  stat_summary(fun.y = mean, color = "darkred", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = TRUE) +
  geom_jitter(width = .05, alpha = .4) +
  guides(fill = "none") +
  theme_bw() +
  labs(
    x = "Rating of client's region",
    y = "Credit amount of loan ($)"
  ) + scale_y_continuous(labels = scales::comma)

credit_amt_region_rate_plot

# Group means for target by # of children
plotmeans(TARGET ~ CNT_CHILDREN, data = cc_fraud, frame = TRUE, n.label = FALSE, ylab = "Group mean of TARGET", 
          xlab = "Number of children client has")

# Group means for target by client region rating
plotmeans(TARGET ~ REGION_RATING_CLIENT, data = cc_fraud, n.label = FALSE, ylab = "Group mean of TARGET", 
          xlab = "Client region rating")

# Data overview
ExpData(data = cc_fraud, type = 1)

# Create quantile functions
quantile_25 = function(x){
  quantile_25 = quantile(x, na.rm = TRUE, 0.1)
}

quantile_75 = function(x){
  quantile_75 = quantile(x, na.rm = TRUE, 0.9)
}

# Data structure
ExpData(data = cc_fraud, type = 2, fun = c("min", "quantile_25", "median", "quantile_75", "max"))

# Breakdown of how many times each categorical feature value is classified as 1 or 0 in TARGET
ExpCTable(cc_fraud, Target="TARGET", margin=1, clim=10,  round=2, per=F)

# Messing with parallel coordinate plots
ExpParcoord(cc_fraud,Group="NAME_CONTRACT_TYPE",Stsize=c(10,15,20),Nvar=c("AMT_CREDIT","AMT_INCOME_TOTAL"), Cvar=c("TARGET"), scale=NULL)

## Feature Engineering ##

# Noticed that "Academic degree" is a rare value for feature NAME_EDUCATION_TYPE so lumping it with "Higher education"
cc_fraud$NAME_EDUCATION_TYPE[cc_fraud$NAME_EDUCATION_TYPE=="Academic degree"] = "Higher education"

# Identify how many "XNA" values we have in each feature
sort(colSums(cc_fraud=="XNA"), decreasing = TRUE)

# Converting "XNA" values to NA
cc_fraud[cc_fraud=="XNA"] = NA

# Identify how many "" values we have in each feature
sort(colSums(cc_fraud==""), decreasing = TRUE)

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

## Data pre-processing ##

# Convert all character features to factors
# Remove all NZV features
# Impute KNN for remaining missing values 
# Center and scale numeric predictors
# Pool infrequently occurring values into category called "other" (applies to NAME_TYPE_SUITE, NAME_INCOME_TYPE, NAME_HOUSING_TYPE)
# Dummy encode all categorical features
# Principal component analysis for numeric predictors(?)
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

transformed_test = bake(blueprint_prep, new_data = cc_test)
