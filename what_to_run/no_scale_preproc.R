library(readr)
library(caret)
library(ggplot2)
library(tidymodels)
library(tidyverse)
library(yardstick)
library(randomForest)

df2 = read.csv("application_data.csv", na = c("NA", "", "XNA"))


num_nas = sort(colSums(is.na(df2)), decreasing = TRUE)

zp =  nearZeroVar(df2, saveMetrics = TRUE)

# col_nearz = c("FLAG_DOCUMENT_2", "FLAG_DOCUMENT_3", FLAG_DOCUMENT_15", "FLAG_DOCUMENT_16", "FLAG_DOCUMENT_17")
# df3 = select(df2, -c("FLAG_DOCUMENT_17"))
df3 = df2[,!zp$nzv]
# nearZeroVar(df3)

df4 = df3[,colSums(is.na(df3)) < 0.10 * nrow(df3)]

# colSums(is.na(df4))

df5 = na.omit(df4)

str_vec = c("NAME_CONTRACT_TYPE", "CODE_GENDER", "FLAG_OWN_CAR", "FLAG_OWN_REALTY", "NAME_TYPE_SUITE",
            "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE",
            "WEEKDAY_APPR_PROCESS_START")

to_dummy = select(df5, all_of(str_vec))
dummy <- dummyVars(" ~ .", data=to_dummy)
newdata <- data.frame(predict(dummy, newdata = to_dummy))

df6 = cbind(newdata, select(df5, -all_of(str_vec)))

df7 = select(df6, -c("SK_ID_CURR"))

df8 = df7

df8$TARGET[df8$TARGET == 0] = 'n'
df8$TARGET[df8$TARGET == 1] = 'p'
df8$TARGET = as.factor(df8$TARGET)
