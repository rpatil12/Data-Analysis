rm(list=ls())
###  Company    : Stevens 
#  Project    : KDDM Final Project
#  Purpose    : Random Forest for feature selection
#  Date       : 11/27/2018
#  Comments   :

#### Install packages ####
if(!require(extraTrees)) {
  install.packages(extraTrees)
}
if(!require(C50)) {
  install.packages(C50)
}
if(!require(nnet)) {
  install.packages(nnet)
}
if(!require(class)) {
  install.packages(class)
}
if(!require(pROC)) {
  install.packages(pROC)
}
if(!require(e1071)) {
  install.packages(e1071)
}
if(!require('rstudioapi')){
  install.packages("rstudioapi")
}
library('rstudioapi')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if(!require('tidyquant')){
  install.packages("tidyquant")
}
library(tidyquant)  # Loads tidyverse and custom ggplot themes
if(!require('unbalanced')){
  install.packages("unbalanced")
}
library(unbalanced) # Methods for dealing with unbalanced data sets

if(!require('randomForest')){
  install.packages("randomForest")
}
if(!require('RWeka')){
  install.packages("RWeka")
}
library(randomForest)

#### Combining training and test data files ####
data1 <- read_csv("training.csv")
data2 <- read_csv("test.csv")

#class distribution
data1$went_on_backorder %>% table() %>% prop.table()
data2$went_on_backorder %>% table() %>% prop.table()

#### Data Preprocessing ####
preprocess <- function(data) {
  data %>%
    select(-sku) %>%
    mutate(perf_6_month_avg = ifelse(perf_6_month_avg == -99.00,NA,perf_6_month_avg)) %>%
    mutate(perf_12_month_avg = ifelse(perf_12_month_avg == -99.00,NA,perf_12_month_avg)) %>%
    mutate_if(is.character, .funs = function(x) ifelse(x == 'Yes',1,0)) %>%
    mutate(went_on_backorder = as.factor(went_on_backorder))
}

preprocess2 <- function(data) {
  mean_lead_time = as.integer(mean(data$lead_time,na.rm = TRUE))
  mean_perf_6_month_avg <- mean(data$perf_6_month_avg, na.rm = TRUE)
  mean_perf_6_month_avg <- round(mean_perf_6_month_avg, 2)
  mean_perf_12_month_avg <- as.numeric(mean(data$perf_12_month_avg, na.rm = TRUE))
  mean_perf_12_month_avg <- round(mean_perf_12_month_avg, 2)
  
  data %>%
    drop_na(national_inv) %>%
    mutate(lead_time = ifelse(is.na(lead_time),mean_lead_time,lead_time)) %>%
    mutate(perf_6_month_avg = ifelse(is.na(perf_6_month_avg),mean_perf_6_month_avg,perf_6_month_avg)) %>%
    mutate(perf_12_month_avg = ifelse(is.na(perf_12_month_avg),mean_perf_12_month_avg,perf_12_month_avg))
}

data1 = preprocess(data1)
data2 = preprocess(data2)
#percentage of complete cases
data1 %>% complete.cases() %>% sum()/nrow(data1)
data2 %>% complete.cases() %>% sum()/nrow(data2)
train = preprocess2(data1)
test = preprocess2(data2)

#Creating PCA dataset
data1.pca <- prcomp(train[,c(1:11,13:15)], center = TRUE,scale. = TRUE)
data2.pca <- predict(data1.pca, test[,c(1:11,13:15)])
train.pca <- cbind(data1.pca$x, train[,c(12,16:22)])
test.pca <- cbind(data2.pca, test[,c(12,16:22)])

#Creating dataset with features selected by Random Forest 
train.rf  <- train %>% select(national_inv, lead_time, forecast_3_month, forecast_6_month, forecast_9_month, sales_1_month, sales_3_month, sales_6_month, sales_9_month, perf_12_month_avg, perf_6_month_avg, in_transit_qty, min_bank, local_bo_qty, went_on_backorder)
test.rf  <- test %>% select(national_inv, lead_time, forecast_3_month, forecast_6_month, forecast_9_month, sales_1_month, sales_3_month, sales_6_month, sales_9_month, perf_12_month_avg, perf_6_month_avg, in_transit_qty, min_bank, local_bo_qty, went_on_backorder)


#### Undersampling to balance the dataset ####
input  <- train %>% select(-went_on_backorder)
output <- train$went_on_backorder 
train_balanced <- ubUnder(input, output, perc = as.integer(runif(1, 45, 50)), method = "percPos", w = NULL)
training <- bind_cols(as.tibble(train_balanced$X), tibble(went_on_backorder = train_balanced$Y))

#Undersampling on PCA dataset
input.pca  <- train.pca %>% select(-went_on_backorder)
output.pca <- train.pca$went_on_backorder 
train.pca_balanced <- ubUnder(input.pca, output.pca, perc = as.integer(runif(1, 45, 50)), method = "percPos", w = NULL)
training.pca <- bind_cols(as.tibble(train.pca_balanced$X), tibble(went_on_backorder = train.pca_balanced$Y))

#Undersampling on RF dataset
input.rf  <- train.rf %>% select(-went_on_backorder)
output.rf <- train.rf$went_on_backorder 
train.rf_balanced <- ubUnder(input.rf, output.rf, perc = as.integer(runif(1, 45, 50)), method = "percPos", w = NULL)
training.rf <- bind_cols(as.tibble(train.rf_balanced$X), tibble(went_on_backorder = train.rf_balanced$Y))

##Undersampling on RF dataset with binning
train_bin_rf <- read_csv("train_bin_rf.csv")
test.bin.rf <- read_csv("test_bin_rf.csv")
input.bin.rf  <- train_bin_rf %>% select(-went_on_backorder)
output.bin.rf <- train_bin_rf$went_on_backorder 
train.bin.rf_balanced <- ubUnder(input.bin.rf, output.bin.rf, perc = as.integer(runif(1, 45, 50)), method = "percPos", w = NULL)
training.bin.rf <- bind_cols(as.tibble(train.bin.rf_balanced$X), tibble(went_on_backorder = train.bin.rf_balanced$Y))

#Generic function for showing ROC measures
roc_measures <- function (pred, actual) {
  pred = as.numeric(pred)
  print(table(pred, actual))
  confusion_matrix = as.numeric(table(pred, actual))
  accuracy = ((confusion_matrix[1]+confusion_matrix[3])/(confusion_matrix[1] + confusion_matrix[2] + confusion_matrix[3] + confusion_matrix[4]))
  print(paste("Accuracy = ", accuracy, "%"))
  tpr = (confusion_matrix[1]/(confusion_matrix[1] + confusion_matrix[2]))
  print(paste("TPR = ", tpr, "%"))
  tnr = (confusion_matrix[4]/(confusion_matrix[3] + confusion_matrix[4]))
  print(paste("TNR = ", tnr, "%"))
  fpr = (confusion_matrix[3]/(confusion_matrix[3] + confusion_matrix[4]))
  print(paste("FPR = ", fpr, "%"))
  fnr = (confusion_matrix[2]/(confusion_matrix[1] + confusion_matrix[2]))
  print(paste("FNR = ", fnr, "%"))
  
  test_val = as.numeric(actual)
  
  auc = roc(test_val, pred)
  print(auc)
  plot(auc)
}

#C5.0 Normal
fit_C5.0 <- C5.0(went_on_backorder ~ ., data = training)
predictions_C5.0 <- predict(fit_C5.0, test[,1:21])
table(pred = predictions_C5.0, actual = test$went_on_backorder)
C5.0_wrong <- (test$went_on_backorder!=predictions_C5.0)
rate <- sum(C5.0_wrong)/length(C5.0_wrong)
rate
test_val = as.numeric(test$went_on_backorder)
predictions_C5.0 = as.numeric(predictions_C5.0)
auc_C5.0 = roc(test_val, predictions_C5.0)
print(auc_C5.0)
plot(auc_C5.0)

#    actual
# pred   0      1
# 0 209585    517
# 1  29802   2171
# rate: 0.1252463
#Area under the curve: 0.8416


#C4.5 RF without binning
fit_C5.0 <- C5.0(went_on_backorder ~ ., data = training.rf)
predictions_C5.0 <- predict(fit_C5.0, test.rf[,1:14])
table(pred = predictions_C5.0, actual = test.rf$went_on_backorder)
C5.0_wrong <- (test.rf$went_on_backorder!=predictions_C5.0)
rate <- sum(C5.0_wrong)/length(C5.0_wrong)
rate
test_val = as.numeric(test.rf$went_on_backorder)
predictions_C5.0 = as.numeric(predictions_C5.0)
auc_C5.0 = roc(test_val, predictions_C5.0)
print(auc_C5.0)
plot(auc_C5.0)

#     actual
# pred 0      1
# 0 209344    530
# 1  30043   2158
# rate: 0.1262956
# Area under the curve: 0.8387


#C4.5 RF with binning
fit_j48 <- J48(went_on_backorder ~ ., data = training.bin.rf)
predictions_j48 <- predict(fit_j48, test.bin.rf[,1:14])

table(pred = predictions_j48, actual = test.bin.rf$went_on_backorder)
C4.5_wrong <- (test.bin.rf$went_on_backorder!=predictions_j48)
rate <- sum(C4.5_wrong)/length(C4.5_wrong)
rate
test_val = as.numeric(test.bin.rf$went_on_backorder)
predictions_j48 = as.numeric(predictions_j48)
auc_j48 = roc(test_val, predictions_j48)
print(auc_j48)
plot(auc_j48)

# actual
# pred   0      1
# 0      0      0
# 1 239387   2688
# rate: [1] 0.988896
# Area under the curve: 0.5


#C4.5 on PCA dataset
fit_C5.0 <- C5.0(went_on_backorder ~ ., data = training.pca)
predictions_C5.0 <- predict(fit_C5.0, test.pca[,1:21])
table(pred = predictions_C5.0, actual = test.pca$went_on_backorder)
C5.0_wrong <- (test.pca$went_on_backorder!=predictions_C5.0)
rate <- sum(C5.0_wrong)/length(C5.0_wrong)
rate
test_val = as.numeric(test.pca$went_on_backorder)
predictions_C5.0 = as.numeric(predictions_C5.0)
auc_C5.0 = roc(test_val, predictions_C5.0)
print(auc_C5.0)
plot(auc_C5.0)

#     actual
# pred  0      1
# 0 196003    933
# 1  43384   1755
# rate [1] 0.1830714
# Area under the curve: 0.7358


#ANN on Normal data
ideal <- class.ind(training$went_on_backorder)
boANN = nnet(training[,-22], ideal, size=10,  softmax=TRUE, maxit = 250)
predANN = predict(boANN, test[,-22], type="class")
table(pred = predANN, actual = test$went_on_backorder)
ann_wrong <- (test$went_on_backorder!=predANN)
rate <- sum(ann_wrong)/length(ann_wrong)
rate
test_val = as.numeric(test$went_on_backorder)
predictions_val = as.numeric(predANN)
auc_val=roc(predictions_val,test_val)
auc_val

#     actual
# pred   0      1
# 0 211427    455
# 1  27960   2233
# rate [1] 0.117381
# Area under the curve: 0.5359

#ANN on RF data
ideal <- class.ind(training.rf$went_on_backorder)
boANN = nnet(training.rf[,-15], ideal, size=10,  softmax=TRUE, maxit = 250)
predANN = predict(boANN, test.rf[,-15], type="class")
table(pred = predANN, actual = test.rf$went_on_backorder)
ann_wrong <- (test.rf$went_on_backorder!=predANN)
rate <- sum(ann_wrong)/length(ann_wrong)
rate
test_val = as.numeric(test.rf$went_on_backorder)
predictions_val = as.numeric(predANN)
auc_val=roc(predictions_val,test_val)
auc_val

#   actual
# pred  0      1
# 0 211335    447
# 1  28052   2241
# Rate: 0.117728
# Area under the curve: 0.5359

#ANN on PCA data
ideal <- class.ind(training.pca$went_on_backorder)
boANN = nnet(training.pca[,-22], ideal, size=10,  softmax=TRUE, maxit = 250)
predANN = predict(boANN, test.pca[,-22], type="class")
confusion_matrix = table(pred = predANN, actual = test.pca$went_on_backorder)
confusion_matrix
# TPR: True positive rate (Sensitivity, Recall)
tpr = (confusion_matrix[1][1])/(confusion_matrix[1][2] + confusion_matrix[1][2])
tpr
# FPR False positive rate (Fall-out)
# FNR: False negative rate (Miss rate)
# TNR: True negative rate (Specificity)
xann_wrong <- (test.pca$went_on_backorder!=predANN)
rate <- sum(ann_wrong)/length(ann_wrong)
rate
test_val = as.numeric(test.pca$went_on_backorder)
predictions_val = as.numeric(predANN)
auc_val=roc(predictions_val,test_val)
auc_val

#   actual
# pred   0      1
# 0 211314    517
# 1  28073   2171
# Rate: 0.1181039
# Area under the curve: 0.5347


#Using ExtraTrees Classifier on normal data
fit_et <- extraTrees(training[,-22], training$went_on_backorder)
predictions_et <- predict(fit_et, test[,-22])
roc_measures(predictions_et, test$went_on_backorder)

#     actual
# pred   0      1
# 1 219035    775
# 2  20352   1913
# "Accuracy =  0.908024372611794 %"
# Area under the curve: 0.8133

#Using ExtraTrees Classifier on RF data
fit_et <- extraTrees(training.rf[,-15], training.rf$went_on_backorder)
predictions_et <- predict(fit_et, test.rf[,-15])
roc_measures(predictions_et, test.rf$went_on_backorder)

# Result same as above
#     actual
# pred   0      1
# 1 219035    775
# 2  20352   1913
# "Accuracy =  0.908024372611794 %"
# Area under the curve: 0.8133

#Using ExtraTrees Classifier on PCA data
fit_et <- extraTrees(training.pca[,-22], training.pca$went_on_backorder)
predictions_et <- predict(fit_et, test.pca[,-22])
roc_measures(predictions_et, test.pca$went_on_backorder)

#     actual
# pred      0      1
# 1 212523   1082
# 2  26864   1606
# "Accuracy =  0.88239182071672 %"
# "TPR =  0.887780038180854 %"
# "TNR =  0.597470238095238 %"
# "FPR =  0.402529761904762 %"
# "FNR =  0.112219961819146 %"
# Area under the curve: 0.7426
# 
