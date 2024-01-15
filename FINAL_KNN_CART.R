###  Company    : Stevens 
#  Project    : KDDM Final Project
#  Purpose    : KNN and CART
#  Date       : 11/27/2018
#  Comments   :

#### Install packages ####
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
library(randomForest)
if(!require('RWeka')){
  install.packages("RWeka")
}
library(RWeka)
if(!require('rpart')){
  install.packages("rpart")
}
library(rpart)
if(!require('rpart.plot')){
  install.packages("rpart.plot")
}
library(rpart.plot)
if(!require('kknn')){
  install.packages("kknn")
}
library(kknn)
rm(list=ls())
#### Combining training and test data files ####
data1 <- read_csv("Kaggle_Test_Dataset_v2.csv")
data2 <- read_csv("Kaggle_Training_Dataset_v2.csv")

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

##Creating PCA dataset
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

##Undersampling on RF dataset
input.rf  <- train.rf %>% select(-went_on_backorder)
output.rf <- train.rf$went_on_backorder 
train.rf_balanced <- ubUnder(input.rf, output.rf, perc = as.integer(runif(1, 45, 50)), method = "percPos", w = NULL)
training.rf <- bind_cols(as.tibble(train.rf_balanced$X), tibble(went_on_backorder = train.rf_balanced$Y))


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

#KKNN on Normal Dataset
knn.model<- kknn(went_on_backorder~., training, test, kernel="rectangular", k=1)
fit <- fitted(knn.model)
roc_measures(fit, test$went_on_backorder)

#        actual
#pred       0       1
#1       1128242    4226
#2        548325    7067
#[1] "Accuracy =  0.670949012358845 %"
#[1] "TPR =  0.672947755741345 %"
#[1] "TNR =  0.625785885061543 %"
#[1] "FPR =  0.374214114938457 %"
#[1] "FNR =  0.327052244258655 %"
#Area under the curve: 0.6494

#KKNN on PCA
knn.model<- kknn(went_on_backorder~., training.pca, test.pca, kernel="rectangular", k=1)
fit <- fitted(knn.model)
roc_measures(fit, test.pca$went_on_backorder)

#actual
#pred       0       1
#1      1134353    4706
#2       542214    6587
#[1] "Accuracy =  0.674853957081748 %"
#[1] "TPR =  0.676592704019583 %"
#[1] "TNR =  0.583281678916143 %"
#[1] "FPR =  0.416718321083857 %"
#[1] "FNR =  0.323407295980417 %"
#Area under the curve: 0.6299


#KKNN on RF
knn.model<- kknn(went_on_backorder~., training.rf, test.rf, kernel="rectangular", k=1)
fit <- fitted(knn.model)
roc_measures(fit, test.rf$went_on_backorder)

#        actual
#pred       0       1
#1       1218797    4008
#2        457770    7285
#[1] "Accuracy =  0.724470631450476 %"
#[1] "TPR =  0.726959912726422 %"
#[1] "TNR =  0.645089878685912 %"
#[1] "FPR =  0.354910121314088 %"
#[1] "FNR =  0.273040087273577 %"
#Area under the curve: 0.686


#CART on Normal Dataset
cart<-rpart(went_on_backorder~.,data=training)
rpart.plot(cart)
prediction<-predict(cart,test,type="class")
roc_measures(prediction, test$went_on_backorder)

#        actual
#pred       0       1
#1       1354626    1418
#2       321941    9875
#[1] "Accuracy =  0.803410235446068 %"
#[1] "TPR =  0.807976060604795 %"
#[1] "TNR =  0.874435491012131 %"
#[1] "FPR =  0.125564508987869 %"
#[1] "FNR =  0.192023939395205 %"
#Area under the curve: 0.8412


#CART on PCA
cart<-rpart(went_on_backorder~.,data=training.pca)
cart
rpart.plot(cart)
prediction<-predict(cart,test.pca,type="class")
roc_measures(prediction, test.pca$went_on_backorder)

#       actual
#pred       0       1
#1 1103839    2945
#2  572728    8348
#[1] "Accuracy =  0.655732110483097 %"
#[1] "TPR =  0.658392417362384 %"
#[1] "TNR =  0.739218985212078 %"
#[1] "FPR =  0.260781014787922 %"
#[1] "FNR =  0.341607582637616 %"
#Area under the curve: 0.6988

#CART on RF
cart<-rpart(went_on_backorder~.,data=training.rf)
rpart.plot(cart)
prediction<-predict(cart,test.rf,type="class")
roc_measures(prediction, test.rf$went_on_backorder)

#   actual
#pred       0       1
#1       1398962    1770
#2        277605    9523
#[1] "Accuracy =  0.829886364982878 %"
#[1] "TPR =  0.834420574900973 %"
#[1] "TNR =  0.843265739838838 %"
#[1] "FPR =  0.156734260161162 %"
#[1] "FNR =  0.165579425099027 %"
#Area under the curve: 0.8388

