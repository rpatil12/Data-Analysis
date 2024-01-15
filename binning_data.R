rm(list=ls())
###  Company    : Stevens 
#  Project    : KDDM Final Project
#  Purpose    : SVM, ANN
#  Date       : 11/27/2018
#  Comments   :

#### Install packages ####
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

#### Reading data ####
data <- read_csv("random_forest_output.csv")
for (i in 1:ncol(data)-1){
  column <- data[,i]
  v <- as.factor(unlist(column))
  if (nlevels(v)>3){
    v <- as.numeric(unlist(data[,i]))
    breaks <- unique(as.vector(unlist(quantile(v, probs = seq(0, 1, 0.1)))))
    if (NROW(breaks)<3){
      breaks <- quantile(breaks, probs = seq(0, 1, 0.5))
    }
    f <- cut(v,breaks = breaks ,labels = c(1:(NROW(breaks)-1)), include.lowest = TRUE)
    f <- unlist(f)
    data[,i] = f
  }
}

write.csv(data,"binned_data.csv", row.names=FALSE)

#### Subsetting data into train test ####
idx <- sample(nrow(data),as.integer(0.70*nrow(data)))
train <- data[idx,]
test <- data[-idx,]

#### Undersampling to balance the dataset ####
input  <- train %>% select(-went_on_backorder)
output <- train$went_on_backorder 
train_balanced <- ubUnder(input, output, perc = as.integer(runif(1, 45, 50)), method = "percPos", w = NULL)

# Recombine the synthetic balanced data
training <- bind_cols(as.tibble(train_balanced$X), tibble(went_on_backorder = train_balanced$Y))

# Inspect class balance after Undersampling
training$went_on_backorder %>% table() %>% prop.table()

#### SVM ####
# library(e1071)
# svm.model <- svm(went_on_backorder ~.,data=train[,-1])
# svm.pred <- predict(svm.model, test[,-1])
# 
# table(actual=test[,ncol(test)],svm.pred)
# svm_wrong <- (test$Class!=svm.pred)
# rate <- sum(svm_wrong)/length(svm_wrong)
# rate
