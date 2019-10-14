rm(list=ls())
###  Company    : Stevens 
#  Project    : KDDM Final Project
#  Purpose    : Random Forest for feature selection
#  Group      : Hadia, Bhumika, Rucha, Lindsay
#  Id			    : 10440803
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

if(!require('randomForest')){
  install.packages("randomForest")
}
library(randomForest)

if(!require('ROCR')){
  install.packages("ROCR")
}
library(ROCR)

#### Combining training and test data files ####
data1 <- read_csv("training.csv")
data2 <- read_csv("test.csv")
raw_data <-rbind(data1,data2)
data <- raw_data

rm(data1)
rm(data2)

#class distribution
data$went_on_backorder %>% table() %>% prop.table()

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
  data %>%
   drop_na(lead_time) %>%
   drop_na(perf_6_month_avg) %>%
   drop_na(perf_12_month_avg)
  # mean_lead_time = as.integer(mean(data$lead_time,na.rm = TRUE))
  # mean_perf_6_month_avg <- mean(data$perf_6_month_avg, na.rm = TRUE)
  # mean_perf_6_month_avg <- round(mean_perf_6_month_avg, 2)
  # mean_perf_12_month_avg <- as.numeric(mean(data$perf_12_month_avg, na.rm = TRUE))
  # mean_perf_12_month_avg <- round(mean_perf_12_month_avg, 2)

  # data %>%
  #   drop_na(national_inv) %>%
  #   mutate(lead_time = ifelse(is.na(lead_time),mean_lead_time,lead_time)) %>%
  #   mutate(perf_6_month_avg = ifelse(is.na(perf_6_month_avg),mean_perf_6_month_avg,perf_6_month_avg)) %>%
  #   mutate(perf_12_month_avg = ifelse(is.na(perf_12_month_avg),mean_perf_12_month_avg,perf_12_month_avg))
}

data = preprocess(data)

#percentage of complete cases
data %>% complete.cases() %>% sum()/nrow(data)
data = preprocess2(data)
glimpse(data)

#### Subsetting data into train test ####
idx <- sample(nrow(data),as.integer(0.70*nrow(data)))
train <- data[idx,]
test <- data[-idx,]
# #subsetting traing into train validation
# idx <- sample(nrow(train),as.integer(0.90*nrow(train)))
# validation <- train[-idx,]
# train <- train[idx,]
# glimpse(validation)


#### Undersampling to balance the dataset ####
input  <- train %>% select(-went_on_backorder)
output <- train$went_on_backorder 
train_balanced <- ubUnder(input, output, perc = as.integer(runif(1, 45, 50)), method = "percPos", w = NULL)

# Recombine the synthetic balanced data
training <- bind_cols(as.tibble(train_balanced$X), tibble(went_on_backorder = train_balanced$Y))

# Inspect class balance after Undersampling
training$went_on_backorder %>% table() %>% prop.table()

#### RandomForest ####
##GRID SEARCH FOR RADNOM FOREST, RUN ONLY ONCE TO FIND THE OPTIMUM PARAMETERS

# err_rates <- rep(0, ncol(training)-1)
# for (i in 1:NROW(err_rates)) {
#   fit <- randomForest(went_on_backorder ~.,data=training, importance=TRUE,ntree=1000, mtry=i)
#   err = as.data.frame(fit$err.rate)
#   err = err$OOB
#   err_rates[i] = 100*err[NROW(err)]
#   rm(fit)
# }
# x = c(1:NROW(err_rates))
# y = err_rates
# # get the range for the x and y axis 
# xrange <- range(x) 
# yrange <- range(y) 
# 
# # set up the plot 
# plot(xrange, yrange, type="n", xlab="No. of variables tried at each split",
#      ylab="Out-of-Bag error" ) 
# lines(x, y, type="b", lwd=1.5,
#       lty=1, col=rainbow(1), 18) 
# title("OOB error versus no. of variables tried at each split ")
# var_err_rates <- err_rates
# n <- which.min(var_err_rates)
# indx <- 1
# max_trees <- 10000
# x <- seq(500,max_trees,by=500)
# err_rates <- rep(0, NROW(x))
# for (i in x) {
#   fit <- randomForest(went_on_backorder ~.,data=training, importance=TRUE,ntree=i, mtry=n)
#   err = as.data.frame(fit$err.rate)
#   err = err$OOB
#   err_rates[indx] = 100*err[NROW(err)]
#   indx = indx + 1
#   indx
#   rm(fit)
#   
# }
# frame()
# tree_err_rates <- err_rates
# y = tree_err_rates
# # get the range for the x and y axis 
# xrange <- range(x) 
# yrange <- range(y) 
# 
# # set up the plot 
# plot(xrange, yrange, type="n", xlab="Number of trees",
#      ylab="Out-of-Bag error" ) 
# lines(x, y, type="b", lwd=1.5,
#       lty=1, col=rainbow(2), 18) 
# title("OOB error versus no. of trees")
# m <- which.min(tree_err_rates)

#Final Random Forest model after finding the optimum hyperparameters
fit <- randomForest(went_on_backorder ~.,data=training, importance=TRUE,ntree=2000, mtry=7)
important_features <- as.data.frame(importance(fit))
frame()
varImpPlot(fit)
prediction <- predict(fit,test[,-ncol(test)])
table(actual=test$went_on_backorder, prediction)


pred <- prediction( prediction, test$went_on_backorder )

wrong <- (test$went_on_backorder != prediction)
error_rate <- sum(wrong)/length(wrong)
error_rate

#### Feature Selection based on Random Forest ####
important_features <- important_features[order(-important_features$MeanDecreaseGini),]
important_features <- important_features[important_features$MeanDecreaseGini>100,]
lbls <- unlist(labels(important_features)[1])
new_data <- select(data, lbls)
new_data <- cbind(new_data,data[,ncol(data)])
write.csv(new_data,file = "random_forest_output_NA_removed.csv", row.names=FALSE)

