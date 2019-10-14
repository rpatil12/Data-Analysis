rm(list=ls())
###  Company    : Stevens 
#  Project    : KDDM Final Project
#  Purpose    : Pre-processing
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
  ##Uncomment for removing NA values
  # data %>%
  #   drop_na(lead_time) %>%
  #   drop_na(perf_6_month_avg) %>%
  #   drop_na(perf_12_month_avg) %>%
  #   drop_na(national_inv)
  
  
  ##Uncomment for replacing NA values with mean value
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

data = preprocess(data)

#percentage of complete cases
data %>% complete.cases() %>% sum()/nrow(data)
data = preprocess2(data)
glimpse(data)

##Uncomment when removing NA values
#write.csv(data,"preprocessed_NA_removed_data.csv", row.names = FALSE)

##Uncomment when replacing NA values with mean value
write.csv(data,"preprocessed_NA_replaced_data.csv", row.names = FALSE)