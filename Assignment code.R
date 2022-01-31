#-------- Delete all files from workspace
rm(list=ls())
gc()

library(readxl)
library(dplyr)
library(data.table)
library(caret)
library(DataExplorer)

VarPathStore <- getwd()
VarPathSeparator <- "/"
train_tbl <- as.data.table(read.csv(file = paste(VarPathStore,"pml-training.csv", sep=VarPathSeparator)))
test_tbl <- as.data.table(read.csv(file = paste(VarPathStore, "pml-testing.csv", sep = VarPathSeparator)))

train_tbl <- train_tbl %>% na_if("#DIV/0!")
train_tbl <- train_tbl %>% na_if("")
train_tbl <- setDT(train_tbl)
glimpse(train_tbl)

var_category <- c("kurtosis_roll_belt"
                 ,"kurtosis_picth_belt"
                 ,"kurtosis_yaw_belt"
                 ,"skewness_roll_belt"
                 ,"skewness_roll_belt.1"
                 ,"skewness_yaw_belt"
                 ,"max_yaw_belt"
                 ,"min_yaw_belt"
                 ,"amplitude_yaw_forearm"
                 ,"kurtosis_roll_arm"
                 ,"kurtosis_picth_arm"
                 ,"kurtosis_yaw_arm"
                 ,"skewness_roll_arm"
                 ,"skewness_pitch_arm"
                 ,"skewness_yaw_arm"
                 ,"kurtosis_roll_dumbbell"
                 ,"kurtosis_picth_dumbbell"
                 ,"kurtosis_yaw_dumbbell"
                 ,"skewness_roll_dumbbell"
                 ,"skewness_pitch_dumbbell"
                 ,"skewness_yaw_dumbbell"
                 ,"max_yaw_dumbbell"
                 ,"min_yaw_dumbbell"
                 ,"amplitude_yaw_dumbbell"
                 ,"kurtosis_roll_forearm"
                 ,"kurtosis_picth_forearm"
                 ,"kurtosis_yaw_forearm"
                 ,"skewness_roll_forearm"
                 ,"skewness_pitch_forearm"
                 ,"skewness_yaw_forearm"
                 ,"max_yaw_forearm"
                 ,"min_yaw_forearm"
                 ,"amplitude_yaw_belt"
                 )

for (var in var_category){
  var_class <- train_tbl %>% pull(var) %>% class()
  if(!any(var_class %in% c("numeric"))){
    print(var_class)
    train_tbl[, c(var):=as.numeric(get(var))]
  }
}

# glimpse(train_tbl)
introduce(train_tbl)
plot_intro(train_tbl)
plot_missing(train_tbl)
missing <- profile_missing(train_tbl)


col_list <- missing %>% filter(pct_missing<0.5) %>% select(feature)
col_list <-as.vector(unlist(col_list$feature))

train_tbl <- setDT(train_tbl)
train_sb <- train_tbl[, ..col_list]

plot_histogram(train_sb)

remove_col<- c("new_window"
              ,"num_window"
              ,"X"
              ,"user_name"
              ,"raw_timestamp_part_1"
              ,"raw_timestamp_part_2"
              ,"cvtd_timestamp"
              ,"gyros_dumbbell_x"
              ,"gyros_dumbbell_y"
              ,"gyros_dumbbell_z"
              ,"gyros_forearm_y"
              ,"gyros_forearm_z"
              )

train_sb_te <- train_sb[, -..remove_col]

plot_histogram(train_sb_te)
plot_correlation(train_sb_te)



























