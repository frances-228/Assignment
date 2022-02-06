#-------- Delete all files from workspace
rm(list=ls())
gc()

library(readxl)
library(plyr)
library(data.table)
library(caret)
library(DataExplorer)
library(dplyr)

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

glimpse(train_sb_te)
corr_data <- train_sb_te[,-c("classe")]

M <- abs(cor(corr_data))
diag(M) <- 0
which(M>0.8,arr.ind = T)
summary(train_sb_te)


################################@
##### ___A: Data Preprocess #####
################################@

# Standardize the features 
preObj <- preProcess(train_sb_te[,-c("classe")],method = c("center","scale"))
trainData <- predict(preObj,train_sb_te[,-c("classe")])
plot_histogram(trainData)

# PCA with caret
preProc <- preProcess(trainData,method="pca",pcaComp = 24)
trainPC <- predict(preProc,trainData)

target <- train_sb_te[,c("classe")]
trainPC <- cbind(trainPC,target)
# glimpse(trainPC)

##############################@
##### ___B: Fit the model #####
##############################@
train_sb_te$classe <- factor(train_sb_te$classe)
trainPC$classe <- factor(trainPC$classe)

mod_gbm <- train(classe~., method="gbm",data=trainPC,verbose=FALSE)

mod_gbm_test <- train(classe~., method="gbm",data=train_sb_te,verbose=FALSE)
mod_gbm_test_pred <- predict(mod_gbm_test,train_sb_te[,-c("classe")])
mod_gbm_pred <- predict(mod_gbm_test,test_tbl)


confusionMatrix(data=mod_gbm_test_pred,reference=trainPC$classe)

summary(mod_gbm_test)




