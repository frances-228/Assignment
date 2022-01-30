#-------- Delete all files from workspace
rm(list=ls())
gc()

library(readxl)
library(dplyr)
library(data.table)
library(caret)

VarPathStore <- getwd()
VarPathSeparator <- "/"
train_tbl <- as.data.table(read.csv(file = paste(VarPathStore,"pml-training.csv", sep=VarPathSeparator)))
test_tbl <- as.data.table(read.csv(file = paste(VarPathStore, "pml-testing.csv", sep = VarPathSeparator)))
