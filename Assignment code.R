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

# glimpse(train_tbl)
introduce(train_tbl)
plot_intro(train_tbl)
plot_missing(train_tbl)
missing <- profile_missing(train_tbl)

non_missing <- missing %>% filter(pct_missing<0.5)













