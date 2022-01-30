 
#####################################@
##### ___A: Set Up R Workspace #####
#####################################@

#-------- Delete all files from workspace
rm(list=ls())
gc()


#load libraries
library(magrittr)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(stringr)
library(stringi)
library(haven)


##-------  Global settings
#whether to keep expected financial of future months
keep_future_month = TRUE

#prefix of the RBP monitoring run
VarPrefix <- 'x202106'

#min/max dates of data to include
min_monitoring_month <- '2018-10-01'          #only include contracts and GL records after this date 
max_GL_month <- as.Date('2021-05-01')         #1 month time lag (only up to Jan 2021 in the March RBP run) (! the end date of actual OPEX need to be aligned with this !)
max_contract_month <- as.Date('2021-05-01')   #better to be one month before the max GL month


VarLocation <- "Azure"
VarPathSeparator <- "/"
if(VarLocation == "H"){   
  VarPathCode          <- 'H:/Risk Analytics/Pricing/Price Optimisation/Official Project/Monitoring/code/'
  VarPathData          <- 'H:/Risk Analytics/Pricing/Price Optimisation/Official Project/Monitoring/data/'}
if(VarLocation == "C"){   
  VarPathCode          <- 'C:/Users/TWU/OneDrive - TOYOTA FINANCE AUSTRALIA LTD/Work/Projects/2021-03-04 Contract Profitability Tracking/code/'
  VarPathData          <- 'C:/Users/TWU/OneDrive - TOYOTA FINANCE AUSTRALIA LTD/Work/Projects/2021-03-04 Contract Profitability Tracking/data/' 
}
if(VarLocation == "Azure"){   
  #VarPathCode          <- '/home/twu/monitoring_PROC/'
  VarPathData          <- '/azure/pxdisk/Monitoring/Data/' 
}

#set working folder
setwd(VarPathData)
getwd()

#disable scientific notation
options(scipen=999)



#####################################@
##### ___B: Load Contract details #####
#####################################@

#### Load Application/contract details from RBP run
load(file = paste0(VarPathData, 'Financial/', paste0(VarPrefix,"_data_fin.RData"))) #application details
Data_App <- copy(data_fin)
rm(data_fin)


#### Link any hardship contract back to their original contracts (old system - which requires unbook and rebook for hardship)
#Load the previous contract mapping data
data_previous <- as.data.table(read_sas(paste0(VarPathData, 'Extract/', paste0(VarPrefix,"_data_previous.sas7bdat")))) 

#load all application/contracts to identify hardship contracts 
load(file = paste0(VarPathData, 'Extract/', paste0(VarPrefix, "_data_new.RData"))) #application details
Data_Hardship <- copy(data_new[!is.na(ID_Contract_ID), .(ID_Contract_ID, I_Hardship, I_ResRewrites, X_Campaign_Type)])
rm(data_new)


#Tidy up the mapping data sets
data_previous[, ID_Contract_ID         :=as.integer(ID_Contract_ID)]
data_previous[, ID_Contract_Previous_ID:=as.integer(ID_Contract_Previous_ID)]
data_previous<-data_previous[ID_Contract_ID!=ID_Contract_Previous_ID,]

#Prepare for merging - this is the one that will be the right in each merge
setkey(data_previous, ID_Contract_ID)

#Set up the left in each merge, the master, with "Original", "Current" and a check to see when to break the loop, "Remaining_previous"
data_previous_master <- data_previous[!is.na(ID_Contract_ID),]
setnames(data_previous_master, "ID_Contract_ID", "Current")
remaining_previous<-data_previous_master[!is.na(ID_Contract_Previous_ID), .N]
data_previous_master[, Original:=ID_Contract_Previous_ID]

#Loop through checking for prior contracts
for(i in 1:10){
  #Merge on to see if the previous contract has a further previous contract
  setkey(data_previous_master, ID_Contract_Previous_ID)
  data_previous_master<-data_previous[data_previous_master]
  setnames(data_previous_master, "ID_Contract_ID",paste("Prev_",i,sep=""))
  
  #If there is a previous one, then make this one the "original" (at least until next loop)
  data_previous_master[!is.na(ID_Contract_Previous_ID), Original:=ID_Contract_Previous_ID]
  
  #Check how many "original's" still need to be checked if there's one further back. If this is less than last time, loop again.
  remaining<-data_previous_master[!is.na(ID_Contract_Previous_ID), .N]
  cat(paste("\n",remaining,sep=""))
  if(remaining==remaining_previous){
    data_previous_master[, ID_Contract_Previous_ID:=NULL]
    break
  } else {
    remaining_previous<-remaining
  }
}

#Clean up the mapping table
data_previous_master_clean<-data_previous_master[, c("Current", "Original"), with=FALSE]
setnames(data_previous_master_clean, "Current", "ID_Contract_ID")
setnames(data_previous_master_clean, "Original","ID_Original_Contract_ID")

#merge on hardship flag and keep hardship mapping only
setkey(data_previous_master_clean, ID_Contract_ID)
setkey(Data_Hardship, ID_Contract_ID)
data_previous_master_clean <- Data_Hardship[data_previous_master_clean]
data_previous_master_clean[ID_Original_Contract_ID>13000000,] %>% count(I_Hardship, I_ResRewrites, X_Campaign_Type)
Data_Hardship_Old_Mapping <- data_previous_master_clean[I_Hardship==TRUE, .(ID_Contract_ID, ID_Original_Contract_ID)]  #keep hardship mapping only, not variation or rewrite
rm(Data_Hardship, data_previous_master_clean)


#### Identify hardship contract (new system since Jun 2020 - which doesn't require unbook and rebook for hardship)
Data_Hardship_New <- as.data.table(read_sas(paste0(VarPathData, 'Extract/', paste0(VarPrefix, "_data_hardship_new.sas7bdat")))) 

##dedupe (duplicate hardship records with the same start period)
Data_Hardship_New <- Data_Hardship_New[order(ID_Contract_ID, Hardship_Start_MOB, desc(Z_Hardship_App_Date))]
Data_Hardship_New <- Data_Hardship_New[, .SD[1], by= .(ID_Contract_ID, Hardship_Start_MOB)]
Data_Hardship_New %>% count(ID_Contract_ID, Hardship_Start_MOB) %>% arrange(-n) 

##clean up hardship type
Data_Hardship_New %>% count(Z_Hardship_Type)
Data_Hardship_New[, Z_Hardship_Type := case_when(Z_Hardship_Type %in% c('Capped arrears', 'Cappedarrears')    ~ 'Capped arrears'
                                                 ,grepl('Moratorium', Z_Hardship_Type, ignore.case = T)        ~ 'Moratorium'
                                                 ,grepl('Payment reduction', Z_Hardship_Type, ignore.case = T) ~ 'Payment reduction'
                                                 ,TRUE                                                         ~ Z_Hardship_Type)]
Data_Hardship_New %>% count(Z_Hardship_Type)
Data_Hardship_New[ID_Contract_ID==13031043, ]



################################################################@
##### ___C01: Load GL transactions and derive summary data  #####
################################################################@

#### Load processed extraction from the RBP moniotring run
Data_GL <-as.data.table(read_sas(paste0(VarPathData, 'Extract/', paste0(VarPrefix,"_data_recon.sas7bdat")))) #Extracted GL (since 2016)
Data_GL[, ID_Contract_ID := as.integer(ID_Contract_ID)]
Data_GL[, X_DAF := NULL] #will get it from Commission table
# main PL items
setnames(Data_GL, 'X_GL_Recon_Month', 'GL_Month')
setnames(Data_GL, 'T_Written_Off', 'A_Net_Written_Off')
setnames(Data_GL, 'T_Interest', 'A_Interest')
setnames(Data_GL, 'T_Transfer_Price', 'A_Transfer_Price')
setnames(Data_GL, 'X_Finance_Commission', 'A_Amort_Commission_Upfront_exclGST')  #amortised commission in GL table is exclude GST
setnames(Data_GL, 'X_Subvention_Amount', 'A_Amort_Subvention')
setnames(Data_GL, 'T_Fee_Early_Termination', 'A_Fee_Early_Termination')
setnames(Data_GL, 'T_Fee_Establishment', 'A_Fee_Establishment')
setnames(Data_GL, 'T_Fee_Monthly', 'A_Fee_Monthly')
setnames(Data_GL, 'T_Expense_GFV_Option', 'A_Expense_GFV_Option')
# component details
setnames(Data_GL, 'T_Written_Off_Total', 'A_Written_Off_Total')
setnames(Data_GL, 'T_Written_Off_Recovery', 'A_WO_Recovery_beforeComm')
setnames(Data_GL, 'T_Interest_Contracted', 'A_Interest_Contracted')
setnames(Data_GL, 'T_Interest_Misc', 'A_Interest_Misc')

#formatting
Data_GL[,GL_Month:=as.Date(as.character(GL_Month))]
Data_GL[, Financial_Year := paste0('FY', ifelse(month(GL_Month)<=3, year(GL_Month), year(GL_Month)+1) %% 100)]
Data_GL %>% count(Financial_Year)


#### Derive activity counts for actual OPEX calculation (make sure to count the full financial years)
OPEX_Activity_Counts <- Data_GL[LoanCode %in% c('10', '38', '40', '48') & GL_Month>= '2018-04-01'
                                , .(Count_Admin_Month = sum(A_Fee_Monthly<0, na.rm = T)
                                    ,Count_Establishment = sum(A_Fee_Establishment<0, na.rm = T)
                                    ,Count_Early_Termination = sum(A_Fee_Early_Termination<0, na.rm = T)
                                ), by=.(Financial_Year, GL_Month)][order(Financial_Year, GL_Month)]


#### Derive write-off recovery amount for actual recovery adjustment (make sure to count the full financial years)
Recovery_Amount <- Data_GL[LoanCode %in% c('10', '38', '40', '48') & GL_Month>= '2018-04-01'
                           , .(Amt_WO_Recovery = sum(A_WO_Recovery_beforeComm, na.rm = T)
                           ), by=.(Financial_Year, GL_Month)][order(Financial_Year, GL_Month)]






################################################################@
##### ___C02: Prepare GL transactions for later calculation #####
################################################################@

#### Merge on Original contract ID for those hardship contracts (for the old system before 2020-06)
setkey(Data_GL, ID_Contract_ID)
setkey(Data_Hardship_Old_Mapping, ID_Contract_ID)
Data_GL <- Data_Hardship_Old_Mapping[Data_GL]
Data_GL[, ID_Original_Contract_ID := ifelse(!is.na(ID_Original_Contract_ID), ID_Original_Contract_ID, ID_Contract_ID)]

# mark hardship period in GL table 
Data_GL[, I_Hardship_period := ifelse(ID_Contract_ID != ID_Original_Contract_ID, 1, 0)] 
chk <- Data_GL[ID_Original_Contract_ID == 13373666, ] #example

# combine duplicate month records between original contract and current contract (for old hardship)
#Data_GL_original <- copy(Data_GL)
Data_GL <- Data_GL[, .(I_Hardship_period = max(I_Hardship_period)
                       ,A_Net_Written_Off = sum(A_Net_Written_Off, na.rm = T)
                       ,A_Interest = sum(A_Interest, na.rm = T)
                       ,A_Transfer_Price = sum(A_Transfer_Price, na.rm = T)
                       ,A_Amort_Commission_Upfront_exclGST = sum(A_Amort_Commission_Upfront_exclGST, na.rm = T)
                       ,A_Amort_Subvention = sum(A_Amort_Subvention, na.rm = T)
                       ,A_Fee_Early_Termination = sum(A_Fee_Early_Termination, na.rm = T)
                       ,A_Fee_Establishment = sum(A_Fee_Establishment, na.rm = T)
                       ,A_Fee_Monthly = sum(A_Fee_Monthly, na.rm = T)
                       ,A_Expense_GFV_Option = sum(A_Expense_GFV_Option, na.rm = T)
                       #detailed components 
                       ,A_Written_Off_Total = sum(A_Written_Off_Total, na.rm = T)
                       ,A_WO_Recovery_beforeComm = sum(A_WO_Recovery_beforeComm, na.rm = T)
                       ,A_Interest_Contracted = sum(A_Interest_Contracted, na.rm = T)
                       ,A_Interest_Misc = sum(A_Interest_Misc, na.rm = T)
                       ,Unknown = sum(Unknown, na.rm = T)), by=.(ID_Original_Contract_ID, GL_Month)]


#### in GL table, Mark the month-on-book (do it after combining duplicate month records)
Data_GL <- Data_GL[order(ID_Original_Contract_ID, GL_Month)]
Data_GL[, MOB := rowid(ID_Original_Contract_ID)-1] 

# mark the hardship change month
Data_GL[, temp := cumsum(I_Hardship_period), by='ID_Original_Contract_ID']
Data_GL[, Z_Hardship_Month_Flag := ifelse(temp==1, 1, 0)]
Data_GL[, temp := NULL]


#### Merge on new hardship information (for the new system since 2020-06)
setkey(Data_GL          , ID_Original_Contract_ID, MOB)
setkey(Data_Hardship_New, ID_Contract_ID         , Hardship_Start_MOB)
Data_GL <- Data_Hardship_New[Data_GL]
setnames(Data_GL, 'Hardship_Start_MOB', 'MOB')
setnames(Data_GL, 'Hardship_Length', 'Hardship_Length_new')
setnames(Data_GL, 'Z_Hardship_App_Date', 'Z_Hardship_App_Date_new')

# mark the hardship change month (new system)  #TODO: How to mark the new hardship period
Data_GL[, Z_Hardship_Month_Flag := ifelse(Z_Hardship_Month_Flag==0 & !is.na(Z_Hardship_Type), 1, Z_Hardship_Month_Flag)]
Data_GL[, Z_Hardship_Type := ifelse(Z_Hardship_Month_Flag==1 & is.na(Z_Hardship_Type), 'New Contract', Z_Hardship_Type)]
Data_GL[, .N, by=.(Z_Hardship_Month_Flag, Z_Hardship_Type)]
chk <- Data_GL[ID_Contract_ID==13486647,]


#### take contracts since FY2019 and before the reporting month (must not later than the max GL month)
Data_App <- Data_App[X_Loan_Date >= min_monitoring_month & X_Loan_Date < max_contract_month + months(1)
                     & !is.na(ID_Contract_ID),]
Data_GL <- Data_GL[ID_Contract_ID %in% Data_App$ID_Contract_ID]  # do it on original contract ID as the Data_App doesn't have hardship contracts

#### remove the latest month records (it could be incomplete)
if(is.null(max_GL_month)){
  max_GL_month<-Data_GL$GL_Month%>%max() - months(1)  #1 month time lag (only up to Jan 2021 in the March RBP run)
}
Data_GL <- Data_GL[GL_Month <= max_GL_month, ]


#### take a small sample for code debug
# Data_App <- Data_App[X_Loan_Date >= '2019-01-01' & X_Loan_Date < '2019-02-01'
#                      & L_Loan_Term_Num == 24,]
# Data_GL <- Data_GL[ID_Original_Contract_ID %in% Data_App$ID_Contract_ID]

#rm(Data_GL_original); gc()



###################################################@
##### ___E: Merge with model prediction data  #####
###################################################@

## Load the monthly expected financial at origination (from RBP run) for Post-2019
load(file= paste0(VarPathData, 'Scenarios/', paste0(VarPrefix,"_All_Apps, Recon_NoTU_Extend, L_Margin_Rate, Data.RData")))

## Keep the needed columns (data too large for local processing)
Model_Prediction_Month_Post19 <-
  data_fin_processed[!is.na(ID_Contract_ID) & X_Loan_Date>='2019-01-01'
                     , .(ID_Contract_ID, X_Loan_Date, Z_Month, Z_Month_Index, M_Balance, T_Balance
                         ,Z_Termination_Month_Flag_P_Correct, Z_Write_Off_Month_Flag_P_Correct, Z_Termination_Month_Flag, Z_Write_Off_Month_Flag
                         ,M_Interest_Accounting, M_Interest_Total, VarDealerCommission #, Z_Amortisation_Shaped
                         ,T_Fee_Establishment_Cash, T_Expense_Establishment
                         ,T_Interest_Accounting, T_Transfer_Price_Accounting, T_Fee_Monthly, T_Fee_Early_Termination
                         ,T_Written_Off, T_Expense_Monthly, T_Expense_Early_Termination, T_Expense_GFV_Option)]
rm(data_fin_processed);gc() #clean memory


## Load the monthly expected financial at origination (from RBP run) for Pre-2019
load(file= "/azure/pxdisk/Monitoring/Data/Scenarios/x202109_All_Apps, Recon_NoTU_Extend, L_Margin_Rate, Data.RData")

## Keep the needed columns (data too large for local processing)
Model_Prediction_Month_Pre19 <-
  data_fin_processed[!is.na(ID_Contract_ID) & X_Loan_Date<'2019-01-01'
                     , .(ID_Contract_ID, X_Loan_Date, Z_Month, Z_Month_Index, M_Balance, T_Balance
                         ,Z_Termination_Month_Flag_P_Correct, Z_Write_Off_Month_Flag_P_Correct, Z_Termination_Month_Flag, Z_Write_Off_Month_Flag
                         ,M_Interest_Accounting, M_Interest_Total, VarDealerCommission #, Z_Amortisation_Shaped
                         ,T_Fee_Establishment_Cash, T_Expense_Establishment
                         ,T_Interest_Accounting, T_Transfer_Price_Accounting, T_Fee_Monthly, T_Fee_Early_Termination
                         ,T_Written_Off, T_Expense_Monthly, T_Expense_Early_Termination, T_Expense_GFV_Option)]
rm(data_fin_processed);gc() #clean memory

Model_Prediction_Month <- rbind(Model_Prediction_Month_Pre19, Model_Prediction_Month_Post19)
rm(Model_Prediction_Month_Pre19, Model_Prediction_Month_Post19)

## Only keep contracts since FY2019
Model_Prediction_Month <- Model_Prediction_Month[!is.na(ID_Contract_ID) & X_Loan_Date >= min_monitoring_month,]

## save data for debug
# save(Model_Prediction_Month, file= paste0(VarPathData, 'Scenarios/', paste0(VarPrefix,"_Expected_Financial_Raw_Data_Monthly_since_FY19.RData")), compress=T)
# load(file= paste0(VarPathData, 'Scenarios/', paste0(VarPrefix,"_Expected_Financial_Raw_Data_Monthly_since_FY19.RData")))


#keep the same period as the actual if we don't need the future predictions
if(keep_future_month == FALSE){
  Model_Prediction_Month<-Model_Prediction_Month[Z_Month<=max_GL_month]
}

#keep contracts that are available in Actual financial table
Model_Prediction_Month <- Model_Prediction_Month[ID_Contract_ID %in% unique(Data_GL$ID_Contract_ID)] 

#check duplicate columns, only need to keep one as they are all from data_fin
Dup_Cols <- intersect(names(Model_Prediction_Month),names(Data_GL)) 
Dup_Cols <- setdiff(Dup_Cols,'ID_Contract_ID')  #should be no duplicate
Data_GL[,c(Dup_Cols):=NULL]

#merge actual financial on to model prediction (keep model predictions even if the contract actually terminated)
Data_GL[, Z_Actual_MOB_Max := max(MOB), by='ID_Contract_ID']  # up to when is the end of actual data
Data_AvE <- merge(Model_Prediction_Month, Data_GL, by.x=c('ID_Contract_ID','Z_Month_Index'), by.y = c('ID_Contract_ID','MOB'), all.x = T)
Data_AvE[, Z_Actual_MOB_Max := max(Z_Actual_MOB_Max, na.rm = T), by='ID_Contract_ID']  # fill the value to the extra expected P&L records

setnames(Data_AvE, 'Z_Month_Index', 'MOB')
rm(Data_GL, Model_Prediction_Month);gc()


#use schedule balance as the proxy of actual balance (so set 0 after termination)   ##!! Issue: Wrong for hardship !!
Data_AvE[, A_Balance_proxy := ifelse(is.na(A_Transfer_Price) | (A_Transfer_Price==0 & A_Interest==0 & A_Fee_Monthly==0), as.numeric(NA), M_Balance)] 

#expected probability of loan term till now (for expected ROA calculation during a period)
Data_AvE <- Data_AvE[order(ID_Contract_ID, MOB)]
Data_AvE[, Expected_Term_to_Date_P_cum := cumsum(Z_Termination_Month_Flag_P_Correct) + cumsum(Z_Write_Off_Month_Flag_P_Correct), by='ID_Contract_ID']
Data_AvE[, Expected_Term_to_Date_P := ifelse(MOB < max(MOB), Z_Termination_Month_Flag_P_Correct + Z_Write_Off_Month_Flag_P_Correct
                                             , 1-Expected_Term_to_Date_P_cum + Z_Termination_Month_Flag_P_Correct + Z_Write_Off_Month_Flag_P_Correct), by='ID_Contract_ID']
Data_AvE[, Expected_Term_to_Date_P_cum := NULL]


# #merge on some rolled-up contract expected financial 
# load(file = paste0(VarPathData, paste0(VarPrefix,"_All_Apps, Recon_NoTU_Extend, L_Margin_Rate, Data_Apps.RData"))) #application details
# Model_Prediction_contract <- data_fin_processed_apps
# rm(data_fin_processed_apps)
# setnames(Model_Prediction_contract, 'T_Balance', 'T_Balance_Total')
# setnames(Model_Prediction_contract, 'M_Balance', 'M_Balance_Total')
# setnames(Model_Prediction_contract, 'T_Interest_Accounting', 'T_Interest_Total')
# setkey(Data_AvE, ID_Contract_ID)
# setkey(Model_Prediction_contract, ID_Contract_ID)
# Data_AvE <- merge(Data_AvE
#                   , Model_Prediction_contract[, .(ID_Contract_ID, T_Balance_Total, M_Balance_Total, T_Interest_Total)]
#                   , by='ID_Contract_ID', all.x = TRUE)



###################################################@
##### ___C: Merge on App/Contract details  #####
###################################################@

cols_info <- c('ID_Application_ID', 'X_Loan_Date', 'L_Product_Type', 'X_Campaign_Type'
               , 'L_Finance_Rate', 'L_Finance_Rate_Dial_Down_Reducti', 'X_Subvention_Amount', 'X_Finance_Commission'
               , 'L_Net_Amount_Financed', 'L_Loan_Term_Num', 'L_Loan_Term_Num_Rounded', 'V_Make_Band', 'V_New_Used_Flag'
               , 'V_Vehicle_Age', 'C_C1_Age', 'Z_Log_C_C1_Licence_State', 'C_C1_Curr_Address_Ownership', 'Z_Log_L_Applicants_Joint'
               , 'D_Grade', 'E_Veda_Primary', 'H_C1_Prior_Loans_Count', 'X_Dealer_MetroRural')
cols_add <- setdiff(cols_info, colnames(Data_AvE))
Data_AvE <- merge(Data_AvE, Data_App[, c('ID_Contract_ID', cols_add), with=FALSE]
                  , by = 'ID_Contract_ID', all.x = TRUE)


# update the new loan term due to hardship adjustment
Data_AvE[, L_Loan_Term_New := max(coalesce(L_Loan_Term_New, -1)), by='ID_Contract_ID']
Data_AvE[, L_Loan_Term_New := pmax(L_Loan_Term_New, L_Loan_Term_Num, na.rm = T)]



##################################################################@
##### ___D01: Prepare Dealer Commission Amount (Allocated)    #####
##################################################################@
# #### In case that the table was not updated when we extract it from SAS
# WIP <- odbc_connect_retry("WIP") 
# result <- dbSendQuery(WIP,paste0("select --top 100
#     *
# 		from [WIP].[dbo].[RFI_Commission_View]
#     where Commission_L_Commission_Month >= '2015-10-01'
#                                  "))
# 
# Data_Commission <- as.data.table(dbFetch(result))
# dbClearResult(result)
# dbDisconnect(WIP)
# #format
# Data_Commission[, ID_Contract_ID := as.integer(ID_Contract_ID)]


## Contract level allocated commission table (maintained by RF&I Retail Pricing, PB updated quarterly (Jan,Apr,Jul,Oct), HB monthly) 
#Data_Commission <- as.data.table(read_sas(paste0(VarPathData, 'Extract/', paste0(VarPrefix,"_data_commission.sas7bdat")))) #TODO: column name too long, get truncated 
load(paste0(VarPathData, 'Extract/', paste0(VarPrefix,"_Contract_Commission.RData")))  #extract again with R when commission data is not up to date.
#Data_Commission[Commission_L_Rem_Type == 'PB', max(Commission_L_Commission_Month)]


# #### take a small sample for code debug
# Data_Commission <- Data_Commission[ID_Contract_ID %in% Data_App$ID_Contract_ID] 

#transform to wide table
Data_Commission_wide <- dcast(Data_Commission, ID_Contract_ID ~ Commission_L_Rem_Type
                              , value.var = 'Commission_X_Rem_CashInclStickyGST'       #include the part of GST we cannot recover
                              , fun.agg = function(x) sum(x, na.rm = T))
colnames(Data_Commission_wide) <- paste0('X_', colnames(Data_Commission_wide))
setnames(Data_Commission_wide, 'X_ID_Contract_ID', 'ID_Contract_ID')


#check duplicate columns, keep the one from commission table
Dup_Cols <- intersect(names(Data_Commission_wide),names(Data_AvE)) 
Dup_Cols <- setdiff(Dup_Cols,'ID_Contract_ID')  #should be no duplicate
Data_AvE[,c(Dup_Cols):=NULL]

#### Merge on commission details
Data_AvE <- merge(Data_AvE, Data_Commission_wide
                  , by = 'ID_Contract_ID', all.x = TRUE)


#reconcile the GL with contract info
Data_App[ID_Contract_ID==13299507, X_Finance_Commission]                        #upfront commission in leastrac is exclude GST
Data_AvE[ID_Contract_ID==13299507, sum(A_Amort_Commission_Upfront_exclGST, na.rm = T)]  #amortized commission in GL is exclude GST
Data_Commission[ID_Contract_ID=='13299507', .(Commission_L_Rem_Type, Commission_X_Rem_CashExclGST, Commission_X_Rem_CashInclStickyGST)]

rm(Data_Commission, Data_Commission_wide)
gc()


###################################################################@
##### ___D02: Prepare Recovery Commission Amount (Pro rata)    #####
###################################################################@
#load the monthly recovery commission (manually sourced from SAP)
Actual_WO_Recovery_Comm <- as.data.table(read.csv(paste0(VarPathData, 'Extract/', 'Write_Off_Recovery_Commission_Monthly.csv')))
Actual_WO_Recovery_Comm[, Z_Month := dmy(as.character(Z_Month))]

#Combine with the recovery amount to derive ratio
Actual_WO_Recovery_Comm <- merge(Actual_WO_Recovery_Comm, Recovery_Amount, by.x='Z_Month', by.y = 'GL_Month', all.x = T)
Actual_WO_Recovery_Comm <- Actual_WO_Recovery_Comm[Z_Month <= max_GL_month, ]
Actual_WO_Recovery_Comm[, Amt_WO_Recovery := -Amt_WO_Recovery]
Actual_WO_Recovery_Comm[, A_Recovery_Comm_Ratio := A_WO_Recovery_Commission / Amt_WO_Recovery]
Actual_Recovery_Comm_ratio <- Actual_WO_Recovery_Comm[, .(A_Recovery_Comm_Ratio = sum(A_WO_Recovery_Commission) / sum(Amt_WO_Recovery)
                                                          ,A_WO_Recovery_Commission = sum(A_WO_Recovery_Commission)
                                                          ,Amt_WO_Recovery = sum(Amt_WO_Recovery)), by=Financial_Year]
Actual_Recovery_Comm_ratio

#Merge onto financial and  (FY-to-date numbers)
Data_AvE[, Financial_Year := paste0('FY', ifelse(month(Z_Month)<=3, year(Z_Month), year(Z_Month)+1) %% 100)]
Data_AvE <- merge(Data_AvE
                  , Actual_Recovery_Comm_ratio[, .(Financial_Year, A_Recovery_Comm_Ratio)]
                  , by = 'Financial_Year', all.x = TRUE)
#Data_AvE[,A_Recovery_Comm_Ratio := NULL]

#Adjust recovery amount to count for commission cost (GLCode: 53200, not in GL table)
Data_AvE[, A_WO_Recovery_afterComm := A_WO_Recovery_beforeComm * (1- coalesce(A_Recovery_Comm_Ratio,0))]
Data_AvE[, A_Net_Written_Off := A_Written_Off_Total + A_WO_Recovery_afterComm]


###########################################@
##### ___E: Prepare OPEX Allocation (Need to update the ratios in scripts after new year's OPEX review)  #####
###########################################@

## Total retail OPEX of each financial year
#manually updating file; from MA reports or Essbase system
Actual_OPEX_FY <- as.data.table(read.csv(paste0(VarPathData, 'Extract/', 'Actual OPEX Yearly.csv')))
Actual_OPEX_FY <- Actual_OPEX_FY[!is.na(FYTD_TFA_OPEX), ]

## split into retail OPEX with assumption (from previous OPEX review)
Actual_OPEX_FY[Financial_Year=='FY19', FYTD_Retail_OPEX := FYTD_TFA_OPEX * 0.558]
Actual_OPEX_FY[Financial_Year=='FY20', FYTD_Retail_OPEX := FYTD_TFA_OPEX * 0.558]
Actual_OPEX_FY[Financial_Year=='FY21', FYTD_Retail_OPEX := FYTD_TFA_OPEX * 0.651]
Actual_OPEX_FY[Financial_Year=='FY22', FYTD_Retail_OPEX := FYTD_TFA_OPEX * 0.561]

## split into Establishment OPEX, Admin OPEX and Early Termination OPEX (from previous OPEX review)
Actual_OPEX_FY[Financial_Year=='FY19', c('FYTD_OPEX_Establishment', 'FYTD_OPEX_Admin', 'FYTD_OPEX_ET')
               :=  .(0.63*FYTD_Retail_OPEX, 0.35*FYTD_Retail_OPEX, 0.02*FYTD_Retail_OPEX)]

Actual_OPEX_FY[Financial_Year=='FY20', c('FYTD_OPEX_Establishment', 'FYTD_OPEX_Admin', 'FYTD_OPEX_ET')
               :=  .(0.63*FYTD_Retail_OPEX, 0.35*FYTD_Retail_OPEX, 0.02*FYTD_Retail_OPEX)]

Actual_OPEX_FY[Financial_Year=='FY21', c('FYTD_OPEX_Establishment', 'FYTD_OPEX_Admin', 'FYTD_OPEX_ET')
               :=  .(0.49*FYTD_Retail_OPEX, 0.46*FYTD_Retail_OPEX, 0.05*FYTD_Retail_OPEX)]

Actual_OPEX_FY[Financial_Year=='FY22', c('FYTD_OPEX_Establishment', 'FYTD_OPEX_Admin', 'FYTD_OPEX_ET')
               :=  .(0.47*FYTD_Retail_OPEX, 0.47*FYTD_Retail_OPEX, 0.06*FYTD_Retail_OPEX)]

#### derive the per contract OPEXs for each month
# !!!  FY-to-date end month of the total actual need to be the same as the count  !!!
OPEX_Activity_Counts_FY <- OPEX_Activity_Counts[GL_Month <= max_GL_month, 
                                                .(Count_Establishment = sum(Count_Establishment)
                                                  ,Count_Admin_Month = sum(Count_Admin_Month)
                                                  ,Count_Early_Termination = sum(Count_Early_Termination))
                                                ,by=.(Financial_Year)]
Actual_OPEX_FY <- merge(Actual_OPEX_FY, OPEX_Activity_Counts_FY, by='Financial_Year', all.x = T)
Actual_OPEX_FY[, A_OPEX_Establishment := round(FYTD_OPEX_Establishment / Count_Establishment,2)]
Actual_OPEX_FY[, A_OPEX_Admin := round(FYTD_OPEX_Admin / Count_Admin_Month,2)]
Actual_OPEX_FY[, A_OPEX_Early_Termination := round(FYTD_OPEX_ET / Count_Early_Termination,2)]

## save actual OPEX
write.csv(Actual_OPEX_FY
          , file = paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Actual_Retail_OPEX.csv'), na='', row.names=F)
#load(file = paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Actual_Retail_OPEX.csv'))


#### merge the per Contract OPEX to contract monthly data
Data_AvE <- merge(Data_AvE, Actual_OPEX_FY[,.(Financial_Year, A_OPEX_Establishment, A_OPEX_Admin, A_OPEX_Early_Termination)]
                  ,by='Financial_Year', all.x = T)

# Establishment OPEX occur at contract start
Data_AvE[, A_OPEX_Establishment := ifelse(MOB==0, A_OPEX_Establishment, as.numeric(NA))]

# Early termination OPEX occur at early termination
Data_AvE[, MOB_Early_Termination := ifelse(!is.na(A_Fee_Early_Termination) & A_Fee_Early_Termination<0, MOB, -1)]
Data_AvE[, MOB_Early_Termination := max(MOB_Early_Termination, na.rm = TRUE), by='ID_Contract_ID']
Data_AvE[, MOB_Early_Termination := ifelse(MOB_Early_Termination==-1, as.numeric(NA), MOB_Early_Termination)]
Data_AvE[, A_OPEX_Early_Termination := ifelse(MOB==MOB_Early_Termination, A_OPEX_Early_Termination, as.numeric(NA))]

# Admin OPEX occur in contract active months
Data_AvE[,  A_OPEX_Admin := ifelse(!is.na(A_Balance_proxy), A_OPEX_Admin, as.numeric(NA))]



###############################################################@
##### ___F0: Recalculating the expected amortization shape #####
###############################################################@
# ## examples of different interest rate fields
# Data_App[!is.na(ID_Contract_ID) & X_Loan_Date>='2020-09-01', .(ID_Contract_ID, X_APRDISC, X_APRIDC, X_Finance_Rate, X_Transfer_Price, X_Finance_Commission, X_Subvention_Amount)]

## --------------------  use the fields derived from the default L_Finance_Rate  --------------- ##  ##issue: what if finance rate is 0%
## probabilities
Data_AvE[, Z_Terminate_Written_at := Z_Termination_Month_Flag_P_Correct + Z_Write_Off_Month_Flag_P_Correct]
Data_AvE[, Z_Terminate_Written_before  := cumsum(Z_Terminate_Written_at) - Z_Terminate_Written_at, by=.(ID_Contract_ID)]
Data_AvE[, Z_Terminate_Written_after := 1- Z_Terminate_Written_before - Z_Terminate_Written_at]

## schedule shape
Data_AvE[, M_Amort_Shape_if_term_before := 0]    #nothing to recognize if already terminated before this month
Data_AvE[, M_Amort_Shape_if_term_after := M_Interest_Accounting / M_Interest_Total]  #only recognize this month's proportion if survive through this month 
Data_AvE[, M_Amort_Shape_if_term_at := M_Amort_Shape_if_term_after + (1 - cumsum(M_Amort_Shape_if_term_after)), by=.(ID_Contract_ID)]  #recognize this month's proportion and also need to bring forward the rest if terminated in this month


## expected amortization shape
Data_AvE[, T_Amortisation_Shape_recal := M_Amort_Shape_if_term_before * Z_Terminate_Written_before
         + M_Amort_Shape_if_term_at * Z_Terminate_Written_at
         + M_Amort_Shape_if_term_after * Z_Terminate_Written_after]



##############################@
##### ___F1: Amortisation #####
##############################@

## Mark Amortization End Month (need to recognize all unrecognized income/expense)
#write-off
Data_AvE[, MOB_Write_Off := ifelse(!is.na(A_Net_Written_Off) & A_Net_Written_Off>0, MOB, 999)]
Data_AvE[, MOB_Write_Off := min(MOB_Write_Off, na.rm = TRUE), by='ID_Contract_ID']
Data_AvE[, MOB_Write_Off := ifelse(MOB_Write_Off==999, as.numeric(NA), MOB_Write_Off)]

#transfer price transaction become missing without any termination or write-off sign
Data_AvE[, MOB_TransferPrice_End := ifelse(!is.na(A_Transfer_Price) & A_Transfer_Price!=0, MOB, -1)]
Data_AvE[, MOB_TransferPrice_End := max(MOB_TransferPrice_End, na.rm = TRUE), by='ID_Contract_ID']
Data_AvE[, MOB_TransferPrice_End := ifelse(MOB_TransferPrice_End==Z_Actual_MOB_Max, as.numeric(NA), MOB_TransferPrice_End)] #max actual MOB has transfer price record, then not ended

#use month of early termination or wite-off if any; Otherwise, use max of transfer price month and loan term expiry month (after hardship adjustment)
Data_AvE[, MOB_Amotization_End_Interest := ifelse(!is.na(MOB_Early_Termination) | !is.na(MOB_Write_Off), pmin(MOB_Early_Termination, MOB_Write_Off, na.rm = T)
                                                  , pmin(MOB_TransferPrice_End, L_Loan_Term_New, na.rm = T))]   



####--------    amortisation shape    ------------####
## simplest assumption - flat amortization 
Data_AvE[, T_Amortisation_Shape_flat := 1 / L_Loan_Term_Num]  #use loan term set at origination, ignore hardship adjustment
Data_AvE[, T_Amortisation_Shape_flat := ifelse(MOB<L_Loan_Term_Num, T_Amortisation_Shape_flat, 0)]

## amortization shape in RBP financial model
# #1. take from the amortization in the financial model 
# setnames(Data_AvE,'Z_Amortisation_Shaped','T_Amortisation_Shape')
# 
# #2. amortization shape derived from expected interest                                 
# Data_AvE[, T_Amortisation_Shape_interest := T_Interest_Accounting / T_Interest_Total]  
# Data_AvE[, T_Amortisation_Shape_interest := ifelse(is.na(T_Interest_Total) | T_Interest_Total==0, as.numeric(NA)   # shape missing if no interest
#                                                    , T_Amortisation_Shape_interest)]  
# 
# #3. amortization shape derived from expected balance
# Data_AvE[, T_Amortisation_Shape_balance := T_Balance / T_Balance_Total]                   
# Data_AvE[, T_Amortisation_Shape_balance := ifelse(is.na(T_Balance_Total) | T_Balance_Total==0, as.numeric(NA)   # shape missing if no interest
#                                                    , T_Amortisation_Shape_balance)]       


## amortization shape from actual GL records
#1 derived from monthly interest income and total scheduled interest income (issue: the actual interest income includes the late payment penalty and related fees)
#assume no early termination
Data_AvE[, A_Amortisation_Shape_interest := A_Interest/(-M_Interest_Total)] #inconsistent sign with the actual income records
Data_AvE[, A_Amortisation_Shape_interest:= ifelse(is.na(M_Interest_Total) | M_Interest_Total==0, as.numeric(NA)   # shape missing if no interest income
                                                  , A_Amortisation_Shape_interest)] 

#adjustment at amortization end
Data_AvE[, A_Amortisation_Shape_interest_ITD := sum(ifelse(MOB< MOB_Amotization_End_Interest, A_Amortisation_Shape_interest, as.numeric(NA)), na.rm = T), by='ID_Contract_ID']
Data_AvE[, A_Unrecognized_perc_ITD := 1 - A_Amortisation_Shape_interest_ITD] #remaining proportion
Data_AvE[, A_Amortisation_Shape_interest := ifelse(MOB > Z_Actual_MOB_Max, as.numeric(NA),                             # future months
                                                   ifelse(MOB== MOB_Amotization_End_Interest, A_Unrecognized_perc_ITD, # at Amortization end
                                                          ifelse(MOB > MOB_Amotization_End_Interest, as.numeric(NA)    # after Amotization end
                                                                 , A_Amortisation_Shape_interest)))]


#2. derived from amortized finance commission 
Data_AvE[, A_Amortisation_Shape_commission := A_Amort_Commission_Upfront_exclGST / X_Finance_Commission]     #amortized commission in GL is exclude GST
Data_AvE[, A_Amortisation_Shape_commission:=ifelse(is.na(X_Finance_Commission) | X_Finance_Commission==0, as.numeric(NA)   # shape missing if no commission
                                                   , A_Amortisation_Shape_commission)] 


#3. derived from amortized subvention amount
Data_AvE[, A_Amortisation_Shape_subvention := A_Amort_Subvention / (-X_Subvention_Amount)]
Data_AvE[, A_Amortisation_Shape_subvention:=ifelse(is.na(X_Subvention_Amount) | X_Subvention_Amount==0, as.numeric(NA)   # shape missing if no subvention
                                                   , A_Amortisation_Shape_subvention)] 


#4. combine these shapes
#actual shape
Data_AvE[, A_Amortisation_Shape_name := ifelse(sum(!is.na(A_Amortisation_Shape_commission) & A_Amortisation_Shape_commission>0) > 0 , 'Commission'
                                               , ifelse(sum(!is.na(A_Amortisation_Shape_subvention) & A_Amortisation_Shape_subvention>0) > 0, 'Subvention'
                                                        , 'Interest'))
         ,by='ID_Contract_ID']   #prefer the shape derived from commission or subvention, otherwise take the proxy interest shape
Data_AvE[, A_Amortisation_Shape := ifelse(A_Amortisation_Shape_name=='Commission', A_Amortisation_Shape_commission
                                          ,ifelse(A_Amortisation_Shape_name=='Subvention', A_Amortisation_Shape_subvention, A_Amortisation_Shape_interest))]
Data_AvE[, .(cnt = sum(!is.na(A_Amortisation_Shape) & A_Amortisation_Shape>0)), by='ID_Contract_ID'][cnt==0, .N] #chk any contract with no amortization shape

#expected shape
Data_AvE[, T_Amortisation_Shape_final := ifelse(!is.na(L_Finance_Rate) & L_Finance_Rate>0, T_Amortisation_Shape_recal, T_Amortisation_Shape_flat)]


# #check
# Data_AvE[X_Campaign_Type=='Standard RBP',] %>% count(A_Amortisation_Shape_name, is.finite(A_Amortisation_Shape_interest), is.finite(A_Amortisation_Shape_commission), is.finite(A_Amortisation_Shape_subvention))
# Data_AvE[A_Amortisation_Shape_name == 'Interest' & is.finite(A_Amortisation_Shape_commission), .(ID_Contract_ID)] %>% unique
# Data_AvE[is.na(A_Amortisation_Shape_interest)& is.na(A_Amortisation_Shape_commission)& is.na(A_Amortisation_Shape_subvention), ID_Contract_ID] %>% unique
# Data_AvE[ID_Contract_ID == 13289789, .(ID_Contract_ID, Z_Month, MOB, A_Amortisation_Shape_interest, A_Amortisation_Shape_commission, A_Amortisation_Shape_subvention
#                                        , MOB_Early_Termination, Z_Termination_Month_Flag, Z_Write_Off_Month_Flag, A_Amort_Commission_Upfront_exclGST, L_Loan_Term_Num, X_Upfront
#                                        , A_Interest, M_Interest_Total, A_Unrecognized_perc_ITD)]


# #compare the shapes
# example <- Data_AvE[ID_Contract_ID == 13291496, .(ID_Contract_ID, GL_Month, MOB, MOB_Amotization_End_Interest, A_Amortisation_Shape_interest_ITD, A_Amortisation_Shape_interest, A_Amortisation_Shape_commission, A_Amortisation_Shape_subvention, T_Amortisation_Shape_recal)]
# write.csv(example, file='example of actual amortization shape.csv', row.names = F)
# 13291496 13289928  13373666

# 
# # individual contract example
# Data_AvE[X_Loan_Date_Month=='2019-01-01' & L_Loan_Term_New==60 & A_Amortisation_Shape_name=='Commission' & MOB_Amotization_End_Interest>24, .(ID_Contract_ID)]
# example <- Data_AvE[ID_Contract_ID == 13299001, .(ID_Contract_ID, Z_Month, MOB, L_Loan_Term_New, A_Amortisation_Shape_interest
#                                                   , A_Amortisation_Shape_subvention, A_Amortisation_Shape_commission
#                                                   , T_Amortisation_Shape_flat, T_Amortisation_Shape_recal
# )]
# write.csv(example, file='example of actual amortization shape v6.csv', row.names = F)
# 
# # example 2
# chk <- Data_AvE[ID_Contract_ID == 13291496, .(L_Loan_Term_Num, MOB, Z_Terminate_Written_at, Z_Terminate_Written_before, Z_Terminate_Written_after
#                                               , M_Amort_Shape_if_term_at, M_Amort_Shape_if_term_before, M_Amort_Shape_if_term_after, T_Amortisation_Shape_recal
#                                               , A_Amortisation_Shape_commission, A_Amortisation_Shape_interest)]
# Data_App[ID_Contract_ID == 13291496, .(ID_Contract_ID, X_APRDISC, X_APRIDC, X_Finance_Rate, X_Transfer_Price, X_Finance_Commission, X_Subvention_Amount)]
# 
# 
# # segment example
# example <- Data_AvE[X_Loan_Date_Month=='2019-01-01' & L_Loan_Term_New==48 & A_Amortisation_Shape_name=='Commission' #& !is.na(X_Finance_Commission)
#                     , .(A_Amortisation_Shape_interest = mean(A_Amortisation_Shape_interest, na.rm = T)
#                         , A_Amortisation_Shape_subvention = mean(A_Amortisation_Shape_subvention, na.rm = T)
#                         , A_Amortisation_Shape_commission = mean(A_Amortisation_Shape_commission, na.rm = T)
#                         , T_Amortisation_Shape_flat = mean(T_Amortisation_Shape_flat, na.rm = T)
#                         , T_Amortisation_Shape_recal = mean(T_Amortisation_Shape_recal, na.rm = T)
#                         ,n=.N
#                         ,A_Terminated = sum(Z_Termination_Month_Flag, na.rm = T)
#                         ,T_Terminated = sum(Z_Termination_Month_Flag_P_Correct, na.rm = T)
#                         ,A_Written_Off = sum(Z_Write_Off_Month_Flag, na.rm = T)
#                         ,T_Written_Off = sum(Z_Write_Off_Month_Flag_P_Correct, na.rm = T)
#                         ,A_Hardship = sum(Z_Hardship_Month_Flag, na.rm = T))
#                     ,by=.(X_Loan_Date_Month, Z_Month, MOB, L_Loan_Term_New)]
# write.csv(example, file='example of actual amortization shape segment v6.csv', row.names = F)
# 
# 
# example <- Data_AvE[X_Campaign_Type == 'Standard RBP'
#                     , .(A_Amortisation_Shape_interest = mean(A_Amortisation_Shape_interest, na.rm = T)
#                         , A_Amortisation_Shape_subvention = mean(A_Amortisation_Shape_subvention, na.rm = T)
#                         , A_Amortisation_Shape_commission = mean(A_Amortisation_Shape_commission, na.rm = T)
#                         , T_Amortisation_Shape_flat = mean(T_Amortisation_Shape_flat, na.rm = T)
#                         , T_Amortisation_Shape_recal = mean(T_Amortisation_Shape_recal, na.rm = T)
#                         ,n=.N
#                         ,A_Terminated = sum(Z_Termination_Month_Flag, na.rm = T)
#                         ,T_Terminated = sum(Z_Termination_Month_Flag_P_Correct, na.rm = T)
#                         ,A_Written_Off = sum(Z_Write_Off_Month_Flag, na.rm = T)
#                         ,T_Written_Off = sum(Z_Write_Off_Month_Flag_P_Correct, na.rm = T)
#                         ,A_Hardship = sum(Z_Hardship_Month_Flag, na.rm = T))
#                     ,by=.(X_Campaign_Type, MOB)]
# write.csv(example, file='example of actual amortization shape all RBP v6.csv', row.names = F)
# 


####--------    Apply Amortisation    ------------####
## Actual Upfront Finance Commission (add 2.5% to include the sticky GST)
Data_AvE[, A_Amort_Commission_Upfront := A_Amort_Commission_Upfront_exclGST *1.025]

## Actual Allocated dealer-level commissions (amount include sticky GST)
Data_AvE[, A_Amort_Commission_Allocated := (coalesce(X_HB,0) + coalesce(X_PB,0) + coalesce(X_SignOn,0) + coalesce(X_VB,0))*A_Amortisation_Shape]

## Actual Establishment Fee
Data_AvE[, A_Fee_Establishment:=ifelse(is.na(A_Fee_Establishment),0,A_Fee_Establishment)] #replace NAs with 0 
Data_AvE[, A_Amort_Fee_Establishment := min(A_Fee_Establishment,na.rm = T)*A_Amortisation_Shape, by='ID_Contract_ID'] #income, negative number

## Actual Establishment Expense
Data_AvE[, A_OPEX_Establishment:=ifelse(is.na(A_OPEX_Establishment),0,A_OPEX_Establishment)] #replace NAs with 0
Data_AvE[, A_Amort_OPEX_Establishment:= max(A_OPEX_Establishment,na.rm=T)*A_Amortisation_Shape, by='ID_Contract_ID'] #expense, positive number


## Expected Total Commission  #!!!! Issue: Model assumption doesn't count for dealer dialdown, so overestimate it for dialdowned contracts  !!!!
Data_AvE[, T_Total_Commission:=-VarDealerCommission*L_Net_Amount_Financed] #commission assumption in RBP Model
Data_AvE[, T_Total_Commission:=ifelse(is.na(T_Total_Commission),0,T_Total_Commission)] # Replace NAs with 0 (should not be any)
Data_AvE[, T_Amort_Commission_Total:=max(T_Total_Commission,na.rm=T) * T_Amortisation_Shape_final,by='ID_Contract_ID']

#if taking the actual commission as it's known soon after contract origination
Data_AvE[, T2_Amort_Commission_Total:= -(coalesce(X_Upfront, 0)+ coalesce(X_HB,0) + coalesce(X_PB,0) + coalesce(X_SignOn,0) + coalesce(X_VB,0))*T_Amortisation_Shape_final]


## Expected Subvention (total amount from contract actual info)
#Data_AvE[, T_Amort_Subvention := X_Subvention_Amount_Accounting] #amortized in financial model
Data_AvE[, X_Subvention_Amount:=ifelse(is.na(X_Subvention_Amount),0,X_Subvention_Amount)]
Data_AvE[, T_Amort_Subvention:= max(X_Subvention_Amount, na.rm = T) *T_Amortisation_Shape_final, by='ID_Contract_ID']

## Expected Establishment Fee
#Data_AvE[, T_Amort_Fee_Establishment := T_Fee_Establishment_Accounting] #amortized in financial model
Data_AvE[, T_Fee_Establishment_Cash:=ifelse(is.na(T_Fee_Establishment_Cash),0,T_Fee_Establishment_Cash)]
Data_AvE[, T_Amort_Fee_Establishment:=max(T_Fee_Establishment_Cash,na.rm = T)*T_Amortisation_Shape_final, by='ID_Contract_ID']

## Expected Establishment Expense
Data_AvE[, T_Expense_Establishment:=ifelse(is.na(T_Expense_Establishment),0,T_Expense_Establishment)]
Data_AvE[, T_Amort_Expense_Establishment:=min(T_Expense_Establishment,na.rm = T)*T_Amortisation_Shape_final, by='ID_Contract_ID']

# #check
# example <- Data_AvE[ID_Contract_ID == 13295509, .(ID_Contract_ID, MOB, Z_Hardship_Type, A_Transfer_Price, A_Amortisation_Shape, T_Amortisation_Shape_final, A_Amort_Commission_Upfront, A_Amort_Commission_Allocated
#                                        , A_Amort_Fee_Establishment, A_Amort_OPEX_Establishment, T_Amort_Commission_Total, T_Amort_Fee_Establishment, T_Amort_Expense_Establishment, T_Amort_Subvention)]
# write.csv(example, file='example of AvE amortization in ET v5.csv', row.names = F)



#################################################################@
##### ___G: calculate profitability metrics                  #####
#################################################################@

## Derive Profit Metrics - Actual 
Data_AvE[, A_Amort_Finance_Margin := coalesce(A_Interest,0) + coalesce(A_Transfer_Price,0) + coalesce(A_Amort_Commission_Upfront,0) 
         + coalesce(A_Amort_Commission_Allocated,0) + coalesce(A_Amort_Subvention,0)]
Data_AvE[, A_Amort_Net_Contribution := coalesce(A_Amort_Finance_Margin,0) + coalesce(A_Amort_Fee_Establishment,0) + coalesce(A_Fee_Monthly,0) 
         + coalesce(A_Fee_Early_Termination,0)]
Data_AvE[, A_Amort_Net_Profit := coalesce(A_Amort_Net_Contribution,0) + coalesce(A_Net_Written_Off,0) + coalesce(A_Amort_OPEX_Establishment,0) + coalesce(A_OPEX_Admin,0) 
         + coalesce(A_OPEX_Early_Termination,0)]

## Derive Profit Metrics - Expected  
Data_AvE[, T_Amort_Finance_Margin := coalesce(T_Interest_Accounting,0) + coalesce(T_Transfer_Price_Accounting,0) + coalesce(T_Amort_Commission_Total,0) 
         + coalesce(T_Amort_Subvention,0)]
Data_AvE[, T_Amort_Net_Contribution := coalesce(T_Amort_Finance_Margin,0) + coalesce(T_Amort_Fee_Establishment,0) + coalesce(T_Fee_Monthly,0) 
         + coalesce(T_Fee_Early_Termination,0)]
Data_AvE[, T_Amort_Net_Profit := coalesce(T_Amort_Net_Contribution,0) + coalesce(T_Written_Off, 0) + coalesce(T_Amort_Expense_Establishment,0) + coalesce(T_Expense_Monthly,0) 
         + coalesce(T_Expense_Early_Termination,0) 
         #+ coalesce(T_Expense_GFV_Option,0)   #GFV is not included for now. The actual GFV expense is not available. 
         ]

## Derive Profit Metrics - Expected (total commission from actual contract info)
Data_AvE[, T2_Amort_Finance_Margin := coalesce(T_Interest_Accounting,0) + coalesce(T_Transfer_Price_Accounting,0) + coalesce(T2_Amort_Commission_Total,0) 
         + coalesce(T_Amort_Subvention,0)]
Data_AvE[, T2_Amort_Net_Contribution := coalesce(T2_Amort_Finance_Margin,0) + coalesce(T_Amort_Fee_Establishment,0) + coalesce(T_Fee_Monthly,0) 
         + coalesce(T_Fee_Early_Termination,0)]
Data_AvE[, T2_Amort_Net_Profit := coalesce(T2_Amort_Net_Contribution,0) + coalesce(T_Written_Off, 0) + coalesce(T_Amort_Expense_Establishment,0) + coalesce(T_Expense_Monthly,0) 
         + coalesce(T_Expense_Early_Termination,0) 
         #+ coalesce(T_Expense_GFV_Option,0)   #GFV is not included for now. The actual GFV expense is not available. 
         ]


## some dummy columns
Data_AvE[, count_actual   := ifelse(!is.na(A_Balance_proxy), 1, 0)]
Data_AvE[, count_expected := 1] 
Data_AvE[, Z_Actual_PL_Month_Flag := ifelse(Z_Month <= max_GL_month, 1, 0)] 


#Keep columns
cols_info <- c(cols_info, 'X_Upfront', 'X_HB','X_PB','X_SignOn', 'X_VB', 'T_Total_Commission', 'L_Loan_Term_New')
cols_month_info <- c('MOB', 'Z_Month', 'Z_Actual_PL_Month_Flag', 'Z_Hardship_Month_Flag', 'Z_Hardship_Type', 'Hardship_Length_new', 'Z_Termination_Month_Flag', 'Z_Write_Off_Month_Flag', 'Z_Termination_Month_Flag_P_Correct', 'Z_Write_Off_Month_Flag_P_Correct'
                     ,'Expected_Term_to_Date_P' ,'MOB_Amotization_End_Interest', 'A_Amortisation_Shape_name', 'A_Amortisation_Shape', 'T_Amortisation_Shape_final')
cols_Actual <- c('A_Interest', 'A_Transfer_Price', 'A_Amort_Commission_Upfront', 'A_Amort_Commission_Allocated', 'A_Amort_Subvention', 'A_Amort_Fee_Establishment'
                 , 'A_Fee_Monthly', 'A_Fee_Early_Termination', 'A_Net_Written_Off', 'A_Amort_OPEX_Establishment', 'A_OPEX_Admin', 'A_OPEX_Early_Termination'
                 ,'A_Amort_Finance_Margin', 'A_Amort_Net_Contribution', 'A_Amort_Net_Profit', 'A_Balance_proxy')
cols_Expected <- c('T_Interest_Accounting', 'T_Transfer_Price_Accounting', 'T_Amort_Commission_Total', 'T_Amort_Subvention', 'T_Amort_Fee_Establishment'
                   , 'T_Fee_Monthly', 'T_Fee_Early_Termination', 'T_Written_Off', 'T_Amort_Expense_Establishment', 'T_Expense_Monthly', 'T_Expense_Early_Termination'
                   ,'T_Amort_Finance_Margin', 'T_Amort_Net_Contribution', 'T_Amort_Net_Profit', 'T_Balance', 'T2_Amort_Commission_Total', 'T2_Amort_Finance_Margin', 'T2_Amort_Net_Contribution', 'T2_Amort_Net_Profit')
cols_dummy <- c('count_actual', 'count_expected')
Data_AvE_slim <- Data_AvE[, c('ID_Contract_ID', cols_info, cols_month_info, cols_Actual, cols_Expected, cols_dummy), with=FALSE]

#save output
save(Data_AvE, max_GL_month
     , file = paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Actual and Expected Financial - contracts since 2019 v7.RData'),version = 2,compress = T)
#load(file = paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Actual and Expected Financial - contracts since 2019 v7.RData'))
# save(Data_AvE_slim, max_GL_month
#      , file = paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Actual and Expected Financial - contracts since 2019 - slim v7.RData'),version = 2,compress = T)
# load(file = paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Actual and Expected Financial - contracts since 2019 - slim v7.RData'))
# rm(Data_AvE);gc()

# Data_AvE_sample <- Data_AvE[X_Loan_Date>= '2019-04-01' & X_Loan_Date < '2019-07-01' & Z_Month <= max_GL_month, ]
# save(Data_AvE_sample, max_GL_month
#      , file = paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Actual and Expected Financial - sample.RData'),version = 2,compress = T)



#################################################################@
##### ___G: Summarize on the inception-to-date profitability #####
#################################################################@
#load(file = paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Actual and Expected Financial - contracts since 2019 v7.RData'))


# #sumarize to contract level (inception to date)  --- use total commission assumption
# Data_Contract_Profit_ITD <- Data_AvE[Z_Month <= max_GL_month
#                                      , .(MOB_Amotization_End_Interest = min(MOB_Amotization_End_Interest, na.rm = T)
#                                          ,Actual_Ended = max(Z_Termination_Month_Flag, Z_Write_Off_Month_Flag, na.rm = T)
#                                          ,Actual_Months_on_Book = sum(!is.na(A_Balance_proxy)) - 1
#                                          ,Actual_Finance_Margin = -sum(A_Amort_Finance_Margin, na.rm = T)
#                                          ,Actual_Net_Contribution = -sum(A_Amort_Net_Contribution, na.rm = T)
#                                          ,Actual_Net_Profit = -sum(A_Amort_Net_Profit, na.rm = T)
#                                          ,Actual_Interest = -sum(A_Interest, na.rm = T)
#                                          ,Actual_Transfer_Price = -sum(A_Transfer_Price, na.rm = T)
#                                          ,Actual_Commission_Amort = -sum(A_Amort_Commission_Upfront, na.rm = T) - sum(A_Amort_Commission_Allocated, na.rm = T)
#                                          ,Actual_Subvention_Amort = -sum(A_Amort_Subvention, na.rm = T)
#                                          ,Actual_Fee_Establishment_Amort = -sum(A_Amort_Fee_Establishment, na.rm = T)
#                                          ,Actual_Fee_Admin = -sum(A_Fee_Monthly, na.rm = T)
#                                          ,Actual_Fee_Early_Termination = -sum(A_Fee_Early_Termination, na.rm = T)
#                                          ,Actual_Net_Write_Off = -sum(A_Net_Written_Off, na.rm = T)
#                                          ,Actual_OPEX_Establishment_Amort = -sum(A_Amort_OPEX_Establishment, na.rm = T)
#                                          ,Actual_OPEX_Admin = -sum(A_OPEX_Admin, na.rm = T)
#                                          ,Actual_OPEX_Early_Termination = -sum(A_OPEX_Early_Termination, na.rm = T)
#                                          ,Actual_Avg_Balance = sum(A_Balance_proxy, na.rm = T) / sum(!is.na(A_Balance_proxy))
#                                          
#                                          ,Scheduled_Month_on_Book = sum(Z_Month <= max_GL_month, na.rm = T) - 1
#                                          #,Expected_Months_on_Book = sum(MOB*Expected_Term_to_Date_P, na.rm = T) #correct only when keeping all months
#                                          ,Expected_Finance_Margin = sum(T_Amort_Finance_Margin, na.rm = T)
#                                          ,Expected_Net_Contribution = sum(T_Amort_Net_Contribution, na.rm = T)
#                                          ,Expected_Net_Profit = sum(T_Amort_Net_Profit, na.rm = T)
#                                          ,Expected_Interest = sum(T_Interest_Accounting, na.rm = T)
#                                          ,Expected_Transfer_Price = sum(T_Transfer_Price_Accounting, na.rm = T)
#                                          ,Expected_Commission_Amort = sum(T_Amort_Commission_Total, na.rm = T)
#                                          ,Expected_Subvention_Amort = sum(T_Amort_Subvention, na.rm = T)
#                                          ,Expected_Fee_Establishment_Amort = sum(T_Amort_Fee_Establishment, na.rm = T)
#                                          ,Expected_Fee_Admin = sum(T_Fee_Monthly, na.rm = T)
#                                          ,Expected_Fee_Early_Termination = sum(T_Fee_Early_Termination, na.rm = T)
#                                          ,Expected_Net_Write_Off = sum(T_Written_Off, na.rm = T)
#                                          ,Expected_Expense_Establishment = sum(T_Amort_Expense_Establishment,na.rm = T)
#                                          ,Expected_Expense_Monthly = sum(T_Expense_Monthly,na.rm = T)
#                                          ,Expected_Expense_Early_Termination = sum(T_Expense_Early_Termination,na.rm = T)
#                                          ,Expected_Avg_Balance = mean(T_Balance, na.rm = T)
#                                          
#                                          ,Actual_Old_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type=='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T))
#                                          ,Actual_New_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type!='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T)) # a contract can go into harship multiple times
#                                          ,Actual_Termination_count = sum(Z_Termination_Month_Flag, na.rm = T)
#                                          ,Expected_Termination_count = sum(Z_Termination_Month_Flag_P_Correct, na.rm = T)
#                                          ,Actual_WriteOff_count = sum(Z_Write_Off_Month_Flag, na.rm = T)
#                                          ,Expected_WriteOff_count = sum(Z_Write_Off_Month_Flag_P_Correct, na.rm = T)
#                                      ), by=.(ID_Contract_ID, L_Loan_Term_Num, X_Loan_Date, L_Product_Type, X_Campaign_Type
#                                              ,V_Make_Band, D_Grade)]
# 
# Data_Contract_Profit_ITD[, Actual_ROA_ITD := round(Actual_Net_Profit / Actual_Avg_Balance, 5)]
# Data_Contract_Profit_ITD[, Actual_ROA_ITD_annualized := Actual_ROA_ITD / Actual_Months_on_Book * 12]  #might not make sense on terminated contracts
# Data_Contract_Profit_ITD[, Expected_ROA_ITD := round(Expected_Net_Profit / Expected_Avg_Balance, 5)]  #might not make sense on individual contract
# Data_Contract_Profit_ITD[, Expected_ROA_ITD_annualized := Expected_ROA_ITD / Scheduled_Month_on_Book * 12]
# 
# save(Data_Contract_Profit_ITD
#      ,file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Contract_Profit_ITD v7 - use comm assumption.RData'),version=2,compress = T)
# #load(file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Contract_Profit_ITD v7 - use comm assumption.RData'))

# Data_Contract_Profit_ITD[Actual_Ended | Actual_Months_on_Book < Scheduled_Month_on_Book, .(ID_Contract_ID, X_Loan_Month, Actual_Ended, Actual_Months_on_Book, Scheduled_Month_on_Book)]
# Data_AvE[ID_Contract_ID==13200620, .(MOB, Z_Month, A_Balance_proxy, A_Interest)]
# 


#sumarize to contract level (inception to date)  --------  using actual total commission for expected commission (more correct for contract analysis)
Data_Contract_Profit_ITD2 <- Data_AvE[Z_Month <= max_GL_month #& ID_Contract_ID=='13262747'
                                      , .(MOB_Amotization_End_Interest = min(MOB_Amotization_End_Interest, na.rm = T)
                                          ,Actual_Ended = max(Z_Termination_Month_Flag, Z_Write_Off_Month_Flag, na.rm = T)
                                          ,Actual_Months_on_Book = sum(!is.na(A_Balance_proxy)) - 1
                                          ,Actual_Finance_Margin = -sum(A_Amort_Finance_Margin, na.rm = T)
                                          ,Actual_Net_Contribution = -sum(A_Amort_Net_Contribution, na.rm = T)
                                          ,Actual_Net_Profit = -sum(A_Amort_Net_Profit, na.rm = T)
                                          ,Actual_Interest = -sum(A_Interest, na.rm = T)
                                          ,Actual_Transfer_Price = -sum(A_Transfer_Price, na.rm = T)
                                          ,Actual_Commission_Amort = -sum(A_Amort_Commission_Upfront, na.rm = T) - sum(A_Amort_Commission_Allocated, na.rm = T)
                                          ,Actual_Subvention_Amort = -sum(A_Amort_Subvention, na.rm = T)
                                          ,Actual_Fee_Establishment_Amort = -sum(A_Amort_Fee_Establishment, na.rm = T)
                                          ,Actual_Fee_Admin = -sum(A_Fee_Monthly, na.rm = T)
                                          ,Actual_Fee_Early_Termination = -sum(A_Fee_Early_Termination, na.rm = T)
                                          ,Actual_Net_Write_Off = -sum(A_Net_Written_Off, na.rm = T)
                                          ,Actual_OPEX_Establishment_Amort = -sum(A_Amort_OPEX_Establishment, na.rm = T)
                                          ,Actual_OPEX_Admin = -sum(A_OPEX_Admin, na.rm = T)
                                          ,Actual_OPEX_Early_Termination = -sum(A_OPEX_Early_Termination, na.rm = T)
                                          ,Actual_Avg_Balance = sum(A_Balance_proxy, na.rm = T) / sum(!is.na(A_Balance_proxy))
                                          
                                          ,Scheduled_Month_on_Book = sum(Z_Month <= max_GL_month, na.rm = T) - 1
                                          #,Expected_Months_on_Book = sum(MOB*Expected_Term_to_Date_P, na.rm = T) #correct only when keeping all months
                                          ,Expected_Finance_Margin = sum(T2_Amort_Finance_Margin, na.rm = T)
                                          ,Expected_Net_Contribution = sum(T2_Amort_Net_Contribution, na.rm = T)
                                          ,Expected_Net_Profit = sum(T2_Amort_Net_Profit, na.rm = T)
                                          ,Expected_Interest = sum(T_Interest_Accounting, na.rm = T)
                                          ,Expected_Transfer_Price = sum(T_Transfer_Price_Accounting, na.rm = T)
                                          ,Expected_Commission_Amort = sum(T2_Amort_Commission_Total, na.rm = T)
                                          ,Expected_Subvention_Amort = sum(T_Amort_Subvention, na.rm = T)
                                          ,Expected_Fee_Establishment_Amort = sum(T_Amort_Fee_Establishment, na.rm = T)
                                          ,Expected_Fee_Admin = sum(T_Fee_Monthly, na.rm = T)
                                          ,Expected_Fee_Early_Termination = sum(T_Fee_Early_Termination, na.rm = T)
                                          ,Expected_Net_Write_Off = sum(T_Written_Off, na.rm = T)
                                          ,Expected_Expense_Establishment = sum(T_Amort_Expense_Establishment,na.rm = T)
                                          ,Expected_Expense_Monthly = sum(T_Expense_Monthly,na.rm = T)
                                          ,Expected_Expense_Early_Termination = sum(T_Expense_Early_Termination,na.rm = T)
                                          ,Expected_Avg_Balance = mean(T_Balance, na.rm = T)
                                          
                                          ,Actual_Old_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type=='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T))
                                          ,Actual_New_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type!='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T)) # a contract can go into harship multiple times
                                          ,Actual_Termination_count = sum(Z_Termination_Month_Flag, na.rm = T)
                                          ,Expected_Termination_count = sum(Z_Termination_Month_Flag_P_Correct, na.rm = T)
                                          ,Actual_WriteOff_count = sum(Z_Write_Off_Month_Flag, na.rm = T)
                                          ,Expected_WriteOff_count = sum(Z_Write_Off_Month_Flag_P_Correct, na.rm = T)
                                      ), by=.(ID_Contract_ID, L_Loan_Term_Num, X_Loan_Date, L_Product_Type, X_Campaign_Type
                                              ,V_Make_Band, D_Grade)]

Data_Contract_Profit_ITD2[, Actual_ROA_ITD := round(Actual_Net_Profit / Actual_Avg_Balance, 5)]
Data_Contract_Profit_ITD2[, Actual_ROA_ITD_annualized := Actual_ROA_ITD / Actual_Months_on_Book * 12]  #might not make sense on terminated contracts
Data_Contract_Profit_ITD2[, Expected_ROA_ITD := round(Expected_Net_Profit / Expected_Avg_Balance, 5)]  #might not make sense on individual contract
Data_Contract_Profit_ITD2[, Expected_ROA_ITD_annualized := Expected_ROA_ITD / Scheduled_Month_on_Book * 12]

save(Data_Contract_Profit_ITD2
     ,file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Contract_Profit_ITD v7 - use comm actual.RData'),version=2,compress = T)
#load(file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Contract_Profit_ITD v7 - use comm actual.RData'))


# #stats by each Loan Origination Month
# Data_Contract_Profit_ITD[, X_Loan_Month := floor_date(X_Loan_Date, 'month')]
# Stats_Contract_Profit_ITD <- Data_Contract_Profit_ITD[ #L_Loan_Term_Num %in% c(12,24,36,48,60,72,84)
#   , .(N_Contract = .N
#       ,N_Ended = sum(Actual_Ended | Actual_Months_on_Book < Scheduled_Month_on_Book)
#       ,Scheduled_MOB = max(Scheduled_Month_on_Book, na.rm = T)
#       
#       ,Actual_Finance_Margin = sum(Actual_Finance_Margin, na.rm = T) 
#       ,Actual_Net_Contribution = sum(Actual_Net_Contribution, na.rm = T) 
#       ,Actual_Net_Profit = sum(Actual_Net_Profit, na.rm = T) 
#       ,Actual_Interest = sum(Actual_Interest, na.rm = T) 
#       ,Actual_Transfer_Price = sum(Actual_Transfer_Price, na.rm = T) 
#       ,Actual_Commission_Amort = sum(Actual_Commission_Amort, na.rm = T) 
#       ,Actual_Subvention_Amort = sum(Actual_Subvention_Amort, na.rm = T) 
#       ,Actual_Fee_Establishment_Amort = sum(Actual_Fee_Establishment_Amort, na.rm = T) 
#       ,Actual_Fee_Admin = sum(Actual_Fee_Admin, na.rm = T) 
#       ,Actual_Fee_Early_Termination = sum(Actual_Fee_Early_Termination, na.rm = T) 
#       ,Actual_Net_Write_Off = sum(Actual_Net_Write_Off, na.rm = T) 
#       ,Actual_OPEX_Establishment_Amort = sum(Actual_OPEX_Establishment_Amort, na.rm = T) 
#       ,Actual_OPEX_Admin = sum(Actual_OPEX_Admin, na.rm = T) 
#       ,Actual_OPEX_Early_Termination = sum(Actual_OPEX_Early_Termination, na.rm = T) 
#       ,Actual_Avg_Balance = sum(Actual_Avg_Balance, na.rm = T) 
#       ,Actual_ROA_ITD = weighted.mean(Actual_ROA_ITD, Actual_Avg_Balance, na.rm = T)
#       ,Actual_ROA_ITD_annualized = weighted.mean(Actual_ROA_ITD, Actual_Avg_Balance, na.rm = T) / max(Scheduled_Month_on_Book, na.rm = T) *12
#       
#       ,Expected_Finance_Margin = sum(Expected_Finance_Margin, na.rm = T) 
#       ,Expected_Net_Contribution = sum(Expected_Net_Contribution, na.rm = T) 
#       ,Expected_Net_Profit = sum(Expected_Net_Profit, na.rm = T) 
#       ,Expected_Interest = sum(Expected_Interest, na.rm = T) 
#       ,Expected_Transfer_Price = sum(Expected_Transfer_Price, na.rm = T) 
#       ,Expected_Commission_Amort = sum(Expected_Commission_Amort) 
#       ,Expected_Subvention_Amort = sum(Expected_Subvention_Amort, na.rm = T) 
#       ,Expected_Fee_Establishment_Amort = sum(Expected_Fee_Establishment_Amort, na.rm = T) 
#       ,Expected_Fee_Admin = sum(Expected_Fee_Admin, na.rm = T) 
#       ,Expected_Fee_Early_Termination = sum(Expected_Fee_Early_Termination, na.rm = T) 
#       ,Expected_Net_Write_Off = sum(Expected_Net_Write_Off, na.rm = T)
#       ,Expected_Expense_Establishment = sum(Expected_Expense_Establishment,na.rm = T) 
#       ,Expected_Expense_Monthly = sum(Expected_Expense_Monthly,na.rm = T) 
#       ,Expected_Expense_Early_Termination = sum(Expected_Expense_Early_Termination,na.rm = T) 
#       ,Expected_Avg_Balance = sum(Expected_Avg_Balance, na.rm = T) 
#       ,Expected_ROA_ITD = weighted.mean(Expected_ROA_ITD, Expected_Avg_Balance, na.rm = T)
#       ,Expected_ROA_ITD_annualized = weighted.mean(Expected_ROA_ITD, Expected_Avg_Balance, na.rm = T) / max(Scheduled_Month_on_Book, na.rm = T) *12
#       
#       ,Actual_Old_Hardship_count = sum(Actual_Old_Hardship_count, na.rm = T)
#       ,Actual_New_Hardship_count = sum(Actual_New_Hardship_count, na.rm = T)
#       ,Actual_Termination_count = sum(Actual_Termination_count, na.rm = T)
#       ,Expected_Termination_count = sum(Expected_Termination_count, na.rm = T)
#       ,Actual_WriteOff_count = sum(Actual_WriteOff_count, na.rm = T)
#       ,Expected_WriteOff_count = sum(Expected_WriteOff_count, na.rm = T)
#   ), by=.(X_Campaign_Type, X_Loan_Month)]
# #), by=.(X_Campaign_Type, X_Loan_Month, L_Loan_Term_Num)][order(X_Campaign_Type, X_Loan_Month, L_Loan_Term_Num)]
# 
# Stats_Contract_Profit_ITD_Std_RBP<-Stats_Contract_Profit_ITD[X_Campaign_Type=='Standard RBP']
# Stats_Contract_Profit_ITD_Std_Flex<-Stats_Contract_Profit_ITD[X_Campaign_Type=='Standard Flex']
# 
# write.csv(Stats_Contract_Profit_ITD_Std_RBP
#           ,file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Stats_Contract_Profit_ITD_Std_RBP v7.csv'),row.names = F,na='')



#stats by each Loan Origination Month
Data_Contract_Profit_ITD2[, X_Loan_Month := floor_date(X_Loan_Date, 'month')]
Stats_Contract_Profit_ITD2 <- Data_Contract_Profit_ITD2[ #L_Loan_Term_Num %in% c(12,24,36,48,60,72,84)
  , .(N_Contract = .N
      ,N_Ended = sum(Actual_Ended | Actual_Months_on_Book < Scheduled_Month_on_Book)
      ,Scheduled_MOB = max(Scheduled_Month_on_Book, na.rm = T)
      
      ,Actual_Finance_Margin = sum(Actual_Finance_Margin, na.rm = T) 
      ,Actual_Net_Contribution = sum(Actual_Net_Contribution, na.rm = T) 
      ,Actual_Net_Profit = sum(Actual_Net_Profit, na.rm = T) 
      ,Actual_Interest = sum(Actual_Interest, na.rm = T) 
      ,Actual_Transfer_Price = sum(Actual_Transfer_Price, na.rm = T) 
      ,Actual_Commission_Amort = sum(Actual_Commission_Amort, na.rm = T) 
      ,Actual_Subvention_Amort = sum(Actual_Subvention_Amort, na.rm = T) 
      ,Actual_Fee_Establishment_Amort = sum(Actual_Fee_Establishment_Amort, na.rm = T) 
      ,Actual_Fee_Admin = sum(Actual_Fee_Admin, na.rm = T) 
      ,Actual_Fee_Early_Termination = sum(Actual_Fee_Early_Termination, na.rm = T) 
      ,Actual_Net_Write_Off = sum(Actual_Net_Write_Off, na.rm = T) 
      ,Actual_OPEX_Establishment_Amort = sum(Actual_OPEX_Establishment_Amort, na.rm = T) 
      ,Actual_OPEX_Admin = sum(Actual_OPEX_Admin, na.rm = T) 
      ,Actual_OPEX_Early_Termination = sum(Actual_OPEX_Early_Termination, na.rm = T) 
      ,Actual_Avg_Balance = sum(Actual_Avg_Balance, na.rm = T) 
      ,Actual_ROA_ITD = weighted.mean(Actual_ROA_ITD, Actual_Avg_Balance, na.rm = T)
      ,Actual_ROA_ITD_annualized = weighted.mean(Actual_ROA_ITD, Actual_Avg_Balance, na.rm = T) / max(Scheduled_Month_on_Book, na.rm = T) *12
      
      ,Expected_Finance_Margin = sum(Expected_Finance_Margin, na.rm = T) 
      ,Expected_Net_Contribution = sum(Expected_Net_Contribution, na.rm = T) 
      ,Expected_Net_Profit = sum(Expected_Net_Profit, na.rm = T) 
      ,Expected_Interest = sum(Expected_Interest, na.rm = T) 
      ,Expected_Transfer_Price = sum(Expected_Transfer_Price, na.rm = T) 
      ,Expected_Commission_Amort = sum(Expected_Commission_Amort) 
      ,Expected_Subvention_Amort = sum(Expected_Subvention_Amort, na.rm = T) 
      ,Expected_Fee_Establishment_Amort = sum(Expected_Fee_Establishment_Amort, na.rm = T) 
      ,Expected_Fee_Admin = sum(Expected_Fee_Admin, na.rm = T) 
      ,Expected_Fee_Early_Termination = sum(Expected_Fee_Early_Termination, na.rm = T) 
      ,Expected_Net_Write_Off = sum(Expected_Net_Write_Off, na.rm = T)
      ,Expected_Expense_Establishment = sum(Expected_Expense_Establishment,na.rm = T) 
      ,Expected_Expense_Monthly = sum(Expected_Expense_Monthly,na.rm = T) 
      ,Expected_Expense_Early_Termination = sum(Expected_Expense_Early_Termination,na.rm = T) 
      ,Expected_Avg_Balance = sum(Expected_Avg_Balance, na.rm = T) 
      ,Expected_ROA_ITD = weighted.mean(Expected_ROA_ITD, Expected_Avg_Balance, na.rm = T)
      ,Expected_ROA_ITD_annualized = weighted.mean(Expected_ROA_ITD, Expected_Avg_Balance, na.rm = T) / max(Scheduled_Month_on_Book, na.rm = T) *12
      
      ,Actual_Old_Hardship_count = sum(Actual_Old_Hardship_count, na.rm = T)
      ,Actual_New_Hardship_count = sum(Actual_New_Hardship_count, na.rm = T)
      ,Actual_Termination_count = sum(Actual_Termination_count, na.rm = T)
      ,Expected_Termination_count = sum(Expected_Termination_count, na.rm = T)
      ,Actual_WriteOff_count = sum(Actual_WriteOff_count, na.rm = T)
      ,Expected_WriteOff_count = sum(Expected_WriteOff_count, na.rm = T)
  ), by=.(X_Campaign_Type, X_Loan_Month)]
#), by=.(X_Campaign_Type, X_Loan_Month, L_Loan_Term_Num)][order(X_Campaign_Type, X_Loan_Month, L_Loan_Term_Num)]

Stats_Contract_Profit_ITD_Std_RBP2 <-Stats_Contract_Profit_ITD2[X_Campaign_Type=='Standard RBP']
Stats_Contract_Profit_ITD_Std_Flex2 <-Stats_Contract_Profit_ITD2[X_Campaign_Type=='Standard Flex']


write.csv(Stats_Contract_Profit_ITD_Std_RBP2
          ,file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Stats_Contract_Profit_ITD_Std_RBP actualComm v7.csv'),row.names = F,na='')





###################################################################@
##### ___G2: Summarize monthly profit trend of a specific segment #####
###################################################################@
#basic stats on the FY cohorts
Data_AvE[, Financial_Year_Origination := paste0('FY', ifelse(month(X_Loan_Month)<=3, year(X_Loan_Month), year(X_Loan_Month)+1) %% 100)]
Data_AvE[X_Campaign_Type=='Standard RBP' & X_Loan_Month >= '2019-01-01'
         ,.(lifetime_Avg_Balance_Annualized = sum(T_Balance, na.rm = T)/12)
         ,by=.(Financial_Year_Origination)]


#sumarize to segment level
Data_AvE[, X_Loan_Month := floor_date(X_Loan_Date, 'month')]
Data_AvE[, V_New_Used_Flag2 := case_when(V_New_Used_Flag == 'U' ~ 'Used'
                                     ,TRUE                      ~ 'New/Demo')]
Data_AvE[, V_Make_Band2 := case_when(V_Make_Band %in% c('TOYOTA', 'LEXUS', 'MAZDA', 'HINO') ~ as.character(V_Make_Band)
                                     ,TRUE                                                  ~ 'Others')]
Data_AvE$V_Make_Band2 <-  factor(Data_AvE$V_Make_Band2, c('TOYOTA', 'LEXUS', 'MAZDA', 'HINO', 'Others'))
Data_AvE$L_Product_Type <- factor(Data_AvE$L_Product_Type, c('Consumer', 'SoleTrader', 'Company'))

# Stats_Contract_Profit_segment <- Data_AvE[X_Campaign_Type=='Standard RBP' 
#                                          & Z_Month <= max_GL_month       #no future month in summary
#                                          & X_Loan_Month >= '2019-01-01'  #contracts since 2019
#                                          ,.(Actual_Finance_Margin = -sum(A_Amort_Finance_Margin, na.rm = T)
#                                             ,Actual_Net_Contribution = -sum(A_Amort_Net_Contribution, na.rm = T)
#                                             ,Actual_Net_Profit = -sum(A_Amort_Net_Profit, na.rm = T)
#                                             ,Actual_Interest = -sum(A_Interest, na.rm = T)
#                                             ,Actual_Transfer_Price = -sum(A_Transfer_Price, na.rm = T)
#                                             ,Actual_Commission_Amort = -sum(A_Amort_Commission_Upfront, na.rm = T) - sum(A_Amort_Commission_Allocated, na.rm = T)
#                                             ,Actual_Subvention_Amort = -sum(A_Amort_Subvention, na.rm = T)
#                                             ,Actual_Fee_Establishment_Amort = -sum(A_Amort_Fee_Establishment, na.rm = T)
#                                             ,Actual_Fee_Admin = -sum(A_Fee_Monthly, na.rm = T)
#                                             ,Actual_Fee_Early_Termination = -sum(A_Fee_Early_Termination, na.rm = T)
#                                             ,Actual_Net_Write_Off = -sum(A_Net_Written_Off, na.rm = T)
#                                             ,Actual_OPEX_Establishment_Amort = -sum(A_Amort_OPEX_Establishment, na.rm = T)
#                                             ,Actual_OPEX_Admin = -sum(A_OPEX_Admin, na.rm = T)
#                                             ,Actual_OPEX_Early_Termination = -sum(A_OPEX_Early_Termination, na.rm = T)
#                                             ,Actual_Balance = sum(A_Balance_proxy, na.rm = T)
#                                             
#                                             ,Expected_Finance_Margin = sum(T_Amort_Finance_Margin, na.rm = T)
#                                             ,Expected_Net_Contribution = sum(T_Amort_Net_Contribution, na.rm = T)
#                                             ,Expected_Net_Profit = sum(T_Amort_Net_Profit, na.rm = T)
#                                             ,Expected_Interest = sum(T_Interest_Accounting, na.rm = T)
#                                             ,Expected_Transfer_Price = sum(T_Transfer_Price_Accounting, na.rm = T)
#                                             ,Expected_Commission_Amort = sum(T_Amort_Commission_Total, na.rm = T)
#                                             ,Expected_Subvention_Amort = sum(T_Amort_Subvention, na.rm = T)
#                                             ,Expected_Fee_Establishment_Amort = sum(T_Amort_Fee_Establishment, na.rm = T)
#                                             ,Expected_Fee_Admin = sum(T_Fee_Monthly, na.rm = T)
#                                             ,Expected_Fee_Early_Termination = sum(T_Fee_Early_Termination, na.rm = T)    
#                                             ,Expected_Net_Write_Off = sum(T_Written_Off, na.rm = T)
#                                             ,Expected_Expense_Establishment = sum(T_Amort_Expense_Establishment,na.rm = T)
#                                             ,Expected_Expense_Monthly = sum(T_Expense_Monthly,na.rm = T)
#                                             ,Expected_Expense_Early_Termination = sum(T_Expense_Early_Termination,na.rm = T)
#                                             ,Expected_Balance = sum(T_Balance, na.rm = T)
#                                             
#                                             ,Actual_Old_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type=='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T))
#                                             ,Actual_New_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type!='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T)) # a contract can go into harship multiple times
#                                             ,Actual_Termination_count = sum(Z_Termination_Month_Flag, na.rm = T)
#                                             ,Expected_Termination_count = sum(Z_Termination_Month_Flag_P_Correct, na.rm = T)
#                                             ,Actual_WriteOff_count = sum(Z_Write_Off_Month_Flag, na.rm = T)
#                                             ,Expected_WriteOff_count = sum(Z_Write_Off_Month_Flag_P_Correct, na.rm = T)
#                                             ,Actual_Active_count = sum(!is.na(A_Balance_proxy))
#                                          ), by=.(L_Product_Type, V_New_Used_Flag2, V_Make_Band2)][order(L_Product_Type, V_New_Used_Flag2, V_Make_Band2)]
# 
# 
# Stats_Contract_Profit_segment[, Actual_ROA:= round(Actual_Net_Profit / Actual_Balance * 12, 5)]  
# Stats_Contract_Profit_segment[, Expected_ROA:= round(Expected_Net_Profit / Expected_Balance * 12, 5)] 
# 
# write.csv(Stats_Contract_Profit_segment
#           ,file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Stats_Contract_Profit_Segments_Std_RBP v7.csv'),row.names = F,na='')


Stats_Contract_Profit_segment2 <- Data_AvE[X_Campaign_Type=='Standard RBP' 
                                          & Z_Month <= max_GL_month       #no future month in summary
                                          & X_Loan_Month >= '2019-01-01'  #contracts since 2019
                                          ,.(Actual_Finance_Margin = -sum(A_Amort_Finance_Margin, na.rm = T)
                                             ,Actual_Net_Contribution = -sum(A_Amort_Net_Contribution, na.rm = T)
                                             ,Actual_Net_Profit = -sum(A_Amort_Net_Profit, na.rm = T)
                                             ,Actual_Interest = -sum(A_Interest, na.rm = T)
                                             ,Actual_Transfer_Price = -sum(A_Transfer_Price, na.rm = T)
                                             ,Actual_Commission_Amort = -sum(A_Amort_Commission_Upfront, na.rm = T) - sum(A_Amort_Commission_Allocated, na.rm = T)
                                             ,Actual_Subvention_Amort = -sum(A_Amort_Subvention, na.rm = T)
                                             ,Actual_Fee_Establishment_Amort = -sum(A_Amort_Fee_Establishment, na.rm = T)
                                             ,Actual_Fee_Admin = -sum(A_Fee_Monthly, na.rm = T)
                                             ,Actual_Fee_Early_Termination = -sum(A_Fee_Early_Termination, na.rm = T)
                                             ,Actual_Net_Write_Off = -sum(A_Net_Written_Off, na.rm = T)
                                             ,Actual_OPEX_Establishment_Amort = -sum(A_Amort_OPEX_Establishment, na.rm = T)
                                             ,Actual_OPEX_Admin = -sum(A_OPEX_Admin, na.rm = T)
                                             ,Actual_OPEX_Early_Termination = -sum(A_OPEX_Early_Termination, na.rm = T)
                                             ,Actual_Balance = sum(A_Balance_proxy, na.rm = T)
                                             
                                             ,Expected_Finance_Margin = sum(T2_Amort_Finance_Margin, na.rm = T)
                                             ,Expected_Net_Contribution = sum(T2_Amort_Net_Contribution, na.rm = T)
                                             ,Expected_Net_Profit = sum(T2_Amort_Net_Profit, na.rm = T)
                                             ,Expected_Interest = sum(T_Interest_Accounting, na.rm = T)
                                             ,Expected_Transfer_Price = sum(T_Transfer_Price_Accounting, na.rm = T)
                                             ,Expected_Commission_Amort = sum(T2_Amort_Commission_Total, na.rm = T)
                                             ,Expected_Subvention_Amort = sum(T_Amort_Subvention, na.rm = T)
                                             ,Expected_Fee_Establishment_Amort = sum(T_Amort_Fee_Establishment, na.rm = T)
                                             ,Expected_Fee_Admin = sum(T_Fee_Monthly, na.rm = T)
                                             ,Expected_Fee_Early_Termination = sum(T_Fee_Early_Termination, na.rm = T)    
                                             ,Expected_Net_Write_Off = sum(T_Written_Off, na.rm = T)
                                             ,Expected_Expense_Establishment = sum(T_Amort_Expense_Establishment,na.rm = T)
                                             ,Expected_Expense_Monthly = sum(T_Expense_Monthly,na.rm = T)
                                             ,Expected_Expense_Early_Termination = sum(T_Expense_Early_Termination,na.rm = T)
                                             ,Expected_Balance = sum(T_Balance, na.rm = T)
                                             
                                             ,Actual_Old_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type=='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T))
                                             ,Actual_New_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type!='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T)) # a contract can go into harship multiple times
                                             ,Actual_Termination_count = sum(Z_Termination_Month_Flag, na.rm = T)
                                             ,Expected_Termination_count = sum(Z_Termination_Month_Flag_P_Correct, na.rm = T)
                                             ,Actual_WriteOff_count = sum(Z_Write_Off_Month_Flag, na.rm = T)
                                             ,Expected_WriteOff_count = sum(Z_Write_Off_Month_Flag_P_Correct, na.rm = T)
                                             ,Actual_Active_count = sum(!is.na(A_Balance_proxy))
                                          ), by=.(L_Product_Type, V_New_Used_Flag2, V_Make_Band2)][order(L_Product_Type, V_New_Used_Flag2, V_Make_Band2)]
                                          #)]


Stats_Contract_Profit_segment2[, Actual_ROA_annualized:= round(Actual_Net_Profit / Actual_Balance * 12, 5)]  
Stats_Contract_Profit_segment2[, Expected_ROA_annualized:= round(Expected_Net_Profit / Expected_Balance * 12, 5)] 

write.csv(Stats_Contract_Profit_segment2
          ,file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Stats_Contract_Profit_Segments_Std_RBP actualComm v7.csv'),row.names = F,na='')



# 
# #summary of a cohort
# Data_AvE[, X_Loan_Month := floor_date(X_Loan_Date, 'month')]
# Data_Contract_Profit_cohort <- Data_AvE[X_Campaign_Type=='Standard RBP' #& L_Loan_Term_Num==48 
#                                         & X_Loan_Month == '2019-04-01'
#                                         ,.(Actual_Finance_Margin = -sum(A_Amort_Finance_Margin, na.rm = T)
#                                            ,Actual_Net_Contribution = -sum(A_Amort_Net_Contribution, na.rm = T)
#                                            ,Actual_Net_Profit = -sum(A_Amort_Net_Profit, na.rm = T)
#                                            ,Actual_Interest = -sum(A_Interest, na.rm = T)
#                                            ,Actual_Transfer_Price = -sum(A_Transfer_Price, na.rm = T)
#                                            ,Actual_Commission_Amort = -sum(A_Amort_Commission_Upfront, na.rm = T) - sum(A_Amort_Commission_Allocated, na.rm = T)
#                                            ,Actual_Subvention_Amort = -sum(A_Amort_Subvention, na.rm = T)
#                                            ,Actual_Fee_Establishment_Amort = -sum(A_Amort_Fee_Establishment, na.rm = T)
#                                            ,Actual_Fee_Admin = -sum(A_Fee_Monthly, na.rm = T)
#                                            ,Actual_Fee_Early_Termination = -sum(A_Fee_Early_Termination, na.rm = T)
#                                            ,Actual_Net_Write_Off = -sum(A_Net_Written_Off, na.rm = T)
#                                            ,Actual_OPEX_Establishment_Amort = -sum(A_Amort_OPEX_Establishment, na.rm = T)
#                                            ,Actual_OPEX_Admin = -sum(A_OPEX_Admin, na.rm = T)
#                                            ,Actual_OPEX_Early_Termination = -sum(A_OPEX_Early_Termination, na.rm = T)
#                                            ,Actual_Balance = sum(A_Balance_proxy, na.rm = T)
#                                            
#                                            ,Expected_Finance_Margin = sum(T_Amort_Finance_Margin, na.rm = T)
#                                            ,Expected_Net_Contribution = sum(T_Amort_Net_Contribution, na.rm = T)
#                                            ,Expected_Net_Profit = sum(T_Amort_Net_Profit, na.rm = T)
#                                            ,Expected_Interest = sum(T_Interest_Accounting, na.rm = T)
#                                            ,Expected_Transfer_Price = sum(T_Transfer_Price_Accounting, na.rm = T)
#                                            ,Expected_Commission_Amort = sum(T_Amort_Commission_Total, na.rm = T)
#                                            ,Expected_Subvention_Amort = sum(T_Amort_Subvention, na.rm = T)
#                                            ,Expected_Fee_Establishment_Amort = sum(T_Amort_Fee_Establishment, na.rm = T)
#                                            ,Expected_Fee_Admin = sum(T_Fee_Monthly, na.rm = T)
#                                            ,Expected_Fee_Early_Termination = sum(T_Fee_Early_Termination, na.rm = T)    
#                                            ,Expected_Net_Write_Off = sum(T_Written_Off, na.rm = T)
#                                            ,Expected_Expense_Establishment = sum(T_Amort_Expense_Establishment,na.rm = T)
#                                            ,Expected_Expense_Monthly = sum(T_Expense_Monthly,na.rm = T)
#                                            ,Expected_Expense_Early_Termination = sum(T_Expense_Early_Termination,na.rm = T)
#                                            ,Expected_Balance = sum(T_Balance, na.rm = T)
#                                            
#                                            ,Actual_Old_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type=='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T))
#                                            ,Actual_New_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type!='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T)) # a contract can go into harship multiple times
#                                            ,Actual_Termination_count = sum(Z_Termination_Month_Flag, na.rm = T)
#                                            ,Expected_Termination_count = sum(Z_Termination_Month_Flag_P_Correct, na.rm = T)
#                                            ,Actual_WriteOff_count = sum(Z_Write_Off_Month_Flag, na.rm = T)
#                                            ,Expected_WriteOff_count = sum(Z_Write_Off_Month_Flag_P_Correct, na.rm = T)
#                                            ,Actual_Active_count = sum(!is.na(A_Balance_proxy))
#                                            #), by=.(X_Loan_Month, X_Campaign_Type, L_Loan_Term_Num, MOB)]
#                                         ), by=.(X_Loan_Month, X_Campaign_Type, Z_Month)]
# #), by=.(X_Campaign_Type, Z_Month)]
# 
# Data_Contract_Profit_cohort[, Actual_ROA:= round(Actual_Net_Profit / Actual_Balance, 5)]
# Data_Contract_Profit_cohort[, Actual_ROA_annualized := Actual_ROA * 12]  
# Data_Contract_Profit_cohort[, Expected_ROA:= round(Expected_Net_Profit / Expected_Balance, 5)] 
# Data_Contract_Profit_cohort[, Expected_ROA_annualized := Expected_ROA * 12]
# 
# ## cummulative financial
# Data_Contract_Profit_cohort[, Actual_Net_Profit_ITD := cumsum(Actual_Net_Profit)
#                             #, by=.(X_Loan_Month, X_Campaign_Type, L_Loan_Term_Num)]            
#                             , by=.(X_Loan_Month, X_Campaign_Type)]
# #, by=.(X_Campaign_Type)]
# 
# Data_Contract_Profit_cohort[, Expected_Net_Profit_ITD := cumsum(Expected_Net_Profit)
#                             #, by=.(X_Loan_Month, X_Campaign_Type, L_Loan_Term_Num)]            
#                             , by=.(X_Loan_Month, X_Campaign_Type)]
# #, by=.(X_Campaign_Type)]
# 
# write.csv(Data_Contract_Profit_cohort
#           ,file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Stats_Contract_Profit_Cohort_Std_RBP v7.csv'),row.names = F,na='')




## on a cohort
Data_AvE[, X_Loan_Month := floor_date(X_Loan_Date, 'month')]
Stats_Contract_Profit_FY20Cohort <- Data_AvE[X_Campaign_Type=='Standard RBP' 
                                           & Z_Month <= max_GL_month       #no future month in summary
                                           & X_Loan_Month >= '2019-04-01' & X_Loan_Month < '2020-04-01'
                                           ,.(Actual_Finance_Margin = -sum(A_Amort_Finance_Margin, na.rm = T)
                                              ,Actual_Net_Contribution = -sum(A_Amort_Net_Contribution, na.rm = T)
                                              ,Actual_Net_Profit = -sum(A_Amort_Net_Profit, na.rm = T)
                                              ,Actual_Interest = -sum(A_Interest, na.rm = T)
                                              ,Actual_Transfer_Price = -sum(A_Transfer_Price, na.rm = T)
                                              ,Actual_Commission_Amort = -sum(A_Amort_Commission_Upfront, na.rm = T) - sum(A_Amort_Commission_Allocated, na.rm = T)
                                              ,Actual_Subvention_Amort = -sum(A_Amort_Subvention, na.rm = T)
                                              ,Actual_Fee_Establishment_Amort = -sum(A_Amort_Fee_Establishment, na.rm = T)
                                              ,Actual_Fee_Admin = -sum(A_Fee_Monthly, na.rm = T)
                                              ,Actual_Fee_Early_Termination = -sum(A_Fee_Early_Termination, na.rm = T)
                                              ,Actual_Net_Write_Off = -sum(A_Net_Written_Off, na.rm = T)
                                              ,Actual_OPEX_Establishment_Amort = -sum(A_Amort_OPEX_Establishment, na.rm = T)
                                              ,Actual_OPEX_Admin = -sum(A_OPEX_Admin, na.rm = T)
                                              ,Actual_OPEX_Early_Termination = -sum(A_OPEX_Early_Termination, na.rm = T)
                                              ,Actual_Balance = sum(A_Balance_proxy, na.rm = T)
                                              
                                              ,Expected_Finance_Margin = sum(T2_Amort_Finance_Margin, na.rm = T)
                                              ,Expected_Net_Contribution = sum(T2_Amort_Net_Contribution, na.rm = T)
                                              ,Expected_Net_Profit = sum(T2_Amort_Net_Profit, na.rm = T)
                                              ,Expected_Interest = sum(T_Interest_Accounting, na.rm = T)
                                              ,Expected_Transfer_Price = sum(T_Transfer_Price_Accounting, na.rm = T)
                                              ,Expected_Commission_Amort = sum(T2_Amort_Commission_Total, na.rm = T)
                                              ,Expected_Subvention_Amort = sum(T_Amort_Subvention, na.rm = T)
                                              ,Expected_Fee_Establishment_Amort = sum(T_Amort_Fee_Establishment, na.rm = T)
                                              ,Expected_Fee_Admin = sum(T_Fee_Monthly, na.rm = T)
                                              ,Expected_Fee_Early_Termination = sum(T_Fee_Early_Termination, na.rm = T)    
                                              ,Expected_Net_Write_Off = sum(T_Written_Off, na.rm = T)
                                              ,Expected_Expense_Establishment = sum(T_Amort_Expense_Establishment,na.rm = T)
                                              ,Expected_Expense_Monthly = sum(T_Expense_Monthly,na.rm = T)
                                              ,Expected_Expense_Early_Termination = sum(T_Expense_Early_Termination,na.rm = T)
                                              ,Expected_Balance = sum(T_Balance, na.rm = T)
                                              
                                              ,Actual_Old_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type=='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T))
                                              ,Actual_New_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type!='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T)) # a contract can go into harship multiple times
                                              ,Actual_Termination_count = sum(Z_Termination_Month_Flag, na.rm = T)
                                              ,Expected_Termination_count = sum(Z_Termination_Month_Flag_P_Correct, na.rm = T)
                                              ,Actual_WriteOff_count = sum(Z_Write_Off_Month_Flag, na.rm = T)
                                              ,Expected_WriteOff_count = sum(Z_Write_Off_Month_Flag_P_Correct, na.rm = T)
                                              ,Actual_Active_count = sum(!is.na(A_Balance_proxy))
                                           )]


Stats_Contract_Profit_FY20Cohort[, Actual_ROA_annualized:= round(Actual_Net_Profit / Actual_Balance * 12, 5)]  
Stats_Contract_Profit_FY20Cohort[, Expected_ROA_annualized:= round(Expected_Net_Profit / Expected_Balance * 12, 5)] 














#################################################################@
##### ___G: Summarize on the calendar month #####
#################################################################@

##-----------------------------------  contracts since 2019-01   -------------------------------------------------##
# 
# #sumarize to Calendar Month
# stats_Contract_Profit_CalMonth <- Data_AvE[X_Loan_Date>='2019-01-01'
#                                            , .(
#                                              Actual_Finance_Margin = -sum(A_Amort_Finance_Margin, na.rm = T)
#                                              ,Actual_Net_Contribution = -sum(A_Amort_Net_Contribution, na.rm = T)
#                                              ,Actual_Net_Profit = -sum(A_Amort_Net_Profit, na.rm = T)
#                                              ,Actual_Interest = -sum(A_Interest, na.rm = T)
#                                              ,Actual_Transfer_Price = -sum(A_Transfer_Price, na.rm = T)
#                                              ,Actual_Commission_Amort = -sum(A_Amort_Commission_Upfront, na.rm = T) - sum(A_Amort_Commission_Allocated, na.rm = T)
#                                              ,Actual_Subvention_Amort = -sum(A_Amort_Subvention, na.rm = T)
#                                              ,Actual_Fee_Establishment_Amort = -sum(A_Amort_Fee_Establishment, na.rm = T)
#                                              ,Actual_Fee_Admin = -sum(A_Fee_Monthly, na.rm = T)
#                                              ,Actual_Fee_Early_Termination = -sum(A_Fee_Early_Termination, na.rm = T)
#                                              ,Actual_Net_Write_Off = -sum(A_Net_Written_Off, na.rm = T)
#                                              ,Actual_OPEX_Establishment_Amort = -sum(A_Amort_OPEX_Establishment, na.rm = T)
#                                              ,Actual_OPEX_Admin = -sum(A_OPEX_Admin, na.rm = T)
#                                              ,Actual_OPEX_Early_Termination = -sum(A_OPEX_Early_Termination, na.rm = T)
#                                              ,Actual_Balance = sum(A_Balance_proxy, na.rm = T)
#                                              
#                                              ,Expected_Finance_Margin = sum(T_Amort_Finance_Margin, na.rm = T)
#                                              ,Expected_Net_Contribution = sum(T_Amort_Net_Contribution, na.rm = T)
#                                              ,Expected_Net_Profit = sum(T_Amort_Net_Profit, na.rm = T)
#                                              ,Expected_Interest = sum(T_Interest_Accounting, na.rm = T)
#                                              ,Expected_Transfer_Price = sum(T_Transfer_Price_Accounting, na.rm = T)
#                                              ,Expected_Commission_Amort = sum(T_Amort_Commission_Total, na.rm = T)
#                                              ,Expected_Subvention_Amort = sum(T_Amort_Subvention, na.rm = T)
#                                              ,Expected_Fee_Establishment_Amort = sum(T_Amort_Fee_Establishment, na.rm = T)
#                                              ,Expected_Fee_Admin = sum(T_Fee_Monthly, na.rm = T)
#                                              ,Expected_Fee_Early_Termination = sum(T_Fee_Early_Termination, na.rm = T)    
#                                              ,Expected_Net_Write_Off = sum(T_Written_Off, na.rm = T)
#                                              ,Expected_Expense_Establishment = sum(T_Amort_Expense_Establishment,na.rm = T)
#                                              ,Expected_Expense_Monthly = sum(T_Expense_Monthly,na.rm = T)
#                                              ,Expected_Expense_Early_Termination = sum(T_Expense_Early_Termination,na.rm = T)
#                                              ,Expected_Balance = sum(T_Balance, na.rm = T)
#                                              
#                                              ,Actual_Old_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type=='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T))
#                                              ,Actual_New_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type!='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T)) # a contract can go into harship multiple times
#                                              ,Actual_Termination_count = sum(Z_Termination_Month_Flag, na.rm = T)
#                                              ,Expected_Termination_count = sum(Z_Termination_Month_Flag_P_Correct, na.rm = T)
#                                              ,Actual_WriteOff_count = sum(Z_Write_Off_Month_Flag, na.rm = T)
#                                              ,Expected_WriteOff_count = sum(Z_Write_Off_Month_Flag_P_Correct, na.rm = T)
#                                              ,Actual_Active_count = sum(!is.na(A_Balance_proxy))
#                                            ), by=.(X_Campaign_Type, Z_Month)]
# 
# stats_Contract_Profit_CalMonth[, Actual_ROA_month := round(Actual_Net_Profit / Actual_Balance, 6)]
# stats_Contract_Profit_CalMonth[, Actual_ROA_annualized := Actual_ROA_month*12]
# stats_Contract_Profit_CalMonth[, Expected_ROA_month := round(Expected_Net_Profit / Expected_Balance, 6)]
# stats_Contract_Profit_CalMonth[, Expected_ROA_annualized := Expected_ROA_month*12]
# 
# stats_Contract_Profit_CalMonth_RBP<-stats_Contract_Profit_CalMonth[X_Campaign_Type=='Standard RBP']
# 
# save(stats_Contract_Profit_CalMonth
#      ,file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Contract_Profit_Calendar_Month v7.RData'),version=2,compress = T)
# write.csv(stats_Contract_Profit_CalMonth_RBP
#           ,file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Stats_Contract_Profit_CalendarMonth_RBP v7.csv'),row.names = F,na='')



#sumarize to Calendar Month
stats_Contract_Profit_CalMonth2 <- Data_AvE[X_Loan_Date>='2019-01-01'
                                           , .(
                                             Actual_Finance_Margin = -sum(A_Amort_Finance_Margin, na.rm = T)
                                             ,Actual_Net_Contribution = -sum(A_Amort_Net_Contribution, na.rm = T)
                                             ,Actual_Net_Profit = -sum(A_Amort_Net_Profit, na.rm = T)
                                             ,Actual_Interest = -sum(A_Interest, na.rm = T)
                                             ,Actual_Transfer_Price = -sum(A_Transfer_Price, na.rm = T)
                                             ,Actual_Commission_Amort = -sum(A_Amort_Commission_Upfront, na.rm = T) - sum(A_Amort_Commission_Allocated, na.rm = T)
                                             ,Actual_Subvention_Amort = -sum(A_Amort_Subvention, na.rm = T)
                                             ,Actual_Fee_Establishment_Amort = -sum(A_Amort_Fee_Establishment, na.rm = T)
                                             ,Actual_Fee_Admin = -sum(A_Fee_Monthly, na.rm = T)
                                             ,Actual_Fee_Early_Termination = -sum(A_Fee_Early_Termination, na.rm = T)
                                             ,Actual_Net_Write_Off = -sum(A_Net_Written_Off, na.rm = T)
                                             ,Actual_OPEX_Establishment_Amort = -sum(A_Amort_OPEX_Establishment, na.rm = T)
                                             ,Actual_OPEX_Admin = -sum(A_OPEX_Admin, na.rm = T)
                                             ,Actual_OPEX_Early_Termination = -sum(A_OPEX_Early_Termination, na.rm = T)
                                             ,Actual_Balance = sum(A_Balance_proxy, na.rm = T)
                                             
                                             ,Expected_Finance_Margin = sum(T2_Amort_Finance_Margin, na.rm = T)
                                             ,Expected_Net_Contribution = sum(T2_Amort_Net_Contribution, na.rm = T)
                                             ,Expected_Net_Profit = sum(T2_Amort_Net_Profit, na.rm = T)
                                             ,Expected_Interest = sum(T_Interest_Accounting, na.rm = T)
                                             ,Expected_Transfer_Price = sum(T_Transfer_Price_Accounting, na.rm = T)
                                             ,Expected_Commission_Amort = sum(T2_Amort_Commission_Total, na.rm = T)
                                             ,Expected_Subvention_Amort = sum(T_Amort_Subvention, na.rm = T)
                                             ,Expected_Fee_Establishment_Amort = sum(T_Amort_Fee_Establishment, na.rm = T)
                                             ,Expected_Fee_Admin = sum(T_Fee_Monthly, na.rm = T)
                                             ,Expected_Fee_Early_Termination = sum(T_Fee_Early_Termination, na.rm = T)    
                                             ,Expected_Net_Write_Off = sum(T_Written_Off, na.rm = T)
                                             ,Expected_Expense_Establishment = sum(T_Amort_Expense_Establishment,na.rm = T)
                                             ,Expected_Expense_Monthly = sum(T_Expense_Monthly,na.rm = T)
                                             ,Expected_Expense_Early_Termination = sum(T_Expense_Early_Termination,na.rm = T)
                                             ,Expected_Balance = sum(T_Balance, na.rm = T)
                                             
                                             ,Actual_Old_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type=='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T))
                                             ,Actual_New_Hardship_count = as.integer(sum(ifelse(Z_Hardship_Type!='New Contract', Z_Hardship_Month_Flag, 0), na.rm = T)) # a contract can go into harship multiple times
                                             ,Actual_Termination_count = sum(Z_Termination_Month_Flag, na.rm = T)
                                             ,Expected_Termination_count = sum(Z_Termination_Month_Flag_P_Correct, na.rm = T)
                                             ,Actual_WriteOff_count = sum(Z_Write_Off_Month_Flag, na.rm = T)
                                             ,Expected_WriteOff_count = sum(Z_Write_Off_Month_Flag_P_Correct, na.rm = T)
                                             ,Actual_Active_count = sum(!is.na(A_Balance_proxy))
                                           ), by=.(X_Campaign_Type, Z_Month)]

stats_Contract_Profit_CalMonth2[, Actual_ROA_month := round(Actual_Net_Profit / Actual_Balance, 6)]
stats_Contract_Profit_CalMonth2[, Actual_ROA_annualized := Actual_ROA_month*12]
stats_Contract_Profit_CalMonth2[, Expected_ROA_month := round(Expected_Net_Profit / Expected_Balance, 6)]
stats_Contract_Profit_CalMonth2[, Expected_ROA_annualized := Expected_ROA_month*12]

stats_Contract_Profit_CalMonth_RBP2 <-stats_Contract_Profit_CalMonth2[X_Campaign_Type=='Standard RBP']

save(stats_Contract_Profit_CalMonth2
     ,file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Contract_Profit_Calendar_Month actualComm v7.RData'),version=2,compress = T)
write.csv(stats_Contract_Profit_CalMonth_RBP2
          ,file=paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Stats_Contract_Profit_CalendarMonth_RBP actualComm v7.csv'),row.names = F,na='')






#################################################################@
##### ___F: Some case examples #####
#################################################################@
#load(file = paste0(VarPathData, 'Ad Hoc/2021-05 Financial AvE Monitoring/', VarPrefix, '_Actual and Expected Financial - contracts since 2019 v7.RData'))


# ## how a contract make a profit/loss
# Data_AvE[A_WO_Recovery_afterComm <0 & X_Campaign_Type=='Standard RBP' & MOB > 10
#          , .(ID_Contract_ID, L_Loan_Term_Num, Z_Month, MOB, A_Written_Off_Total, A_WO_Recovery_afterComm)]
# Data_AvE[ID_Contract_ID == 13435417, .(ID_Contract_ID, V_Make_Band, V_New_Used_Flag, L_Loan_Term_Num, L_Net_Amount_Financed, L_Finance_Rate, L_Finance_Rate_Dial_Down_Reducti, D_Grade)] %>% unique
# Data_AvE[ID_Contract_ID == 13289858, .(ID_Contract_ID, V_Make_Band, V_New_Used_Flag, L_Loan_Term_Num, L_Net_Amount_Financed, L_Finance_Rate, L_Finance_Rate_Dial_Down_Reducti, D_Grade)] %>% unique
# 
# 
# Data_AvE[, A_Commission_Total := coalesce(X_Upfront, 0) + coalesce(X_PB, 0) + coalesce(X_HB, 0) + coalesce(X_VB, 0) + coalesce(X_SignOn, 0)]
# Data_AvE[, A_Amort_Commission_Total := coalesce(A_Amort_Commission_Upfront,0) + coalesce(A_Amort_Commission_Allocated,0)]
# 
# #cash base - WO
# chk1 <- Data_AvE[ID_Contract_ID == 13435417, .(ID_Contract_ID, L_Net_Amount_Financed, Z_Month, MOB, A_Commission_Total, X_Subvention_Amount, A_Fee_Establishment
#                                               , A_OPEX_Establishment, A_Interest_Contracted, A_Interest_Misc, A_Transfer_Price
#                                               , A_Fee_Monthly, A_OPEX_Admin, A_Fee_Early_Termination, A_OPEX_Early_Termination, A_Written_Off_Total, A_WO_Recovery_afterComm)]
# #accounting base - WO
# chk2 <- Data_AvE[ID_Contract_ID == 13435417, .(ID_Contract_ID, L_Net_Amount_Financed, Z_Month, MOB, A_Amort_Commission_Total, A_Amort_Subvention, A_Amort_Fee_Establishment
#                                               , A_Amort_OPEX_Establishment, A_Interest_Contracted, A_Interest_Misc, A_Transfer_Price
#                                               , A_Fee_Monthly, A_OPEX_Admin, A_Fee_Early_Termination, A_OPEX_Early_Termination, A_Written_Off_Total, A_WO_Recovery_afterComm)]
# write.csv(chk1, file='example of contract monthly PL cash - bad debt v7.csv', row.names = F)
# write.csv(chk2, file='example of contract monthly PL accounting - bad debt v7.csv', row.names = F)
# 
# 
# #actual cash base - ET
# chk1 <- Data_AvE[ID_Contract_ID == 13289858, .(ID_Contract_ID, L_Net_Amount_Financed, Z_Month, MOB, A_Commission_Total, X_Subvention_Amount, A_Fee_Establishment
#                                                , A_OPEX_Establishment, A_Interest_Contracted, A_Interest_Misc, A_Transfer_Price
#                                                , A_Fee_Monthly, A_OPEX_Admin, A_Fee_Early_Termination, A_OPEX_Early_Termination, A_Written_Off_Total, A_WO_Recovery_afterComm)]
# #actual accounting base - ET
# chk2 <- Data_AvE[ID_Contract_ID == 13289858, .(ID_Contract_ID, L_Net_Amount_Financed, Z_Month, MOB, A_Amort_Commission_Total, A_Amort_Subvention, A_Amort_Fee_Establishment
#                                                , A_Amort_OPEX_Establishment, A_Interest_Contracted, A_Interest_Misc, A_Transfer_Price
#                                                , A_Fee_Monthly, A_OPEX_Admin, A_Fee_Early_Termination, A_OPEX_Early_Termination, A_Written_Off_Total, A_WO_Recovery_afterComm)]
# #expected accounting base
# chk3 <- Data_AvE[ID_Contract_ID == 13289858, .(ID_Contract_ID, L_Net_Amount_Financed, Z_Month, MOB, T_Amort_Commission_Total, T_Amort_Subvention, T_Amort_Fee_Establishment
#                                                , T_Amort_Expense_Establishment, T_Interest_Accounting, T_Transfer_Price_Accounting
#                                                , T_Fee_Monthly, T_Expense_Monthly, T_Fee_Early_Termination, T_Expense_Early_Termination, T_Written_Off, T_Amort_Net_Profit
#                                                , Z_Termination_Month_Flag_P_Correct, Z_Write_Off_Month_Flag_P_Correct)]
# write.csv(chk1, file='example of contract monthly PL cash - Early Terminate v7.csv', row.names = F)
# write.csv(chk2, file='example of contract monthly PL accounting - Early Terminate v7.csv', row.names = F)
# write.csv(chk3, file='example of contract monthly PL accounting expected - Early Terminate v7.csv', row.names = F)
# 
# 
# ## early termination
# Data_Contract_Profit_ITD[Actual_Fee_Early_Termination>0,][1:20, .(ID_Contract_ID, L_Loan_Term_Num, MOB_Amotization_End_Interest, Actual_Fee_Early_Termination)]
# example_ET <- Data_AvE_slim[ID_Contract_ID == 13289858, ]
# write.csv(example_ET, file='example - Early Termination.csv', row.names = F)
# 
# #amortisation
# Data_AvE[, X_Allocated_Commission := coalesce(X_PB,0) + coalesce(X_HB,0) + coalesce(X_SignOn,0) + coalesce(X_VB,0)]
# Data_AvE[X_Loan_Date_Month=='2020-04-01' & L_Loan_Term_New==48 & A_Amortisation_Shape_name=='Commission' & MOB_Amotization_End_Interest<12, .(ID_Contract_ID)]
# chk <- Data_AvE[ID_Contract_ID == 13438050, .(L_Loan_Term_Num, MOB, X_Upfront, A_Amort_Commission_Upfront, A_Amortisation_Shape_commission, X_Allocated_Commission
#                                               , A_Amort_Commission_Allocated, A_Fee_Establishment, A_Amort_Fee_Establishment, A_OPEX_Establishment
#                                               , A_Amort_OPEX_Establishment)]
# write.csv(chk, file='example of actual amortization shape ET v5.csv', row.names = F)
# 
# #expected value calculation
# Data_AvE[Z_Termination_Month_Flag==1 & MOB==12,][1:20, .(ID_Contract_ID, L_Loan_Term_Num, MOB)]
# chk <- Data_AvE[ID_Contract_ID == 13290392, .(L_Loan_Term_Num, MOB, M_Interest_Accounting, M_Interest_Accounting_Adj, M_Interest_Accounting_WO, M_Interest_Accounting_ET, T_Interest_Accounting
#                                               , Z_Termination_Month_Flag_P_Correct, Z_Write_Off_Month_Flag_P_Correct)]
# write.csv(chk, file='example of expected value calculation v5.csv', row.names = F)
# 
# 
# #expected amortisation calculation
# Data_AvE[L_Loan_Term_Num==48 & A_Amortisation_Shape_name=='Commission' & MOB==0 & X_Loan_Date_Month=='2019-01-01' & A_Interest< -150,][1:20, .(ID_Contract_ID, L_Loan_Term_Num, L_Finance_Rate)]
# chk <- Data_AvE[ID_Contract_ID == 13289832, .(L_Loan_Term_Num, MOB, M_Interest_Total, M_Interest_Accounting, Z_Terminate_Written_at, Z_Terminate_Written_before, Z_Terminate_Written_after
#                                               , M_Amort_Shape_if_term_at, M_Amort_Shape_if_term_before, M_Amort_Shape_if_term_after, T_Amortisation_Shape_recal
#                                               , T_Total_Commission, T_Amort_Commission_Total
#                                               , A_Amortisation_Shape_commission)]
# write.csv(chk, file='example of expected amortisation calculation v5.csv', row.names = F)
# 
# 
# 
# ## Write-off
# Data_Contract_Profit_ITD[Actual_Net_Write_Off!=0,] %>% count(Actual_Net_Write_Off>0)
# #negative net amount
# Data_Contract_Profit_ITD[Actual_Net_Write_Off<0,]$Actual_Net_Write_Off %>% sum()
# Data_Contract_Profit_ITD[Actual_Net_Write_Off<0,][1:10, .(ID_Contract_ID, L_Loan_Term_Num, MOB_Amotization_End_Interest, Actual_Net_Write_Off)]
# example_WO1 <- Data_AvE_slim[ID_Contract_ID == 13291578, ]write.csv(chk, file='example of actual amortization shape ET v5.csv', row.names = F)
# write.csv(example_WO1, file='example - Write-Off - negative net.csv', row.names = F)
# 
# #positive net amount
# Data_Contract_Profit_ITD[Actual_Net_Write_Off>0,]$Actual_Net_Write_Off %>% sum()
# Data_Contract_Profit_ITD[Actual_Net_Write_Off>0,][1:10, .(ID_Contract_ID, L_Loan_Term_Num, MOB_Amotization_End_Interest, Actual_Net_Write_Off)]
# Data_Contract_Profit_ITD[ID_Contract_ID == 13292660, ]
# example_WO2 <- Data_AvE_slim[ID_Contract_ID == 13292660, ]
# write.csv(example_WO2, file='example - Write-Off - positive net.csv', row.names = F)
# example_WO2[, .(ID_Contract_ID, X_Loan_Date, Z_Month, MOB, A_Interest, A_Transfer_Price, A_Amort_Commission_Upfront, A_Fee_Early_Termination, A_Net_Written_Off)]
# 
# 
# ## Harship
# #old system - contract ID changes and amortization ends with the original contract
# example_Hardship_old <- Data_AvE_slim[ID_Contract_ID == 13373666, ]
# write.csv(example_Hardship_old, file='example - Hardship - Old.csv', row.names = F)
# 
# #new system - keep the original contract going, no significant impact on the on-going GL records
# #payment reduction
# example_Hardship_new1 <- Data_AvE_slim[ID_Contract_ID == 13373666, ]
# write.csv(example_Hardship_new1, file='example - Hardship - New - Reduction.csv', row.names = F)
# 
# #Moratorium
# example_Hardship_new2 <- Data_AvE_slim[ID_Contract_ID == 13323889 ,]
# write.csv(example_Hardship_new2, file='example - Hardship - New - Hold.csv', row.names = F)
# 
# Data_Hardship_New[Contract_Number==13323889, ]
# 
# 
# #amortisation
# chk <- Data_AvE[ID_Contract_ID == 13373666, .(L_Loan_Term_Num, MOB, Z_Hardship_Type, A_Interest, A_Transfer_Price, A_Amort_Commission_Upfront
#                                               , A_Fee_Establishment, A_Amort_Fee_Establishment)]
# write.csv(chk, file='example of actual amortization shape hardship old v5.csv', row.names = F)
# 
# chk <- Data_AvE[ID_Contract_ID == 13323889, .(L_Loan_Term_Num, MOB, Z_Hardship_Type, A_Interest, A_Transfer_Price, A_Amort_Commission_Upfront
#                                               , A_Fee_Establishment, A_Amort_Fee_Establishment)]
# write.csv(chk, file='example of actual amortization shape hardship new v5.csv', row.names = F)
# 
# 
# #Write-off Recovery commission
# chk1 <- Data_AvE[A_WO_Recovery_afterComm < 0,.(ID_Contract_ID)] %>% count(ID_Contract_ID) %>% arrange(-n)
# chk <- Data_AvE[ID_Contract_ID == 13206263, .(ID_Contract_ID, Z_Month, MOB, A_Written_Off_Total, A_WO_Recovery_beforeComm, A_Recovery_Comm_Ratio, A_WO_Recovery_afterComm)]
# write.csv(chk, file='example of actual recovery commission adjustment v7.csv', row.names = F)
