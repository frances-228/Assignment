cat("\n\n##### E01 Financial Model #####\n")

###############################@
##### 01. Set Up Workspace #####
###############################@
cat(paste("\n",now(),"01. Set Up Workspace"))

#Filtering Function
mFilter <- function(dataTableName, Criteria)
{eval(parse(text = paste("m <- ", dataTableName, "[", Criteria ,", .N]")))
  eval(parse(text = paste("n <- ", dataTableName, "[, .N]")))  
  cat(paste("\n ",now()," ",m, " records of ", n, ", or ", round(m/n*100,digits=1),"% removed (",Criteria,")", sep = ""))
  eval(parse(text = paste(dataTableName," <- ", dataTableName, "[!(", Criteria ,"), ]")))
  return(eval(parse(text = dataTableName))) 
}

# Prepare the list of contracts we are especially interested in. This may be filtered on in the next step.
selectedContracts <- c('12824157','12824201','12824482','12827108','12834084','12834098','12883743','12824206','12873002'
                       ,'12824454','12899858','12928969','12824937','12824932','12824247','12824604','12824817','12873214'
                       ,'12840741','12840794','12841084','12845221','12872837','12873422','12872771','12872899','12333764'
                       ,'12333579'
)

# #Variables  ## replaced by an OPEX table already
# # Set up the standard fee and expense amounts. 
# VarMonthlyFee                       <-   6.00 #Pre 2016-05-05 was 5.00
# VarMonthlyExpense                   <-   6.55
# 
# VarEstablishmentFeeConsumer_Old     <- 350.00 #Pre 2016-08-03
# VarEstablishmentFeeConsumer_New     <- 375.00
# VarEstablishmentFeeCommercial_Old   <- 370.00 #Pre 2016-08-03 
# VarEstablishmentFeeCommercial_New   <- 385.00
# 
# VarEstablishmentExpense             <- 600.00
# 
# #Current Termination Fees
# VarEarlyTerminationConsumerBase     <- 750.00 
# VarEarlyTerminationCommercialFactor <-   0.30 #IE We charge 30% of unearned interest
# VarEarlyTerminationExpense          <-  39.55
# 
# #Proposed Termination Fees
# VarEarlyTerminationFactor_New       <-   0.20 #IE We will charge 20% of unearned interest
# VarEarlyTerminationCap_New          <-1500.00
# VarEarlyTerminationDischarge_New    <-  40.00
# 
# #GFV Costs. This is the 30 basis points we charge extra for GFV. 
# # It will be put in as a cost
# VarGFVOptionCost <- 0.3

# ##### Recovery Commission, proportion of Write Off Net LGD
# # "Pro Collect began in Jan 2014 and were on a lower commission than the other 3. We then removed Baycorp from receiving new referrals from August 2014."
# # Therefore we have used the average recovery commission as a proportion of Write Off + Recoveries, from 2014 onewards.
# # See the file "M:\Analysis\Recovery Commissions\Recovery Commissions.xlsb" for the calculation.
# VarRecoveryCommission <- 0.0307 #ie ~3%



#Set up the scenarios matrix
VarScenarioMatrix <- data.table(
  Scenario       = c("Recon_NoTU_NoGlobalAdj_NoTermAdj","Recon_NoTU_NoGlobalAdj", "Recon_NoTU","Recon_NoTU_Extend", "Recon" , "Recon_Extend" , "Forecast_NoTU", "Forecast", "Forecast2015", "Forecast_AssumeTU", "Forecast2019_AssumeTU"),
  TU             = c("Actual"                          ,"Actual"                , "Actual"    ,"Actual"           , "Model" , "Model"        , "Actual"       , "Model"   , "Model"       , "All"              , "All"                  ),
  EcoTPAdjust    = c(-1                                ,-1                      , -1          ,-1                 , -1      , -1             , 2016           , 2016      , 2015          , 2016               , 2019                   ),
  TermFeeAdjust  = c(FALSE                             ,FALSE                   , FALSE       ,FALSE              , FALSE   , FALSE          , FALSE          , FALSE     , FALSE         , FALSE              , FALSE                  ),
  OtherFeeAdjust = c(FALSE                             ,FALSE                   , FALSE       ,FALSE              , FALSE   , FALSE          , TRUE           , TRUE      , TRUE          , TRUE               , TRUE                   ),
  CountAdjust    = c(FALSE                             ,TRUE                    , TRUE        ,TRUE               , TRUE    , TRUE           , TRUE           , TRUE      , TRUE          , TRUE               , TRUE                   ),
  FinancialAdjust= c(FALSE                             ,FALSE                   , TRUE        ,TRUE               , TRUE    , TRUE           , TRUE           , TRUE      , TRUE          , TRUE               , TRUE                   ),
  Date           = c("AsData"                          ,"AsData"                , "AsData"    ,"All"              , "AsData", "All"          , "All"          , "All"     , "All"         , "All"              , "All"                  ))

#Check that valid selection has been made
if(!(VarScenario %in% VarScenarioMatrix[,Scenario])){stop("Invalid Scenario Selected.")}
if(!{VarScenario %in% c("Forecast_AssumeTU","Forecast2019_AssumeTU")} & VarMargin=="Goal_Seek" ){stop("Must have either Forecast_AssumeTU or Forecast2019_AssumeTU scenario if are running a Goal Seek.")}

#Set up the variables if we are to do a goal seek
VarProfitTargetVariable      <- "T_ROSABC" #The profit seeking target variable is always this.
VarProfitMarginsToTest       <- c(0,2,3,4,5,6,8,10,12,14,16,18,20,25,30) #The margins to test initially. Best to have these quite close so you don't miss shape.
VarProfitMarginDecimalPlaces <- 2     # How many places to return eg 3.41 is a valid margin
VarProfitRunWholeRange       <- FALSE # If you want to run the full From to To range then make this "TRUE", otherwise it will stop when the goal is hit
VarProfitPlusOrMinus         <- 0.01  # If the goal seeked one is plus or minus this, it will accept it

#Source the actual function where the work occurs
source(paste(VarPath, "Programs","D02 Financial Model Function.R", sep=VarPathSeparator)) 



#########################################@
###### 02. Load data, remove columns #####
#########################################@
cat(paste("\n",now(),"02. Load data, remove columns"))

#Load the data
if(VarFileName=="data_fin"){
  load(paste(VarPathStore,"Data","Financial",paste(VarPrefix,"_data_fin.RData",sep=""),sep=VarPathSeparator))
} else {
  load(paste(VarPathStore,"Data","Financial",paste(VarFileName,".RData",sep=""),sep=VarPathSeparator))
}

#OPEX table needs updates whenever the cost/fee assumptions change
OPEX_tbl <- as.data.table(read.csv(file = paste(VarPathStore,"Data","Standard","OPEX table.csv", sep=VarPathSeparator)))
OPEX_tbl[,StartDate:=as.Date(StartDate,format="%d/%m/%Y")]
OPEX_tbl[,EndDate:=as.Date(EndDate,format="%d/%m/%Y")]

data_fin[,Index:= case_when(X_Loan_Date>="2010-01-01" & X_Loan_Date<="2016-08-02" ~ 1,
                            X_Loan_Date>="2016-08-03" & X_Loan_Date<="2019-03-31" ~ 2, 
                            X_Loan_Date>="2019-04-01" & X_Loan_Date<="2020-03-31" ~ 3,
                            X_Loan_Date>="2020-04-01" & X_Loan_Date<="2020-06-30" ~ 4,
                            X_Loan_Date>="2020-07-01" & X_Loan_Date<="2021-03-31" ~ 5,
                            TRUE                                                  ~ 6)]
setkey(data_fin,"Index")
setkey(OPEX_tbl,"Index")
data_fin <- OPEX_tbl[data_fin]


#set Commission Assumption by Make of Truck or non-Truck
data_fin[, V_Make_full := as.factor(ifelse(is.na(V_Make_full), as.character(V_Make), as.character(V_Make_full)))]
data_fin[, V_Make_full := toupper(V_Make_full)]
data_fin[, VarDealerCommission := ifelse(V_Make_full %in% c('HINO', 'FREIGHTLINER', 'IVECO', 'KENWORTH',
                                                            'UD', 'YUTONG', 'MITSUBISHI FUSO')
                                         ,VarDealerCommission_Truck
                                         ,VarDealerCommission_Car)]
data_fin[, c('VarDealerCommission_Truck', 'VarDealerCommission_Car') := NULL]



# data_fin <- sqldf("select a.*, 
#                                      b.VarMonthlyFee, 
#                                      b.VarMonthlyExpense, 
#                                      b.VarEstablishmentFeeConsumer, 
#                                      b.VarEstablishmentFeeCommercial,
#                                      b.VarEstablishmentExpense,
#                                      b.VarEarlyTerminationConsumerBase,
#                                      b.VarEarlyTerminationCommercialFactor,
#                                      b.VarEarlyTerminationExpense,
#                                      b.VarEarlyTerminationFactor,
#                                      b.VarEarlyTerminationCap,
#                                      b.VarEarlyTerminationDischarge,
#                                      b.VarGFVOptionCost,
#                                      b.VarRecoveryCommission
# 
#                 from data_fin as a
#                 left join OPEX_tbl as b
#                 on (a.X_Loan_Date>=b.StartDate and a.X_Loan_Date<=b.EndDate)
#                 ")


#Remove variables that aren't used anywhere
data_fin[, c("C_A_C1_Total"
             ,"C_A_C2_Total"
             ,"C_A_CO_Total"
             ,"C_C1_Applicant_Entity"
             ,"C_C1_Curr_Address_Postcode"
             ,"C_C1_Curr_Address_SA1_07"
             ,"C_C1_Curr_Address_SA1_11"
             ,"C_C1_Prev_Employment_Status"
             ,"C_C1_Residential_Status"
             ,"C_C2_Age"
             ,"C_C2_Applicant_Entity"
             ,"C_C2_Curr_Address_Duration"
             ,"C_C2_Curr_Address_Ownership"
             ,"C_C2_Curr_Address_Postcode"
             ,"C_C2_Curr_Address_SA1_07"
             ,"C_C2_Curr_Address_SA1_11"
             ,"C_C2_Curr_Employment_Status"
             ,"C_C2_Dependants"
             ,"C_C2_Gender"
             ,"C_C2_Marital_Status"
             ,"C_C2_Mortgage_Rent_Num"
             ,"C_C2_Prev_Address_Duration"
             ,"C_C2_Prev_Employ_Duration"
             ,"C_C2_Prev_Employment_Status"
             ,"C_C2_Prev_Industry_Band"
             ,"C_C2_Residential_Status"
             ,"C_CO_Address_Postcode"
             ,"C_CO_Address_SA1_07"
             ,"C_CO_Address_SA1_11"
             ,"C_CO_Includes_Non_Director"
             ,"C_CO_Includes_Trust"
             ,"C_CO_Mortgage_Rent_Num"
             ,"C_CO_Registration"
             ,"C_CO_Type_Subclass"
             ,"C_E_C2_Total"
             ,"C_E_CO_Total"
             ,"C_I_CO_Total"
             ,"C_L_C1_Total"
             ,"C_L_C2_Total"
             ,"C_L_CO_Total"
             ,"D_Decision_Interim"
             ,"D_Internal_Score_Company"
             ,"D_Internal_Score_Consumer"
             ,"D_Internal_Score_SoleTrader"
             ,"E_C1_Veda_Curr_New_File"
             ,"E_C1_Veda_Individual_1_1"
             ,"E_C1_Veda_Individual_2_0CR"
             ,"E_C1_Veda_Individual_2_0NR"
             ,"E_C1_Veda_Orig_New_File"
             ,"E_C2_Veda_Curr_Age_File_Months"
             ,"E_C2_Veda_Curr_New_File"
             ,"E_C2_Veda_Individual_1_1"
             ,"E_C2_Veda_Individual_2_0CR"
             ,"E_C2_Veda_Individual_2_0NR"
             ,"E_C2_Veda_Orig_Age_File_Months"
             ,"E_C2_Veda_Orig_New_File"
             ,"E_CO_Veda_Company"
             ,"E_Veda_Primary_Imputed"
             ,"G_ECO_C1_AWE"
             ,"G_ECO_C1_AWE_ChgA"
             ,"G_ECO_C1_CPI"
             ,"G_ECO_C1_CPI_ChgA"
             ,"G_ECO_C1_UR"
             ,"G_ECO_C1_UR_ChgA"
             ,"H_C2_Last_Loan_Days_Since_Start"
             ,"H_C2_Months_As_Customer"
             ,"H_C2_Prior_Loans_Count"
             ,"H_C2_Prior_Loans_Max_AAD_Band"
             ,"H_C2_Years_As_Customer_Max_10"
             ,"I_APPNonStructured"
             ,"I_Extract_Set"
             ,"I_Final_Application"
             ,"ID_Application_ID_19"
             ,"ID_Application_ID_Suffix"
             # ,"ID_Dealer_Group_ID" #Stopped removing these 2017-09-26 as are needed in remuneration
             # ,"ID_Dealer_Make" #Stopped removing these as they could be useful for Rem
             # ,"ID_Dealer_PMA" #Stopped removing these as they could be useful for Rem
             ,"ID_Original_Contract_ID"
             ,"L_Ins_CashBenefit"
             ,"L_Ins_Count"
             ,"L_Ins_RoadsideAssistance"
             ,"L_Ins_Warranty"
             ,"L_LVR"
             ,"L_Month_Balloon"
             ,"L_Month_Regular_End"
             ,"V_NVIC"
             ,"V_Variant"
             ,"X_APRDISC"
             ,"X_APRIDC"
             ,"X_Balloon_Amount"
             # ,"X_Dealer_ID" #Stopped removing this as we want to know the end dealer as well.
             ,"X_Early_Term_Fee"
             ,"X_Establishment_Fee"
             # ,"X_Finance_Rate" #Stopped removing this as required to get a correct "max" rate.
             ,"X_Hardship_Extended_Contract"
             ,"X_InceptionDate"
             # ,"X_Loan_Date_Year" #Stop removing this as we want to report on it
             ,"X_Loan_Term_Num"
             ,"X_Margin_Rate"
             ,"X_Monthly_Fee"
             ,"X_Net_Amount_Financed"
             ,"X_Payment"
             ,"X_Payment_Structure"
             ,"X_SetUpDate"
             # ,"X_Subvention_Amount" #Stopped removing these 2017-09-26 as are needed in remuneration
             ,"X_Subvention_Flag"
             ,"X_Transfer_Price"
             ,"Z_Rand_App_1"
             ,"Z_Rand_App_2"
             ,"Z_Rand_App_3"
             ,"Z_Rand_App_TestValid"
             ,"Z_Recovery_Dollar"
             ,"Z_Termination_Date"
             ,"Z_Termination_Month"
             ,"Z_Write_Off_Date"
             ,"Z_Write_Off_Dollar"
             ,"Z_Write_Off_Month"
             ,"Index"
             ,"StartDate"
             ,"EndDate"
):=NULL]
names(data_fin)

##Because Counts_Adjustments only includes contracts with LoanTerm<=84mth, any contracts that has loan term> 84 months would cause an error. 
##Only includes Applications since 2019 due to the memory issue.
data_fin <- data_fin[L_Loan_Term_Num_Rounded<=84 & X_Loan_Date_Year>=2019,]


##################################@
###### 03. Filter (VarSample) #####
##################################@
cat(paste("\n",now(),"03. Filter"))

##################################@
###### ___03a. Filter by Time #####
##################################@

#Filter based on variables set up on Run Script page

#In all scenarios except when we do the goal seek (ie 2016_NonSubvented_InclRejected_Apps), we exclude non-approved applications.
#   In the goal seek we need to included non-approved so we can price them.
#   Definition of non-approved is "Either Application Approved, or Taken Up"

#First 6 months filters
if(VarSample == "201007-201012_Apps"){
  cat(" [Sample: 201007-201012_Apps]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2010-07-01\") | X_Loan_Date >= as.Date(\"2011-01-01\")")}

if(VarSample == "201007-201012_Contracts"){
  cat(" [Sample: 201007-201012_Contracts]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2010-07-01\") | X_Loan_Date >= as.Date(\"2011-01-01\")")
  data_fin <- mFilter("data_fin", "Z_Taken_Up==FALSE")}

#5 years filters
if(VarSample == "201007-201511_Apps"){
  cat(" [Sample: 201007-201511_Apps]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2010-07-01\") | X_Loan_Date >= as.Date(\"2015-12-01\")")}

if(VarSample == "201007-201511_Contracts"){
  cat(" [Sample: 201007-201511_Contracts]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2010-07-01\") | X_Loan_Date >= as.Date(\"2015-12-01\")")
  data_fin <- mFilter("data_fin", "Z_Taken_Up==FALSE")}

#All Data
if(VarSample == "All_Contracts"){
  cat(" [Sample: All_Contracts]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "Z_Taken_Up==FALSE")}

if(VarSample == "All_Apps"){
  cat(" [Sample: All_Apps]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
}

#2015 (well, not really 2015, actually Dec2014 to Nov2015)
if(VarSample == "2015_Apps"){
  cat(" [Sample: 2015_Apps]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2014-12-01\") | X_Loan_Date >= as.Date(\"2015-12-01\")")}

if(VarSample == "2015_Contracts"){
  cat(" [Sample: 2015_Contracts]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2014-12-01\") | X_Loan_Date >= as.Date(\"2015-12-01\")")
  data_fin <- mFilter("data_fin", "Z_Taken_Up==FALSE")}

#2015CY (ie the real 2015)
if(VarSample == "2015CY_Apps"){
  cat(" [Sample: 2015CY_Apps]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2015-01-01\") | X_Loan_Date >= as.Date(\"2016-01-01\")")}

#2016
if(VarSample == "2016_Apps"){
  cat(" [Sample: 2016_Apps]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2016-01-01\") | X_Loan_Date >= as.Date(\"2017-01-01\")")}

if(VarSample == "2016_Contracts"){
  cat(" [Sample: 2016_Contracts]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2016-01-01\") | X_Loan_Date >= as.Date(\"2017-01-01\")")
  data_fin <- mFilter("data_fin", "Z_Taken_Up==FALSE")}

#2018
if(VarSample == "2018_Apps"){
  cat(" [Sample: 2018_Apps]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2018-01-01\") | X_Loan_Date >= as.Date(\"2019-01-01\")")}

#Special data set for rate cards - in this one we don't exclude non-approved
if(VarSample == "2016_NonSubvented_InclRejected_Apps"){
  cat(" [Sample: 2016_NonSubvented_Apps]")
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2016-01-01\") | X_Loan_Date >= as.Date(\"2017-01-01\") | L_Subvention_Flag==TRUE")}

#After March price increase, before June price increase
if(VarSample == "201904-201906"){
  cat(" [Sample: 201904-201906]")
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2019-04-01\") | X_Loan_Date >= as.Date(\"2019-06-28\")")}

#Period RBP was implemented
if(VarSample == "201811-201906"){
  cat(" [Sample: 201811-201906]")
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2018-11-01\") | X_Loan_Date >= as.Date(\"2019-07-01\")")}

##################################@
###### ___03b. Filter by Type #####
##################################@

if(VarSample == "Recent_Week"){
  cat(paste0(" [Sample: ", VarSample, "]"))
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  LatestDate_end <- max(data_fin$X_Loan_Date)
  LatestDate_start <- LatestDate_end; 
  lubridate::week(LatestDate_start) <- lubridate::week(LatestDate_start) - 1
  data_fin <- mFilter("data_fin", paste0("X_Loan_Date < as.Date(\"", LatestDate_start, "\") | X_Loan_Date >= as.Date(\"", LatestDate_end, "\")"))
}

if(VarSample == "10k"){
  cat(" [Sample: 10k]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- data_fin[sample(.N,(min(data_fin[, .N],10000)))]}

if(VarSample == "RandomRated"){
  #Sample
  cat(" [Sample: RandomRated]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- data_fin[sample(.N,(min(data_fin[, .N],99)))]
  #Work out UID1
  data_fin[, ID_Application_ID_PartA:=.I]
  data_fin[, ID_Application_ID_PartA]
  #Repeat each row enough times
  data_fin <- data_fin[rep(seq.int(1,nrow(data_fin)), 151), ]
  #Work out the UID2
  data_fin[, ID_Application_ID_PartB := as.integer(ave(ID_Application_ID==ID_Application_ID, ID_Application_ID, FUN=cumsum) - 1L)]
  #Work out the correct ID_Application_ID by joining the UIDs
  data_fin[, ID_Application_ID:=paste(formatC(ID_Application_ID_PartA, width=9, flag="0")
                                      ,formatC(ID_Application_ID_PartB, width=8, flag="0")
                                      ,"0A"
                                      , sep="")]
  #Work out the Finance Rate
  data_fin[, L_Finance_Rate := ID_Application_ID_PartB/10]
  data_fin[, c("ID_Application_ID_PartA", "ID_Application_ID_PartB"):=NULL]
  #Update all the derivative fields
  data_fin[, L_Margin_Rate := L_Finance_Rate - L_Transfer_Price]
  data_fin[, L_Payment_Monthly_ExGST:=
             ifelse(L_Finance_Rate == 0,
                    (L_Net_Amount_Financed - L_Balloon_Amount) /
                      (L_Loan_Term_Num -
                         ifelse(L_Balloon_Amount > 0 & L_Payment_Structure == "IN ARREAR",1,0)),
                    ifelse(L_Balloon_Amount > 0,
                           ifelse(L_Payment_Structure == "IN ARREAR",
                                  pmt(r  = L_Finance_Rate/100/12,
                                      n  = L_Loan_Term_Num - 1,
                                      pv = -(L_Net_Amount_Financed +
                                               pv(r    = L_Finance_Rate/100/12,
                                                  n    = L_Loan_Term_Num,
                                                  fv   = L_Balloon_Amount,
                                                  pmt  = 0,
                                                  type =  0)),
                                      fv = 0,
                                      type = 0),
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = L_Balloon_Amount,
                                      type = 1)),
                           ifelse(L_Payment_Structure == "IN ARREAR",
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = 0,
                                      type = 0),
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = 0,
                                      type = 1)))) +
             L_Fee_Monthly]
  data_fin[, L_Payment_Incl_Monthly_Fee := 
             ifelse(L_Loan_Type == 60, L_Payment_Monthly_ExGST * 1.10, L_Payment_Monthly_ExGST)
           * ifelse(L_Payment_Frequency == "YEARLY", 12, 
                    ifelse(L_Payment_Frequency == "SIXMTHLY", 6,
                           ifelse(L_Payment_Frequency == "QTRLY", 3,1)))]
  data_fin[, L_Margin_Rate_Modelling   :=L_Margin_Rate]
  data_fin[, L_Transfer_Price_Modelling:=L_Transfer_Price]
}

###ROSABC slope analysis
if(VarSample == "ROSABC for Optimisation"){
  #Sample
  cat(" [Sample: ROSABC for Optimisation]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  
  #Get all customers from last 6 months
  data_fin <- data_fin[X_Loan_Date>= as.Date("2019-07-01")]
  
  #Work out UID1
  data_fin[, ID_Application_ID_PartA:=.I]
  
  #Creat new dataset which have 8 rows for each Application
  data_app <- data_fin[,.(ID_Application_ID)]
  repeat_vector <- rep(seq(1,length(data_app$ID_Application_ID),1),each=9)
  L_POC_Scenario <- rep(c(0,0.5,1,1.5,2,-0.5,-1,-1.5,-2),length(data_app$ID_Application_ID))
  data_app <- data_app[repeat_vector,]
  data_app <- cbind(data_app,L_POC_Scenario)
  
  #Merge L_POC_Scenario onto data_fin
  setkey(data_fin,ID_Application_ID)
  setkey(data_app,ID_Application_ID)
  data_fin <- merge(x=data_app,y=data_fin,all.x=TRUE,by.x="ID_Application_ID",by.y="ID_Application_ID")
  
  #Work out the UID2
  data_fin[, ID_Application_ID_PartB := as.integer(ave(ID_Application_ID==ID_Application_ID, ID_Application_ID, FUN=cumsum) - 1L)]
  #Work out the correct ID_Application_ID by joining the UIDs
  data_fin[, ID_Application_ID:=paste(formatC(ID_Application_ID_PartA, width=9, flag="0")
                                      ,formatC(ID_Application_ID_PartB, width=8, flag="0")
                                      ,"0A"
                                      , sep="")]
  #Work out the Finance Rate
  data_fin[, L_Finance_Rate := L_Finance_Rate + L_POC_Scenario]
  data_fin[, c("ID_Application_ID_PartA", "ID_Application_ID_PartB"):=NULL]
  #Update all the derivative fields
  data_fin[, L_Margin_Rate := L_Finance_Rate - L_Transfer_Price]
  data_fin[, L_Payment_Monthly_ExGST:=
             ifelse(L_Finance_Rate == 0,
                    (L_Net_Amount_Financed - L_Balloon_Amount) /
                      (L_Loan_Term_Num -
                         ifelse(L_Balloon_Amount > 0 & L_Payment_Structure == "IN ARREAR",1,0)),
                    ifelse(L_Balloon_Amount > 0,
                           ifelse(L_Payment_Structure == "IN ARREAR",
                                  pmt(r  = L_Finance_Rate/100/12,
                                      n  = L_Loan_Term_Num - 1,
                                      pv = -(L_Net_Amount_Financed +
                                               pv(r    = L_Finance_Rate/100/12,
                                                  n    = L_Loan_Term_Num,
                                                  fv   = L_Balloon_Amount,
                                                  pmt  = 0,
                                                  type =  0)),
                                      fv = 0,
                                      type = 0),
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = L_Balloon_Amount,
                                      type = 1)),
                           ifelse(L_Payment_Structure == "IN ARREAR",
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = 0,
                                      type = 0),
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = 0,
                                      type = 1)))) +
             L_Fee_Monthly]
  data_fin[, L_Payment_Incl_Monthly_Fee := 
             ifelse(L_Loan_Type == 60, L_Payment_Monthly_ExGST * 1.10, L_Payment_Monthly_ExGST)
           * ifelse(L_Payment_Frequency == "YEARLY", 12, 
                    ifelse(L_Payment_Frequency == "SIXMTHLY", 6,
                           ifelse(L_Payment_Frequency == "QTRLY", 3,1)))]
  data_fin[, L_Margin_Rate_Modelling   :=L_Margin_Rate]
  data_fin[, L_Transfer_Price_Modelling:=L_Transfer_Price]
}


if(VarSample == "Hardship_Extended"){
  cat(" [Sample: Hardship_Extended]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Hardship_Extended_Contract==FALSE")}

if(VarSample == "Written_Off"){
  cat(" [Sample: Written_Off]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "is.na(Z_Write_Off_Month_Fixed)")}

if(VarSample == "Selected_Contracts"){
  cat(" [Sample: Selected_Contracts]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "!(ID_Contract_ID %in% selectedContracts)")}

if(VarSample == "Random"){
  cat(" [Sample: Selected_Contracts]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- data_fin[seq(1, .N, 10000)]}

if(VarSample == "2018Subvented"){
  cat(" [Sample: Selected_Contracts]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- data_fin[L_Subvention_Flag==TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2018-01-01\") | X_Loan_Date >= as.Date(\"2019-01-01\")")}

#Just 1 Month for testing
if(VarSample == "1Month"){
  cat(" [Sample: 1Month]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- mFilter("data_fin", "X_Loan_Date < as.Date(\"2018-01-01\") | X_Loan_Date >= as.Date(\"2018-02-01\")")}

if(VarSample == "POC"){
  cat(" [Sample: POC]")
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin[, ID_Application_ID_19:=substring(ID_Application_ID,1,19)]
  data_fin <- data_fin[ID_Application_ID_19 %in% c("0118120180112142959"	,"0118020180115100619"	,"0118020180115102634"	,"0118120180116101720"	,"0118120180117152410"
                                                   ,"0118220180116145807"	,"0118120180119092954"	,"0118020171128173437"	,"0118120180123173851"	,"0118020180123140530"
                                                   ,"0095320180125131139"	,"0118420180129133326"	,"0118020180130110016"	,"0118120180131163050"	,"0118020180201131800"
                                                   ,"0118020180201181015"	,"0118120180205175749"	,"0118020180205161446"	,"0118320180205112338"	,"0161220180206145630"
                                                   ,"0118120180130123353"	,"0118220180206153745"	,"0161220180206102101"	,"0161220180205102615"	,"0161220180208141308"
                                                   ,"0161320180209104440"	,"0161320180209131424"	,"0116320180210145425"	,"0161320180210173051"	,"0161320180210134206"
                                                   ,"0161320180210170031"	,"0161320180210175646"	,"0161220180210171302"	,"0161220180210104902"	,"0161220180210160845"
                                                   ,"0161220180210094627"	,"0161220180210122532"	,"0161220180210130245"	,"0161320180211114555"	,"0161320180211172633"
                                                   ,"0161220180211132457"	,"0161220180210163126"	,"0073520180212143138"	,"0161320180212095256"	,"0161320180212164857"
                                                   ,"0161220180212164412"	,"0161220180211114758"	,"0062120180212144632"	,"0116320180213151429"	,"0003420180214110300"
                                                   ,"0026320180214123632"	,"0073520180214120155"	,"0116320180213173557"	,"0073520180215085641"	,"0073520180215130625"
                                                   ,"0073520180215141310"	,"0095320180213101129"	,"0161220180215122741"	,"0062120180215135755"	,"0062120180215173250"
                                                   ,"0026320180216142223"	,"0073520180216072657"	,"0073520180216120715"	,"0095320180216103151"	,"0095320180216162742"
                                                   ,"0116320180214173517"	,"0161220180216164153"	,"0003420180217111041"	,"0026320180217123621"	,"0026320180217135637"
                                                   ,"0073520180217084121"	,"0073520180217121542"	,"0095320180216174023"	,"0116320180217133637"	,"0161320180217125653"
                                                   ,"0161220180217112628"	,"0161220180217145130"	,"0062120180217111239"	,"0062120180217093940"	,"0062120180217134629"
                                                   ,"0095320180217170433"	,"0095320180218105847"	,"0095320180218180320"	,"0095320180218181750"	,"0095320180218151455"
                                                   ,"0161220180217144327"	,"0161220180218134820"	,"0003420180219081929"	,"0026320180219123458"	,"0026320180219171027"
                                                   ,"0095320180218183004"	,"0095320180219141847"	,"0095320180218131934"	,"0095320180219122157"	,"0116320180219083727"
                                                   ,"0116320180219085600"	,"0116320180219171306"	,"0116320180219174227"	,"0116320180219115426"	,"0161220180213100636"
                                                   ,"0161220180216153735"	,"0161220180217165625"	,"0003420180220144941"	,"0026320180217092229"	,"0026320180220102139"
                                                   ,"0095320180220155110"	,"0062120180220104038"	,"0026320180221105901"	,"0026320180221132854"	,"0095320180221111352"
                                                   ,"0095320180221125606"	,"0095320180221110644"	,"0062120180221141436"	,"0003420180222181352"	,"0095320180221160347"
                                                   ,"0095320180219185456"	,"0116320180223105658"	,"0062120180222150639"	,"0003420180224103748"	,"0003420180224132650"
                                                   ,"0003420180224175212"	,"0026320180224114006"	,"0095320180224114205"	,"0116320180224113727"	,"0062120180224164214"
                                                   ,"0062120180224164221"	,"0062120180224115921"	,"0095320180224093547"	,"0095320180224101803"	,"0037820180226092812"
                                                   ,"0095320180223161449"	,"0116320180226125624"	,"0042020180226105044"	,"0062120180226114128"	,"0062120180226140420"
                                                   ,"0040120180227105126"	,"0037420180227094749"	,"0037820180227083730"	,"0040020180227135103"	,"0062120180227100855"
                                                   ,"0040120180228123703"	,"0040120180228161931"	,"0037820180228160331"	,"0040020180228192715"	,"0040020180228200932"
                                                   ,"0116320180215163600"	,"0040120180301100537"	,"0037820180227094116"	,"0037820180301094205"	,"0062120180301162441"
                                                   ,"0040120180302104229"	,"0040120180302165331"	,"0037420180302111555"	,"0037420180302142940"	,"0040020180302175335"
                                                   ,"0040020180301181303"	,"0042020171230132838"	,"0040120180303142018"	,"0040020180303101018"	,"0040120180305095952"
                                                   ,"0040120180305105124"	,"0040120180305174559"	,"0040120180305173235"	,"0037820180305115444"	,"0042020180302175812"
                                                   ,"0037420180306170122"	,"0026320180306090218"	,"0040020180306172722"	,"0042020180303162039"	,"0042020180303174138"
                                                   ,"0161320180306104638"	,"0040120180226161258"	,"0040120180306152407"	,"0037820180307120448"	,"0040020180307194531"
                                                   ,"0037820180308170709"	,"0040020180307205845"	,"0040120180308180503"	,"0037420180310104021")
                       ,]
  data_fin[, ID_Application_ID_19:=NULL]
}

#TCUV Analysis
if(VarSample == "TCUV"){
  #Sample
  cat(" [Sample: TCUV]")
  #Get the average balloon percent for GFV to be prepared
  VarBalloon<-data_fin[I_GFV==TRUE, sum(L_Balloon_Amount)/sum(L_Net_Amount_Financed)]
  
  #Filter the data
  data_fin <- data_fin[D_Decision_Final == 'Application Approved' | Z_Taken_Up == TRUE,]
  data_fin <- data_fin[L_Subvention_Flag == FALSE & V_TCUV==TRUE,]
  
  #Create the 3 scenarios
  #A: As is
  data_fin[, ID_Application_ID   :=paste("A",substr(ID_Application_ID,   2,20),sep="")]
  data_fin_merged<-data_fin[!is.na(ID_Application_ID),]
  #B: Priced as if new (but with same other characteristics)
  data_fin[, ID_Application_ID   :=paste("B",substr(ID_Application_ID,   2,20),sep="")]
  data_fin[, V_New_Used_Flag     :=as.factor("N")]
  data_fin_merged<-rbindlist(list(data_fin_merged,data_fin))
  #C: Priced as if GFV, and adjusted to look like GFV
  data_fin[, ID_Application_ID       :=paste("C",substr(ID_Application_ID,   2,20),sep="")]
  data_fin[, V_New_Used_Flag         :=as.factor("U")]
  data_fin[, I_GFV                   :=TRUE]
  data_fin[, L_Loan_Type             :=as.factor(ifelse(L_Loan_Type==10,40,48))]
  data_fin[, L_Balloon_Amount        :=L_Net_Amount_Financed*VarBalloon]
  data_fin[, L_Balloon_Amount_of_NAF :=VarBalloon]
  data_fin[, L_Loan_Term_Num         :=pmin(48,L_Loan_Term_Num)]
  data_fin[, L_Loan_Term_Num_Rounded :=pmin(48,L_Loan_Term_Num_Rounded)]
  data_fin_merged<-rbindlist(list(data_fin_merged,data_fin))
  #Check
  data_fin<-data_fin_merged
  rm(data_fin_merged)
  data_fin[, TCUV_Group:=substr(ID_Application_ID,1,1)]
  data_fin[, .(Count=.N,Term_Max=max(L_Loan_Term_Num)), c("TCUV_Group", "V_New_Used_Flag")]
  
  
  #For the second scenario, also need to update the transfer price
  #Transfer Price 
  data_transfer_price <- as.data.table(read_sas(paste(VarPathStore, "Data", "Extract",paste(VarPrefix[length(VarPrefix)],"_data_transfer_price.sas7bdat", sep=""), sep=VarPathSeparator)))
  #Filter, sort, remove fields
  data_transfer_price<-data_transfer_price[L_TP_SALESCHANNEL=="R" 
                                           & L_TP_USEDIND=="N" & L_TP_PRODTYPE==0
                                           & as.Date(L_TP_EFFECTDATE) >= as.Date("2015-01-01") 
                                           ,c("L_TP_APRTP", "L_TP_EFFECTDATE", "L_TP_RESIDUALCODE", "L_TP_TERM")
                                           , with=FALSE][order(L_TP_EFFECTDATE, L_TP_TERM, L_TP_RESIDUALCODE)]
  #Fix the dates - round them to the nearest month start (ie the rounddown of the date + 5 days)
  data_transfer_price[, L_TP_EFFECTDATE:=as.Date(L_TP_EFFECTDATE)+5
                      -lubridate::day(as.Date(L_TP_EFFECTDATE)+5)+1 ]
  #Add the appropriate (temporary) merge fields in the main data set
  data_fin[, L_TP_EFFECTDATE  := L_Loan_Application_Date - lubridate::day(L_Loan_Application_Date)+1]
  data_fin[, L_TP_RESIDUALCODE:= L_RV_Band/10 -1]
  data_fin[, L_TP_TERM        := L_Loan_Term_Num_Rounded]
  #Ensure that the merge fields don't fall outside the bounds of the transfer price data
  data_fin[, L_TP_EFFECTDATE:=ifelse(L_TP_EFFECTDATE>max(data_transfer_price$L_TP_EFFECTDATE),max(data_transfer_price$L_TP_EFFECTDATE),
                                     ifelse(L_TP_EFFECTDATE<min(data_transfer_price$L_TP_EFFECTDATE),min(data_transfer_price$L_TP_EFFECTDATE),
                                            L_TP_EFFECTDATE))]
  class(data_fin$L_TP_EFFECTDATE)<-"Date"
  data_fin[, L_TP_TERM:=ifelse(L_TP_TERM>max(data_transfer_price$L_TP_TERM),max(data_transfer_price$L_TP_TERM),
                               ifelse(L_TP_TERM<min(data_transfer_price$L_TP_TERM),min(data_transfer_price$L_TP_TERM),
                                      L_TP_TERM))]
  #Do the merge
  setkey(data_transfer_price, L_TP_EFFECTDATE, L_TP_RESIDUALCODE, L_TP_TERM)
  setkey(data_fin,            L_TP_EFFECTDATE, L_TP_RESIDUALCODE, L_TP_TERM)
  data_fin<-data_transfer_price[data_fin]
  rm(data_transfer_price)
  #Replace the L_Transfer_Price for the C scenario
  data_fin[TCUV_Group == "C",L_Transfer_Price:=L_TP_APRTP*100]
  data_fin[, mean(L_Transfer_Price),TCUV_Group]
  #Load the adjustments
  #These are due to the change in methodology for Transfer Prices from 2016-10.
  #These adjustments will simulate the new methodology on the old data.
  data_transfer_price_adjustment<-fread(paste(VarPathStore,"Data","Standard","TransferPricesNormalisation.csv", sep=VarPathSeparator))
  #Merge them on
  setkey(data_transfer_price_adjustment, L_TP_RESIDUALCODE, L_TP_TERM)
  setkey(data_fin,                       L_TP_RESIDUALCODE, L_TP_TERM)
  data_fin<-data_transfer_price_adjustment[data_fin]
  rm(data_transfer_price_adjustment)
  #Create the "Modelling" Transfer Price and Rate Margin, ie overlaying the adjustment if the original was before 2016-10
  data_fin[,                                        L_Transfer_Price_Modelling:=L_Transfer_Price]
  data_fin[L_TP_EFFECTDATE < as.Date("2016-10-01"), L_Transfer_Price_Modelling:=L_Transfer_Price + L_TP_APRTP_DIFF]
  #Remove the addditional fields
  data_fin[, c("L_TP_EFFECTDATE", "L_TP_RESIDUALCODE", "L_TP_TERM", "L_TP_APRTP", "L_TP_APRTP_DIFF"):=NULL ]
  #Add the calculated fields.
  data_fin[, L_Margin_Rate_Modelling := L_Finance_Rate - L_Transfer_Price_Modelling] 
  data_fin[, L_Margin_Rate:=L_Finance_Rate - L_Transfer_Price]
  data_fin[, mean(L_Margin_Rate), TCUV_Group]
  data_fin[, mean(L_Transfer_Price), TCUV_Group]
  data_fin[, mean(L_Finance_Rate), TCUV_Group]
}

if(VarSample == "") cat(" [Sample: {blank} not filter any data points]")

rm(mFilter)

#########################################@
###### 04. Adjust Margin (VarMargin) #####
#########################################@
cat(paste("\n",now(),"04. Adjust Margin"))

#Save a copy of the original margin
data_fin[, L_Margin_Rate_Original:=L_Margin_Rate]

#Do different things if we are adjusting the prices or not.
if(VarMargin=="L_Margin_Rate" | VarMargin=="Goal_Seek"){
  #If we're not adjusting the prices
  cat(" [Margin: Not Adjusted]")
  data_fin[, ID_Price:=VarMargin]
  data_fin[, L_Margin_Rate_New:=as.numeric(NA)]
  data_fin[, Z_ToExclude:="Keep"]
} else {
  #If we are adjusting the prices
  cat(paste(" [Margin: Adjusted to ",VarMargin,"]", sep=""))
  #Load the Master Pricing File (that is updated whenever a profit-calculating run is done)
  load(paste(VarPathStore,"Data","Standard","data_prices.RData", sep=VarPathSeparator))
  #Grab the relevant fields (the merging, and the margin to use)
  data_prices <- data_prices[, c("ID_Application_ID", VarMargin), with=FALSE]
  #Rename the Margin field
  setnames(data_prices, VarMargin, "L_Margin_Rate_New")
  #Merge on to the main data
  setkey(data_fin,    ID_Application_ID)
  setkey(data_prices, ID_Application_ID)
  data_fin<-data_prices[data_fin]
  rm(data_prices)
  
  #Fix key the variables
  data_fin[, ID_Price :=ifelse(is.na(L_Margin_Rate_New), "L_Margin_Rate", VarMargin)]
  data_fin[, L_Margin_Rate:=ifelse(is.na(L_Margin_Rate_New), L_Margin_Rate, L_Margin_Rate_New)]
  
  #Remove the GFV calculation before doing the Cap & Decline, then add it back on at the end
  data_fin[, L_Margin_Rate:=L_Margin_Rate - ifelse(I_GFV==TRUE,0.3,0)]
  
  #Work out which cap & decline scenario
  cat(paste(" [Scenario: ",VarCapDecline,"]", sep=""))
  #Charge all customers at the Rate Card Rate
  if(VarCapDecline=="CardAll"){
    data_fin[, Z_ToExclude :="Keep"]
  }
  #Charge Toyota at the Rate Card Rate, Decline Other Manufacturers
  if(VarCapDecline=="CardT-DeclineOM"){
    data_fin[, Z_ToExclude :=
               ifelse(L_Margin_Rate <= VarProfitMaxMargin | ID_Price=="L_Margin_Rate", "Keep",
                      ifelse(V_Make_Band=="TOYOTA" | V_Make_Band=="LEXUS" | V_Make_Band=="HINO" | V_Make_Band=="MAZDA"
                             ,"Cap","Exclude"))]
  }
  #Charge Toyota at the Cap, Decline Other Manufacturers
  if(VarCapDecline=="CapT-DeclineOM"){
    data_fin[, Z_ToExclude :=
               ifelse(L_Margin_Rate <= VarProfitMaxMargin | ID_Price=="L_Margin_Rate", "Keep",
                      ifelse(V_Make_Band=="TOYOTA" | V_Make_Band=="LEXUS"| V_Make_Band=="HINO" | V_Make_Band=="MAZDA"
                             ,"Cap","Exclude"))]
    data_fin[, L_Margin_Rate :=
               ifelse(Z_ToExclude=="Cap",VarProfitMaxMargin,L_Margin_Rate)]
  }
  #Charge Toyota New at the Rate Card, Decline Used and Other Manufacturers
  if(VarCapDecline=="CardTN-DeclineUON"){
    data_fin[, Z_ToExclude :=
               ifelse(L_Margin_Rate <= VarProfitMaxMargin | ID_Price=="L_Margin_Rate", "Keep",
                      ifelse((V_Make_Band=="TOYOTA" | V_Make_Band=="LEXUS" | V_Make_Band=="HINO" | V_Make_Band=="MAZDA")
                             &(V_New_Used_Flag=="N"  | V_New_Used_Flag=="D")
                             ,"Cap","Exclude"))]
    data_fin[, Z_ToExclude := ifelse(Z_To_Exclude != "Exclude", Z_To_Exclude,
                                     ifelse(L_Loan_Type %in% c(40,48), "Cap",
                                            ifelse(is.na(E_Veda_Primary), "Cap","Exclude")))]
  }
  #Charge Toyota New at the Cap, Decline Used and Other Manufacturers #STANDARD SCENARIO
  if(VarCapDecline=="CapTN-DeclineUON"){
    data_fin[, Z_ToExclude :=
               ifelse(L_Margin_Rate <= VarProfitMaxMargin | ID_Price=="L_Margin_Rate", "Keep",
                      ifelse((V_Make_Band=="TOYOTA" | V_Make_Band=="LEXUS"| V_Make_Band=="HINO" | V_Make_Band=="MAZDA")
                             &(V_New_Used_Flag=="N"  | V_New_Used_Flag=="D")
                             ,"Cap","Exclude"))]
    data_fin[, L_Margin_Rate :=
               ifelse(Z_ToExclude=="Cap",VarProfitMaxMargin,L_Margin_Rate)]
  }
  #Decline all over the cap
  if(VarCapDecline=="DeclineAll"){
    data_fin[, Z_ToExclude :=
               ifelse(L_Margin_Rate <= VarProfitMaxMargin | ID_Price=="L_Margin_Rate", "Keep"
                      ,"Exclude")]
  }
  #Charge all over the cap at the cap
  if(VarCapDecline=="CapAll"){
    data_fin[, Z_ToExclude :=
               ifelse(L_Margin_Rate <= VarProfitMaxMargin | ID_Price=="L_Margin_Rate", "Keep"
                      ,"Cap")]
    data_fin[, L_Margin_Rate :=
               ifelse(Z_ToExclude=="Cap",VarProfitMaxMargin,L_Margin_Rate)]
  }
  
  #Work out all the derivative variables
  data_fin[, L_Margin_Rate:=L_Margin_Rate + ifelse(I_GFV==TRUE,0.3,0)]
  data_fin[, L_Finance_Rate := L_Transfer_Price + L_Margin_Rate]
  data_fin[, L_Payment_Monthly_ExGST:=
             ifelse(L_Finance_Rate == 0,
                    (L_Net_Amount_Financed - L_Balloon_Amount) /
                      (L_Loan_Term_Num -
                         ifelse(L_Balloon_Amount > 0 & L_Payment_Structure == "IN ARREAR",1,0)),
                    ifelse(L_Balloon_Amount > 0,
                           ifelse(L_Payment_Structure == "IN ARREAR",
                                  pmt(r  = L_Finance_Rate/100/12,
                                      n  = L_Loan_Term_Num - 1,
                                      pv = -(L_Net_Amount_Financed +
                                               pv(r    = L_Finance_Rate/100/12,
                                                  n    = L_Loan_Term_Num,
                                                  fv   = L_Balloon_Amount,
                                                  pmt  = 0,
                                                  type =  0)),
                                      fv = 0,
                                      type = 0),
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = L_Balloon_Amount,
                                      type = 1)),
                           ifelse(L_Payment_Structure == "IN ARREAR",
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = 0,
                                      type = 0),
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = 0,
                                      type = 1)))) +
             L_Fee_Monthly]
  data_fin[, L_Payment_Incl_Monthly_Fee := 
             ifelse(L_Loan_Type == 60, L_Payment_Monthly_ExGST * 1.10, L_Payment_Monthly_ExGST)
           * ifelse(L_Payment_Frequency == "YEARLY", 12, 
                    ifelse(L_Payment_Frequency == "SIXMTHLY", 6,
                           ifelse(L_Payment_Frequency == "QTRLY", 3,1)))]
  #Update the modelling versions as well. 
  #   The modelling versions equal the actuals, for dates after 2016-10, and for any forecasting.
  data_fin[, L_Margin_Rate_Modelling   :=L_Margin_Rate]
  data_fin[, L_Transfer_Price_Modelling:=L_Transfer_Price]
  
}


#######################################@
###### 05. Open additional data  #######
#######################################@
cat(paste("\n",now(),"05. Open additional data"))

###########################@
###### ___05a. GBMs  #######
###########################@
cat(paste("\n",now(),"___05a. GBMs"))

#Load the GBMs, along with a list of them and the best iterations
load(paste(VarPathStore, "Data", "Standard", "GBMs.RData", sep=VarPathSeparator))
dealerAdj[,ID_Dealer_ID:= str_pad(ID_Dealer_ID, 5, pad = "0")]

##############################@
###### ___05b. Economic #######
##############################@
cat(paste("\n",now(),"___05b. Economic"))

#Load the economic data
load(file=paste(VarPathStore, "Data", "Standard","data_eco.RData", sep=VarPathSeparator))

#Remove fields not required
data_eco[, c("G_ECO_C1_AWE","G_ECO_C1_AWE_ChgA"
             ,"G_ECO_C1_CPI", "G_ECO_C1_CPI_ChgA"
             ,"G_ECO_C1_UR", "G_ECO_C1_UR_ChgA"):=NULL]

#Also load the forecast economic data if required, and replace the economic with the forecast economic
if(VarScenarioMatrix[Scenario==VarScenario,EcoTPAdjust]>0){
  #IF THE SCENARIO IS TO USE FORECAST ECONOMIC#  
  cat(paste(" [Scenario: Forecast Economic ",VarScenarioMatrix[Scenario==VarScenario,EcoTPAdjust],"]",sep=""))
  
  #Load the data
  data_eco_forecast <- fread(paste(VarPathStore, "Data","Standard","Inflation_Forecasts_Data.csv",sep=VarPathSeparator))
  
  #Select either the 2015 or 2016 data as appropriate
  data_eco_forecast<-data_eco_forecast[Model==VarScenarioMatrix[Scenario==VarScenario,EcoTPAdjust], ]
  data_eco_forecast[, Model:=NULL]
  
  #Remove old data from data_eco
  data_eco <- data_eco[, c("Quarter_Fixed","State"), with=FALSE]
  
  #Merge on the forecast versions
  setkey(data_eco,          State)
  setkey(data_eco_forecast, State)  
  data_eco<-data_eco_forecast[data_eco]
  rm(data_eco_forecast)
} else {
  cat(" [Scenario: Actual Economic]")
}

#Create a version of this data for the month-versions, and fix the names as we will merge this on to the month versions
data_eco_month<-data_eco
names(data_eco_month)<-gsub("G_ECO","G_ECO_Month",names(data_eco))


########################################@
###### ___05c. Counts Adjustments #######
########################################@
cat(paste("\n",now(),"___05c. Counts Adjustments"))

#If we applying global counts adjustments we need to load the data. These are global counts of Write Off and Termination adjustments.
if(VarScenarioMatrix[Scenario==VarScenario,CountAdjust]==TRUE){
  #IF THE SCENARIO IS TO APPLY COUNTS ADJUSTMENTS# 
  cat(" [Scenario: Counts Adjustments Loaded]")
  #Load the global adjustments. These can't be merged on until later, as they are based on the Z_Month_Index, which is created later
  CountsAdjustments <- fread(paste(VarPathStore, "Data","Standard","CountsAdjustments.csv",sep=VarPathSeparator))
  TermAdjustments <- fread(paste(VarPathStore, "Data","Standard","CountsAdjustments - Termination - 2021 - Implement.csv",sep=VarPathSeparator))
  WOAdjustments <- fread(paste(VarPathStore, "Data","Standard","CountsAdjustments - WriteOff - 2021 - Implement.csv",sep=VarPathSeparator))
  
} else {
  cat(" [Scenario: No Counts Adjustments Loaded]")
}


###########################################@
###### ___05d. Financial Adjustments #######
###########################################@
cat(paste("\n",now(),"___05d. Financial Adjustments"))

#Scenarios if we applying global financial adjustments
if(VarScenarioMatrix[Scenario==VarScenario,FinancialAdjust]==TRUE){
  #IF THE SCENARIO IS TO APPLY FINANCIAL ADJUSTMENTS# 
  cat(" [Scenario: Financial Adjustments Loaded]")
  #Load the global adjustments. These can't be merged on until later, as they are based on the Z_Month_Index, which is created later
  FinancialAdjustments      <- fread(paste(VarPathStore, "Data","Standard","FinancialAdjustments.csv",sep=VarPathSeparator))
} else {
  cat(" [Scenario: No Financial Adjustments Loaded]")
}

##############################################@
###### ___05e. Forecast Transfer Prices #######
##############################################@
cat(paste("\n",now(),"___05e. Forecast Transfer Price"))

#Different scenarios if we are using forecast forecast transfer prices.
if(VarScenarioMatrix[Scenario==VarScenario,EcoTPAdjust]>0){
  #IF THE SCENARIO IS TO USE FORECAST TRANSFER PRICES#  
  cat(paste(" [Scenario: Forecast Transfer Prices ",VarScenarioMatrix[Scenario==VarScenario,EcoTPAdjust],"]",sep=""))
  
  #Load the transfer price data
  load(file=paste(VarPathStore, "Data", "Extract", paste(VarPrefix,"_data_transfer_price.RData",sep=""), sep=VarPathSeparator))
  
  #Set up what is the appropriate date to filter on
  if(VarScenarioMatrix[Scenario==VarScenario,EcoTPAdjust]==2015){VarTPDate<-as.Date("2016-07-01")}
  if(VarScenarioMatrix[Scenario==VarScenario,EcoTPAdjust]==2016){VarTPDate<-as.Date("2017-05-01")}
  if(VarScenarioMatrix[Scenario==VarScenario,EcoTPAdjust]==2019){VarTPDate<-as.Date("2019-06-01")}
  
  #Filter for most recent, which will become the forecast
  data_transfer_price <- data_transfer_price[L_TP_EFFECTDATE==VarTPDate,]
  data_transfer_price[, L_TP_EFFECTDATE:=NULL]
  
  #Add the appropriate (temporary) merge fields in the main data set
  data_fin[, L_TP_RESIDUALCODE:= L_RV_Band/10 -1]
  data_fin[, L_TP_TERM        := L_Loan_Term_Num_Rounded]
  
  #Ensure that the merge fields don't fall outside the bounds of the transfer price data
  data_fin[, L_TP_TERM:=ifelse(L_TP_TERM>max(data_transfer_price$L_TP_TERM),max(data_transfer_price$L_TP_TERM),
                               ifelse(L_TP_TERM<min(data_transfer_price$L_TP_TERM),min(data_transfer_price$L_TP_TERM),
                                      L_TP_TERM))]
  
  #Do the merge
  setkey(data_transfer_price, L_TP_RESIDUALCODE, L_TP_TERM)
  setkey(data_fin,            L_TP_RESIDUALCODE, L_TP_TERM)
  data_fin<-data_transfer_price[data_fin]
  
  #Replace the L_Transfer_Price
  data_fin[,L_Transfer_Price:=L_TP_APRTP*100]
  
  #Fix the variables
  data_fin[, L_Finance_Rate  :=pmax(0,L_Transfer_Price + L_Margin_Rate)] #The finance rate can't go below 0. 
  data_fin[, L_Margin_Rate   :=L_Finance_Rate - L_Transfer_Price]        #And if the finance rate was fixed above, need to also fix the margin.
  data_fin[, L_Payment_Monthly_ExGST:=
             ifelse(L_Finance_Rate == 0,
                    (L_Net_Amount_Financed - L_Balloon_Amount) /
                      (L_Loan_Term_Num -
                         ifelse(L_Balloon_Amount > 0 & L_Payment_Structure == "IN ARREAR",1,0)),
                    ifelse(L_Balloon_Amount > 0,
                           ifelse(L_Payment_Structure == "IN ARREAR",
                                  pmt(r  = L_Finance_Rate/100/12,
                                      n  = L_Loan_Term_Num - 1,
                                      pv = -(L_Net_Amount_Financed +
                                               pv(r    = L_Finance_Rate/100/12,
                                                  n    = L_Loan_Term_Num,
                                                  fv   = L_Balloon_Amount,
                                                  pmt  = 0,
                                                  type =  0)),
                                      fv = 0,
                                      type = 0),
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = L_Balloon_Amount,
                                      type = 1)),
                           ifelse(L_Payment_Structure == "IN ARREAR",
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = 0,
                                      type = 0),
                                  pmt(r = L_Finance_Rate/100/12,
                                      n = L_Loan_Term_Num,
                                      pv = -L_Net_Amount_Financed,
                                      fv = 0,
                                      type = 1)))) +
             L_Fee_Monthly]
  data_fin[, L_Payment_Incl_Monthly_Fee := 
             ifelse(L_Loan_Type == 60, L_Payment_Monthly_ExGST * 1.10, L_Payment_Monthly_ExGST)
           * ifelse(L_Payment_Frequency == "YEARLY", 12, 
                    ifelse(L_Payment_Frequency == "SIXMTHLY", 6,
                           ifelse(L_Payment_Frequency == "QTRLY", 3,1)))]
  
  #Update the modelling versions as well
  #   The modelling versions equal the actuals, for dates after 2016-10, and for any forecasting.
  data_fin[, L_Margin_Rate_Modelling   :=L_Margin_Rate]
  data_fin[, L_Transfer_Price_Modelling:=L_Transfer_Price]
  
  #Remove the addditional fields
  data_fin[, c("L_TP_RESIDUALCODE", "L_TP_TERM", "L_TP_APRTP"):=NULL ]
  rm(data_transfer_price)
  
} else {
  cat(" [Scenario: Actual Transfer Prices]")
} 



#######################################@
###### ___05f. Goal Seek Targets #######
#######################################@
cat(paste("\n",now(),"___05f. Goal Seek Targets"))

#Two scenarios - if we are doing the goal seek or not
if(VarMargin!="Goal_Seek"){
  #IF THE SCENARIO IS TO NOT DO A GOAL SEEK# 
  cat(" [Scenario: Targets Not Loaded]")
} else {
  #IF THE SCENARIO IS TO DO A GOAL SEEK# 
  cat(" [Scenario: Targets Loaded]")
  load(paste(VarPathStore, "Data", "Financial", "targets.RData", sep=VarPathSeparator))
  targets
}


##################################################@
###### 06. START Goal Seek Loop if required #######
##################################################@
cat(paste("\n",now(),"06. START Goal Seek Loop"))

#Work out how many times to iterate (i_Profit_Max)
if(VarMargin=="Goal_Seek"){
  i_Profit_Max <- 100
} else {
  i_Profit_Max <- 1
  cat(" [Scenario: No Goal Seek]")}

#Work out the total number of rows so we can see how it is progressing
VarPreLoopRows <- data_fin[, .N]

#Start the loop, using i_Profit as the iteration number
for (i_Profit in seq(1, i_Profit_Max, by=1)){
  
  #Output something so we can see where we are at
  if(VarMargin=="Goal_Seek"){
    cat(paste("\n",                                                                                              sep=""))
    cat(paste("\n#####",                                                                                         sep=""))
    cat(paste("\n##### Goal Seek Iteration Number: ", i_Profit,                                                  sep=""))
    cat(paste("\n##### Data still seeking:         ", floor(1000*data_fin[, .N] / VarPreLoopRows)/10, "%",sep=""))
    cat(paste("\n#####",                                                                                         sep=""))
    cat(paste("\n",                                                                                              sep=""))}
  
  #If we are on the first run, populate the "L_Margin_Rate" with the first margin to test, otherwise it is covered at the end of the loop
  if(VarMargin=="Goal_Seek" & i_Profit == 1){data_fin[, L_Margin_Rate := VarProfitMarginsToTest[1]]}
  
  #Adjust the other variables, now we've changed the L_Margin_Rate
  if(VarMargin=="Goal_Seek"){
    data_fin[, L_Finance_Rate := L_Transfer_Price + L_Margin_Rate]
    data_fin[, L_Payment_Monthly_ExGST:=
               ifelse(L_Finance_Rate == 0,
                      (L_Net_Amount_Financed - L_Balloon_Amount) /
                        (L_Loan_Term_Num -
                           ifelse(L_Balloon_Amount > 0 & L_Payment_Structure == "IN ARREAR",1,0)),
                      ifelse(L_Balloon_Amount > 0,
                             ifelse(L_Payment_Structure == "IN ARREAR",
                                    pmt(r  = L_Finance_Rate/100/12,
                                        n  = L_Loan_Term_Num - 1,
                                        pv = -(L_Net_Amount_Financed +
                                                 pv(r    = L_Finance_Rate/100/12,
                                                    n    = L_Loan_Term_Num,
                                                    fv   = L_Balloon_Amount,
                                                    pmt  = 0,
                                                    type =  0)),
                                        fv = 0,
                                        type = 0),
                                    pmt(r = L_Finance_Rate/100/12,
                                        n = L_Loan_Term_Num,
                                        pv = -L_Net_Amount_Financed,
                                        fv = L_Balloon_Amount,
                                        type = 1)),
                             ifelse(L_Payment_Structure == "IN ARREAR",
                                    pmt(r = L_Finance_Rate/100/12,
                                        n = L_Loan_Term_Num,
                                        pv = -L_Net_Amount_Financed,
                                        fv = 0,
                                        type = 0),
                                    pmt(r = L_Finance_Rate/100/12,
                                        n = L_Loan_Term_Num,
                                        pv = -L_Net_Amount_Financed,
                                        fv = 0,
                                        type = 1)))) +
               L_Fee_Monthly]
    data_fin[, L_Payment_Incl_Monthly_Fee := 
               ifelse(L_Loan_Type == 60, L_Payment_Monthly_ExGST * 1.10, L_Payment_Monthly_ExGST)
             * ifelse(L_Payment_Frequency == "YEARLY", 12, 
                      ifelse(L_Payment_Frequency == "SIXMTHLY", 6,
                             ifelse(L_Payment_Frequency == "QTRLY", 3,1)))]
    
    #Update the modelling versions as well
    #   The modelling versions equal the actuals, for dates after 2016-10, and for any forecasting.
    data_fin[, L_Margin_Rate_Modelling   :=L_Margin_Rate]
    data_fin[, L_Transfer_Price_Modelling:=L_Transfer_Price]
  }
  
  ########################################@
  ###### 07. START Percentages Loop #######
  ########################################@
  cat(paste("\n",now(),"07. START Percentages Loop"))
  
  # Label the data with which chunk it will go in to
  numberOfChunks <- 100
  data_fin$ID<-seq.int(nrow(data_fin))
  varChunkSize <- max(10000,ceiling(max(data_fin$ID)/numberOfChunks)) #So we don't create too small a set
  data_fin[, ID_Group := floor(ID/varChunkSize)+1]
  data_fin[, ID:=NULL]
  i_max <- max(data_fin$ID_Group)
  remove(numberOfChunks, varChunkSize)
  
  for (i in 1:i_max){
    if(i_max==1){cat(" [Scenario: No Percentages Loop]")}
    if(i_max > 1 & i_max < 100){cat(paste("\n ",now(), " ",i," of ",i_max, sep=""))}
    if(i_max >= 100){cat(paste("\n ",now(), " ", i,"%", sep=""))}
    
    #Do a cleanup of the memory just in case
    gc()
    
    #Get the batch for this single percentage
    data_fin_batch <- data_fin[ID_Group ==i,]
    
    #Chunk this into bits for AWS
    numberOfChunks_AWS <- 36
    data_fin_batch$ID_AWS<-seq.int(nrow(data_fin_batch))
    varChunkSize_AWS <- max(100,ceiling(max(data_fin_batch$ID_AWS)/numberOfChunks_AWS)) 
    data_fin_batch[, ID_Group_AWS := floor(ID_AWS/varChunkSize_AWS)+1]
    data_fin_batch[, ID_AWS:=NULL]
    i_max_AWS <- data_fin_batch[, max(ID_Group_AWS)]
    remove(numberOfChunks_AWS, varChunkSize_AWS)
    
    ################################@
    ###### 08. START AWS Loop #######
    ################################@
    if(i==1){cat(paste("\n",now(),"08. START AWS Loop (per Percentage Loop)"))}
    
    #If there are more than 1 chunks and we are on AWS, then run through AWS, otherwise just run the function directly
    if(i_max_AWS > 1 & VarLocation == "azure" ){
      
      if(i==1){cat(paste(" [Scenario: On AWS]"))}
      #Make these chunks into an input list
      inputList <- vector("list",i_max_AWS)
      for(i_AWS in 1:i_max_AWS) {inputList[[i_AWS]] <- data_fin_batch[ID_Group_AWS == i_AWS,]}
      remove(data_fin_batch)
      
      #Run the above function through AWS
      stackedModelFile <- parallel::mclapply(inputList, mMainFunction, mc.cores=CPU_CORE, mc.preschedule=FALSE)
      remove(inputList)
      data_fin_run = rbindlist(stackedModelFile)
      remove(stackedModelFile)
      
    } else {
      if(i==1){cat(paste(" [Scenario: Not on AWS]"))}
      #Just run the data
      data_fin_run <- mMainFunction(data_fin_batch)
      remove(data_fin_batch)
    }
    
    #Consolidate the data from the percentage loops (ie add this data onto the previous sets)
    if(i==1){data_fin_processed <- data_fin_run} else {data_fin_processed <- rbindlist(list(data_fin_processed, data_fin_run))}
    remove(data_fin_run)
  }
  
  ##### !! DEBUGGING !! #####
  #If you are just manually running the code, then you'll need to un-comment out and run this below line, after running the first part of "E01 Financial Model.R"
  #data_fin_processed<-data_fin_padded
  
  
  #############################################@
  ###### 16. END AWS & Percentages Loops #######
  #############################################@
  cat(paste("\n",now(),"16. END AWS & Percentages Loops"))
  
  #Remove additional objects
  remove(i, i_max_AWS)
  
  #Remove additional variables
  data_fin_processed[, c("ID_Group", "ID_Group_AWS"):=NULL]
  
  ###############################@
  ###### 17. Profitability #######
  ###############################@
  cat(paste("\n",now(),"17. Profitability"))
  
  #Work out all the possible targets 
  summary05ContractProfit <- data_fin_processed[, .(T_Net_Before_Commission_Cash    = sum(T_Net_Before_Commission_Cash),
                                                    T_Net_After_Commission_Cash      = sum(T_Net_After_Commission_Cash),
                                                    L_Net_Amount_Financed      = mean(L_Net_Amount_Financed),
                                                    L_Margin_Rate              = mean(L_Margin_Rate),
                                                    T_Balance                  = sum(T_Balance),
                                                    M_Balance                  = sum(M_Balance)), 
                                                by=c("ID_Application_ID", "L_Product_Type", "V_Make_Band", "V_New_Used_Flag")]
  summary05ContractProfit[, T_Profit_Per_Thou       := T_Net_Before_Commission_Cash          / (L_Net_Amount_Financed / 1000)]
  summary05ContractProfit[, T_Contribution_Ratio    := 100 * T_Net_Before_Commission_Cash    / (T_Balance / 12)]
  summary05ContractProfit[, T_ROSABC                := 100 * T_Net_Before_Commission_Cash    / (M_Balance / 12)]
  summary05ContractProfit[, T_ROA                   := 100 * T_Net_After_Commission_Cash     / (T_Balance / 12)]
  
  ####################################@
  ###### 18. Goal Seek Analysis #######
  ####################################@
  cat(paste("\n",now(),"18. Goal Seek Analysis"))
  
  #The below is all goal seek stuff
  if(VarMargin!="Goal_Seek"){
    #IF THE SCENARIO IS TO NOT DO A GOAL SEEK# 
    cat(" [Scenario: No Goal Seek Analysis]")
  } else {
    #IF THE SCENARIO IS TO DO A GOAL SEEK# 
    cat(" [Scenario: Goal Seek Analysis]")
    
    ###########################################@
    ###### ___18a. Set up Goal Seek info #######
    ###########################################@
    
    profit01Calc <- summary05ContractProfit
    remove(summary05ContractProfit)
    profit01Calc[, Iteration:=i_Profit]
    
    #ADD IN HERE ALL THE MISSING VARIABLES THAT YOU CALCULATE BELOW AND WANT TO BE PART OF RBINDLIST
    profit01Calc[, L_Margin_Rate_Previous        :=as.numeric(NA)]
    profit01Calc[, Diff                          :=as.numeric(NA)]
    profit01Calc[, Abs                           :=as.numeric(NA)]
    profit01Calc[, Abs_Previous                  :=as.numeric(NA)]
    profit01Calc[, Sign                          :=as.numeric(NA)]
    profit01Calc[, Stage                         :=as.character(NA)]
    profit01Calc[, Stage_Next                    :=as.character(NA)]
    profit01Calc[, L_Margin_Rate_Result          :=as.numeric(NA)]
    profit01Calc[, L_Margin_Rate_To_Test_Calc    :=as.numeric(NA)]
    profit01Calc[, L_Margin_Rate_To_Test         :=as.numeric(NA)]
    profit01Calc[, Closest_Flag                  :=as.numeric(NA)]
    
    #Merge in the new set with the old data
    if(i_Profit==1){profit03Analysis <- profit01Calc} else {
      setcolorder(profit01Calc, names(profit03Analysis))
      profit03Analysis <- rbindlist(list(profit03Analysis, profit01Calc))}
    remove(profit01Calc)
    
    #Work out how close we are to the goal
    eval(parse(text=paste("profit03Analysis[, Diff:= ",VarProfitTargetVariable," 
                          - ifelse(L_Product_Type=='Consumer',targets[L_Product_Type=='Consumer',Target],
                          ifelse(L_Product_Type=='SoleTrader',targets[L_Product_Type=='SoleTrader',Target],
                          targets[L_Product_Type=='Company',Target]))]",sep = "")))
    profit03Analysis[, Abs:= abs(Diff)]
    profit03Analysis[, Sign:= ifelse(Diff<0,-1,1)]
    
    #######################################@
    ##### ___18b. What stage are we at #####
    #######################################@
    
    #Work out what stage this application is up to with the margin we've just tested.
    #For this we should be ordered based on iteration, to get the last iteration.
    profit03Analysis <- profit03Analysis[order(ID_Application_ID, Iteration)]
    profit03Analysis[, Stage:=ifelse(Iteration==1,"1: Work through increments",
                                     shift(Stage_Next,1L,type="lag"))]
    
    
    #The theoretical next stage for them all is as follows, but this might be overwritten with "5. End" later.
    #This "next stage" is based on the last iteration's stage, so we should sort by iteration.
    profit03Analysis <- profit03Analysis[order(ID_Application_ID, Iteration)]
    profit03Analysis[, Stage_Next:= ifelse(Iteration != i_Profit,                     Stage_Next,
                                           ifelse(Iteration == 1, "1: Work through increments",
                                                  ifelse(Abs <= VarProfitPlusOrMinus, "5. End",
                                                         ifelse(Stage     == "1: Work through increments", 
                                                                ifelse(shift(Sign,1L,type="lag") != Sign,"2: Split to get zero diff",
                                                                       ifelse(i_Profit < length(VarProfitMarginsToTest), "1: Work through increments","3: Start closest splits")),
                                                                ifelse(Stage     == "2: Split to get zero diff","2: Split to get zero diff",
                                                                       ifelse(Stage     == "3: Start closest splits",  "4. Split to get closest",
                                                                              ifelse(Stage     == "4. Split to get closest",  "4. Split to get closest", "??")))))))]
    
    ###########################################@
    ##### ___18c. End as within goal range #####
    ###########################################@
    
    #"5: End"
    profit03Analysis[Iteration == i_Profit & Stage_Next == "5. End", L_Margin_Rate_Result := L_Margin_Rate]
    profit03Analysis[Iteration == i_Profit & Stage_Next == "5. End" , L_Margin_Rate_To_Test:= as.numeric(NA)]
    
    
    ####################################################@
    ##### ___18d. Working through margin increments #####
    ####################################################@
    
    #"1: Work through increments".
    # This is the easiest, we just grab the next margin and put it in.
    profit03Analysis[Iteration == i_Profit & Stage_Next == "1: Work through increments", 
                     L_Margin_Rate_To_Test := VarProfitMarginsToTest[i_Profit + 1]]
    profit03Analysis[Iteration == i_Profit & Stage_Next == "1: Work through increments", 
                     L_Margin_Rate_Result := as.numeric(NA)]
    
    #############################################################@
    ##### ___18e. Crossed the goal, so split to get the goal #####
    #############################################################@
    
    #"2: Split to get zero diff"
    # As we are using "lead" and "lag", we can't filter before doing the rules
    # Also, for this we need to order by margin, so we can find the one that is with the other sign
    profit03Analysis <- profit03Analysis[order(ID_Application_ID, L_Margin_Rate)]
    # profit03Analysis[, Lead_Or_Lag := ifelse(Iteration != i_Profit | Stage_Next != "2: Split to get zero diff", Lead_Or_Lag,
    #                                   ifelse(shift(Sign, 1L, type="lag")!=Sign, "lag", "lead"))]                       
    profit03Analysis[, L_Margin_Rate_Previous := ifelse(Iteration != i_Profit | Stage_Next != "2: Split to get zero diff", L_Margin_Rate_Previous,
                                                        ifelse(shift(Sign, 1L, type="lag")!=Sign, shift(L_Margin_Rate, 1L, type="lag"), shift(L_Margin_Rate, 1L, type="lead")))]
    profit03Analysis[, Abs_Previous := ifelse(Iteration != i_Profit | Stage_Next != "2: Split to get zero diff", Abs_Previous,
                                              ifelse(shift(Sign, 1L, type="lag")!=Sign, shift(Abs, 1L, type="lag"), shift(Abs, 1L, type="lead")))]
    profit03Analysis[, L_Margin_Rate_To_Test_Calc := ifelse(Iteration != i_Profit | Stage_Next != "2: Split to get zero diff", L_Margin_Rate_To_Test_Calc,
                                                            round((L_Margin_Rate + L_Margin_Rate_Previous)/2, digits=VarProfitMarginDecimalPlaces))]
    
    #Test this is a valid new margin to test, or same as one of the previous ones, in which case we overwrite to "5. End"
    #Using abs(x-y)<0.0001 rather than x==y, due to floating point issues
    profit03Analysis[, L_Margin_Rate_To_Test := ifelse(Iteration != i_Profit | Stage_Next != "2: Split to get zero diff", L_Margin_Rate_To_Test,
                                                       ifelse(abs(L_Margin_Rate_To_Test_Calc - L_Margin_Rate_Previous) < 0.0001 | 
                                                                abs(L_Margin_Rate_To_Test_Calc - L_Margin_Rate)          < 0.0001,
                                                              NA, 
                                                              L_Margin_Rate_To_Test_Calc))]
    profit03Analysis[, L_Margin_Rate_Result  := ifelse(Iteration != i_Profit | Stage_Next != "2: Split to get zero diff", L_Margin_Rate_Result,
                                                       ifelse(!is.na(L_Margin_Rate_To_Test), 
                                                              NA, 
                                                              ifelse(Abs_Previous<Abs, L_Margin_Rate_Previous, L_Margin_Rate)))]
    profit03Analysis[, Stage_Next            := ifelse(Iteration != i_Profit | Stage_Next != "2: Split to get zero diff", Stage_Next,
                                                       ifelse(!is.na(L_Margin_Rate_To_Test), 
                                                              "2: Split to get zero diff",
                                                              "5. End"))] 
    
    ################################################################@
    ##### ___18f. Arrived at end of increments without crossing #####
    ################################################################@
    
    #"3: Start closest splits"
    #This one is a bit more complicated as first you need to work out the closest, and start again from there.
    #Grab all the contracts in this stage
    profit03AnalysisTempA <- profit03Analysis[ID_Application_ID %in% 
                                                profit03Analysis[Iteration == i_Profit & Stage_Next == "3: Start closest splits", ID_Application_ID]]
    if(profit03AnalysisTempA[, .N] > 0){
      profit03AnalysisTempA[, Closest_Flag:= ifelse(Abs == min(Abs),1,0), by=ID_Application_ID]
      profit03AnalysisTempA[, L_Margin_Rate_To_Test_Special:= ifelse(Closest_Flag == 0, as.numeric(NA),
                                                                     round((L_Margin_Rate + 
                                                                              ifelse(L_Margin_Rate == min(VarProfitMarginsToTest), shift(L_Margin_Rate, 1L, type="lead"),
                                                                                     ifelse(L_Margin_Rate == max(VarProfitMarginsToTest), shift(L_Margin_Rate, 1L, type="lag"),
                                                                                            ifelse(shift(Abs, 1L, type="lead") < shift(Abs, 1L, type="lag"),
                                                                                                   shift(L_Margin_Rate, 1L, type="lead"),
                                                                                                   shift(L_Margin_Rate, 1L, type="lag")))))/2, digits=VarProfitMarginDecimalPlaces))]
      profit03AnalysisTempB <- profit03AnalysisTempA[Closest_Flag == 1, c("ID_Application_ID", "L_Margin_Rate_To_Test_Special"), with=FALSE]
      
      #Merge this back on to the main data, at the last iteration one
      profit03AnalysisTempB[, Iteration:=i_Profit]
      setkey(profit03AnalysisTempB, ID_Application_ID, Iteration)
      setkey(profit03Analysis, ID_Application_ID, Iteration)
      profit03Analysis <- profit03AnalysisTempB[profit03Analysis]
      rm(profit03AnalysisTempB)
      
      #Set up the "next ones to test" for these rows
      profit03Analysis[, L_Margin_Rate_To_Test := ifelse(Iteration != i_Profit | Stage_Next != "3: Start closest splits", L_Margin_Rate_To_Test,
                                                         L_Margin_Rate_To_Test_Special)]
      profit03Analysis[, L_Margin_Rate_Result  := ifelse(Iteration != i_Profit | Stage_Next != "3: Start closest splits", L_Margin_Rate_Result,
                                                         as.numeric(NA))]
      profit03Analysis[, L_Margin_Rate_To_Test_Special := NULL]}
    
    #Whether or not it is populated, need to remove it
    rm(profit03AnalysisTempA)
    
    #########################################@
    ##### ___18g. Find "Closest" to goal #####
    #########################################@
    
    #"4. Split to get closest"
    # As we are using "lead" and "lag", we can't filter before doing the rules
    # Also, for this we need to order by margin, so we can find the one that is with the other sign
    profit03Analysis <- profit03Analysis[order(ID_Application_ID, L_Margin_Rate)]
    
    #If we happen to get a different sign, then definitely split this way, otherwise split by closest
    profit03Analysis[, L_Margin_Rate_Previous := ifelse(Iteration != i_Profit | Stage_Next != "4. Split to get closest", L_Margin_Rate_Previous,
                                                        ifelse(shift(Sign, 1L, type="lag")!=Sign,  shift(L_Margin_Rate, 1L, type="lag"), 
                                                               ifelse(shift(Sign, 1L, type="lead")!=Sign, shift(L_Margin_Rate, 1L, type="lead"),
                                                                      ifelse(shift(Abs, 1L, type="lag")<shift(Abs, 1L, type="lead"), 
                                                                             shift(L_Margin_Rate, 1L,  type="lag"), shift(L_Margin_Rate, 1L,  type="lead")))))]                       
    profit03Analysis[, Abs_Previous := ifelse(Iteration != i_Profit | Stage_Next != "4. Split to get closest", Abs_Previous,
                                              ifelse(shift(Sign, 1L, type="lag")!=Sign,  shift(Abs, 1L,  type="lag"), 
                                                     ifelse(shift(Sign, 1L, type="lead")!=Sign, shift(Abs, 1L,  type="lead"),
                                                            ifelse(shift(Abs, 1L, type="lag")<shift(Abs, 1L, type="lead"), 
                                                                   shift(Abs, 1L,  type="lag"), shift(Abs, 1L,  type="lead")))))]                       
    profit03Analysis[, L_Margin_Rate_To_Test_Calc := ifelse(Iteration != i_Profit | Stage_Next != "4. Split to get closest", L_Margin_Rate_To_Test_Calc,
                                                            round((L_Margin_Rate + L_Margin_Rate_Previous)/2, digits=VarProfitMarginDecimalPlaces))]
    
    #Test this is a valid new margin to test, or same as one of the previous ones, in which case we overwrite to "5. End"
    #Using abs(x-y)<0.0001 rather than x==y, due to floating point issues
    profit03Analysis[, L_Margin_Rate_To_Test := ifelse(Iteration != i_Profit | Stage_Next != "4. Split to get closest", L_Margin_Rate_To_Test,
                                                       ifelse(abs(L_Margin_Rate_To_Test_Calc - L_Margin_Rate_Previous) < 0.0001 | 
                                                                abs(L_Margin_Rate_To_Test_Calc - L_Margin_Rate)          < 0.0001,
                                                              NA, 
                                                              L_Margin_Rate_To_Test_Calc))]
    profit03Analysis[, L_Margin_Rate_Result  := ifelse(Iteration != i_Profit | Stage_Next != "4. Split to get closest", L_Margin_Rate_Result,
                                                       ifelse(!is.na(L_Margin_Rate_To_Test), 
                                                              NA, 
                                                              ifelse(Abs_Previous<Abs,L_Margin_Rate_Previous, L_Margin_Rate)))]
    profit03Analysis[, Stage_Next            := ifelse(Iteration != i_Profit | Stage_Next != "4. Split to get closest", Stage_Next,
                                                       ifelse(!is.na(L_Margin_Rate_To_Test), 
                                                              "4. Split to get closest",
                                                              "5. End"))] 
    
    ###########################################@
    ##### ___18h. Set up next data to test #####
    ###########################################@
    
    #Set up the data to merge back on
    profit04Small <- profit03Analysis[Iteration==i_Profit, ]
    
    #Merge this data back on to the main
    profit04Small <- profit04Small[, c("ID_Application_ID", "L_Margin_Rate_To_Test"), with=FALSE]
    setkey(profit04Small, ID_Application_ID)
    setkey(data_fin, ID_Application_ID)
    data_fin <- profit04Small[data_fin]
    remove(profit04Small)
    
    #Remove the rows we don't need to test again
    data_fin <- data_fin[!(is.na(L_Margin_Rate_To_Test)),]
    
    #If we don't need to run any again, break out of the loop
    if(data_fin[, .N]==0){break}
    
    #Set up the next margin to test
    data_fin[, L_Margin_Rate := L_Margin_Rate_To_Test]
    data_fin[, L_Margin_Rate_To_Test:=NULL]
    
  }
  
  #Clean up
  gc()
  
} #This line ends the huge goal seek loop

####################################@
###### 19. END Goal Seek Loop #######
####################################@
cat(paste("\n",now(),"19. END Goal Seek Loop"))
remove(data_fin, i_max, i_Profit, i_Profit_Max)
remove(list = ls(pattern = "gbm_")) 

#########################################@
###### ___19a. Goal Seek Summaries #######
#########################################@

if(VarMargin!="Goal_Seek"){
  #IF THE SCENARIO IS TO NOT DO A GOAL SEEK# 
  cat(" [Scenario: No Goal Seek]")
} else {
  #IF THE SCENARIO IS TO DO A GOAL SEEK# 
  cat(" [Scenario: Goal Seek End]")
  
  #Work out the naming convention of the files 
  VarFilePrefix <- paste(VarSample, ", ", VarScenario, ", ", VarMarginName, sep="")
  
  #Add variables to the data saying what scenario etc it is
  profit03Analysis[, ZZ_Scenario := VarScenario]
  profit03Analysis[, ZZ_Sample   := VarSample]
  VarDate <- Sys.time()
  profit03Analysis[, ZZ_Date     := VarDate]
  remove(VarDate)
  profit03Analysis[, ZZ_Target_Variable:= VarProfitTargetVariable]
  profit03Analysis[, ZZ_Target   :=ifelse(L_Product_Type=="Consumer",  targets[L_Product_Type=="Consumer"  ,Target],
                                          ifelse(L_Product_Type=="SoleTrader",targets[L_Product_Type=="SoleTrader",Target],
                                                 targets[L_Product_Type=="Company",Target]))]
  
  #Put the ZZ Variables at the front
  VarZZNamesPlus <- c("ZZ_Scenario", "ZZ_Sample", "ZZ_Date", "ZZ_Target_Variable", "ZZ_Target")
  setcolorder(profit03Analysis, c(VarZZNamesPlus, setdiff(names(profit03Analysis),VarZZNamesPlus)))
  remove(VarZZNamesPlus)
  
  #Do other cleaning up
  profit03Analysis[, Iterations_Total:=max(Iteration), by=ID_Application_ID]
  profit04Results <- profit03Analysis[!is.na(L_Margin_Rate_Result),]
  profit04Results[, L_Margin_Rate:=NULL]
  
  #Save outputs
  write.csv(profit03Analysis, file = paste(VarPathStore,"Output","Financial",paste(VarFilePrefix,", Goal Seek Analysis.csv",sep=""),sep=VarPathSeparator), row.names=FALSE)
  write.csv(profit04Results,  file = paste(VarPathStore,"Output","Financial",paste(VarFilePrefix,", Goal Seek Results.csv" ,sep=""),sep=VarPathSeparator), row.names=FALSE)
  
  #Load the master prices file
  load(paste(VarPathStore,"Data","Standard","data_prices.RData", sep=VarPathSeparator))
  
  #If the column name already exists, remove it
  if(c(VarMarginName) %in% names(data_prices)){data_prices[, c(VarMarginName):=NULL]}
  
  #Merge on the new data
  profit05ToMerge <- profit04Results[, c("ID_Application_ID", "L_Margin_Rate_Result"), with=FALSE]
  setnames(profit05ToMerge,"L_Margin_Rate_Result",VarMarginName)
  setkey(profit05ToMerge, ID_Application_ID)
  setkey(data_prices,     ID_Application_ID)
  data_prices <- profit05ToMerge[data_prices]
  
  #If any rows weren't in the original data set, need to add them on
  profit05ToMerge <- profit05ToMerge[!(ID_Application_ID %in% data_prices[,ID_Application_ID]),]
  data_prices<-rbindlist(list(data_prices,profit05ToMerge), use.names=TRUE, fill=TRUE)
  rm(profit05ToMerge)
  
  #Save the new file, and also save an archived version
  save(data_prices,file=paste(VarPathStore,"Data","Standard","data_prices.RData", sep=VarPathSeparator))
  save(data_prices,file=paste(VarPathStore,"Data","Standard","Archive",paste("data_prices_",format(now(),"%Y-%m-%d-%H-%M"),".RData",sep=""), sep=VarPathSeparator))
  
  #Clean up
  remove(targets, data_fin_processed)
  remove(mMainFunction
         ,biasAdj,dealerAdj
         ,selectedContracts
         ,data_eco, data_eco_month
         ,VarEarlyTerminationCommercialFactor, VarEarlyTerminationConsumerBase,VarEarlyTerminationExpense
         ,VarEarlyTerminationCap_New, VarEarlyTerminationDischarge_New, VarEarlyTerminationFactor_New
         ,VarEstablishmentFeeCommercial_New, VarEstablishmentFeeConsumer_New
         ,VarEstablishmentFeeCommercial_Old, VarEstablishmentFeeConsumer_Old
         ,VarEstablishmentExpense
         ,VarMonthlyFee, VarMonthlyExpense
         ,VarGFVOptionCost
         ,VarProfitTargetVariable, VarProfitMarginDecimalPlaces, VarProfitMarginsToTest
         ,VarProfitPlusOrMinus, VarProfitRunWholeRange
         ,VarRecoveryCommission
         ,VarPreLoopRows)
  if(VarScenarioMatrix[Scenario==VarScenario,FinancialAdjust]==TRUE){rm(FinancialAdjustments)}
  if(VarScenarioMatrix[Scenario==VarScenario,CountAdjust]==TRUE){rm(CountsAdjustments)}
  remove(VarFilePrefix, VarScenarioMatrix)
  
  #Stop the macro
  stop("No further summaries will be created in Goal Seek.")
}

#########################@
###### 20. Tidy Up #######
#########################@
cat(paste("\n",now(),"20. Tidy Up"))

#######################################@
###### ___20a. Additional Fields #######
#######################################@

#Work out the naming convention of the files 
VarFilePrefix <- paste(VarSample, ", ", VarScenario, ", ", VarMargin, sep="")
if(VarMargin != "L_Margin_Rate" & VarMargin != "Goal_Seek"){
  VarFilePrefix <- paste(VarFilePrefix,", ",VarProfitMaxMargin,VarCapDecline, sep="")
}

#Add variables to the data saying what scenario etc it is
data_fin_processed[, ZZ_Scenario := VarScenario]
data_fin_processed[, ZZ_Sample   := VarSample]
VarDate <- Sys.time()
data_fin_processed[, ZZ_Date     := VarDate]
remove(VarDate)
if(VarMargin != "L_Margin_Rate" & VarMargin != "Goal_Seek"){
  data_fin_processed[, ZZ_Margin   := paste(VarMargin,", ",VarProfitMaxMargin,VarCapDecline, sep="")]
} else {
  data_fin_processed[, ZZ_Margin   := VarMargin]
}

#Put the ZZ Variables at the front
VarZZNames <- c("ZZ_Scenario", "ZZ_Sample", "ZZ_Margin", "ZZ_Date")
setcolorder(data_fin_processed, c(VarZZNames, setdiff(names(data_fin_processed),VarZZNames)))

#############################################@
###### ___20b. Additional Calculations #######
#############################################@

#Years for timing
data_fin_processed[, Z_Month_Year    :=lubridate::year(Z_Month)]

#Work out the margin rate difference
data_fin_processed[, L_Margin_Rate_Diff:=round(L_Margin_Rate-L_Margin_Rate_Original, digits=2)]

#Work out the TDN Flag
data_fin_processed[, V_TDN_Flag:=ifelse((V_Make_Band=="TOYOTA" | V_Make_Band == "LEXUS") 
                                        & (V_New_Used_Flag=="D" | V_New_Used_Flag=="N")
                                        , TRUE, FALSE)]
data_fin_processed[, V_TDNH_Flag:=ifelse((V_Make_Band=="TOYOTA" | V_Make_Band == "LEXUS" | V_Make_Band == "HINO") 
                                         & (V_New_Used_Flag=="D" | V_New_Used_Flag=="N")
                                         , TRUE, FALSE)]

################################@
###### 21. Save & Outputs #######
################################@
cat(paste("\n",now(),"21. Save & Outputs"))

########################################@
###### ___21a. Save Month Per Row #######
########################################@

#Save the full data set
save(data_fin_processed, file=paste(VarPathStore,"Data","Scenarios",paste(VarPrefix,"_",VarFilePrefix,", Data.RData",sep=""),sep=VarPathSeparator), compress=TRUE)

#####################################@
###### ___21b. Special Outputs #######
#####################################@

##### ___ ___Counts Reconciliation #####
#Only output this if we are matching the recon data (ie "AsData") and we are showing Contracts not Apps.
if(VarScenarioMatrix[Scenario==VarScenario,Date]=="AsData" & grepl("Contracts", VarSample)==TRUE){
  mSum <- function(x) {
    output_data <- sum(as.numeric(x), na.rm=TRUE)
    return(output_data)
  }
  summary01ReconCounts <- 
    data_fin_processed[Z_Month <= I_Extract_Up_To #Only data we can reconcile
                       , lapply(.SD, mSum)
                       , by=c(VarZZNames 
                              ,"L_Product_Type","L_Subvention_Flag"
                              ,"L_Loan_Application_Year","L_Loan_Term_Num_Rounded","Z_Month_Index")
                       , .SDcols=c("Z_Taken_Up", "Z_Taken_Up_P"
                                   , "Z_Termination_Month_Flag","Z_Termination_Month_Flag_P", "Z_Termination_Month_Flag_P_Correct"
                                   , "Z_Write_Off_Month_Flag","Z_Write_Off_Month_Flag_P_Unbias", "Z_Write_Off_Month_Flag_P_Correct")]
  summary01ReconCounts[, Z_Taken_Up  :=ifelse(Z_Month_Index==0, Z_Taken_Up,   0)]
  summary01ReconCounts[, Z_Taken_Up_P:=ifelse(Z_Month_Index==0, Z_Taken_Up_P, 0)]
  write.csv(summary01ReconCounts, file = paste(VarPathStore,"Output","Financial",paste(VarPrefix,"_",VarFilePrefix,", 01ReconCounts.csv",sep=""), sep=VarPathSeparator), row.names=FALSE)
  
}

##### ___ ___Financials Reconciliation #####
#Only output this if we are matching the recon data (ie "AsData")
if(VarScenarioMatrix[Scenario==VarScenario,Date]=="AsData"){
  mSum <- function(x) {
    output_data <- sum(as.numeric(x), na.rm=TRUE)
    return(output_data)
  }
  summary02ReconFinancials <- 
    data_fin_processed[Z_Month <= I_Extract_Up_To #Only data we can reconcile
                       , lapply(.SD, mSum)
                       , by=c(VarZZNames 
                              ,"L_Product_Type","L_Subvention_Flag"
                              ,"L_Loan_Application_Year","L_Loan_Term_Num_Rounded","Z_Month_Index")
                       , .SDcols=c("T_Fee_Early_Termination"
                                   ,"T_Fee_Establishment_Cash"
                                   ,"T_Fee_Monthly"
                                   ,"T_Interest_Accounting"
                                   ,"T_Transfer_Price_Accounting"
                                   ,"T_Written_Off")]
  summary02ReconFinancials<-melt(summary02ReconFinancials
                                 ,id.vars=c(VarZZNames 
                                            ,"L_Product_Type","L_Subvention_Flag"
                                            ,"L_Loan_Application_Year","L_Loan_Term_Num_Rounded","Z_Month_Index")
                                 ,measure.vars=c("T_Fee_Early_Termination"
                                                 ,"T_Fee_Establishment_Cash"
                                                 ,"T_Fee_Monthly"
                                                 ,"T_Interest_Accounting"
                                                 ,"T_Transfer_Price_Accounting"
                                                 ,"T_Written_Off"))
  setnames(summary02ReconFinancials, "variable", "Z_GL_Name")
  setnames(summary02ReconFinancials, "value", "Z_Amount")
  summary02ReconFinancials<-summary02ReconFinancials[Z_Amount!=0,]
  write.csv(summary02ReconFinancials, file = paste(VarPathStore,"Output","Financial",paste(VarPrefix,"_",VarFilePrefix,", 02ReconFinancials.csv",sep=""), sep=VarPathSeparator), row.names=FALSE)
}

######################################@
###### ___21c. Save App per Row #######
######################################@

#Work out the by vars. Need to include here all possible ones that you are going to report on.
VarByVarsInit <- c(VarZZNames
                   ,"ID_Application_ID", "ID_Contract_ID"
                   ,"Z_ToExclude"
                   #,"ID_Dealer_Group_Name", "ID_Dealer_Group_ID", "ID_Dealer_Name", "ID_Dealer_ID", "ID_Dealer_Region", "ID_Dealer_PMA" #All these are replaced by the X versions, which are more accurate
                   ,"X_Dealer_Group_Name",  "X_Dealer_Group_ID",  "X_Dealer_Name",  "X_Dealer_ID",  "X_Dealer_Region",  "X_Dealer_PMA"
                   ,"X_Campaign_Name", "X_Campaign_Type"
                   ,"L_Product_Type", "L_Loan_Type", "L_Subvention_Flag"
                   ,"V_Make_Band","V_Make_Category","V_Model_Band","V_Model_ToyLex","V_New_Used_Flag", "V_TDN_Flag", "V_TDNH_Flag", "V_Vehicle_Age"
                   ,"D_Grade","L_Financier", "L_Finance_Brand"
                   ,"X_Loan_Date_Month","X_Loan_Date_Year" #Can't have Z_Month_Year as it makes it no longer line-per-app
                   ,"X_Finance_FlexOrNAF", "L_Finance_Rate_Type", "X_Loan_Date_Year", "V_TCUV", "L_Financier", "L_Finance_Brand")

#Summarise down to one row per app
data_fin_processed_apps<-
  data_fin_processed[, .(#One per app
    L_Net_Amount_Financed              = sum(L_Net_Amount_Financed  * (Z_Month_Index==0))
    ,L_RV_Band                          = sum(L_RV_Band              * (Z_Month_Index==0))
    ,L_Margin_Rate                      = sum(L_Margin_Rate          * (Z_Month_Index==0))
    ,L_Finance_Rate                     = sum(L_Finance_Rate         * (Z_Month_Index==0))
    ,X_Finance_Rate                     = sum(X_Finance_Rate         * (Z_Month_Index==0))
    ,L_Margin_Rate_Diff                 = sum(L_Margin_Rate_Diff     * (Z_Month_Index==0))
    ,Z_Taken_Up                         = sum(Z_Taken_Up             * (Z_Month_Index==0))
    ,Z_Taken_Up_P                       = sum(Z_Taken_Up_P           * (Z_Month_Index==0))
    ,X_DAF                              = sum(X_DAF                  * (Z_Month_Index==0))
    ,X_DOF                              = sum(X_DOF                  * (Z_Month_Index==0))
    #Commission Numbers
    ,X_Finance_Commission               = sum(X_Finance_Commission   * (Z_Month_Index==0))
    ,X_Finance_Clawback                 = sum(X_Finance_Clawback     * (Z_Month_Index==0))
    ,X_Subvention_Amount                = sum(X_Subvention_Amount    * (Z_Month_Index==0))
    #Commission In Month Numbers
    ,X_Finance_Commission_Accounting    = sum(X_Finance_Commission_Accounting)
    ,X_Finance_Clawback_InMonth         = sum(X_Finance_Clawback_InMonth, na.rm=TRUE)
    ,X_Subvention_Amount_Accounting     = sum(X_Subvention_Amount_Accounting)
    #Term Calculations
    ,L_Loan_Term_Num                    = sum(L_Loan_Term_Num * (Z_Month_Index==0))
    ,L_Loan_Term_Num_Expected           = sum(Z_Month_Index * (Z_Termination_Month_Flag_P_Correct + Z_Write_Off_Month_Flag_P_Correct))
    ,L_Loan_Term_Num_Balance            = sum(T_Balance) / sum(L_Net_Amount_Financed  * (Z_Month_Index==0))
    #One per month
    ,T_Balance                          = sum(T_Balance)
    ,M_Balance                          = sum(M_Balance)
    ,X_Unrecog_Total                    = sum(X_Unrecog_Total)
    ,T_Unrecog_Fee_Establishment        = sum(T_Unrecog_Fee_Establishment)
    ,X_Unrecog_Finance_Commission       = sum(X_Unrecog_Finance_Commission)
    ,X_Unrecog_Subvention_Amount        = sum(X_Unrecog_Subvention_Amount)
    ,T_Fee_Establishment_Accounting     = sum(T_Fee_Establishment_Accounting)
    ,M_Interest_Accounting              = sum(M_Interest_Accounting)
    ,T_Interest_Accounting              = sum(T_Interest_Accounting)
    ,M_Transfer_Price_Accounting        = sum(M_Transfer_Price_Accounting)
    ,T_Transfer_Price_Accounting        = sum(T_Transfer_Price_Accounting)
    ,M_Fee_Monthly                      = sum(M_Fee_Monthly)
    ,T_Fee_Monthly                      = sum(T_Fee_Monthly)
    ,T_Fee_Early_Termination            = sum(T_Fee_Early_Termination)
    ,T_Written_Off                      = sum(T_Written_Off)
    ,T_Expense_Establishment            = sum(T_Expense_Establishment) 
    ,T_Expense_Monthly                  = sum(T_Expense_Monthly) 
    ,T_Expense_Early_Termination        = sum(T_Expense_Early_Termination) 
    ,T_Expense_GFV_Option               = sum(T_Expense_GFV_Option) 
    ,T_Net_Before_Commission_Cash       = sum(T_Net_Before_Commission_Cash)
    ,T_Net_After_Commission_Cash        = sum(T_Net_After_Commission_Cash)
    ,T_Net_Before_Commission_Accounting = sum(T_Net_Before_Commission_Accounting)
    ,Z_Write_Off_Month_Flag_P_Correct   = sum(Z_Write_Off_Month_Flag_P_Correct)
    ,Z_Write_Off_Month_Flag             = sum(Z_Write_Off_Month_Flag)
    ,Z_Termination_Month_Flag_P_Correct = sum(Z_Termination_Month_Flag_P_Correct)
    ,Z_Termination_Month_Flag           = sum(Z_Termination_Month_Flag)
    ,Z_Write_Off_Net_Dollar_P           = sum(Z_Write_Off_Net_Dollar_P)
    ,Z_Write_Off_Net_Dollar_MoveHS      = sum(Z_Write_Off_Net_Dollar_MoveHS)
  ),by=c(VarByVarsInit)]

#Add Profitability measures
data_fin_processed_apps[,T_Profit_Per_Thou    := T_Net_Before_Commission_Cash       / (L_Net_Amount_Financed / 1000)]
data_fin_processed_apps[,T_Contribution_Ratio := 100 * T_Net_Before_Commission_Cash / (T_Balance / 12)]
data_fin_processed_apps[,T_ROSABC             := 100 * T_Net_Before_Commission_Cash / (M_Balance / 12)]
data_fin_processed_apps[,T_ROA                := 100 * T_Net_After_Commission_Cash  / (T_Balance / 12)]

#Save this data set
save(data_fin_processed_apps, file=paste(VarPathStore,"Data","Scenarios",paste(VarPrefix,"_",VarFilePrefix,", Data_Apps.RData",sep=""),sep=VarPathSeparator), compress=TRUE)

##############################@
###### ___21e. Clean Up #######
##############################@

# Remove the models
remove(list = ls(pattern = "gbm_")) 

# Remove other items
remove(biasAdj,dealerAdj
       # ,mSum
       ,mMainFunction
       ,selectedContracts
       ,data_eco, data_eco_month
       ,VarEarlyTerminationCommercialFactor, VarEarlyTerminationConsumerBase,VarEarlyTerminationExpense
       ,VarEarlyTerminationCap_New, VarEarlyTerminationDischarge_New, VarEarlyTerminationFactor_New
       ,VarEstablishmentFeeCommercial_New, VarEstablishmentFeeConsumer_New
       ,VarEstablishmentFeeCommercial_Old, VarEstablishmentFeeConsumer_Old
       ,VarEstablishmentExpense
       ,VarMonthlyFee, VarMonthlyExpense
       ,VarGFVOptionCost
       ,VarProfitTargetVariable, VarProfitMarginDecimalPlaces, VarProfitMarginsToTest
       ,VarProfitPlusOrMinus, VarProfitRunWholeRange
       ,VarRecoveryCommission
       ,VarPreLoopRows
       ,VarZZNames, VarByVarsInit)
if(VarScenarioMatrix[Scenario==VarScenario,FinancialAdjust]==TRUE){rm(FinancialAdjustments)}
if(VarScenarioMatrix[Scenario==VarScenario,CountAdjust]    ==TRUE){rm(CountsAdjustments)}
remove(VarScenarioMatrix)
