##### !! DEBUGGING !! #####
#If you are just manually running the code, then you'll need to un-comment out and run this below line, after running the first part of "E01 Financial Model.R"
# data_fin_working <- data_fin

mMainFunction <- function(data_fin_working) {
  
  ##################################################@
  ############## 09. Unpadded incl GBM ##############
  ##################################################@
  cat(paste("\n",now(),"09. Unpadded"))
  
  #################################################@
  ############## ___09a. Calculations ##############
  #################################################@
  
  #Payments are actually rounded, then corrected in the final payment
  #ASSUMPTION 1: All Payments Monthly
  data_fin_working[, L_Payment_Monthly_ExGST:=round(L_Payment_Monthly_ExGST,digits=2)]
  
  # Work out how far to pad The Data. We always pad at least 36 as we need that to amortise over.
  #   However we ignore past Z_Months_To_Pad_Full in the predictive models, as these were built to do 36 for a 12 month term
  data_fin_working[, Z_Months_To_Pad_Full  := L_Month_All_End + 12]
  data_fin_working[, Z_Months_To_Pad_Amort := pmax(36,Z_Months_To_Pad_Full)]
  
  #Adjust the economic/timing variables if required
  if(VarScenarioMatrix[Scenario==VarScenario,EcoTPAdjust]>0){
    #IF THE SCENARIO IS TO USE FORECAST ECONOMIC TIMING VARIABLES#  
    cat(" [Scenario: Forecast Economic/Timing]")
    
    #When forecasting, the following variables need to be pushed to their maximum that is stable in the GBMs.
    data_fin_working[, L_Loan_Application_Year        :=lubridate::year(I_Extract_Up_To)]
    data_fin_working[, L_Loan_Application_Month_Index := as.integer(12*(lubridate::year(I_Extract_Up_To)-2010) + lubridate::month(I_Extract_Up_To))]
    
    #And for the economic variables, replace the old ones with the forecast ones calculated in 
    #   "Data\Financial\Inflation_Forecasts_Data.xlsb"
    
    #Remove the existing variables
    data_fin_working[, grep("G_ECO",names(data_fin_working), value=TRUE):=NULL]
    #Create the merging variables
    data_fin_working[, Quarter_Fixed:=paste(lubridate::year(L_Loan_Application_Date),"Q",floor((lubridate::month(L_Loan_Application_Date)-1)/3)+1,sep="")]
    data_fin_working[, State        :=C_C1_Curr_Address_State]
    #Merge on the two data sets
    setkey(data_fin_working,    Quarter_Fixed, State)
    setkey(data_eco,            Quarter_Fixed, State)
    data_fin_working<-data_eco[data_fin_working]
    #Remove ancilliary fields
    data_fin_working[, c("Quarter_Fixed", "State"):=NULL]
  } else {
    cat(" [Scenario: Actual Economic/Timing]")
  }   
  
  #Merge in the dealer adjustments
  setkey(dealerAdj, ID_Dealer_ID)
  setkey(data_fin_working, ID_Dealer_ID)
  data_fin_working<-dealerAdj[data_fin_working]
  
  #Fix up the simple fees if we are forecasting
  if(VarScenarioMatrix[Scenario==VarScenario,OtherFeeAdjust]==TRUE){
    #IF THE SCENARIO IS TO USE NEW OTHER FEES# 
    cat(" [Scenario: New Other Fees]")
    
    #New customer monthly fee
    #First change the monthly payment
    data_fin_working[,L_Payment_Monthly_ExGST:=L_Payment_Monthly_ExGST + 
                       ifelse(L_Fee_Monthly==0,0,-L_Fee_Monthly+VarMonthlyFee)]
    #Then change the monthly fee itself
    data_fin_working[,L_Fee_Monthly := ifelse(L_Fee_Monthly==0, 0, VarMonthlyFee)]
    
    #New establishment fee
    #IE multiply by the (new/old), unless we are already at the new when you multiply by new/new ie 1.
    data_fin_working[, L_Establishment_Fee:=round(
      L_Establishment_Fee * 
        ifelse(L_Product_Type=="Consumer", 
               VarEstablishmentFeeConsumer_New/pmax(L_Establishment_Fee,VarEstablishmentFeeConsumer_Old), 
               VarEstablishmentFeeCommercial_New/pmax(L_Establishment_Fee,VarEstablishmentFeeCommercial_Old)),
      digits=2)]
    
  } else {
    cat(" [Scenario: Actual Other Fees]")
  }   
  
  ################################@
  ###### ___09b. Takeup GBM #######
  ################################@
  
  #In this there are actually 3 scenarios
  if(VarScenarioMatrix[Scenario==VarScenario,TU]=="Actual"){
    #IF THE SCENARIO IS TO IGNORE THE TU MODEL# 
    cat(" [Scenario: Take Up Model Ignored]")
    data_fin_working[, Z_Taken_Up_P:=as.numeric(Z_Taken_Up)]
    
    #Fix so that the Cap & Decline excluded contracts get 0, to exclude them
    data_fin_working[Z_ToExclude == "Exclude", 
                     Z_Taken_Up_P:=0]
    
  }
  if(VarScenarioMatrix[Scenario==VarScenario,TU]=="All"){
    #IF THE SCENARIO IS TO ASSUME ALL TAKEN UP# 
    cat(" [Scenario: Assume All Taken Up]")
    data_fin_working[, Z_Taken_Up_P:=1]
  }
  if(VarScenarioMatrix[Scenario==VarScenario,TU]=="Model"){
    #IF THE SCENARIO IS TO DO THE TU MODEL#  
    cat(" [Scenario: Take Up Model Run]")
    
    #GBMs can't deal with "Logical", so convert to Integer, and store list so can convert back later
    VarsLogical<-c()
    for (i in names(data_fin_working)){
      if(is.logical(data_fin_working[, get(i)])){
        VarsLogical<-c(VarsLogical,i)
        data_fin_working[, c(i):=as.integer(get(i))]
      }
    }
    rm(i)
    
    #Run the Take-up GBM (with the appropriate loan type)
    data_fin_working[, Z_Taken_Up_P:=as.numeric(NA)]
    data_fin_working[L_Product_Type=="Consumer",]$Z_Taken_Up_P   <- 
      predict.gbm(object=gbm_tu_Consumer,   
                  newdata=data_fin_working[L_Product_Type=="Consumer",],   
                  n.trees=gbm_best_iter[model=="gbm_tu_Consumer",iters], 
                  type="response")
    data_fin_working[L_Product_Type=="SoleTrader",]$Z_Taken_Up_P <- 
      predict.gbm(object=gbm_tu_SoleTrader, 
                  newdata=data_fin_working[L_Product_Type=="SoleTrader",], 
                  n.trees=gbm_best_iter[model=="gbm_tu_SoleTrader",iters], 
                  type="response")
    data_fin_working[L_Product_Type=="Company",]$Z_Taken_Up_P    <- 
      predict.gbm(object=gbm_tu_Company,    
                  newdata=data_fin_working[L_Product_Type=="Company",],    
                  n.trees=gbm_best_iter[model=="gbm_tu_Company",iters], 
                  type="response")
    
    #Put the variables back to boolean
    for(i in VarsLogical){data_fin_working[, c(i):=as.logical(get(i))]}
    rm(i)
    
    #Fix so that National Direct Centre, Residual Rewrites, and Hardship always get 1 or 0, as they are out of scope or TU model
    data_fin_working[I_National_Direct_Centre == TRUE | I_ResRewrites == TRUE | I_Hardship == TRUE, 
                     Z_Taken_Up_P:=as.numeric(Z_Taken_Up)]
    
    #Fix so that the Cap & Decline excluded contracts get 0, to exclude them
    data_fin_working[Z_ToExclude == "Exclude", 
                     Z_Taken_Up_P:=0]
    
  } #This is the end of the "Do Take Up Model or Not" scenario
  
  
  ###############################@
  ###### 10. Padded incl GBM #####
  ###############################@
  cat(paste("\n",now(),"10. Padded incl GBM"))
  
  ##############################@
  ###### ___10a. Do padding #####
  ##############################@
  cat(paste("\n",now(),"___10a. Do padding"))
  
  #Note that this pads to the expected end plus 12 months, ie longer than the predictive modelling data
  # (which only pads as far as we have data)
  data_fin_padded <- data_fin_working[rep(seq.int(1,nrow(data_fin_working)), Z_Months_To_Pad_Amort), ]
  remove(data_fin_working)  
  
  ########################################@
  ###### ___10b. Calculations for GBM #####
  ########################################@
  cat(paste("\n",now(),"___10b. Calculations for GBM"))
  
  #Month-y fields
  #Note that the below uses an "after-the-fact" variable - X_Loan_Date. 
  #  But this is only for putting the data into the appropriate month, not for any actual forecasting.
  data_fin_padded[, Z_Month_Index := as.integer(ave(ID_Application_ID==ID_Application_ID, ID_Application_ID, FUN=cumsum) - 1L)]
  data_fin_padded[, Z_Month := floor_date(X_Loan_Date %m+% months(Z_Month_Index), "month")] #package: lubridate
  data_fin_padded[, Z_Months_Before_Expected_End  := L_Month_All_End - Z_Month_Index]
  data_fin_padded[, Z_Proportion_Of_Expected_Term := Z_Month_Index / L_Month_All_End]
  data_fin_padded[, Z_Month_Index_From_2010 := as.integer(12*(lubridate::year(Z_Month) - 2010) + lubridate::month(Z_Month))]
  data_fin_padded[, Z_Month_0 := as.logical(Z_Month_Index==0)]
  
  #Add the target variables (for comparison, where we know them)
  data_fin_padded[, Z_Termination_Month_Flag:=ifelse(is.na(Z_Termination_Month_Fixed),0L,
                                                     ifelse(Z_Termination_Month_Fixed==Z_Month_Index,1L,0L))]
  data_fin_padded[, Z_Write_Off_Month_Flag  :=ifelse(is.na(Z_Write_Off_Month_Fixed),0L,
                                                     ifelse(Z_Write_Off_Month_Fixed==Z_Month_Index,1L,0L))]
  data_fin_padded[, Z_Write_Off_Net_Dollar_MoveHS:=ifelse(is.na(Z_Write_Off_Month_Fixed), 0,
                                                          ifelse(Z_Write_Off_Month_Fixed==Z_Month_Index,Z_Write_Off_Net_Dollar_MoveHS,0))]
  
  #Economic variables for the time of the event, rather than the time of the application.
  #Note that we earlier adjusted data_eco_month to be forecast versions if required, 
  #    so no need to do any special work for forecast versions here.
  
  #Need the pmin as we only have eco data up to the I_Extract_Up_To date
  data_fin_padded[, ECO_Date:=pmin(Z_Month,I_Extract_Up_To)]
  
  #Create the merging variables.
  data_fin_padded[, Quarter_Fixed:=paste(lubridate::year(ECO_Date),"Q",floor((lubridate::month(ECO_Date)-1)/3)+1,sep="")]
  data_fin_padded[, State        :=C_C1_Curr_Address_State]
  #Merge on the two data sets
  setkey(data_fin_padded,    Quarter_Fixed, State)
  setkey(data_eco_month,     Quarter_Fixed, State)
  data_fin_padded<-data_eco_month[data_fin_padded]
  #Remove ancilliary fields
  data_fin_padded[, c("ECO_Date","Quarter_Fixed", "State"):=NULL]
  
  #Adjust the economic/timing variables if required
  if(VarScenarioMatrix[Scenario==VarScenario,EcoTPAdjust]>0){
    #IF THE SCENARIO IS TO USE FORECAST TIMING VARIABLES#  
    cat(" [Scenario: Forecast Timing]")
    #Also fix this variable, which we should use as per the last GBM data, like above L_Loan_Application_Year
    data_fin_padded[, Z_Month_Index_From_2010 := as.integer(12*(lubridate::year(I_Extract_Up_To) - 2010) + lubridate::month(I_Extract_Up_To))]
  } else {
    cat(" [Scenario: Actual Timing]")
  }     
  
  #The Development Months
  if(VarScenarioMatrix[Scenario==VarScenario,Date]=="AsData"){
    #IF THE SCENARIO IS TO ONLY DEVELOP UP TO THE END OF THE DATA# 
    cat(" [Scenario: Develop as per Data]")
    data_fin_padded[, Z_Development_Months := pmax(as.integer(interval(Z_Month,I_Extract_Up_To) %/% months(1)),0)]
  } else {
    #IF THE SCENARIO IS TO DEVELOP FULLY# 
    #"Full Development" is 60 months
    cat(" [Scenario: Develop Fully]")
    data_fin_padded[, Z_Development_Months := 60]
  }  
  
  ####################################@
  ###### ___10c. WO/LGD/Term GBMs #####
  ####################################@
  cat(paste("\n",now(),"___10c. WO/LGD/Term GBMs"))
  
  #GBMs can't deal with "Logical", so convert to Integer, and store list to can convert back later
  VarsLogical<-c()
  for (i in names(data_fin_padded)){
    if(is.logical(data_fin_padded[, get(i)])){
      VarsLogical<-c(VarsLogical,i)
      data_fin_padded[, c(i):=as.integer(get(i))]
    }
  }
  rm(i)
  
  # Termination GBM
  data_fin_padded[, Z_Termination_Month_Flag_P:=as.numeric(NA)]
  data_fin_padded[L_Product_Type=="Consumer",]$Z_Termination_Month_Flag_P   <- 
    predict.gbm(object=gbm_term_Consumer,   
                newdata=data_fin_padded[L_Product_Type=="Consumer",],   
                n.trees=gbm_best_iter[model=="gbm_term_Consumer",iters], 
                type="response")
  data_fin_padded[L_Product_Type=="SoleTrader",]$Z_Termination_Month_Flag_P <- 
    predict.gbm(object=gbm_term_SoleTrader, 
                newdata=data_fin_padded[L_Product_Type=="SoleTrader",], 
                n.trees=gbm_best_iter[model=="gbm_term_SoleTrader",iters], 
                type="response")
  data_fin_padded[L_Product_Type=="Company",]$Z_Termination_Month_Flag_P    <- 
    predict.gbm(object=gbm_term_Company,    
                newdata=data_fin_padded[L_Product_Type=="Company",],    
                n.trees=gbm_best_iter[model=="gbm_term_Company",iters], 
                type="response")
  
  # Write Off GBM
  data_fin_padded[, Z_Write_Off_Month_Flag_P:=as.numeric(NA)]
  data_fin_padded[L_Product_Type=="Consumer",]$Z_Write_Off_Month_Flag_P   <- 
    predict.gbm(object=gbm_wo_Consumer,   
                newdata=data_fin_padded[L_Product_Type=="Consumer",],   
                n.trees=gbm_best_iter[model=="gbm_wo_Consumer",iters], 
                type="response")
  data_fin_padded[L_Product_Type=="SoleTrader",]$Z_Write_Off_Month_Flag_P <- 
    predict.gbm(object=gbm_wo_SoleTrader, 
                newdata=data_fin_padded[L_Product_Type=="SoleTrader",], 
                n.trees=gbm_best_iter[model=="gbm_wo_SoleTrader",iters], 
                type="response")
  data_fin_padded[L_Product_Type=="Company",]$Z_Write_Off_Month_Flag_P    <- 
    predict.gbm(object=gbm_wo_Company,    
                newdata=data_fin_padded[L_Product_Type=="Company",],    
                n.trees=gbm_best_iter[model=="gbm_wo_Company",iters], 
                type="response")
  
  #Full Recovery GBM
  data_fin_padded$Z_Full_Recovery_P    <- 
    predict.gbm(object=gbm_fr,    
                newdata=data_fin_padded,    
                n.trees=gbm_best_iter[model=="gbm_fr",iters], 
                type="response")
  
  #Loss Given Default GBM
  data_fin_padded$Z_Write_Off_Net_per_NAF_P    <- 
    predict.gbm(object=gbm_lgd,    
                newdata=data_fin_padded,    
                n.trees=gbm_best_iter[model=="gbm_lgd",iters], 
                type="response")
  
  #Put the variables back to boolean
  for(i in VarsLogical){data_fin_padded[, c(i):=as.logical(get(i))]}
  rm(i)
  
  #Fix so that items past Z_Months_To_Pad_Full are set to 0
  #We need this as although we pad out to a at least 36 months, some contracts only run for eg 12, so 
  #   our predictive models only work for an extra 12, so in this case up to 24 months. So we need to remove items after that.
  data_fin_padded[Z_Month_Index>=Z_Months_To_Pad_Full
                  ,c("Z_Termination_Month_Flag_P", "Z_Write_Off_Month_Flag_P"):=0]
  
  ########################################@
  ###### ___10d. Write-Off Biases  ########
  ########################################@
  cat(paste("\n",now(),"___10d. Write-Off Biases"))
  
  # Apply bias adjustment for Write Off Flag
  data_fin_padded[, Z_Write_Off_Month_Flag_P_Eta := log(Z_Write_Off_Month_Flag_P / (1 - Z_Write_Off_Month_Flag_P)) - 
                    ifelse(L_Product_Type=="Consumer",biasAdj[L_Product_Type=="Consumer",  Z_biasAdj_wo], 
                           ifelse(L_Product_Type=="Company", biasAdj[L_Product_Type=="Company",   Z_biasAdj_wo], 
                                  biasAdj[L_Product_Type=="SoleTrader",Z_biasAdj_wo]))]
  data_fin_padded[, Z_Write_Off_Month_Flag_P_Unbias := exp(Z_Write_Off_Month_Flag_P_Eta) / (1 + exp(Z_Write_Off_Month_Flag_P_Eta))]
  data_fin_padded[, Z_Write_Off_Month_Flag_P_Eta:=NULL]
  
  ############################################################@
  ###### ___10e. Capping, Scaling and Counts Overlays  ########
  ############################################################@
  cat(paste("\n",now(),"___10e. Capping, Scaling and Counts Overlays"))
  
  #Two scenarios - to overlay the global counts adjustments or not
  if(VarScenarioMatrix[Scenario==VarScenario,CountAdjust]==FALSE){
    #IF THE SCENARIO IS TO NOT OVERLAY COUNTS ADJUSTMENTS# 
    cat(" [Scenario: No Counts Adjustments]")
    data_fin_padded[, Z_Termination_Month_Flag_P_Calc:=Z_Termination_Month_Flag_P]
    data_fin_padded[, Z_Write_Off_Month_Flag_P_Calc  :=Z_Write_Off_Month_Flag_P_Unbias]
  } else {
    #IF THE SCENARIO IS TO OVERLAY COUNTS ADJUSTMENTS# 
    cat(" [Scenario: Counts Adjustments]")
    #Merge the termination factors on to the main data [These are the global adjustments for termination]
    TermAdjustments_sd <- TermAdjustments[,.(Z_Month_Index
                                             ,L_Product_Type
                                             ,L_Loan_Term_Num_Rounded
                                             ,Factor)]
    
    WOAdjustments_sd <- WOAdjustments[,.(L_Product_Type,Model,Factor)]
    
    WOAdjustments_ssd <- dcast(WOAdjustments_sd, L_Product_Type~Model,value.var = c("Factor"))
    
    setnames(TermAdjustments_sd,"Factor","Term_Factor")
    setnames(WOAdjustments_ssd,"WO","WO_Factor")
    setnames(WOAdjustments_ssd,"FR","FR_Factor")
    setnames(WOAdjustments_ssd,"LGD","LGD_Factor")
    
    setkey(data_fin_padded,   L_Product_Type, L_Loan_Term_Num_Rounded, Z_Month_Index)
    setkey(CountsAdjustments, L_Product_Type, L_Loan_Term_Num_Rounded, Z_Month_Index)
    setkey(TermAdjustments_sd, L_Product_Type, L_Loan_Term_Num_Rounded, Z_Month_Index)
    setkey(WOAdjustments_ssd,L_Product_Type)
    
    data_fin_padded <- CountsAdjustments[data_fin_padded]
    data_fin_padded <- TermAdjustments_sd[data_fin_padded]
    data_fin_padded <- WOAdjustments_ssd[data_fin_padded]
    
    #Multiply by the factor (Original PX count adjustment)
    data_fin_padded[, Z_Termination_Month_Flag_P_Calc := Z_Termination_Month_Flag_P      * Termination_Factor]
    data_fin_padded[, Z_Write_Off_Month_Flag_P_Calc   := Z_Write_Off_Month_Flag_P_Unbias * WriteOff_Factor]
    
    #Recalibration Adjustment in Sep 2021
    data_fin_padded[, Z_Termination_Month_Flag_P_Calc := Z_Termination_Month_Flag_P_Calc * Term_Factor]
    data_fin_padded[, Z_Write_Off_Month_Flag_P_Calc   := Z_Write_Off_Month_Flag_P_Calc * WO_Factor]
    data_fin_padded[, Z_Full_Recovery_P   := Z_Full_Recovery_P * FR_Factor]
    data_fin_padded[, Z_Write_Off_Net_per_NAF_P   := Z_Write_Off_Net_per_NAF_P * LGD_Factor]
    
    #Clean up
    data_fin_padded[, c("Termination_Factor"
                        ,"WriteOff_Factor"
                        ,"Term_Factor"
                        ,"WO_Factor"
                        ,"FR_Factor"
                        ,"LGD_Factor"):=NULL]
  }
  
  #Scale the Write-Offs down so that the total for each contract doesn't go over the VarWriteOffCap
  varWriteOffCap <- 0.9
  data_fin_padded[, Z_Write_Off_Month_Flag_P_Factor:=if_else(sum(Z_Write_Off_Month_Flag_P_Calc)>varWriteOffCap,
                                                             varWriteOffCap / sum(Z_Write_Off_Month_Flag_P_Calc),
                                                             1)                                  
                  ,by=ID_Application_ID]
  data_fin_padded[, Z_Write_Off_Month_Flag_P_Correct:=Z_Write_Off_Month_Flag_P_Calc * Z_Write_Off_Month_Flag_P_Factor]
  data_fin_padded[, Z_Write_Off_Month_Flag_P_Factor:=NULL]
  
  
  #Scale the Terminations so that the sum of the Terminations and Write-Offs equals 1
  data_fin_padded[, Z_Termination_Month_Flag_P_Factor:=(1-sum(Z_Write_Off_Month_Flag_P_Correct))
                  /sum(Z_Termination_Month_Flag_P_Calc)                                 
                  ,by=ID_Application_ID]
  data_fin_padded[, Z_Termination_Month_Flag_P_Correct:=Z_Termination_Month_Flag_P_Calc * Z_Termination_Month_Flag_P_Factor]
  data_fin_padded[, c("Z_Termination_Month_Flag_P_Factor","Z_Termination_Month_Flag_P_Calc"):=NULL]
  
  #Scale the Full Recovery so that the probability is less than or equal to 1
  data_fin_padded[,Z_Full_Recovery_P   := ifelse(Z_Full_Recovery_P>1,1,Z_Full_Recovery_P)]
  
  #########################################@
  ###### ___10f. Clean up after GBMs #######
  #########################################@
  cat(paste("\n",now(),"___10f. Clean up after GBMs"))
  
  #Remove the anciliary variables
  VarsToRemove<-c("C_C1_Age"
                  ,"C_C1_Curr_Address_Duration"
                  ,"C_C1_Curr_Address_Ownership"
                  ,"C_C1_Curr_Address_State"
                  ,"C_C1_Curr_Employ_Duration"
                  ,"C_C1_Curr_Employment_Status"
                  ,"C_C1_Curr_Industry_Band"
                  ,"C_C1_Curr_Occup_Band"
                  ,"C_C1_Dependants"
                  ,"C_C1_Gender"
                  ,"C_C1_Marital_Status"
                  ,"C_C1_Mortgage_Rent_Num"
                  ,"C_C1_Prev_Address_Duration"
                  ,"C_C1_Prev_Employ_Duration"
                  ,"C_C1_Prev_Industry_Band"
                  ,"C_C1_Prev_Occup_Band"
                  ,"C_C2_Curr_Address_State"
                  ,"C_C2_Curr_Employ_Duration"
                  ,"C_C2_Curr_Industry_Band"
                  ,"C_C2_Curr_Occup_Band"
                  ,"C_C2_Prev_Occup_Band"
                  ,"C_CO_Address_Duration"
                  ,"C_CO_Address_Ownership"
                  ,"C_CO_Address_State"
                  ,"C_CO_Industry_Band"
                  ,"C_CO_Type"
                  ,"C_CO_Years_Established"
                  ,"C_E_C1_Total"
                  ,"C_I_C1_Total"
                  ,"C_I_C2_Total"
                  ,"D_Internal_Score_Consolidated"
                  ,"E_C1_Veda_Curr_Age_File_Months"
                  ,"E_C1_Veda_Orig_Age_File_Months"
                  ,"E_Veda_Primary"
                  ,"E_Veda_Secondary"
                  ,"G_ABS_C1_Age_0_24"
                  ,"G_ABS_C1_Age_25_44"
                  ,"G_ABS_C1_Age_45_64"
                  ,"G_ABS_C1_Age_65_Plus"
                  ,"G_ABS_C1_Aus_Birthplace"
                  ,"G_ABS_C1_Aus_Citizen"
                  ,"G_ABS_C1_Avg_HH_Size"
                  ,"G_ABS_C1_Avg_No_Vehicles"
                  ,"G_ABS_C1_Completed_Year_12"
                  ,"G_ABS_C1_Couple_Child"
                  ,"G_ABS_C1_Couple_No_Child"
                  ,"G_ABS_C1_D_Apartment"
                  ,"G_ABS_C1_D_Sep_House"
                  ,"G_ABS_C1_Has_Broadband"
                  ,"G_ABS_C1_Median_HH_Wkly_Inc"
                  ,"G_ABS_C1_Median_Mortgage_Mthly"
                  ,"G_ABS_C1_Median_Rent_Wkly"
                  ,"G_ABS_C1_Occ_Manager_Profes"
                  ,"G_ABS_C1_One_Parent_Family"
                  ,"G_ABS_C1_Popn_Per_SqKm"
                  ,"G_ABS_C1_T_Own_Outright"
                  ,"G_ABS_C1_T_Own_With_Mortgage"
                  ,"G_ABS_C1_T_Rent_All"
                  ,"G_ABS_C1_Veh_Per_SqKm"
                  ,"G_ECO_C1_AWE_ChgQ"
                  ,"G_ECO_C1_Cash"
                  ,"G_ECO_C1_CPI_ChgQ"
                  ,"G_ECO_C1_Mortgage"
                  ,"G_ECO_C1_UR_ChgQ"
                  ,"G_ECO_Month_C1_AWE_ChgQ"
                  ,"G_ECO_Month_C1_Cash"
                  ,"G_ECO_Month_C1_CPI_ChgQ"
                  ,"G_ECO_Month_C1_Mortgage"
                  ,"G_ECO_Month_C1_UR_ChgQ"
                  ,"G_SEIFA_C1_Adv_Disadv"
                  ,"G_SEIFA_C1_Disadv"
                  ,"G_SEIFA_C1_Eco_Resource"
                  ,"G_SEIFA_C1_Edu_Occ"
                  ,"H_C1_Last_Loan_Days_Since_Start"
                  ,"H_C1_Months_As_Customer"
                  ,"H_C1_Prior_Loans_Count"
                  ,"H_C1_Prior_Loans_Max_AAD"
                  ,"H_C1_Prior_Loans_Max_AAD_Band"
                  ,"H_C1_Years_As_Customer_Max_10"
                  ,"H_C2_Prior_Loans_Max_AAD"
                  ,"ID_Dealer_MetroRural"
                  ,"L_App_Percent_Through_Month"
                  ,"L_Application_Count"
                  ,"L_Balloon_Amount_of_NAF"
                  ,"L_Deposit_Amount"
                  ,"L_Deposit_Amount_of_NAF"
                  ,"L_Individuals_Count"
                  ,"L_Ins_ConsumerCredit"
                  ,"L_Ins_DealerWarranty"
                  ,"L_Ins_GAP"
                  ,"L_Ins_MotorVehicle"
                  ,"L_Ins_Total"
                  ,"L_Loan_Application_Month"
                  ,"L_Loan_Application_Month_Index"
                  ,"L_LVR_Capped"
                  ,"L_Margin_Rate_Modelling"
                  ,"L_Product_SubType"
                  ,"L_Trade_In_Amount"
                  ,"L_Trade_In_Amount_of_NAF"
                  ,"L_Transfer_Price_Modelling"
                  ,"V_Accessories_Ratio"
                  ,"V_Accessories_Value"
                  ,"V_Book_Value"
                  ,"V_Current_Odometer"
                  ,"V_Discount"
                  # ,"V_Model_Band" #Stopped removing this 2018-04-20 as per request by Ruwan
                  ,"V_Retail_Price_At_Purchase"
                  ,"V_Vehicle_Type"
                  ,"X_Campaign_ID"
                  # ,"X_Campaign_Name" #Stopped removing these 2018-03-22 as requested by Gary
                  # ,"X_DAF" #Stopped removing these 2017-09-26 as are needed in remuneration
                  # ,"X_DOF" #Stopped removing these 2017-09-26 as are needed in remuneration
                  # ,"X_Finance_Commission" #Stopped removing these 2017-09-26 as are needed in remuneration
                  ,"Z_Development_Months"
                  ,"Z_Month_0"
                  ,"Z_Month_Index_From_2010"
                  ,"Z_Months_Before_Expected_End"
                  ,"Z_Months_To_Pad_Full"
                  ,"Z_Proportion_Of_Expected_Term"
                  ,"Z_Termination_Month_Fixed"
                  ,"Z_dealerAdj_tu_Company"
                  ,"Z_dealerAdj_tu_Consumer"
                  ,"Z_dealerAdj_tu_SoleTrader"
                  ,"Z_dealerAdj_wo_Company"
                  ,"Z_dealerAdj_wo_Consumer"
                  ,"Z_dealerAdj_wo_SoleTrader")
  
  data_fin_padded <- data_fin_padded[, c(VarsToRemove):=NULL]
  
  #Sort the data appropriately
  data_fin_padded<-data_fin_padded[order(ID_Application_ID, Z_Month_Index)]
  
  ###################################@
  ###### 11. Scheduled Amounts #######
  ###################################@
  cat(paste("\n",now(),"11. Scheduled Amounts"))
  
  ################################################@
  ###### ___11a. Daily Interest Preparation #######
  ################################################@
  
  # Note we need to split each month into 4 parts to do the daily interest calculations, as follows:
  #BABM - "Before Anniversary and Before Middle"
  #AABM - "After Anniversary  but Before Middle"
  #BAAM - "Before Anniversary but After Middle"
  #AAAM - "After Anniversary  and After Middle"
  # Each of these different groups will have different calculations for 
  # - Interest Charged vs Recognised (which is split based on the anniversary), and 
  # - Early Termination overlay (which is split based on the middle of the month)
  
  # Calculate the number of days in each month, as well as the middle date and anniversary date
  data_fin_padded[, Month_Previous := floor_date(Z_Month - days(1), "month") ]
  data_fin_padded[, Month_Previous_Days_Count := as.numeric(ceiling_date(Month_Previous, "month", TRUE) - floor_date(Month_Previous, "month"))]
  data_fin_padded[, Month_Previous_Anniv  := L_Loan_Application_Date %m+% months(Z_Month_Index - 1)]
  data_fin_padded[, Month_Previous_Middle := Month_Previous + days(floor(Month_Previous_Days_Count/2))]
  data_fin_padded[, Month_Current_Days_Count := as.numeric(ceiling_date(Z_Month, "month", TRUE) - floor_date(Z_Month, "month"))]
  data_fin_padded[, Month_Current_Anniv := L_Loan_Application_Date %m+% months(Z_Month_Index)]
  data_fin_padded[, Month_Current_Middle := Z_Month + days(floor(Month_Current_Days_Count/2))]
  data_fin_padded[, Month_Next := ceiling_date(Z_Month, "month", TRUE)]
  
  # Calculate the days before and after these anniversary and middle dates
  data_fin_padded[, Current_BABM := ifelse(Month_Current_Anniv < Month_Current_Middle,
                                           Month_Current_Anniv - Z_Month,
                                           Month_Current_Middle - Z_Month)]
  data_fin_padded[, Current_AABM := ifelse(Month_Current_Anniv < Month_Current_Middle,
                                           Month_Current_Middle - Month_Current_Anniv,
                                           0)]
  data_fin_padded[, Current_BAAM := ifelse(Month_Current_Anniv < Month_Current_Middle,
                                           0,
                                           Month_Current_Anniv - Month_Current_Middle)]
  data_fin_padded[, Current_AAAM := ifelse(Month_Current_Anniv < Month_Current_Middle,
                                           Month_Next - Month_Current_Middle,
                                           Month_Next - Month_Current_Anniv)]
  data_fin_padded[, Previous_AABM := ifelse(Month_Previous_Anniv < Month_Previous_Middle,
                                            Month_Previous_Middle - Month_Previous_Anniv,
                                            0)]
  data_fin_padded[, Previous_AAAM := ifelse(Month_Previous_Anniv < Month_Previous_Middle,
                                            Z_Month - Month_Previous_Middle,
                                            Z_Month - Month_Previous_Anniv)]
  
  #Test. Current_Test should always be 0
  data_fin_padded[, Current_Test := Current_BABM + Current_AABM + Current_BAAM + Current_AAAM - Month_Current_Days_Count]
  
  #Remove anciliary variables
  data_fin_padded[, c("Month_Previous", "Month_Previous_Anniv", "Month_Previous_Middle","Month_Previous_Days_Count"
                      ,"Month_Current_Anniv", "Month_Current_Middle", "Current_Test"
                      ,"Month_Next"):= NULL]
  
  #######################################@
  ###### ___11b. Shape Preparation #######
  #######################################@
  
  #Shaping for amortisation is usually based on APRDISC and APRIDC, but these are not known at application,
  #  because we don't know the subvention amount nor the commission amount. However, we can use an approximation
  #  the Finance Rate, or the Transfer Price if the Finance Rate is below the Transfer Price (assuming a Subvention).
  
  #To do these calculations therefore we need to work out the rate we will use, as well as the monthly payments for this rate.
  
  #Shaping Rate
  data_fin_padded[, L_Finance_Rate_Shaping:=pmax(L_Finance_Rate, L_Transfer_Price)]
  
  #Payment for shaping
  data_fin_padded[L_Finance_Rate_Shaping==0, L_Payment_Monthly_ExGST_Shaping:=
                    (L_Net_Amount_Financed - L_Balloon_Amount) / 
                    (L_Loan_Term_Num - as.numeric(L_Balloon_Amount>0 & L_Payment_Structure == "IN ARREAR"))]  
  data_fin_padded[L_Finance_Rate_Shaping!=0, L_Payment_Monthly_ExGST_Shaping:=
                    pmt(r  = L_Finance_Rate_Shaping/100/12,
                        n  = L_Loan_Term_Num - as.numeric(L_Balloon_Amount>0 & L_Payment_Structure=="IN ARREAR"),
                        pv = -(L_Net_Amount_Financed +
                                 as.numeric(L_Balloon_Amount>0 & L_Payment_Structure=="IN ARREAR")*
                                 pv(r    = L_Finance_Rate_Shaping/100/12,
                                    n    = L_Loan_Term_Num,
                                    fv   = L_Balloon_Amount,
                                    pmt  = 0,
                                    type =  0)),
                        fv = as.numeric(L_Payment_Structure=="IN ADVANCE")*L_Balloon_Amount,
                        type = as.numeric(L_Payment_Structure=="IN ADVANCE"))]
  data_fin_padded[,L_Payment_Monthly_ExGST_Shaping:=round(L_Payment_Monthly_ExGST_Shaping+L_Fee_Monthly,digits=2)]
  
  ##############################@
  ###### ___11b. Month 0  #######
  ##############################@
  
  #For the fees there is only one version. 
  #But for the other fields there's a calculation on the Transfer Price as well, 
  #   so we know how to shape eg the the 0% rate ones.
  
  #Interest Charged: Month 0's interest charged is always 0, both in actuality and for shaping
  data_fin_padded[Z_Month_Index == 0 
                  , M_Interest_Cash         := 0]
  data_fin_padded[Z_Month_Index == 0 
                  , M_Interest_Cash_Shaping := 0]
  
  #Fee_Monthly: First month's fee is only applicable if fees are starting to be charged in month 0. This is defined above based on Payment Month.
  #No separate shaping fee numbers, as these are the same for shaping and non-shaping
  data_fin_padded[Z_Month_Index == 0
                  , M_Fee_Monthly := ifelse(L_Month_Fee_Start == 0, L_Fee_Monthly, 0)]
  
  #Payment: The payment for month 0 is only applicable if the start of payments is 0. This is defined above based on Arrears/Advance.
  data_fin_padded[Z_Month_Index == 0
                  , M_Payment         := ifelse(L_Month_Regular_Start == 0, - L_Payment_Monthly_ExGST, 0)]
  data_fin_padded[Z_Month_Index == 0
                  , M_Payment_Shaping := ifelse(L_Month_Regular_Start == 0, - L_Payment_Monthly_ExGST_Shaping, 0)]
  
  #Balance: This is the NAF minus plus any fees minus any payments
  data_fin_padded[Z_Month_Index == 0
                  , M_Balance         := L_Net_Amount_Financed + M_Payment         + M_Fee_Monthly]
  data_fin_padded[Z_Month_Index == 0
                  , M_Balance_Shaping := L_Net_Amount_Financed + M_Payment_Shaping + M_Fee_Monthly]
  
  #Interest Recongnised: Month 0's interest recognised is dependant on how many days were in the month post-signing (ie post-anniversary)
  data_fin_padded[Z_Month_Index == 0 
                  , M_Interest_Accounting := round(
                    M_Balance         * (L_Finance_Rate / 100)         * (Current_AABM / 365) + 
                      M_Balance         * (L_Finance_Rate / 100)         * (Current_AAAM / 365)
                    ,digits=2)]
  data_fin_padded[Z_Month_Index == 0 
                  , M_Interest_Accounting_Shaping := round(
                    M_Balance_Shaping * (L_Finance_Rate_Shaping / 100) * (Current_AABM / 365) + 
                      M_Balance_Shaping * (L_Finance_Rate_Shaping / 100) * (Current_AAAM / 365)
                    ,digits=2)]
  
  
  #############################@
  ###### ___11c. Month n #######
  #############################@
  
  for (i in 1:data_fin_padded[, max(Z_Month_Index)]){
    
    #First you need to bring forward "previous month's balance"
    data_fin_padded[
      , M_Balance_Previous         := shift(M_Balance)]
    data_fin_padded[
      , M_Balance_Previous_Shaping := shift(M_Balance_Shaping)]
    
    # Then calculate the interest charged, daily. 
    # Note that all 4 parts need to be calculated. 
    # The special issue where for Commercial In Arrears the first payment is multiplied by (number of days in month)/30 (eg 31/30)
    # is not material and will not be modelled.
    data_fin_padded[Z_Month_Index == i 
                    , M_Interest_Cash := round(
                      M_Balance_Previous * (L_Finance_Rate / 100) * (Previous_AABM / 365) + 
                        M_Balance_Previous * (L_Finance_Rate / 100) * (Previous_AAAM / 365) +
                        M_Balance_Previous * (L_Finance_Rate / 100) * (Current_BABM  / 365) +
                        M_Balance_Previous * (L_Finance_Rate / 100) * (Current_BAAM  / 365)
                      ,digits=2)]
    data_fin_padded[Z_Month_Index == i
                    , M_Interest_Cash_Shaping := round(
                      M_Balance_Previous_Shaping * L_Finance_Rate_Shaping / 100 / 12
                      ,digits=2)]
    
    #Monthly Fee: This is only applicable if we are still in the period where fees are charged, ie up to L_Month_Fee_End. 
    data_fin_padded[Z_Month_Index == i
                    , M_Fee_Monthly := ifelse(L_Month_Fee_End >= i, L_Fee_Monthly, 0)]
    
    #Payments - which are different if you are before the end of the term, at the end of term, or after.
    #   The at the end of term payment is not based on the rounded L_Payment_Monthly_ExGST, 
    #   but are rather the sum of all amounts due so fixes the rounding problem.
    data_fin_padded[Z_Month_Index == i, M_Payment :=
                      ifelse(i <  L_Month_All_End, - L_Payment_Monthly_ExGST,
                             ifelse(i == L_Month_All_End, - M_Balance_Previous         - M_Interest_Cash         - M_Fee_Monthly,
                                    0))]
    data_fin_padded[Z_Month_Index == i, M_Payment_Shaping := 
                      ifelse(i <  L_Month_All_End, - L_Payment_Monthly_ExGST_Shaping,
                             ifelse(i == L_Month_All_End, - M_Balance_Previous_Shaping - M_Interest_Cash_Shaping - M_Fee_Monthly,
                                    0))]
    
    #Balance. Need to round this to avoid floating point errors
    data_fin_padded[Z_Month_Index == i
                    , M_Balance         := round(M_Balance_Previous         + M_Payment 
                                                 + M_Fee_Monthly + M_Interest_Cash, digits=2)]
    data_fin_padded[Z_Month_Index == i
                    , M_Balance_Shaping := round(M_Balance_Previous_Shaping + M_Payment_Shaping 
                                                 + M_Fee_Monthly + M_Interest_Cash_Shaping, digits=2)]
    
    
    #Interest Recongnised: Is partly based on previous month's balance, and partly on this month's
    #Not required for shaping
    data_fin_padded[Z_Month_Index == i 
                    , M_Interest_Accounting := 
                      round(
                        M_Balance_Previous         * (L_Finance_Rate / 100)         * (Current_BABM / 365) + 
                          M_Balance_Previous         * (L_Finance_Rate / 100)         * (Current_BAAM / 365) +
                          M_Balance                  * (L_Finance_Rate / 100)         * (Current_AABM / 365) +
                          M_Balance                  * (L_Finance_Rate / 100)         * (Current_AAAM / 365)
                        ,digits=2)]
    data_fin_padded[Z_Month_Index == i 
                    , M_Interest_Accounting_Shaping := 
                      round(
                        M_Balance_Previous_Shaping * (L_Finance_Rate_Shaping / 100) * (Current_BABM / 365) + 
                          M_Balance_Previous_Shaping * (L_Finance_Rate_Shaping / 100) * (Current_BAAM / 365) +
                          M_Balance_Shaping          * (L_Finance_Rate_Shaping / 100) * (Current_AABM / 365) +
                          M_Balance_Shaping          * (L_Finance_Rate_Shaping / 100) * (Current_AAAM / 365)
                        ,digits=2)]
  }
  
  #Remove all the shaping variables not required later on
  data_fin_padded[, c("L_Payment_Monthly_ExGST_Shaping"
                      ,"M_Interest_Cash_Shaping"
                      ,"M_Payment_Shaping"):=NULL]
  
  #######################################@
  ###### 12. Scheduled Corrections #######
  #######################################@
  cat(paste("\n",now(),"12. Scheduled Corrections"))
  
  #####################################@
  ###### ___12c. Fees & Expenses #######
  #####################################@
  
  #Transfer Price
  #Fix the M_Balance_Previous first as this might be NA
  data_fin_padded[Z_Month_Index == 0, M_Balance_Previous         := 0]
  data_fin_padded[Z_Month_Index == 0, M_Balance_Previous_Shaping := 0]
  
  # Then calculate the charged version
  data_fin_padded[, M_Transfer_Price_Cash := -(M_Balance_Previous * (L_Transfer_Price / 100) * (Previous_AABM / 365) + 
                                                 M_Balance_Previous * (L_Transfer_Price / 100) * (Previous_AAAM / 365) +
                                                 M_Balance_Previous * (L_Transfer_Price / 100) * (Current_BABM  / 365) +
                                                 M_Balance_Previous * (L_Transfer_Price / 100) * (Current_BAAM  / 365))]
  
  #Work it out as a percentage of the balance, for the recognised version
  data_fin_padded[, M_Transfer_Price_Accounting := -(M_Balance_Previous * (L_Transfer_Price / 100) * (Current_BABM / 365) + 
                                                       M_Balance_Previous * (L_Transfer_Price / 100) * (Current_BAAM / 365) +
                                                       M_Balance          * (L_Transfer_Price / 100) * (Current_AABM / 365) +
                                                       M_Balance          * (L_Transfer_Price / 100) * (Current_AAAM / 365))]
  
  #Establishment Fee - Cash
  data_fin_padded[, M_Fee_Establishment_Cash := ifelse(Z_Month_Index == 0, L_Establishment_Fee, 0)]
  
  #Establishment Expense
  data_fin_padded[, M_Expense_Establishment := ifelse(Z_Month_Index == 0, -VarEstablishmentExpense, 0)]
  
  #Monthly Expense - only applicable between start and end of fees
  data_fin_padded[, M_Expense_Monthly := ifelse(Z_Month_Index >= L_Month_Fee_Start & Z_Month_Index <= L_Month_Fee_End, -VarMonthlyExpense, 0)]
  
  #Clean up
  data_fin_padded[, c("L_Month_Fee_Start","L_Month_Fee_End", "L_Month_Regular_Start"):=NULL]
  
  
  ##############################@
  ###### 13. GBM Overlays #######
  ##############################@
  cat(paste("\n",now(),"13. GBM Overlays"))
  
  #There are various types of overlays to be done as follows.
  
  #1. TAKEUP - Simply overlaying the take-up chance occurs for the following variables:
  # M_Interest_Cash_TU
  # M_Interest_Accounting_TU
  # M_Fee_Monthly_TU
  # M_Transfer_Price_Cash_TU
  # M_Transfer_Price_Accounting_TU
  # M_Fee_Establishment_Cash_TU
  # M_Expense_Establishment_TU
  # M_Expense_Monthly_TU
  
  # 2. PURE ET AND WO EFFECTS - Based on the sum of base + take_up:
  # M_Fee_Early_Termination_ET
  # M_Expense_Early_Termination_ET
  # M_Written_Off_WO
  
  # 3. MONTHLY FEES FOR ET AND WO - Based on the sum of base + take_up
  # M_Fee_Monthly_ET / M_Fee_Monthly_WO
  # M_Expense_Monthly_ET / M_Expense_Monthly_WO
  
  # 4. INTEREST FOR ET AND WO - Based on the sum of base + take_up:
  # M_Interest_Cash_ET / M_Interest_Cash_WO
  # M_Interest_Accounting_ET / M_Interest_Accounting_WO
  # M_Transfer_Price_Cash_ET / M_Transfer_Price_Cash_WO
  # M_Transfer_Price_Accounting_ET / M_Transfer_Price_Accounting_WO
  
  ##############################@
  ###### ___13a. Take_Up ########
  ##############################@
  
  #Determine what variables need to change
  Vars_To_Change = c('M_Interest_Cash',
                     'M_Interest_Accounting',
                     'M_Fee_Monthly',
                     'M_Expense_Monthly',
                     'M_Transfer_Price_Cash',
                     'M_Transfer_Price_Accounting',
                     'M_Fee_Establishment_Cash',
                     'M_Expense_Establishment')
  
  #Change the variables
  for(i in Vars_To_Change){
    data_fin_padded[, paste(i,"_TU",sep=""):=-get(i)*(1-Z_Taken_Up_P)]}
  
  #Clean up
  rm(i, Vars_To_Change)
  
  
  ####################################################@
  ###### ___13b. Termination Fee Max - Consumer #######
  ####################################################@
  
  #Work out the termination fee for consumer loans. The Factor is to account for when in the month the contract started
  # This is how many months into the loan are they.
  # This will be the month index, plus the proportion of this month from the anniversary to the end of the month,
  data_fin_padded[,Term_Fee_Factor := ifelse(Z_Month_Index == 0, 1, 
                                             Z_Month_Index + (Current_AABM + Current_AAAM) / Month_Current_Days_Count)]
  data_fin_padded[L_Loan_Type %in% c(10,11,14,40), 
                  Term_Fee_Max := (VarEarlyTerminationConsumerBase / L_Loan_Term_Num) * 
                    ifelse(L_Loan_Term_Num < Term_Fee_Factor, 0, L_Loan_Term_Num - Term_Fee_Factor)]
  
  
  ######################################################@
  ###### ___13c. Termination Fee Max - Commercial #######
  ######################################################@
  
  # First work out the cumulative interest charged, and the total interest charged
  data_fin_padded[, M_Interest_Cumulative := ave(M_Interest_Cash,ID_Application_ID,FUN=cumsum)]
  data_fin_padded[, M_Interest_Total      := sum(M_Interest_Cash), by=ID_Application_ID]
  
  #Then work out the unearned interest
  data_fin_padded[, M_Interest_Unearned := M_Interest_Total - M_Interest_Cumulative]
  
  
  #Grab the next month's unearned interest as it will be needed (and correct all final months so it doesn't grab it from the subsequent)
  data_fin_padded[, M_Interest_Unearned_Next := shift(M_Interest_Unearned, 1L, type = "lead")]
  data_fin_padded[Z_Month_Index == Z_Months_To_Pad_Amort - 1, M_Interest_Unearned_Next := 0]
  data_fin_padded[, Z_Months_To_Pad_Amort:=NULL]
  
  #Work out the termination fee
  # Note that we need to work out the chances that the termination occured before and after the anniversary, 
  # as these are different unearned interests.
  data_fin_padded[!(L_Loan_Type %in% c(10,11,14,40)), 
                  Term_Fee_Max := VarEarlyTerminationCommercialFactor * 
                    (M_Interest_Unearned * (Current_BABM + Current_BAAM) 
                     + M_Interest_Unearned_Next * (Current_AABM + Current_AAAM)) / 
                    Month_Current_Days_Count]
  
  
  ##############################@
  ###### ___13d. New Fees #######
  ##############################@
  
  #Scenarios if we use new fees
  if(VarScenarioMatrix[Scenario==VarScenario,TermFeeAdjust]==TRUE){
    #IF THE SCENARIO IS TO USE NEW TERMINATION FEES# 
    cat(" [Scenario: New Termination Fees]")
    
    #Calculate the new termination fee max
    data_fin_padded[, Term_Fee_Max_New_Calc := VarEarlyTerminationFactor_New * 
                      (M_Interest_Unearned * (Current_BABM + Current_BAAM) 
                       + M_Interest_Unearned_Next * (Current_AABM + Current_AAAM)) / 
                      Month_Current_Days_Count]
    data_fin_padded[, Term_Fee_Max :=  ifelse(Term_Fee_Max_New_Calc > VarEarlyTerminationCap_New,
                                              VarEarlyTerminationCap_New, Term_Fee_Max_New_Calc) + 
                      ifelse(L_Loan_Term_Num < Term_Fee_Factor,0, VarEarlyTerminationDischarge_New)]
    data_fin_padded[, Term_Fee_Max_New_Calc:=NULL]    
  } else {
    cat(" [Scenario: Actual Termination Fees]")
  }    
  
  #Clean up
  data_fin_padded[, c("M_Interest_Unearned_Next","M_Interest_Unearned"
                      ,"Month_Current_Days_Count"):=NULL]   
  
  ################################################################@
  ###### ___13e. Termination Fee, Termination Expense, LGD ########
  ################################################################@
  
  #Termination Expense Max
  data_fin_padded[, Term_Expense_Max := ifelse(L_Loan_Term_Num < Term_Fee_Factor, 0, - VarEarlyTerminationExpense) ]
  data_fin_padded[, Term_Fee_Factor :=NULL]
  
  #Early Termination Fee & Expense. 
  # The "Max" versions already have the timing-within-the-month sorted, so no additional work here.
  # Also need to factor in the take-up model here
  # Note that the "max" versions are assuming that the contract is being paid off to schedule (ie no factoring in of 
  #    the early termination) to work out unearned interest.
  #    In fact, if the customer is ahead or behind, they use the ahead or behing number to work out the unearned inteset, but we don't predict that.
  #    But we found that the ahead/behind effect isn't huge, compared to the early terminations. 
  data_fin_padded[, M_Fee_Early_Termination_ET     := Term_Fee_Max     * Z_Taken_Up_P * Z_Termination_Month_Flag_P_Correct]
  data_fin_padded[, M_Expense_Early_Termination_ET := Term_Expense_Max * Z_Taken_Up_P * Z_Termination_Month_Flag_P_Correct]
  data_fin_padded[, c("Term_Expense_Max","Term_Fee_Max"):=NULL]
  
  #Loss Given Default
  # Again needs Take_Up applied though
  # This is purely a function of the GBMs and the NAF, so no timing-within-the-month work here either.
  data_fin_padded[, M_Written_Off_WO     :=   Z_Write_Off_Month_Flag_P_Correct * (1-Z_Full_Recovery_P) #Change of written off + not rull recovery
                  * Z_Write_Off_Net_per_NAF_P * (-L_Net_Amount_Financed)     #Cost of Write Off
                  * Z_Taken_Up_P]                                            #Take Up Effect
  
  #Recoveries Commission
  # This is purely a function written off amount:
  data_fin_padded[, M_Written_Off_Recovery_Comms_WO := M_Written_Off_WO * VarRecoveryCommission]
  
  #Full Recovery benefit
  data_fin_padded[, M_Written_Off_Full_Recovery_Benefit_WO := Z_Write_Off_Month_Flag_P_Correct * Z_Full_Recovery_P #Change of written off + full recovery
                  * frAdj * L_Net_Amount_Financed                        #Benefit of Full Recovery
                  * Z_Taken_Up_P]                                        #Taken Up Factor
  
  ##############################################@
  ###### ___13f. Monthly Fees & Expenses ########
  ##############################################@
  
  # Work out the cumulative chances of termination and write off
  data_fin_padded[, Z_WO_Cumulative   := ave(Z_Write_Off_Month_Flag_P_Correct,   ID_Application_ID, FUN=cumsum)]
  data_fin_padded[, Z_Term_Cumulative := ave(Z_Termination_Month_Flag_P_Correct, ID_Application_ID, FUN=cumsum)]
  
  #Also need the previous (and previous previous) versions of these for the calculations. 
  # For these you also need to correct so you don't get ones from different contracts
  data_fin_padded[, Z_WO_Cumulative_Prev := shift(Z_WO_Cumulative, 1L, type = "lag")]
  data_fin_padded[, Z_WO_Cumulative_Prev_Prev := shift(Z_WO_Cumulative, 2L, type = "lag")]
  data_fin_padded[, Z_Term_Cumulative_Prev := shift(Z_Term_Cumulative, 1L, type = "lag")]
  data_fin_padded[, Z_Term_Cumulative_Prev_Prev := shift(Z_Term_Cumulative, 2L, type = "lag")]
  data_fin_padded[Z_Month_Index <= 0 , Z_WO_Cumulative_Prev := 0]
  data_fin_padded[Z_Month_Index <= 0 , Z_Term_Cumulative_Prev := 0]
  data_fin_padded[Z_Month_Index <= 1 , Z_WO_Cumulative_Prev_Prev := 0]
  data_fin_padded[Z_Month_Index <= 1 , Z_Term_Cumulative_Prev_Prev := 0]
  
  #M_Fee_Monthly
  #Note that if the customer's anniversary is before the middle of the month, 
  # then they will probably terminate after their anniversary, 
  # so will still be charged the fee for that month, 
  # so we need to multiply the fee by the previous month's probabilty.
  data_fin_padded[, M_Fee_Monthly_ET := -(M_Fee_Monthly + M_Fee_Monthly_TU) * 
                    ifelse(Current_AABM == 0, Z_Term_Cumulative, Z_Term_Cumulative_Prev)]
  data_fin_padded[, M_Fee_Monthly_WO := -(M_Fee_Monthly + M_Fee_Monthly_TU) * 
                    ifelse(Current_AABM == 0, Z_WO_Cumulative, Z_WO_Cumulative_Prev)]
  
  #M_Expense_Monthly (which works exactly the same way)
  data_fin_padded[, M_Expense_Monthly_ET := -(M_Expense_Monthly + M_Expense_Monthly_TU) * 
                    ifelse(Current_AABM == 0, Z_Term_Cumulative, Z_Term_Cumulative_Prev)]
  data_fin_padded[, M_Expense_Monthly_WO := -(M_Expense_Monthly + M_Expense_Monthly_TU) * 
                    ifelse(Current_AABM == 0, Z_WO_Cumulative, Z_WO_Cumulative_Prev)]
  
  
  #########################################################@
  ###### ___13g. Interest Income and Transfer Price ########
  #########################################################@
  
  #Just replicate the original calculations, but with the appropriate probabilities added. 
  # No need to cycle through this time as we are not adjusting the balances, just overlaying on top.
  # Also no rounding, as this is not real cash like the scheduled, but probabilities
  
  data_fin_padded[, M_Interest_Cash_ET := -(M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Previous_AABM / 365) * Z_Term_Cumulative_Prev_Prev + 
                                              M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Previous_AAAM / 365) * Z_Term_Cumulative_Prev +
                                              M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_BABM  / 365) * Z_Term_Cumulative_Prev +
                                              M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_BAAM  / 365) * Z_Term_Cumulative)]
  
  data_fin_padded[, M_Interest_Accounting_ET := -(M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_BABM  / 365) * Z_Term_Cumulative_Prev + 
                                                    M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_BAAM  / 365) * Z_Term_Cumulative + 
                                                    M_Balance          * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_AABM  / 365) * Z_Term_Cumulative_Prev +
                                                    M_Balance          * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_AAAM  / 365) * Z_Term_Cumulative)]
  
  data_fin_padded[, M_Interest_Cash_WO := -(M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Previous_AABM / 365) * Z_WO_Cumulative_Prev_Prev + 
                                              M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Previous_AAAM / 365) * Z_WO_Cumulative_Prev +
                                              M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_BABM  / 365) * Z_WO_Cumulative_Prev +
                                              M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_BAAM  / 365) * Z_WO_Cumulative)]
  
  data_fin_padded[, M_Interest_Accounting_WO := -(M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_BABM  / 365) * Z_WO_Cumulative_Prev + 
                                                    M_Balance_Previous * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_BAAM  / 365) * Z_WO_Cumulative + 
                                                    M_Balance          * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_AABM  / 365) * Z_WO_Cumulative_Prev +
                                                    M_Balance          * Z_Taken_Up_P * (L_Finance_Rate / 100) * (Current_AAAM  / 365) * Z_WO_Cumulative)]
  
  #Transfer Price
  data_fin_padded[, M_Transfer_Price_Cash_ET := M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Previous_AABM / 365) * Z_Term_Cumulative_Prev_Prev  + 
                    M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Previous_AAAM / 365) * Z_Term_Cumulative_Prev  +
                    M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_BABM  / 365) * Z_Term_Cumulative_Prev  +
                    M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_BAAM  / 365) * Z_Term_Cumulative ]
  
  data_fin_padded[, M_Transfer_Price_Accounting_ET := M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_BABM / 365) * Z_Term_Cumulative_Prev + 
                    M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_BAAM / 365) * Z_Term_Cumulative  +
                    M_Balance          * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_AABM / 365) * Z_Term_Cumulative_Prev  +
                    M_Balance          * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_AAAM / 365) * Z_Term_Cumulative]
  
  data_fin_padded[, M_Transfer_Price_Cash_WO := M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Previous_AABM / 365) * Z_WO_Cumulative_Prev_Prev  + 
                    M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Previous_AAAM / 365) * Z_WO_Cumulative_Prev  +
                    M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_BABM  / 365) * Z_WO_Cumulative_Prev  +
                    M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_BAAM  / 365) * Z_WO_Cumulative ]
  
  data_fin_padded[, M_Transfer_Price_Accounting_WO := M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_BABM / 365) * Z_WO_Cumulative_Prev + 
                    M_Balance_Previous * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_BAAM / 365) * Z_WO_Cumulative  +
                    M_Balance          * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_AABM / 365) * Z_WO_Cumulative_Prev  +
                    M_Balance          * Z_Taken_Up_P * (L_Transfer_Price / 100) * (Current_AAAM / 365) * Z_WO_Cumulative]
  
  #M_Interest_Accounting_Shaping (No need to factor Taken Up as we are ignoring that for shaping)
  data_fin_padded[, M_Interest_Accounting_Shaping_ET := M_Balance_Previous_Shaping * (L_Finance_Rate_Shaping / 100) * (Current_BABM / 365) * Z_Term_Cumulative_Prev + 
                    M_Balance_Previous_Shaping * (L_Finance_Rate_Shaping / 100) * (Current_BAAM / 365) * Z_Term_Cumulative  +
                    M_Balance_Shaping          * (L_Finance_Rate_Shaping / 100) * (Current_AABM / 365) * Z_Term_Cumulative_Prev  +
                    M_Balance_Shaping          * (L_Finance_Rate_Shaping / 100) * (Current_AAAM / 365) * Z_Term_Cumulative]
  
  data_fin_padded[, M_Interest_Accounting_Shaping_WO := M_Balance_Previous_Shaping * (L_Finance_Rate_Shaping / 100) * (Current_BABM / 365) * Z_WO_Cumulative_Prev + 
                    M_Balance_Previous_Shaping * (L_Finance_Rate_Shaping / 100) * (Current_BAAM / 365) * Z_WO_Cumulative  +
                    M_Balance_Shaping          * (L_Finance_Rate_Shaping / 100) * (Current_AABM / 365) * Z_WO_Cumulative_Prev  +
                    M_Balance_Shaping          * (L_Finance_Rate_Shaping / 100) * (Current_AAAM / 365) * Z_WO_Cumulative]
  
  
  #Clean up variables not required
  data_fin_padded[, c("Current_AAAM","Current_AABM","Current_BAAM","Current_BABM"
                      ,"Previous_AAAM","Previous_AABM"
                      ,"M_Balance_Previous"
                      ,"M_Balance_Previous_Shaping","M_Balance_Shaping", "L_Finance_Rate_Shaping"
                      ,"Z_Term_Cumulative_Prev","Z_Term_Cumulative_Prev_Prev"
                      ,"Z_WO_Cumulative_Prev","Z_WO_Cumulative_Prev_Prev"):=NULL]
  
  ########################################@
  ###### ___13h. GFV Return Option ########
  ########################################@
  
  #The theoretical maximum Return Option amount. 
  data_fin_padded[, M_Expense_GFV_Option_Max:= ifelse(I_GFV == FALSE, 0, - M_Balance * VarGFVOptionCost/12/100)] 
  
  #Then this is summed, and put in the final month of the contract, where it would theoretically be included                                                        
  data_fin_padded[, M_Expense_GFV_Option    := ifelse(Z_Month_Index==L_Month_All_End,sum(M_Expense_GFV_Option_Max),0), 
                  by=ID_Application_ID]
  data_fin_padded[, c("M_Expense_GFV_Option_Max","L_Month_All_End"):=NULL]
  
  #The decrements for TU, WO and Term can be represented by the ratio of the original Transfer Price
  # to the particular decrement for transfer price (over the whole application). Thus:
  data_fin_padded[, M_Expense_GFV_Option_TU := ifelse(M_Expense_GFV_Option==0,0,M_Expense_GFV_Option * sum(M_Transfer_Price_Cash_TU) / sum(M_Transfer_Price_Cash)), by=ID_Application_ID]
  data_fin_padded[, M_Expense_GFV_Option_WO := ifelse(M_Expense_GFV_Option==0,0,M_Expense_GFV_Option * sum(M_Transfer_Price_Cash_WO) / sum(M_Transfer_Price_Cash)), by=ID_Application_ID]
  data_fin_padded[, M_Expense_GFV_Option_ET := ifelse(M_Expense_GFV_Option==0,0,M_Expense_GFV_Option * sum(M_Transfer_Price_Cash_ET) / sum(M_Transfer_Price_Cash)), by=ID_Application_ID]
  
  
  #############################@
  ###### ___13i. Shaping #######
  #############################@
  
  ##### ___ ___ Calculate Shapes #####
  
  #Work out the unearned interest each month
  data_fin_padded[, M_Interest_Shaping_Cumulative := ave(M_Interest_Accounting_Shaping,ID_Application_ID,FUN=cumsum)]
  data_fin_padded[, M_Interest_Shaping_Total      := sum(M_Interest_Accounting_Shaping), by=ID_Application_ID]
  data_fin_padded[, M_Interest_Shaping_Unearned   := M_Interest_Shaping_Total - M_Interest_Shaping_Cumulative]
  data_fin_padded[, c("M_Interest_Shaping_Cumulative", "M_Interest_Shaping_Total"):=NULL]
  
  #Bring forward the unearned interest, as per the termination chance
  data_fin_padded[, T_Interest_Shaping:= M_Interest_Accounting_Shaping 
                  +M_Interest_Accounting_Shaping_ET 
                  +M_Interest_Accounting_Shaping_WO]
  data_fin_padded[, T_Interest_Shaping_InclBroughtForward := 
                    T_Interest_Shaping 
                  + M_Interest_Shaping_Unearned 
                  * (Z_Write_Off_Month_Flag_P_Correct + Z_Termination_Month_Flag_P_Correct)]
  
  #Work out the shape
  data_fin_padded[, Z_Amortisation_Shaped := T_Interest_Shaping_InclBroughtForward 
                  / sum(T_Interest_Shaping_InclBroughtForward)
                  , by=ID_Application_ID]
  
  #Remove calculation fields
  data_fin_padded[, c("M_Interest_Accounting_Shaping","M_Interest_Accounting_Shaping_ET", "M_Interest_Accounting_Shaping_WO"
                      ,"T_Interest_Shaping","T_Interest_Shaping_InclBroughtForward"):=NULL]
  
  #Work out the unrecognised shape (so we can work out balances not recognised to add on to the Receivables Balance)
  data_fin_padded[, Z_Amortisation_Shaped_Cumulative := ave(Z_Amortisation_Shaped,ID_Application_ID,FUN=cumsum)]
  data_fin_padded[, Z_Amortisation_Shaped_Total      := sum(Z_Amortisation_Shaped), by=ID_Application_ID]
  data_fin_padded[, Z_Amortisation_Shaped_Unearned   := Z_Amortisation_Shaped_Total - Z_Amortisation_Shaped_Cumulative]
  data_fin_padded[, c("Z_Amortisation_Shaped_Cumulative", "Z_Amortisation_Shaped_Total"):=NULL]
  
  
  ##### ___ ___ Apply Shapes #####
  
  #Establishment Fee - Amortised
  data_fin_padded[, M_Fee_Establishment_Accounting := L_Establishment_Fee * Z_Amortisation_Shaped]
  
  #Establishment Fee - Amortised - Taken Up
  data_fin_padded[, M_Fee_Establishment_Accounting_TU:=-M_Fee_Establishment_Accounting*(1-Z_Taken_Up_P)]
  
  #Finance Commission - Cash
  data_fin_padded[, X_Finance_Commission_Cash := ifelse(Z_Month_Index == 0, X_Finance_Commission, 0)]
  
  #Finance Commission - Amortised
  data_fin_padded[, X_Finance_Commission_Accounting := X_Finance_Commission * Z_Amortisation_Shaped]
  
  #Subvention - Cash
  data_fin_padded[, X_Subvention_Amount_Cash := ifelse(Z_Month_Index == 0, X_Subvention_Amount, 0)]
  
  #Subvention - Amortised
  data_fin_padded[, X_Subvention_Amount_Accounting := X_Subvention_Amount * Z_Amortisation_Shaped]
  
  #Clawback
  data_fin_padded[, X_Finance_Clawback_InMonth := ifelse(Z_Month == X_Finance_Clawback_Month, X_Finance_Clawback, NA)]
  
  #Clean up
  data_fin_padded[, c("Z_Amortisation_Shaped"):=NULL]
  
  
  #######################################@
  ###### 14. Financial Adjustments #######
  #######################################@
  cat(paste("\n",now(),"14. Apply the Financial Adjustments"))
  
  #2 Scenarios if we applying global financial adjustments or not
  if(VarScenarioMatrix[Scenario==VarScenario,FinancialAdjust]==FALSE){
    #IF THE SCENARIO IS TO NOT APPLY FINANCIAL ADJUSTMENTS# 
    cat(" [Scenario: No Financial Adjustments]")
    #All the relevant fields are set to 0
    data_fin_padded[, M_Interest_Accounting_Adj:=0]
    data_fin_padded[, M_Interest_Cash_Adj:=0]
    data_fin_padded[, M_Transfer_Price_Accounting_Adj:=0]
    data_fin_padded[, M_Transfer_Price_Cash_Adj:=0]
    data_fin_padded[, M_Fee_Early_Termination_Adj:=0]
    data_fin_padded[, M_Expense_Early_Termination_Adj:=0]
    data_fin_padded[, M_Fee_Establishment_Cash_Adj:=0]
    data_fin_padded[, M_Fee_Establishment_Accounting_Adj:=0]
    data_fin_padded[, M_Written_Off_Adj:=0]
  } else {
    #IF THE SCENARIO IS TO APPLY GLOBAL ADJUSTMENTS# 
    cat(" [Scenario: Financial Adjustments]")
    #Merge in the data
    setkey(data_fin_padded,   L_Product_Type, L_Loan_Term_Num_Rounded, Z_Month_Index)
    setkey(FinancialAdjustments, L_Product_Type, L_Loan_Term_Num_Rounded, Z_Month_Index)
    data_fin_padded <- FinancialAdjustments[data_fin_padded]
    
    #Create the relevant fields
    data_fin_padded[, M_Interest_Accounting_Adj := 
                      ifelse(is.na(M_Interest_Accounting_Factor),0,
                             (M_Interest_Accounting_Factor - 1) * 
                               (M_Interest_Accounting +  
                                  M_Interest_Accounting_TU + 
                                  M_Interest_Accounting_WO + 
                                  M_Interest_Accounting_ET))]
    data_fin_padded[, M_Interest_Cash_Adj := M_Interest_Accounting_Adj]
    data_fin_padded[, M_Transfer_Price_Accounting_Adj := 
                      ifelse(is.na(M_Transfer_Price_Accounting_Factor),0,
                             (M_Transfer_Price_Accounting_Factor - 1) * 
                               (M_Transfer_Price_Accounting +  
                                  M_Transfer_Price_Accounting_TU + 
                                  M_Transfer_Price_Accounting_WO + 
                                  M_Transfer_Price_Accounting_ET))]
    data_fin_padded[, M_Transfer_Price_Cash_Adj := M_Transfer_Price_Accounting_Adj]
    data_fin_padded[, M_Fee_Early_Termination_Adj :=
                      ifelse(is.na(M_Fee_Early_Termination_Factor),0,
                             (M_Fee_Early_Termination_Factor - 1) * 
                               M_Fee_Early_Termination_ET)]
    data_fin_padded[, M_Expense_Early_Termination_Adj := 
                      ifelse(is.na(M_Fee_Early_Termination_Factor),0,
                             (M_Fee_Early_Termination_Factor - 1) * 
                               M_Expense_Early_Termination_ET)]
    data_fin_padded[, M_Fee_Establishment_Accounting_Adj := 
                      ifelse(is.na(M_Fee_Establishment_Accounting_Factor),0,
                             (M_Fee_Establishment_Accounting_Factor - 1) * 
                               (M_Fee_Establishment_Accounting + 
                                  M_Fee_Establishment_Accounting_TU))]
    data_fin_padded[, M_Fee_Establishment_Cash_Adj := 
                      ifelse(is.na(M_Fee_Establishment_Accounting_Factor),0,
                             (M_Fee_Establishment_Accounting_Factor - 1) * 
                               (M_Fee_Establishment_Cash + 
                                  M_Fee_Establishment_Cash_TU))]
    data_fin_padded[, M_Written_Off_Adj := 
                      ifelse(is.na(M_Written_Off_Factor),0,
                             (M_Written_Off_Factor - 1) * 
                               (M_Written_Off_WO + 
                                  M_Written_Off_Full_Recovery_Benefit_WO +
                                  M_Written_Off_Recovery_Comms_WO))]
    #Remove the ancilliary variables
    data_fin_padded[, c("M_Interest_Accounting_Factor","M_Transfer_Price_Accounting_Factor","M_Fee_Early_Termination_Factor"
                        ,"M_Fee_Establishment_Accounting_Factor","M_Written_Off_Factor"):=NULL]
  }
  
  #####################################@
  ###### 15. Clean Up and Totals #######
  #####################################@
  cat(paste("\n",now(),"15. Clean Up and Totals"))
  
  ###########################@
  ###### ___15a. Flags #######
  ###########################@
  
  #Add some flags that only work on padded data. 
  #  IE when we're counting, as we have padded, sometimes just put the variable on Row 0, so doesn't duplicate count
  data_fin_padded[, Z_Applied_For       := ifelse(Z_Month_Index != 0,0,1)]
  
  #LGD as a dollar
  data_fin_padded[, Z_Write_Off_Net_Dollar_P:=
                    Z_Write_Off_Month_Flag_P_Correct * Z_Write_Off_Net_per_NAF_P * L_Net_Amount_Financed]
  
  #Add the M_Balance_Adj
  data_fin_padded[, M_Balance_Adj := ifelse(M_Interest_Accounting == 0, 
                                            #When there's no interest, use the Z_Taken_Up to work out the adjustment.
                                            -M_Balance * (1 - Z_Taken_Up_P) 
                                            -M_Balance * Z_Taken_Up_P * (Z_WO_Cumulative + Z_Term_Cumulative), 
                                            #Otherwise the adjustment is the combined effect of take-up, write-off and early termination interest
                                            M_Balance * 
                                              (M_Interest_Accounting_TU + 
                                                 M_Interest_Accounting_WO + 
                                                 M_Interest_Accounting_ET + 
                                                 M_Interest_Accounting_Adj)
                                            / M_Interest_Accounting) ]
  data_fin_padded[, c("Z_WO_Cumulative","Z_Term_Cumulative"):=NULL]
  
  ############################@
  ###### ___15b. Totals #######
  ############################@
  
  #Totals
  data_fin_padded[, T_Balance:= M_Balance + M_Balance_Adj]
  
  data_fin_padded[, T_Interest_Cash:= 
                    M_Interest_Cash + 
                    M_Interest_Cash_TU + 
                    M_Interest_Cash_WO + 
                    M_Interest_Cash_ET + 
                    M_Interest_Cash_Adj]
  
  data_fin_padded[, T_Interest_Accounting:= 
                    M_Interest_Accounting + 
                    M_Interest_Accounting_TU + 
                    M_Interest_Accounting_WO + 
                    M_Interest_Accounting_ET + 
                    M_Interest_Accounting_Adj]
  
  data_fin_padded[, T_Fee_Establishment_Cash:= 
                    M_Fee_Establishment_Cash + 
                    M_Fee_Establishment_Cash_TU +
                    M_Fee_Establishment_Cash_Adj]
  
  data_fin_padded[, T_Fee_Establishment_Accounting:= 
                    M_Fee_Establishment_Accounting + 
                    M_Fee_Establishment_Accounting_TU +
                    M_Fee_Establishment_Accounting_Adj]
  
  data_fin_padded[, T_Fee_Monthly:= 
                    M_Fee_Monthly +
                    M_Fee_Monthly_TU +
                    M_Fee_Monthly_WO + 
                    M_Fee_Monthly_ET]
  
  data_fin_padded[, T_Fee_Early_Termination:= 
                    M_Fee_Early_Termination_ET + 
                    M_Fee_Early_Termination_Adj]
  
  data_fin_padded[, T_Transfer_Price_Cash:= 
                    M_Transfer_Price_Cash + 
                    M_Transfer_Price_Cash_TU + 
                    M_Transfer_Price_Cash_WO + 
                    M_Transfer_Price_Cash_ET +
                    M_Transfer_Price_Cash_Adj] 
  
  data_fin_padded[, T_Transfer_Price_Accounting:= 
                    M_Transfer_Price_Accounting + 
                    M_Transfer_Price_Accounting_TU + 
                    M_Transfer_Price_Accounting_WO + 
                    M_Transfer_Price_Accounting_ET +
                    M_Transfer_Price_Accounting_Adj] 
  
  data_fin_padded[, T_Written_Off:= 
                    M_Written_Off_WO + 
                    M_Written_Off_Full_Recovery_Benefit_WO + 
                    M_Written_Off_Recovery_Comms_WO +
                    M_Written_Off_Adj] 
  
  data_fin_padded[, T_Expense_Establishment:= 
                    M_Expense_Establishment + 
                    M_Expense_Establishment_TU]
  
  data_fin_padded[, T_Expense_Monthly:= 
                    M_Expense_Monthly +
                    M_Expense_Monthly_TU +
                    M_Expense_Monthly_WO + 
                    M_Expense_Monthly_ET]
  
  data_fin_padded[, T_Expense_Early_Termination:= 
                    M_Expense_Early_Termination_ET + 
                    M_Expense_Early_Termination_Adj]
  
  data_fin_padded[, T_Expense_GFV_Option:= 
                    M_Expense_GFV_Option + 
                    M_Expense_GFV_Option_TU +
                    M_Expense_GFV_Option_WO + 
                    M_Expense_GFV_Option_ET]
  
  data_fin_padded[, T_Net_Before_Commission_Cash:=
                    T_Interest_Cash +
                    T_Fee_Establishment_Cash +
                    T_Fee_Monthly +
                    T_Fee_Early_Termination +
                    T_Transfer_Price_Cash +
                    T_Written_Off +
                    T_Expense_Establishment +
                    T_Expense_Monthly +
                    T_Expense_Early_Termination +
                    T_Expense_GFV_Option]
  
  data_fin_padded[, T_Net_After_Commission_Cash:=
                    T_Net_Before_Commission_Cash -
                    ifelse(Z_Month_Index==0,VarDealerCommission*L_Net_Amount_Financed,0)]
  
  data_fin_padded[, T_Net_Before_Commission_Accounting:=
                    T_Interest_Accounting +
                    T_Fee_Establishment_Accounting +
                    T_Fee_Monthly +
                    T_Fee_Early_Termination +
                    T_Transfer_Price_Accounting +
                    T_Written_Off +
                    T_Expense_Establishment +
                    T_Expense_Monthly +
                    T_Expense_Early_Termination +
                    T_Expense_GFV_Option]
  
  data_fin_padded[, T_Net_After_Commission_Accounting:=
                    T_Net_Before_Commission_Accounting -
                    ifelse(Z_Month_Index==0,VarDealerCommission*L_Net_Amount_Financed,0)]
  
  #Balance Sheet additions based on these totals
  data_fin_padded[, T_Unrecog_Fee_Establishment  :=sum(T_Fee_Establishment_Accounting)  * Z_Amortisation_Shaped_Unearned, by=ID_Application_ID]
  data_fin_padded[, X_Unrecog_Finance_Commission :=sum(X_Finance_Commission_Accounting) * Z_Amortisation_Shaped_Unearned, by=ID_Application_ID]
  data_fin_padded[, X_Unrecog_Subvention_Amount  :=sum(X_Subvention_Amount_Accounting)  * Z_Amortisation_Shaped_Unearned, by=ID_Application_ID]
  #And set to 0 if NA
  data_fin_padded[, T_Unrecog_Fee_Establishment  :=ifelse(is.na(T_Unrecog_Fee_Establishment), 0,T_Unrecog_Fee_Establishment)]
  data_fin_padded[, X_Unrecog_Finance_Commission :=ifelse(is.na(X_Unrecog_Finance_Commission),0,X_Unrecog_Finance_Commission)]
  data_fin_padded[, X_Unrecog_Subvention_Amount  :=ifelse(is.na(X_Unrecog_Subvention_Amount), 0,X_Unrecog_Subvention_Amount)]
  #And sum
  data_fin_padded[, X_Unrecog_Total:=T_Unrecog_Fee_Establishment + X_Unrecog_Finance_Commission + X_Unrecog_Subvention_Amount]
  data_fin_padded[, c("Z_Amortisation_Shaped_Unearned"):=NULL]
  
  
  ##############################@
  ###### ___15c. Rounding #######
  ##############################@
  
  #Round all the fields that should be rounded
  VarsToRound <- c(#Payments
    "M_Payment",
    #Balance
    "T_Balance",
    "M_Balance",
    "M_Balance_Adj",
    #Unrecognised Amounts
    "X_Unrecog_Total",
    "T_Unrecog_Fee_Establishment",
    "X_Unrecog_Finance_Commission",
    "X_Unrecog_Subvention_Amount",
    #Interest
    "T_Interest_Cash",
    "M_Interest_Cash",
    "M_Interest_Cash_Adj",
    "M_Interest_Cash_ET",
    "M_Interest_Cash_TU",
    "M_Interest_Cash_WO",
    "T_Interest_Accounting",
    "M_Interest_Accounting",
    "M_Interest_Accounting_Adj",
    "M_Interest_Accounting_ET",
    "M_Interest_Accounting_TU",
    "M_Interest_Accounting_WO",
    #Establishment Fee
    "T_Fee_Establishment_Cash",
    "M_Fee_Establishment_Cash",
    "M_Fee_Establishment_Cash_TU",
    "M_Fee_Establishment_Cash_Adj",
    "T_Fee_Establishment_Accounting",
    "M_Fee_Establishment_Accounting",
    "M_Fee_Establishment_Accounting_TU",
    "M_Fee_Establishment_Accounting_Adj",
    #Monthly Fee
    "T_Fee_Monthly",
    "M_Fee_Monthly",
    "M_Fee_Monthly_ET",
    "M_Fee_Monthly_TU",
    "M_Fee_Monthly_WO",
    #Early Termination Fee
    "T_Fee_Early_Termination",
    "M_Fee_Early_Termination_Adj",
    "M_Fee_Early_Termination_ET",
    #Transfer Price
    "T_Transfer_Price_Cash",
    "M_Transfer_Price_Cash",
    "M_Transfer_Price_Cash_Adj",
    "M_Transfer_Price_Cash_ET",
    "M_Transfer_Price_Cash_TU",
    "M_Transfer_Price_Cash_WO",
    "T_Transfer_Price_Accounting",
    "M_Transfer_Price_Accounting",
    "M_Transfer_Price_Accounting_Adj",
    "M_Transfer_Price_Accounting_ET",
    "M_Transfer_Price_Accounting_TU",
    "M_Transfer_Price_Accounting_WO",
    #Written Off
    "T_Written_Off",
    "M_Written_Off_WO",
    "M_Written_Off_Full_Recovery_Benefit_WO",
    "M_Written_Off_Recovery_Comms_WO",
    "M_Written_Off_Adj",
    #Establishment Exense
    "T_Expense_Establishment",
    "M_Expense_Establishment",
    "M_Expense_Establishment_TU",
    #Expense Monthly
    "T_Expense_Monthly",
    "M_Expense_Monthly",
    "M_Expense_Monthly_ET",
    "M_Expense_Monthly_TU",
    "M_Expense_Monthly_WO",
    #Early Termination Expesnse
    "T_Expense_Early_Termination",
    "M_Expense_Early_Termination_Adj",
    "M_Expense_Early_Termination_ET",
    #GFV Expense
    "T_Expense_GFV_Option",
    "M_Expense_GFV_Option",
    "M_Expense_GFV_Option_TU",
    "M_Expense_GFV_Option_WO",
    "M_Expense_GFV_Option_ET",
    #Totals
    "T_Net_Before_Commission_Cash", 
    "T_Net_Before_Commission_Accounting",
    #Commissions Values
    "X_Finance_Commission_Cash",
    "X_Finance_Commission_Accounting",
    "X_Subvention_Amount_Cash",
    "X_Subvention_Amount_Accounting",
    "X_Finance_Clawback_InMonth"
  )
  
  #Do the rounding
  for(i in VarsToRound){
    data_fin_padded[, c(i):=round(get(i), digits=4)]}
  remove(i,VarsToRound)
  
  #######################################@
  ###### ___15d. Cash vs Amortised #######
  #######################################@
  
  #For all the amortised values, see if there's any difference from the cash one and add it on to the 0th month.
  VarsCash<-grep("_Cash",names(data_fin_padded), value=TRUE)
  
  #Work through the variables
  for(iVarsCash in VarsCash){
    iVarsAccounting <- gsub("_Cash","_Accounting",iVarsCash)
    data_fin_padded[, Temp_Adjustment:=sum(get(iVarsCash)) - sum(get(iVarsAccounting)), by=ID_Application_ID]
    data_fin_padded[Z_Month_Index==0
                    , c(iVarsAccounting):=get(iVarsAccounting) + Temp_Adjustment]
    data_fin_padded[, Temp_Adjustment:=NULL]
  }
  
  ##################################@
  ###### ___15e. Sort Columns #######
  ##################################@
  
  #Sort the columns, but leave the ID_Application_ID at the front
  bringtofront <- c("ID_Application_ID", "ID_Contract_ID")
  sortorder    <- c(bringtofront, sort(setdiff(names(data_fin_padded), bringtofront)))
  data_fin_padded<-data_fin_padded[, c(sortorder), with=FALSE]
  rm(bringtofront, sortorder)
  
  #Return this data 
  return(data_fin_padded)
  
}

