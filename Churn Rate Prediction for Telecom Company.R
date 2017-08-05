#Configure Working Directory
getwd()
setwd("<Dataset File Path>")
getwd()

#Install some commonly required package and import the librarys

library(dplyr) #Work with Data Frame and carry out some common manipulation;#linear Model or simple regression model function useage
library(gains) # Gains Chart, Gains tables and lift chart for prediction algorithms
library(irr) #Kappa matrix; Model Accuracy algorithm 
library(ROCR) #ROCR Curve; Cutoff Parameterized Performance Curve design
library(caret) #Confusion Matrix: Classification and Regression Training 
#install.packages("lm.beta")
#library(lm.beta) #linear model object in MASS package
library(car) #multicollinearity Type I , Type III calcualtions  OR
#install.packages("VIF")
#library(VIF)

#Read given csv file for analysys
telecomChurn_Study <- read.csv("<Telecom Comapany Survey Or Subscriber Information Data>", header=T, stringsAsFactors = T)

#Explore Insights of Telecome Data Set
dim(telecomChurn_Study)          # R&C 66297    81
colnames(telecomChurn_Study)     # data_documentation.csv has some meaning of each variable  OR names(telecomChurn_Study)
str(telecomChurn_Study)          # Data Frame with Differnt Data Type Columns
class(telecomChurn_Study)
head(telecomChurn_Study)
tail(telecomChurn_Study)         # Top and Bottom values in data Set Check
View(telecomChurn_Study)         # View data's in Grid Format
summary(telecomChurn_Study)      # Summary of each column data's

#Disable Scientific Notation (to enable use options(scipen = 0) )
options(scipen = 999)

#Understanding data: Creating data quality report(DQR)

#Check Anamolys / Missed and Fix If any using desired Method
colSums(is.na(telecomChurn_Study))

#DO it for all 81 columns in Data Set(Extracting)
telecomChurn_Study_Variables <- names(telecomChurn_Study)
length(telecomChurn_Study_Variables)  

#Mark all 81 columns in Data Set to removed List
telecomChurn_Study_Variables_Removed <- telecomChurn_Study_Variables
VariableName <- telecomChurn_Study_Variables
telecomChurn_Study_Variables_DQR <- as.data.frame(telecomChurn_Study_Variables)
length(telecomChurn_Study_Variables_Removed)  

#Remove Objects from Environment 
View(telecomChurn_Study_Variables_Removed)
rm(telecomChurn_Study_Variables_Removed)

#Apply a Function over a List or Vector(Data type for each variable)
telecomChurn_Study_Variables_DQR$DataType <- sapply(telecomChurn_Study, class)

#Check Sum of Records available for all variable  
telecomChurn_Study_Variables_DQR$NoOfRecords<-nrow(telecomChurn_Study)

#Check Sum of unique Records available for all variable  
for(i in 1:ncol(telecomChurn_Study))
{
  telecomChurn_Study_Variables_DQR$UniqueRecords[i] <- length(unique(telecomChurn_Study[,i]))
}

#Check Sum of Observations available for all variable and % of Observations
telecomChurn_Study_Variables_DQR$DataAvailable <- colSums(!is.na(telecomChurn_Study))
telecomChurn_Study_Variables_DQR$AvailablePercent <- round(colMeans(!is.na(telecomChurn_Study)),4)

#Check Sum of Missed Values / anamolys for all variable and % of Missing Values
telecomChurn_Study_Variables_DQR$Missing <- colSums(is.na(telecomChurn_Study))
telecomChurn_Study_Variables_DQR$MissingPercent <- round(colMeans(is.na(telecomChurn_Study)),4)

#Quick Check with New additions
str(telecomChurn_Study_Variables_DQR)

#Find Numerical Measures result (Check Minimum, Maximum, Mean, Quantile values for all variables)
for(i in 1:ncol(telecomChurn_Study))
{
  telecomChurn_Study_Variables_DQR$Minimum[i]                <- round(ifelse(class(telecomChurn_Study[,i]) == "integer" | class(telecomChurn_Study[,i]) == "numeric",min(telecomChurn_Study[,i],na.rm=T),0),2)
  telecomChurn_Study_Variables_DQR$Maximum[i]                <- round(ifelse(class(telecomChurn_Study[,i]) == "integer" | class(telecomChurn_Study[,i]) == "numeric",max(telecomChurn_Study[,i],na.rm=T),0),2)
  telecomChurn_Study_Variables_DQR$Mean[i]                   <- round(ifelse(class(telecomChurn_Study[,i]) == "integer" | class(telecomChurn_Study[,i]) == "numeric",mean(telecomChurn_Study[,i],na.rm=T),0),2)
  telecomChurn_Study_Variables_DQR$FifthPercentile[i]        <- round(ifelse(class(telecomChurn_Study[,i]) == "integer" | class(telecomChurn_Study[,i]) == "numeric",quantile(telecomChurn_Study[,i],p=0.05,na.rm=T),0),2)
  telecomChurn_Study_Variables_DQR$TenthPercentile[i]        <- round(ifelse(class(telecomChurn_Study[,i]) == "integer" | class(telecomChurn_Study[,i]) == "numeric",quantile(telecomChurn_Study[,i],p=0.10,na.rm=T),0),2)
  telecomChurn_Study_Variables_DQR$TwentyFifthPercentile[i]  <- round(ifelse(class(telecomChurn_Study[,i]) == "integer" | class(telecomChurn_Study[,i]) == "numeric",quantile(telecomChurn_Study[,i],p=0.25,na.rm=T),0),2)
  telecomChurn_Study_Variables_DQR$FiftythPercentile[i]      <- round(ifelse(class(telecomChurn_Study[,i]) == "integer" | class(telecomChurn_Study[,i]) == "numeric",quantile(telecomChurn_Study[,i],p=0.50,na.rm=T),0),2)
  telecomChurn_Study_Variables_DQR$SeventyFifthPercentile[i] <- round(ifelse(class(telecomChurn_Study[,i]) == "integer" | class(telecomChurn_Study[,i]) == "numeric",quantile(telecomChurn_Study[,i],p=0.75,na.rm=T),0),2)
  telecomChurn_Study_Variables_DQR$NinetythPercentile[i]     <- round(ifelse(class(telecomChurn_Study[,i]) == "integer" | class(telecomChurn_Study[,i]) == "numeric",quantile(telecomChurn_Study[,i],p=0.90,na.rm=T),0),2)
  telecomChurn_Study_Variables_DQR$NinetyfifthPercentile[i]  <- round(ifelse(class(telecomChurn_Study[,i]) == "integer" | class(telecomChurn_Study[,i]) == "numeric",quantile(telecomChurn_Study[,i],p=0.95,na.rm=T),0),2)
}

#Quick Check with New additions
str(telecomChurn_Study_Variables_DQR)

#Export in .csv Data Quality Report Before Missing value Treatment
write.csv(telecomChurn_Study_Variables_DQR, "Data Quality Report for Telecom Chur Study.csv", row.names = T)

#Missing Values / Anamolys Treatment for variable "retdays" and Add Dummy variable with Missing value fills
str(telecomChurn_Study$retdays)
summary(telecomChurn_Study$retdays)  #Mean 234.40
sort(unique(telecomChurn_Study$retdays), na.last = F)

#Dummy variable (Prevent 'retdays' from Omit activing with 15% scale)
#variable :  [67] "retdays_Dummy"
telecomChurn_Study$retdays_Dummy <- ifelse(is.na(telecomChurn_Study$retdays) == TRUE, 0, 1)
str(telecomChurn_Study$retdays_Dummy)
summary(telecomChurn_Study$retdays_Dummy) #Mean 0.03249

#Reject variables with more than 15% anamolys and Assign New data set name for further analysys Process
telecomChurn_Study_Cleaned <- telecomChurn_Study[, colMeans(is.na(telecomChurn_Study)) <= 0.15]
telecomChurn_Study_Cleaned$churn <- telecomChurn_Study$churn #used for rate Check
str(telecomChurn_Study_Cleaned)
summary(telecomChurn_Study_Cleaned)  #'data.frame':	66297 obs. of  68 variables(included Dummy): (Rejected 14)

#Ref: data_documentation: variable natures are also combination of others
#Variable drop_blk_Means is one such: it shows its appearence in combination of 
# blck_dat_Mean, Blck_vce_Mean, drop_dat_Mean, drop_vce_mean
summary(telecomChurn_Study_Cleaned$drop_blk_Means)
summary(telecomChurn_Study_Cleaned$blck_dat_Mean)
summary(telecomChurn_Study_Cleaned$Blck_vce_Mean)
summary(telecomChurn_Study_Cleaned$drop_dat_Mean)
summary(telecomChurn_Study_Cleaned$drop_vce_mean)

#Reject / omit variable blck_dat_Mean
#Check its position
names(telecomChurn_Study_Cleaned) #Remove Position 50 (68 become 67 in total)
telecomChurn_Study_Cleaned <- telecomChurn_Study_Cleaned[,-50] 

#Check Anamolys Now
colSums(is.na(telecomChurn_Study_Cleaned))

#Variable Profiling: Continuous Variables, Categorical Variables
#Deciling Continuous Variables; finding out event rate (churn rate in this case)

names(telecomChurn_Study_Cleaned)
str(telecomChurn_Study_Cleaned)
View(telecomChurn_Study_Cleaned)

#variable :  [1] "mou_Mean"
summary(telecomChurn_Study_Cleaned$mou_Mean)
dat_mou_Mean             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_mou_Mean$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]							
dat_mou_Mean$churn_perc  <- round(dat_mou_Mean$N / dat_mou_Mean$N,2)
dat_mou_Mean$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]		
dat_mou_Mean$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]		
dat_mou_Mean$varname     <- rep("mou_Mean",nrow(dat_mou_Mean))

#variable :  [2] "totmrc_Mean"
summary(telecomChurn_Study_Cleaned$totmrc_Mean)
dat_totmrc_Mean             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_totmrc_Mean$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]							
dat_totmrc_Mean$churn_perc  <- round(dat_totmrc_Mean$N / dat_totmrc_Mean$N,2)
dat_totmrc_Mean$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]		
dat_totmrc_Mean$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]		
dat_totmrc_Mean$varname     <- rep("totmrc_Mean",nrow(dat_totmrc_Mean))

#variable :  [3] "rev_Range"
summary(telecomChurn_Study_Cleaned$rev_Range)
dat_rev_Range             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_rev_Range$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]							
dat_rev_Range$churn_perc  <- round(dat_rev_Range$N / dat_rev_Range$N,2)
dat_rev_Range$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]		
dat_rev_Range$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]		
dat_rev_Range$varname     <- rep("rev_Range",nrow(dat_rev_Range))

#variable :  [4] "mou_Range"
summary(telecomChurn_Study_Cleaned$mou_Range)
dat_mou_Range             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_mou_Range$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]							
dat_mou_Range$churn_perc  <- round(dat_mou_Range$N / dat_mou_Range$N,2)
dat_mou_Range$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]		
dat_mou_Range$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]		
dat_mou_Range$varname     <- rep("mou_Range",nrow(dat_mou_Range))

#variable :  [5] "change_mou"
summary(telecomChurn_Study_Cleaned$change_mou)
dat_change_mou             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_change_mou$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]							
dat_change_mou$churn_perc  <- round(dat_change_mou$N / dat_change_mou$N,2)
dat_change_mou$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]		
dat_change_mou$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]		
dat_change_mou$varname     <- rep("change_mou",nrow(dat_change_mou))

#variable :  [6] "drop_blk_Mean"
summary(telecomChurn_Study_Cleaned$drop_blk_Mean)
dat_drop_blk_Mean             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_drop_blk_Mean$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]							
dat_drop_blk_Mean$churn_perc  <- round(dat_drop_blk_Mean$N / dat_drop_blk_Mean$N,2)
dat_drop_blk_Mean$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]		
dat_drop_blk_Mean$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]		
dat_drop_blk_Mean$varname     <- rep("drop_blk_Mean",nrow(dat_drop_blk_Mean))

#variable :  [7] "drop_vce_Range" 
summary(telecomChurn_Study_Cleaned$drop_vce_Range)
dat_drop_vce_Range             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_drop_vce_Range$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]							
dat_drop_vce_Range$churn_perc  <- round(dat_drop_vce_Range$N / dat_drop_vce_Range$N,2)
dat_drop_vce_Range$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]		
dat_drop_vce_Range$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]		
dat_drop_vce_Range$varname     <- rep("drop_vce_Range",nrow(dat_drop_vce_Range))

#variable :  [8] "owylis_vce_Range"
summary(telecomChurn_Study_Cleaned$owylis_vce_Range)
dat_owylis_vce_Range             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_owylis_vce_Range$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]							
dat_owylis_vce_Range$churn_perc  <- round(dat_owylis_vce_Range$N / dat_owylis_vce_Range$N,2)
dat_owylis_vce_Range$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]		
dat_owylis_vce_Range$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]		
dat_owylis_vce_Range$varname     <- rep("owylis_vce_Range",nrow(dat_owylis_vce_Range))

#variable :  [9] "mou_opkv_Range"
summary(telecomChurn_Study_Cleaned$mou_opkv_Range)
dat_mou_opkv_Range              <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_mou_opkv_Range $N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]							
dat_mou_opkv_Range $churn_perc  <- round(dat_mou_opkv_Range $N / dat_mou_opkv_Range $N,2)
dat_mou_opkv_Range $GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]		
dat_mou_opkv_Range $LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]		
dat_mou_opkv_Range $varname     <- rep("mou_opkv_Range",nrow(dat_mou_opkv_Range ))

#variable :  [10] "months"
summary(telecomChurn_Study_Cleaned$months)
dat_months             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_months$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]							
dat_months$churn_perc  <- round(dat_months$N / dat_months$N,2)
dat_months$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]		
dat_months$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]		
dat_months$varname     <- rep("months",nrow(dat_months))

#variable :  [11] "totcalls"
summary(telecomChurn_Study_Cleaned$totcalls)
dat_totcalls             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_totcalls$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]							
dat_totcalls$churn_perc  <- round(dat_totcalls$N / dat_totcalls$N,2)
dat_totcalls$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]		
dat_totcalls$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]		
dat_totcalls$varname     <- rep("totcalls",nrow(dat_totcalls))

#Some are dicile based
#variable :  [12] "eqpdays"
summary(telecomChurn_Study_Cleaned$eqpdays)
index <- which(is.na(telecomChurn_Study_Cleaned$eqpdays))
telecomChurn_Study_Cleaned <- telecomChurn_Study_Cleaned[-index,]
dat_eqpdays             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_eqpdays$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]							
dat_eqpdays$churn_perc  <- round(dat_eqpdays$n / dat_eqpdays$N,2)
dat_eqpdays$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]		
dat_eqpdays$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]		
dat_eqpdays$varname     <- rep("eqpdays",nrow(dat_eqpdays))

#variable :  [18] "adjqty"    
summary(telecomChurn_Study_Cleaned$adjqty)
dat_adjqty             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_adjqty$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]							
dat_adjqty$churn_perc  <- round(dat_adjqty$n / dat_adjqty$N,2)
dat_adjqty$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]		
dat_adjqty$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]		
dat_adjqty$varname     <- rep("adjqty",nrow(dat_adjqty))

#variable :  [19] "ovrrev_Mean"    
summary(telecomChurn_Study_Cleaned$ovrrev_Mean)
dat_ovrrev_Mean             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_ovrrev_Mean$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(ovrrev_Mean,n=8))%>%count(dec)%>%unname())[[2]]							
dat_ovrrev_Mean$churn_perc  <- round(dat_ovrrev_Mean$n / dat_ovrrev_Mean$N,2)
dat_ovrrev_Mean$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(ovrrev_Mean,n=8))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]		
dat_ovrrev_Mean$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(ovrrev_Mean,n=8))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]		
dat_ovrrev_Mean$varname     <- rep("ovrrev_Mean",nrow(dat_ovrrev_Mean))

#variable :  [20] "rev_Mean"    
summary(telecomChurn_Study_Cleaned$rev_Mean)
dat_rev_Mean             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_rev_Mean$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]							
dat_rev_Mean$churn_perc  <- round(dat_rev_Mean$n / dat_rev_Mean$N,2)
dat_rev_Mean$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]		
dat_rev_Mean$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]		
dat_rev_Mean$varname     <- rep("rev_Mean",nrow(dat_rev_Mean))

#variable :  [22] "comp_vce_Mean"    
summary(telecomChurn_Study_Cleaned$comp_vce_Mean)
dat_comp_vce_Mean             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_comp_vce_Mean$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]							
dat_comp_vce_Mean$churn_perc  <- round(dat_comp_vce_Mean$n / dat_comp_vce_Mean$N,2)
dat_comp_vce_Mean$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]		
dat_comp_vce_Mean$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]		
dat_comp_vce_Mean$varname     <- rep("comp_vce_Mean",nrow(dat_comp_vce_Mean))

#variable :  [23] "plcd_vce_Mean"  
summary(telecomChurn_Study_Cleaned$plcd_vce_Mean)
dat_plcd_vce_Mean             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_plcd_vce_Mean$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]							
dat_plcd_vce_Mean$churn_perc  <- round(dat_plcd_vce_Mean$n / dat_plcd_vce_Mean$N,2)
dat_plcd_vce_Mean$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]		
dat_plcd_vce_Mean$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]		
dat_plcd_vce_Mean$varname     <- rep("plcd_vce_Mean",nrow(dat_plcd_vce_Mean))

#variable :  [24] "avg3mou"    
summary(telecomChurn_Study_Cleaned$avg3mou)
dat_avg3mou             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avg3mou$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]							
dat_avg3mou$churn_perc  <- round(dat_avg3mou$n / dat_avg3mou$N,2)
dat_avg3mou$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]		
dat_avg3mou$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]		
dat_avg3mou$varname     <- rep("avg3mou",nrow(dat_avg3mou))

#variable :  [25] "avgmou" 
summary(telecomChurn_Study_Cleaned$avgmou)
dat_avgmou             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avgmou$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]							
dat_avgmou$churn_perc  <- round(dat_avgmou$n / dat_avgmou$N,2)
dat_avgmou$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]		
dat_avgmou$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]		
dat_avgmou$varname     <- rep("avgmou",nrow(dat_avgmou))

#variable :  [26] "avg3qty" 
summary(telecomChurn_Study_Cleaned$avg3qty)
dat_avg3qty             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avg3qty$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]							
dat_avg3qty$churn_perc  <- round(dat_avg3qty$n / dat_avg3qty$N,2)
dat_avg3qty$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]		
dat_avg3qty$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]		
dat_avg3qty$varname     <- rep("avg3qty",nrow(dat_avg3qty))

#variable :  [27] "avgqty"
summary(telecomChurn_Study_Cleaned$avgqty)
dat_avgqty             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avgqty$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]							
dat_avgqty$churn_perc  <- round(dat_avgqty$n / dat_avgqty$N,2)
dat_avgqty$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]		
dat_avgqty$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]		
dat_avgqty$varname     <- rep("avgqty",nrow(dat_avgqty))

#variable :  [28] "avg6mou" 
summary(telecomChurn_Study_Cleaned$avg6mou)
dat_avg6mou             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avg6mou$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]							
dat_avg6mou$churn_perc  <- round(dat_avg6mou$n / dat_avg6mou$N,2)
dat_avg6mou$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]		
dat_avg6mou$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]		
dat_avg6mou$varname     <- rep("avg6mou",nrow(dat_avg6mou))

#variable :  [29] "avg6qty"  
summary(telecomChurn_Study_Cleaned$avg6qty)
dat_avg6qty             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avg6qty$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]							
dat_avg6qty$churn_perc  <- round(dat_avg6qty$n / dat_avg6qty$N,2)
dat_avg6qty$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]		
dat_avg6qty$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]		
dat_avg6qty$varname     <- rep("avg6qty",nrow(dat_avg6qty))

#variable :  [38] "age1"   #Omit
summary(telecomChurn_Study_Cleaned$age1)
dat_age1             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(age1,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_age1$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(age1,n=9))%>%count(dec)%>%unname())[[2]]							
dat_age1$churn_perc  <- round(dat_age1$n / dat_age1$N,2)
dat_age1$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(age1,n=9))%>%group_by(dec)%>%summarise(min(age1)))[[2]]		
dat_age1$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(age1,n=9))%>%group_by(dec)%>%summarise(max(age1)))[[2]]		
dat_age1$varname     <- rep("age1",nrow(dat_age1))

#variable :  [39] "age2"   #Omit
summary(telecomChurn_Study_Cleaned$age2)
dat_age2             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(age2,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_age2$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(age2,n=7))%>%count(dec)%>%unname())[[2]]							
dat_age2$churn_perc  <- round(dat_age2$n / dat_age2$N,2)
dat_age2$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(age2,n=7))%>%group_by(dec)%>%summarise(min(age2)))[[2]]		
dat_age2$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(age2,n=7))%>%group_by(dec)%>%summarise(max(age2)))[[2]]		
dat_age2$varname     <- rep("age2",nrow(dat_age2))

#variable :  [40] "models" #Omit
summary(telecomChurn_Study_Cleaned$models)
dat_models             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(models,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_models$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(models,n=5))%>%count(dec)%>%unname())[[2]]							
dat_models$churn_perc  <- round(dat_models$n / dat_models$N,2)
dat_models$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(models,n=5))%>%group_by(dec)%>%summarise(min(models)))[[2]]		
dat_models$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(models,n=5))%>%group_by(dec)%>%summarise(max(models)))[[2]]		
dat_models$varname     <- rep("models",nrow(dat_models))

#variable :  [41] "hnd_price"   
summary(telecomChurn_Study_Cleaned$hnd_price)
dat_hnd_price             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_hnd_price$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]							
dat_hnd_price$churn_perc  <- round(dat_hnd_price$n / dat_hnd_price$N,2)
dat_hnd_price$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]		
dat_hnd_price$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]		
dat_hnd_price$varname     <- rep("hnd_price",nrow(dat_hnd_price))


#variable :  [42] "actvsubs"   #Omit
summary(telecomChurn_Study_Cleaned$actvsubs)
dat_actvsubs             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(actvsubs,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_actvsubs$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(actvsubs,n=4))%>%count(dec)%>%unname())[[2]]							
dat_actvsubs$churn_perc  <- round(dat_actvsubs$n / dat_actvsubs$N,2)
dat_actvsubs$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(actvsubs,n=4))%>%group_by(dec)%>%summarise(min(actvsubs)))[[2]]		
dat_actvsubs$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(actvsubs,n=4))%>%group_by(dec)%>%summarise(max(actvsubs)))[[2]]		
dat_actvsubs$varname     <- rep("actvsubs",nrow(dat_actvsubs))

#variable :  [43] "uniqsubs" #Omit
summary(telecomChurn_Study_Cleaned$uniqsubs)
dat_uniqsubs             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(uniqsubs,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_uniqsubs$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(uniqsubs,n=5))%>%count(dec)%>%unname())[[2]]							
dat_uniqsubs$churn_perc  <- round(dat_uniqsubs$n / dat_uniqsubs$N,2)
dat_uniqsubs$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(uniqsubs,n=5))%>%group_by(dec)%>%summarise(min(uniqsubs)))[[2]]		
dat_uniqsubs$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(uniqsubs,n=5))%>%group_by(dec)%>%summarise(max(uniqsubs)))[[2]]		
dat_uniqsubs$varname     <- rep("uniqsubs",nrow(dat_uniqsubs))

#variable :  [44] "forgntvl" #Omit
summary(telecomChurn_Study_Cleaned$forgntvl)
dat_forgntvl             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(forgntvl,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_forgntvl$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(forgntvl,n=4))%>%count(dec)%>%unname())[[2]]							
dat_forgntvl$churn_perc  <- round(dat_forgntvl$n / dat_forgntvl$N,2)
dat_forgntvl$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(forgntvl,n=4))%>%group_by(dec)%>%summarise(min(forgntvl)))[[2]]		
dat_forgntvl$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(forgntvl,n=4))%>%group_by(dec)%>%summarise(max(forgntvl)))[[2]]		
dat_forgntvl$varname     <- rep("forgntvl",nrow(dat_forgntvl))

#variable :  [59] "drop_vce_Mean"    
summary(telecomChurn_Study_Cleaned$drop_vce_Mean)
dat_drop_vce_Mean             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_drop_vce_Mean$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]							
dat_drop_vce_Mean$churn_perc  <- round(dat_drop_vce_Mean$n / dat_drop_vce_Mean$N,2)
dat_drop_vce_Mean$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]		
dat_drop_vce_Mean$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]		
dat_drop_vce_Mean$varname     <- rep("drop_vce_Mean",nrow(dat_drop_vce_Mean))

#variable :  [60] "adjmou"    
summary(telecomChurn_Study_Cleaned$adjmou)
dat_adjmou             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_adjmou$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]							
dat_adjmou$churn_perc  <- round(dat_adjmou$n / dat_adjmou$N,2)
dat_adjmou$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]		
dat_adjmou$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]		
dat_adjmou$varname     <- rep("adjmou",nrow(dat_adjmou))

#variable :  [61] "totrev"  
summary(telecomChurn_Study_Cleaned$totrev)
dat_totrev             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_totrev$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]							
dat_totrev$churn_perc  <- round(dat_totrev$n / dat_totrev$N,2)
dat_totrev$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]		
dat_totrev$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]		
dat_totrev$varname     <- rep("totrev",nrow(dat_totrev))

#variable :  [62] "adjrev"  
summary(telecomChurn_Study_Cleaned$adjrev)
dat_adjrev             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_adjrev$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]							
dat_adjrev$churn_perc  <- round(dat_adjrev$n / dat_adjrev$N,2)
dat_adjrev$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]		
dat_adjrev$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]		
dat_adjrev$varname     <- rep("adjrev",nrow(dat_adjrev))

#variable :  [63] "avgrev"  
summary(telecomChurn_Study_Cleaned$avgrev)
dat_avgrev             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avgrev$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]							
dat_avgrev$churn_perc  <- round(dat_avgrev$n / dat_avgrev$N,2)
dat_avgrev$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]		
dat_avgrev$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]		
dat_avgrev$varname     <- rep("avgrev",nrow(dat_avgrev))

#variable :  [64] "Customer_ID"     
summary(telecomChurn_Study_Cleaned$Customer_ID)
dat_Customer_ID             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(Customer_ID,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_Customer_ID$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(Customer_ID,n=10))%>%count(dec)%>%unname())[[2]]							
dat_Customer_ID$churn_perc  <- round(dat_Customer_ID$n / dat_Customer_ID$N,2)
dat_Customer_ID$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(Customer_ID,n=10))%>%group_by(dec)%>%summarise(min(Customer_ID)))[[2]]		
dat_Customer_ID$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(Customer_ID,n=10))%>%group_by(dec)%>%summarise(max(Customer_ID)))[[2]]		
dat_Customer_ID$varname     <- rep("opk_dat_Mean",nrow(dat_Customer_ID))

#variable :  [67] "retdays_Dummy"     
summary(telecomChurn_Study_Cleaned$retdays_Dummy)
dat_retdays_Dummy             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(retdays_Dummy,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_retdays_Dummy$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(retdays_Dummy,n=4))%>%count(dec)%>%unname())[[2]]							
dat_retdays_Dummy$churn_perc  <- round(dat_retdays_Dummy$n / dat_retdays_Dummy$N,2)
dat_retdays_Dummy$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(retdays_Dummy,n=4))%>%group_by(dec)%>%summarise(min(retdays_Dummy)))[[2]]		
dat_retdays_Dummy$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(retdays_Dummy,n=4))%>%group_by(dec)%>%summarise(max(retdays_Dummy)))[[2]]		
dat_retdays_Dummy$varname     <- rep("retdays_Dummy",nrow(dat_retdays_Dummy))


##Below variables Getting Less Deciles; can Omit this 
##Data Transfermation and Count Delete Or Missed value Treatment later #not this variable: used for rate [51] "churn" check

#variable :              [13] "custcare_Mean", [14] "callwait_Mean", [15] "iwylis_vce_Mean", 
#,[17] "ccrndmou_Range", [21] "ovrmou_Mean",   [45] "opk_dat_Mean",  [51] "churn",   [16] "callwait_Range"   
#,[48] "roam_Mean",      [49] "recv_sms_Mean", [50] "mou_pead_Mean", [54] "da_Mean"  [65] "comp_dat_Mean",         
#,[55] "da_Range",       [56] "datovr_Mean",   [57] "datovr_Range",  [58] "drop_dat_Mean",    
#,[66] "plcd_dat_Mean",  [67] "retdays_Dummy" 

#Later this can be factor converted
#[38] "age1",           [39] "age2" ,    [40] "models"
#[42] "actvsubs",       [43] "uniqsubs", [44] "forgntvl", [46] "mtrcycle", [47] "truck"

#[30] to [37] & [52], [53] are Factor Data Type "crclscod","asl_flag","prizm_social_one","area","refurb_new","hnd_webcap",marital","ethnic" 
#Above will calculatd in Categorical variable Section

names(telecomChurn_Study_Cleaned) #position Check

#Create Dummy variables
#variable :  [69] "plcd_Attempt_Mean_Dummy"  
telecomChurn_Study_Cleaned$plcd_Attempt_Mean_Dummy <- telecomChurn_Study_Cleaned$plcd_vce_Mean + telecomChurn_Study_Cleaned$plcd_dat_Mean

summary(telecomChurn_Study_Cleaned$plcd_Attempt_Mean_Dummy)
dat_plcd_Attempt_Mean_Dummy             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(plcd_Attempt_Mean_Dummy,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_plcd_Attempt_Mean_Dummy$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(plcd_Attempt_Mean_Dummy,n=10))%>%count(dec)%>%unname())[[2]]							
dat_plcd_Attempt_Mean_Dummy$churn_perc  <- round(dat_plcd_Attempt_Mean_Dummy$n / dat_plcd_Attempt_Mean_Dummy$N,2)
dat_plcd_Attempt_Mean_Dummy$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(plcd_Attempt_Mean_Dummy,n=10))%>%group_by(dec)%>%summarise(min(plcd_Attempt_Mean_Dummy)))[[2]]		
dat_plcd_Attempt_Mean_Dummy$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(plcd_Attempt_Mean_Dummy,n=10))%>%group_by(dec)%>%summarise(max(plcd_Attempt_Mean_Dummy)))[[2]]		
dat_plcd_Attempt_Mean_Dummy$varname     <- rep("plcd_Attempt_Mean_Dummy",nrow(dat_plcd_Attempt_Mean_Dummy))

#variable :  [70] "complete_Mean_Dummy" 
telecomChurn_Study_Cleaned$complete_Mean_Dummy <- telecomChurn_Study_Cleaned$comp_vce_Mean + telecomChurn_Study_Cleaned$comp_dat_Mean

summary(telecomChurn_Study_Cleaned$complete_Mean_Dummy)
dat_complete_Mean_Dummy             <- telecomChurn_Study_Cleaned%>%mutate(dec=ntile(complete_Mean_Dummy,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_complete_Mean_Dummy$N           <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(complete_Mean_Dummy,n=10))%>%count(dec)%>%unname())[[2]]							
dat_complete_Mean_Dummy$churn_perc  <- round(dat_complete_Mean_Dummy$n / dat_complete_Mean_Dummy$N,2)
dat_complete_Mean_Dummy$GreaterThan <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(complete_Mean_Dummy,n=10))%>%group_by(dec)%>%summarise(min(complete_Mean_Dummy)))[[2]]		
dat_complete_Mean_Dummy$LessThan    <- unclass(telecomChurn_Study_Cleaned%>%mutate(dec=ntile(complete_Mean_Dummy,n=10))%>%group_by(dec)%>%summarise(max(complete_Mean_Dummy)))[[2]]		
dat_complete_Mean_Dummy$varname     <- rep("complete_Mean_Dummy",nrow(dat_complete_Mean_Dummy))

#Add above created objects to create data Object for telecom Study
#13:17,21,22:23,38:40,42:50,54:58,65:66 are ommitted
telecomChurn_Study_DataObject_Continuous <- rbind(dat_mou_Mean, dat_totmrc_Mean, dat_rev_Range,dat_mou_Range, dat_change_mou,
                                                  dat_drop_blk_Mean,dat_drop_vce_Range, dat_owylis_vce_Range, dat_mou_opkv_Range, dat_months,
                                                  dat_totcalls,dat_eqpdays,dat_adjqty, dat_rev_Mean, dat_comp_vce_Mean, 
                                                  dat_plcd_vce_Mean,dat_avg3mou,dat_avgmou,dat_avg3qty,dat_avgqty,
                                                  dat_avg6mou,dat_avg6qty,dat_hnd_price,dat_drop_vce_Mean,dat_adjmou,
                                                  dat_totrev,dat_adjrev,dat_avgrev,dat_Customer_ID,dat_retdays_Dummy,
                                                  dat_plcd_Attempt_Mean_Dummy,dat_complete_Mean_Dummy)
str(telecomChurn_Study_DataObject_Continuous)
summary(telecomChurn_Study_DataObject_Continuous)
ifelse(is.na(telecomChurn_Study_DataObject_Continuous) == TRUE, 0, 1)

#Export into .csv file all deciled variables
write.csv(telecomChurn_Study_DataObject_Continuous, "Continuous Variable Deciled Output.csv",row.names = F)

#If needed we can Omit columns based on dicile output to avoid insignificant info in model; 
#Also Transfered value it converted as Dummy now for further calculations(Clean step)
## 

names(telecomChurn_Study_Cleaned) #Total 70
str(telecomChurn_Study_Cleaned) #Avoid Vectors  30:37, 52:53
telecomChurn_Study_Cleaned_deciled <- telecomChurn_Study_Cleaned[, -c(13:17,21:23,45,48:50,54:58,65:66)] #Keep it 38:44 46,47
names(telecomChurn_Study_Cleaned_deciled) #Total 49 (Dropped 20)
View(telecomChurn_Study_Cleaned_deciled)

##Deciling Categorical Variables; finding out event rate (churn rate in this case)

#variable :  [30] "crclscod" 
summary(telecomChurn_Study_Cleaned_deciled$crclscod)
dat_crclscod             <- telecomChurn_Study_Cleaned_deciled%>%count(churn,levels=crclscod)%>%filter(churn==1)
dat_crclscod$N           <- unclass(telecomChurn_Study_Cleaned_deciled%>%filter(crclscod%in%dat_crclscod$levels)%>%count(crclscod))[[2]]
dat_crclscod$ChurnPerc   <- dat_crclscod$n/dat_crclscod$N
dat_crclscod$Var.Name    <- rep("crclscod",nrow(dat_crclscod))

#variable :  [31] "asl_flag" 
summary(telecomChurn_Study_Cleaned_deciled$asl_flag)
dat_asl_flag            <- telecomChurn_Study_Cleaned_deciled%>%count(churn,levels=asl_flag)%>%filter(churn==1)
dat_asl_flag$N          <- unclass(telecomChurn_Study_Cleaned_deciled%>%filter(asl_flag%in%dat_asl_flag$levels)%>%count(asl_flag))[[2]]
dat_asl_flag$ChurnPerc  <- dat_asl_flag$n/dat_asl_flag$N
dat_asl_flag$Var.Name   <- rep("asl_flag",nrow(dat_asl_flag))

#variable :  [32] "prizm_social_one" 
summary(telecomChurn_Study_Cleaned_deciled$prizm_social_one)
dat_prizm_social_one <- telecomChurn_Study_Cleaned_deciled%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)
dat_prizm_social_one$N<-unclass(telecomChurn_Study_Cleaned_deciled%>%filter(prizm_social_one%in%dat_prizm_social_one$levels)%>%count(prizm_social_one))[[2]]
dat_prizm_social_one$ChurnPerc<-dat_prizm_social_one$n/dat_prizm_social_one$N
dat_prizm_social_one$Var.Name<-rep("prizm_social_one",nrow(dat_prizm_social_one))

#variable :  [33] "area"  
summary(telecomChurn_Study_Cleaned_deciled$area)
dat_area <- telecomChurn_Study_Cleaned_deciled%>%count(churn,levels=area)%>%filter(churn==1)
dat_area$N<-unclass(telecomChurn_Study_Cleaned_deciled%>%filter(area%in%dat_area$levels)%>%count(area))[[2]]
dat_area$ChurnPerc<-dat_area$n/dat_area$N
dat_area$Var.Name<-rep("area",nrow(dat_area))

#variable :  [34] "refurb_new" 
summary(telecomChurn_Study_Cleaned_deciled$refurb_new)
telecomChurn_Study_Cleaned_deciled$refurb_new_R <- ifelse(telecomChurn_Study_Cleaned_deciled$refurb_new == "R",1,0)

dat_refurb_new_R <- telecomChurn_Study_Cleaned_deciled%>%count(churn,levels=refurb_new_R)%>%filter(churn==1)
dat_refurb_new_R$N<-unclass(telecomChurn_Study_Cleaned_deciled%>%filter(refurb_new_R%in%dat_refurb_new_R$levels)%>%count(refurb_new_R))[[2]]
dat_refurb_new_R$ChurnPerc<-dat_refurb_new_R$n/dat_refurb_new_R$N
dat_refurb_new_R$Var.Name<-rep("refurb_new_R",nrow(dat_refurb_new_R))

#variable :  [35] "hnd_webcap"
summary(telecomChurn_Study_Cleaned_deciled$hnd_webcap)
dat_hnd_webcap <- telecomChurn_Study_Cleaned_deciled%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)
dat_hnd_webcap$N<-unclass(telecomChurn_Study_Cleaned_deciled%>%filter(hnd_webcap%in%dat_hnd_webcap$levels)%>%count(hnd_webcap))[[2]]
dat_hnd_webcap$ChurnPerc<-dat_hnd_webcap$n/dat_hnd_webcap$N
dat_hnd_webcap$Var.Name<-rep("hnd_webcap",nrow(dat_hnd_webcap))

#variable :  [36] "marital"    
summary(telecomChurn_Study_Cleaned_deciled$marital)
dat_marital <- telecomChurn_Study_Cleaned_deciled%>%count(churn,levels=marital)%>%filter(churn==1)
dat_marital$N<-unclass(telecomChurn_Study_Cleaned_deciled%>%filter(marital%in%dat_marital$levels)%>%count(marital))[[2]]
dat_marital$ChurnPerc<-dat_marital$n/dat_marital$N
dat_marital$Var.Name<-rep("marital",nrow(dat_marital))

#variable :  [37] "ethnic" 
summary(telecomChurn_Study_Cleaned_deciled$ethnic)
dat_ethnic <- telecomChurn_Study_Cleaned_deciled%>%count(churn,levels=ethnic)%>%filter(churn==1)
dat_ethnic$N<-unclass(telecomChurn_Study_Cleaned_deciled%>%filter(ethnic%in%dat_ethnic$levels)%>%count(ethnic))[[2]]
dat_ethnic$ChurnPerc<-dat_ethnic$n/dat_ethnic$N
dat_ethnic$Var.Name<-rep("ethnic",nrow(dat_ethnic))

#variable :  [52] "car_buy" 
summary(telecomChurn_Study_Cleaned_deciled$car_buy)
dat_car_buy <- telecomChurn_Study_Cleaned_deciled%>%count(churn,levels=car_buy)%>%filter(churn==1)
dat_car_buy$N<-unclass(telecomChurn_Study_Cleaned_deciled%>%filter(car_buy%in%dat_car_buy$levels)%>%count(car_buy))[[2]]
dat_car_buy$ChurnPerc<-dat_car_buy$n/dat_car_buy$N
dat_car_buy$Var.Name<-rep("car_buy",nrow(dat_car_buy))

#variable :  [53] "csa" 
summary(telecomChurn_Study_Cleaned_deciled$csa)
dat_csa <- telecomChurn_Study_Cleaned_deciled%>%count(churn,levels=csa)%>%filter(churn==1)
dat_csa$N<-unclass(telecomChurn_Study_Cleaned_deciled%>%filter(csa%in%dat_csa$levels)%>%count(csa))[[2]]
dat_csa$ChurnPerc<-dat_csa$n/dat_csa$N
dat_csa$Var.Name<-rep("csa",nrow(dat_csa))

#variable :  [39] "retdays_Dummy" #Datatype Conversion
summary(telecomChurn_Study_Cleaned_deciled$retdays_Dummy)
telecomChurn_Study_Cleaned_deciled$retdays_Dummy <- as.factor(telecomChurn_Study_Cleaned_deciled$retdays_Dummy)
dat_retdays_Dummy <- telecomChurn_Study_Cleaned_deciled%>%count(churn,levels=retdays_Dummy)%>%filter(churn==1)
dat_retdays_Dummy$N<-unclass(telecomChurn_Study_Cleaned_deciled%>%filter(retdays_Dummy%in%dat_retdays_Dummy$levels)%>%count(retdays_Dummy))[[2]]
dat_retdays_Dummy$ChurnPerc<-dat_retdays_Dummy$n/dat_retdays_Dummy$N
dat_retdays_Dummy$Var.Name<-rep("retdays_Dummy",nrow(dat_retdays_Dummy))

#Add above created objects to create data Object for telecom Study
telecomChurn_Study_DataObject_Categorical <- rbind(dat_crclscod,dat_asl_flag,dat_prizm_social_one,dat_area,
                                                   dat_hnd_webcap,dat_marital,dat_ethnic,dat_car_buy,dat_csa,dat_retdays_Dummy)

#Export into .csv file all deciled variables
write.csv(telecomChurn_Study_DataObject_Categorical, "Categorical Variable Deciled Output.csv",row.names = F)

names(telecomChurn_Study_DataObject_Categorical)
summary(telecomChurn_Study_DataObject_Categorical) #100% 
#No more omit all are > 20% in churn rate(not taken Deciled; not all 10 set of group)

#vaiables for data Preparations step
telecomChurn_Study_Data_Preparation_Variables <- telecomChurn_Study_Cleaned_deciled

#Data Preparation
#Outlier Treatment for continuous, Categorical Variables

#Box Ploting will help better to identify
#list of variables
str(telecomChurn_Study_Data_Preparation_Variables)
names(telecomChurn_Study_Data_Preparation_Variables)

#Assign list of variables in different type
telecomChurn_Study_Data_Preparation_continuous <- telecomChurn_Study_Data_Preparation_Variables[-c(21:29, 39,40,41,47)]
telecomChurn_Study_Data_Preparation_Categorical <- telecomChurn_Study_Data_Preparation_Variables[-c(1:20,29:38,41:46,48,49)]

str(telecomChurn_Study_Data_Preparation_Categorical)
str(telecomChurn_Study_Data_Preparation_continuous)
summary(telecomChurn_Study_Data_Preparation_continuous)

#Factor variables 11
telecomChurn_Study_Data_Preparation_continuous <- names(telecomChurn_Study_Data_Preparation_continuous)
str(telecomChurn_Study_Cleaned_deciled)

#Outlier Identifying Plot (Top 10)
par(mfrow=c(2,5)) #figure margins too large issue for high matrix
for(i in 1:length(telecomChurn_Study_Data_Preparation_continuous))
{
  boxplot(telecomChurn_Study_Cleaned_deciled[,telecomChurn_Study_Data_Preparation_continuous[i]], main=telecomChurn_Study_Data_Preparation_continuous[i])
}

#Outlier Check
for(i in 1:length(telecomChurn_Study_Data_Preparation_continuous))
{
  plot(telecomChurn_Study_Cleaned_deciled[,telecomChurn_Study_Data_Preparation_continuous[i]], main=telecomChurn_Study_Data_Preparation_continuous[i])
}

#Apply Outlier Calculation
for(i in 1:length(telecomChurn_Study_Data_Preparation_continuous))
{
  Outlrplot <- boxplot(telecomChurn_Study_Cleaned_deciled[,telecomChurn_Study_Data_Preparation_continuous[i]], main=telecomChurn_Study_Data_Preparation_continuous[i])
  Outlr     <- Outlrplot$Outlr
  index     <- which(telecomChurn_Study_Cleaned_deciled[,telecomChurn_Study_Data_Preparation_continuous[i]]%in% Outlrplot$Outlr)
  telecomChurn_Study_Cleaned_deciled[index,telecomChurn_Study_Data_Preparation_continuous[i]]<-mean(telecomChurn_Study_Cleaned_deciled[,telecomChurn_Study_Data_Preparation_continuous[i]], na.rm = T)
  rm(Outlrplot)
  rm(Outlr)
}

#ReRun Outlier after calculation
for(i in 1:length(telecomChurn_Study_Data_Preparation_continuous))
{
  boxplot(telecomChurn_Study_Cleaned_deciled[,telecomChurn_Study_Data_Preparation_continuous[i]], main=telecomChurn_Study_Data_Preparation_continuous[i])
}

for(i in 1:length(telecomChurn_Study_Data_Preparation_continuous))
{
  plot(telecomChurn_Study_Cleaned_deciled[,telecomChurn_Study_Data_Preparation_continuous[i]], main=telecomChurn_Study_Data_Preparation_continuous[i])
}

dev.off() #Exit

names(telecomChurn_Study_Cleaned_deciled)
summary(telecomChurn_Study_Cleaned_deciled) #12 variable has Missing values

#Remove first 5 variables
index_Missing <- which(is.na(telecomChurn_Study_Cleaned_deciled[,c(1:5)]))
telecomChurn_Study_Cleaned_deciled <- telecomChurn_Study_Cleaned_deciled[-index_Missing,]
summary(telecomChurn_Study_Cleaned_deciled)

#Remove variables(change_mou, area, marital)
index_change_mou <- which(is.na(telecomChurn_Study_Cleaned_deciled$change_mou))
telecomChurn_Study_Cleaned_deciled <- telecomChurn_Study_Cleaned_deciled[-index_change_mou,]

index_area <- which(is.na(telecomChurn_Study_Cleaned_deciled$area))
telecomChurn_Study_Cleaned_deciled <- telecomChurn_Study_Cleaned_deciled[-index_area,]

index_marital <- which(is.na(telecomChurn_Study_Cleaned_deciled$marital))
telecomChurn_Study_Cleaned_deciled <- telecomChurn_Study_Cleaned_deciled[-index_marital,]
summary(telecomChurn_Study_Cleaned_deciled)

#Impute Mean for Missing values
telecomChurn_Study_Cleaned_deciled$avg6mou[is.na(telecomChurn_Study_Cleaned_deciled$avg6mou)]     <- mean(telecomChurn_Study_Cleaned_deciled$avg6mou,na.rm=T)
telecomChurn_Study_Cleaned_deciled$avg6qty[is.na(telecomChurn_Study_Cleaned_deciled$avg6qty)]     <- mean(telecomChurn_Study_Cleaned_deciled$avg6qty,na.rm=T)
telecomChurn_Study_Cleaned_deciled$hnd_price[is.na(telecomChurn_Study_Cleaned_deciled$hnd_price)] <- mean(telecomChurn_Study_Cleaned_deciled$hnd_price,na.rm=T)

summary(telecomChurn_Study_Cleaned_deciled)

#Pending variables : prizm_social_one, hnd_webcap (Create category "Missing")

telecomChurn_Study_Cleaned_deciled$prizm_social_one_Dummy <- ifelse(is.na(telecomChurn_Study_Cleaned_deciled$prizm_social_one), "Missing", as.factor(telecomChurn_Study_Cleaned_deciled$prizm_social_one))
telecomChurn_Study_Cleaned_deciled$prizm_social_one_Dummy <- as.factor(telecomChurn_Study_Cleaned_deciled$prizm_social_one_Dummy)
telecomChurn_Study_Cleaned_deciled$prizm_social_one_Dummy <- factor(telecomChurn_Study_Cleaned_deciled$prizm_social_one_Dummy, labels= c("C","R","S","T","U","Missing"))
summary(telecomChurn_Study_Cleaned_deciled$prizm_social_one)
summary(telecomChurn_Study_Cleaned_deciled$prizm_social_one_Dummy)

telecomChurn_Study_Cleaned_deciled$hnd_webcap_Dummy <- ifelse(is.na(telecomChurn_Study_Cleaned_deciled$hnd_webcap), "Missing", as.factor(telecomChurn_Study_Cleaned_deciled$hnd_webcap))
telecomChurn_Study_Cleaned_deciled$hnd_webcap_Dummy <- as.factor(telecomChurn_Study_Cleaned_deciled$hnd_webcap_Dummy)
telecomChurn_Study_Cleaned_deciled$hnd_webcap_Dummy <- factor(telecomChurn_Study_Cleaned_deciled$hnd_webcap_Dummy, labels= c("UNKW","WC","WCMB","Missing"))
summary(telecomChurn_Study_Cleaned_deciled$hnd_webcap)
summary(telecomChurn_Study_Cleaned_deciled$hnd_webcap_Dummy)

#Remove Duplicate with Dummy's
names(telecomChurn_Study_Cleaned_deciled)
telecomChurn_Study_Cleaned_deciled <- telecomChurn_Study_Cleaned_deciled[, -c(24,26)]
names(telecomChurn_Study_Cleaned_deciled)

#Cross Check Churn Rate with Original and Latest data set
table(telecomChurn_Study$churn) / nrow(telecomChurn_Study)
table(telecomChurn_Study_Cleaned_deciled$churn) / nrow(telecomChurn_Study_Cleaned_deciled)  

names(telecomChurn_Study_Cleaned_deciled)

#Create Few more Dummy Variable and convert some variable to Vector for further process
telecomChurn_Study_Cleaned_deciled$age1_Dummy <- ifelse(telecomChurn_Study_Cleaned_deciled$age1==0,"None",
                                                        ifelse(telecomChurn_Study_Cleaned_deciled$age1<=30,"Young",
                                                               ifelse(telecomChurn_Study_Cleaned_deciled$age1>30 & telecomChurn_Study_Cleaned_deciled$age1<=55,"Mid Age","Senior")))
telecomChurn_Study_Cleaned_deciled$age1_Dummy <- as.factor(telecomChurn_Study_Cleaned_deciled$age1_Dummy)

telecomChurn_Study_Cleaned_deciled$age2_Dummy <- ifelse(telecomChurn_Study_Cleaned_deciled$age2==0,"None",
                                                        ifelse(telecomChurn_Study_Cleaned_deciled$age2<=30,"Young",
                                                               ifelse(telecomChurn_Study_Cleaned_deciled$age2>30 & telecomChurn_Study_Cleaned_deciled$age2<=55,"Mid Age","Senior")))
telecomChurn_Study_Cleaned_deciled$age2_Dummy <- as.factor(telecomChurn_Study_Cleaned_deciled$age2_Dummy)

names(telecomChurn_Study_Cleaned_deciled)
#Remove Duplicate Variable with Dummy Creation
telecomChurn_Study_Cleaned_deciled <- telecomChurn_Study_Cleaned_deciled[, -c(28,29)]

#Vector Convertion in data type for further calculation
telecomChurn_Study_Cleaned_deciled$models    <- as.factor(telecomChurn_Study_Cleaned_deciled$models)
telecomChurn_Study_Cleaned_deciled$hnd_price <- as.factor(telecomChurn_Study_Cleaned_deciled$hnd_price)
telecomChurn_Study_Cleaned_deciled$actvsubs  <- as.factor(telecomChurn_Study_Cleaned_deciled$actvsubs)
telecomChurn_Study_Cleaned_deciled$uniqsubs  <- as.factor(telecomChurn_Study_Cleaned_deciled$uniqsubs)
telecomChurn_Study_Cleaned_deciled$forgntvl  <- as.factor(telecomChurn_Study_Cleaned_deciled$forgntvl)
telecomChurn_Study_Cleaned_deciled$mtrcycle  <- as.factor(telecomChurn_Study_Cleaned_deciled$mtrcycle)
telecomChurn_Study_Cleaned_deciled$truck     <- as.factor(telecomChurn_Study_Cleaned_deciled$truck)

str(telecomChurn_Study_Cleaned_deciled)

#Splitting dataset sample into Traning and Testing Samples
set.seed(200)
index <- sample(nrow(telecomChurn_Study_Cleaned_deciled), 0.70*nrow(telecomChurn_Study_Cleaned_deciled),replace=F)
training <- telecomChurn_Study_Cleaned_deciled[index,]
testing  <- telecomChurn_Study_Cleaned_deciled[-index,]

#Quick Check with Churn Rate
table(training$churn) / nrow(training)
table(testing$churn) / nrow(testing)

names(training)
summary(training)

#Some More Dummy Variable preparation Here to put in section for significant level
View(telecomChurn_Study_Cleaned_deciled)

summary(telecomChurn_Study_Cleaned_deciled$asl_flag)
training$asl_flag_Yes <- ifelse(training$asl_flag == "Y",1,0)
testing$asl_flag_Yes  <- ifelse(testing$asl_flag == "Y",1,0)

summary(training$area)

training$CALIFORNIA_NORTH_AREA <- ifelse(training$area == "CALIFORNIA NORTH AREA",1,0)
testing$CALIFORNIA_NORTH_AREA  <- ifelse(testing$area == "CALIFORNIA NORTH AREA",1,0)

training$CENTRAL_R_SOUTH_TEXAS_AREA <- ifelse(training$area == "CENTRAL/SOUTH TEXAS AREA ",1,0)
testing$CENTRAL_R_SOUTH_TEXAS_AREA  <- ifelse(testing$area == "CENTRAL/SOUTH TEXAS AREA ",1,0)

training$NORTH_FLORIDA_AREA <- ifelse(training$area == "NORTH FLORIDA AREA",1,0)
testing$NORTH_FLORIDA_AREA  <- ifelse(testing$area == "NORTH FLORIDA AREA",1,0)     

training$NORTHWEST_R_ROCKY_MOUNTAIN_AREA <- ifelse(training$area == "NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
testing$NORTHWEST_R_ROCKY_MOUNTAIN_AREA  <- ifelse(testing$area == "NORTHWEST/ROCKY MOUNTAIN AREA",1,0) 

training$SOUTH_FLORIDA_AREA <- ifelse(training$area == "SOUTH FLORIDA AREA",1,0)
testing$SOUTH_FLORIDA_AREA  <- ifelse(testing$area == "SOUTH FLORIDA AREA",1,0) 

training$SOUTHWEST_AREA <- ifelse(training$area == "SOUTHWEST AREA",1,0)
testing$SOUTHWEST_AREA  <- ifelse(testing$area == "SOUTHWEST AREA",1,0) 

training$TENNESSEE_AREA  <- ifelse(training$area == "TENNESSEE AREA",1,0)
testing$TENNESSEE_AREA   <- ifelse(testing$area == "TENNESSEE AREA",1,0) 

summary(training$ethnic)

training$ethnic_C  <- ifelse(training$ethnic == "C",1,0)
testing$ethnic_C   <- ifelse(testing$ethnic == "C",1,0) 

training$ethnic_H  <- ifelse(training$ethnic == "H",1,0)
testing$ethnic_H  <- ifelse(testing$ethnic == "H",1,0) 

training$ethnic_N  <- ifelse(training$ethnic == "N",1,0)
testing$ethnic_N  <- ifelse(testing$ethnic == "N",1,0) 

training$ethnic_O  <- ifelse(training$ethnic == "O",1,0)
testing$ethnic_O  <- ifelse(testing$ethnic == "O",1,0) 

training$ethnic_S  <- ifelse(training$ethnic == "S",1,0)
testing$ethnic_S  <- ifelse(testing$ethnic == "S",1,0) 

training$ethnic_U  <- ifelse(training$ethnic == "U",1,0)
testing$ethnic_U  <- ifelse(testing$ethnic == "U",1,0) 

training$ethnic_Z  <- ifelse(training$ethnic == "Z",1,0)
testing$ethnic_Z  <- ifelse(testing$ethnic == "Z",1,0) 

summary(training$hnd_price)

training$hnd_price_29.98  <- ifelse(training$hnd_price == "29.98999023",1,0)
testing$hnd_price_29.98   <- ifelse(testing$hnd_price == "29.98999023",1,0) 

training$hnd_price_59.98  <- ifelse(training$hnd_price == "59.98999023",1,0)
testing$hnd_price_59.98   <- ifelse(testing$hnd_price == "59.98999023",1,0) 

training$hnd_price_79.98  <- ifelse(training$hnd_price == "79.98999023",1,0)
testing$hnd_price_79.98   <- ifelse(testing$hnd_price == "79.98999023",1,0) 

training$hnd_price_105.08  <- ifelse(training$hnd_price == "105.083038078331",1,0)
testing$hnd_price_105.08   <- ifelse(testing$hnd_price == "105.083038078331",1,0)

training$hnd_price_129.98  <- ifelse(training$hnd_price == "129.9899902",1,0)
testing$hnd_price_129.98   <- ifelse(testing$hnd_price == "129.9899902",1,0)

training$hnd_price_149.98  <- ifelse(training$hnd_price == "149.9899902",1,0)
testing$hnd_price_149.98   <- ifelse(testing$hnd_price == "149.9899902",1,0)

training$hnd_price_199.98  <- ifelse(training$hnd_price == "199.9899902",1,0)
testing$hnd_price_199.98   <- ifelse(testing$hnd_price == "199.9899902",1,0)

training$hnd_price_249.98  <- ifelse(training$hnd_price == "249.9899902",1,0)
testing$hnd_price_249.98   <- ifelse(testing$hnd_price == "249.9899902",1,0)

training$hnd_price_299.98  <- ifelse(training$hnd_price == "299.9899902",1,0)
testing$hnd_price_299.98   <- ifelse(testing$hnd_price == "299.9899902",1,0)

summary(training$uniqsubs)

training$uniqsubs_1  <- ifelse(training$uniqsubs == "1",1,0)
testing$uniqsubs_1   <- ifelse(testing$uniqsubs == "1",1,0)

training$uniqsubs_2  <- ifelse(training$uniqsubs == "2",1,0)
testing$uniqsubs_2   <- ifelse(testing$uniqsubs == "2",1,0)

training$uniqsubs_3  <- ifelse(training$uniqsubs == "3",1,0)
testing$uniqsubs_3   <- ifelse(testing$uniqsubs == "3",1,0)

training$uniqsubs_4  <- ifelse(training$uniqsubs == "4",1,0)
testing$uniqsubs_4   <- ifelse(testing$uniqsubs == "4",1,0)

training$uniqsubs_5  <- ifelse(training$uniqsubs == "5",1,0)
testing$uniqsubs_5   <- ifelse(testing$uniqsubs == "5",1,0)

training$uniqsubs_6  <- ifelse(training$uniqsubs == "6",1,0)
testing$uniqsubs_6   <- ifelse(testing$uniqsubs == "6",1,0)

training$uniqsubs_7  <- ifelse(training$uniqsubs == "7",1,0)
testing$uniqsubs_7   <- ifelse(testing$uniqsubs == "7",1,0)

training$uniqsubs_8  <- ifelse(training$uniqsubs == "8",1,0)
testing$uniqsubs_8   <- ifelse(testing$uniqsubs == "8",1,0)

training$uniqsubs_9  <- ifelse(training$uniqsubs == "9",1,0)
testing$uniqsubs_9   <- ifelse(testing$uniqsubs == "9",1,0)

summary(training$prizm_social_one_Dummy)

training$prizm_social_one_Dummy_C  <- ifelse(training$prizm_social_one_Dummy == "C",1,0)
testing$prizm_social_one_Dummy_C   <- ifelse(testing$prizm_social_one_Dummy == "C",1,0)

training$prizm_social_one_Dummy_R  <- ifelse(training$prizm_social_one_Dummy == "R",1,0)
testing$prizm_social_one_Dummy_R   <- ifelse(testing$prizm_social_one_Dummy == "R",1,0)

training$prizm_social_one_Dummy_T  <- ifelse(training$prizm_social_one_Dummy == "T",1,0)
testing$prizm_social_one_Dummy_T   <- ifelse(testing$prizm_social_one_Dummy == "T",1,0)

summary(training$age1_Dummy)

training$age1_Dummy_Mid_Age  <- ifelse(training$age1_Dummy == "Mid Age",1,0)
testing$age1_Dummy_Mid_Age   <- ifelse(testing$age1_Dummy == "Mid Age",1,0)

training$age1_Dummy_Young  <- ifelse(training$age1_Dummy == "Young",1,0)
testing$age1_Dummy_Young   <- ifelse(testing$age1_Dummy == "Young",1,0)

training$age1_Dummy_Senior  <- ifelse(training$age1_Dummy == "Senior",1,0)
testing$age1_Dummy_Senior   <- ifelse(testing$age1_Dummy == "Senior",1,0)

summary(training$age2_Dummy)

training$age2_Dummy_Mid_Age  <- ifelse(training$age2_Dummy == "Mid Age",1,0)
testing$age2_Dummy_Mid_Age   <- ifelse(testing$age2_Dummy == "Mid Age",1,0)

training$age2_Dummy_Young  <- ifelse(training$age2_Dummy == "Young",1,0)
testing$age2_Dummy_Young   <- ifelse(testing$age2_Dummy == "Young",1,0)

training$age2_Dummy_Senior  <- ifelse(training$age2_Dummy == "Senior",1,0)
testing$age2_Dummy_Senior   <- ifelse(testing$age2_Dummy == "Senior",1,0)

names(training)

#Build Logistic Regression Model
logRegression_ModelStage1 <- glm(churn~ totmrc_Mean + rev_Range + mou_Mean + change_mou + drop_blk_Mean + drop_vce_Mean
                                 + mou_opkv_Range + months + eqpdays + owylis_vce_Range + avgqty + avg6mou 
                                 +ovrrev_Mean + refurb_new_R + asl_flag_Yes + CALIFORNIA_NORTH_AREA + NORTH_FLORIDA_AREA 
                                 + NORTHWEST_R_ROCKY_MOUNTAIN_AREA + SOUTH_FLORIDA_AREA + SOUTHWEST_AREA + TENNESSEE_AREA 
                                 + ethnic_C + ethnic_N + ethnic_O + ethnic_S + ethnic_U + ethnic_Z 
                                 + hnd_price_79.98 + hnd_price_105.08 + hnd_price_129.98 + hnd_price_149.98 + hnd_price_199.98 
                                 + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_7 + adjmou 
                                 + totrev + retdays_Dummy + prizm_social_one_Dummy_T + prizm_social_one_Dummy_R 
                                 + age1_Dummy_Mid_Age + age1_Dummy_Senior + age2_Dummy_Senior, family="binomial", data=training)

summary(logRegression_ModelStage1)

#All the variables looks Significant
#Model Diagnostics
#Multicollinearity Check
vif(logRegression_ModelStage1)

#If Error Exist with Aliased Coefficients
#Check Linear Dependent Variables
#fit <- lm(logRegression_ModelStage1)
#ld.vars <- attributes(alias(fit)$Complete)$dimnames[[1]]

#Remove Linear Dependent Variables
#logRegression_ModelStage1.new <- as.formula(paste(paste(deparse(logRegression_ModelStage1), 
#                                 collapse = ""),paste(ld.vars,collapse = "-"),sep="-"))

#fit.new <- lm(logRegression_ModelStage1.new)
#vif(fit.new)

#avgqty, complete_Mean_Dummy, adjmou, totrev this 4 variable looks > 5 VIF value
#Remove and recheck

#RERun Model
logRegression_ModelStage2 <- glm(churn~ totmrc_Mean + rev_Range + mou_Mean + change_mou + drop_blk_Mean + drop_vce_Mean
                                 + mou_opkv_Range + months + eqpdays + owylis_vce_Range + avg6mou 
                                 +ovrrev_Mean + refurb_new_R + asl_flag_Yes + CALIFORNIA_NORTH_AREA + NORTH_FLORIDA_AREA 
                                 + NORTHWEST_R_ROCKY_MOUNTAIN_AREA + SOUTH_FLORIDA_AREA + SOUTHWEST_AREA + TENNESSEE_AREA 
                                 + ethnic_C + ethnic_N + ethnic_O + ethnic_S + ethnic_U + ethnic_Z 
                                 + hnd_price_79.98 + hnd_price_105.08 + hnd_price_129.98 + hnd_price_149.98 + hnd_price_199.98 
                                 + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_7 
                                 + retdays_Dummy + prizm_social_one_Dummy_T + prizm_social_one_Dummy_R 
                                 + age1_Dummy_Mid_Age + age1_Dummy_Senior + age2_Dummy_Senior, family="binomial", data=training)

summary(logRegression_ModelStage2)
vif(logRegression_ModelStage2)

## Conclude : Al are < 5 in Return VIF value, so No Multicollinearity and We can Take this a Final Model


##Confidence interval
confint(logRegression_ModelStage2)


##Model testing
##Predict Probability of customer Churning

predict_CustomerChurn <- predict(logRegression_ModelStage2, type="response", newdata=testing)
head(predict_CustomerChurn)

#Cut off probability as per Churn Rate
table(telecomChurn_Study_Cleaned_deciled$churn) / nrow(telecomChurn_Study_Cleaned_deciled)

#Choose Cut off By Kappa
sequenceValue <- seq(0.25,1,0.01)
n<-1
accuracy <- as.vector(length(sequenceValue))

for(i in sequenceValue)
{
  print(i)
  testing$predictedvaluesCutoff <- ifelse(predict_CustomerChurn > i,1,0)
  accuracy[n] <- confusionMatrix(testing$predictedvaluesCutoff,testing$churn,positive = "1")$overall[2]
  print(n)
  n=n+1
}


#accuracy has differnt kappa matrix for differnt cut-off
#Extract cut-off with maximum kappa using 'which function"

index <- which(accuracy==max(accuracy))

#cutoff for maximum kappa
accuracy[index]   

#[1] 0.1457745 max Cut off value for accuracy 

predictionChurn <- ifelse(predict_CustomerChurn >= 0.1457745,1,0)
table(predictionChurn)

#This custof gives best value of 89% Result in >0.14 value


#Check Prediction Quality

kappa2(data.frame(testing$churn, predictionChurn))

#Confusion Matrix

confusionMatrix(predictionChurn, testing$churn, positive = "1")
table(testing$churn)

#Prediction     
#   0                               1
#0  1891  (IN Correct Events)       264  (In-Correct Non-Events) 
#1  12923 (Correct Non-Events)      4338 (Correct Events)

#Final Model is ok in Prediction

#ROCR Curve
predictionRocr <- prediction(predictionChurn, testing$churn)
performanceRocr <- performance(predictionRocr, "tpr", "fpr")
plot(performanceRocr, col="red")
abline(0,1,lty=8,col="grey")
rocrAccuracy <- performance(predictionRocr,"auc")
rocrAccuracy <- unlist(slot(rocrAccuracy,"y.values"))
rocrAccuracy  #0.54

#The Auc is 0.54 and which is More than 0.50, Also The curve is above the gray Line. 
#Model Looks like Fine and Acceptable


#Gains Chart
gains(testing$churn, predict(logRegression_ModelStage2, type="response", newdata=testing), groups = 10)

#Chart table shows that top 40% probability contains more than 50% (53.5) Likely to Churn.


testing$predictedvaluesCutoff <- predict(logRegression_ModelStage2, type="response", newdata = testing)
quantile(testing$predictedvaluesCutoff, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#Top 50% of the probability is between 0.2329689 - 0.2535021 #By using this data we can predict who highly likely with Churn.


#####Top Line Questions of Interest to Senior Management:##### 
##C:\Jig12097\Topic 13 -  Final Case Study Course Wrap up\Solution\Capstone_Telecom_Churn-Case_Study.R

#1. What are the top five factors driving likelihood of churn at Telecom Company?

head(sort(abs(logRegression_ModelStage1$coefficients),decreasing = T),500)

#<Top 5 Factors>    <Beta Coefficient>

#hnd_price           1.10816208927                       
#ethnic              0.97310479651                        
#retdays             0.69855937217
#uniqsubs            0.69446847701
#asl_flag            0.42208556809

#Looks 7 Unique customer type in Ethinic level C of factor retdays who use price level > 1.108 beta coefficient 
#irrespective of minimum consideration of Flag driving increase in churn at Telecom Company

#above Amoung ##Confidence interval
# totmrc_Mean                     -0.00277039982 -0.00035084997
# rev_Range                        0.00042533497  0.00122810249
# mou_Mean                        -0.00090308244 -0.00058947712
# change_mou                      -0.00018387325  0.00001125971
# drop_blk_Mean                   -0.00007329722  0.00432651442
# drop_vce_Mean                    0.00397237321  0.01158579698
# mou_opkv_Range                  -0.00005228460  0.00027028588
# months                          -0.01505118663 -0.00936696659
# eqpdays                          0.00081352621  0.00104929531
# owylis_vce_Range                 0.00006644863  0.00239518480
# avg6mou                          0.00029122576  0.00059108438
# ovrrev_Mean                      0.00243359911  0.00460822183
# refurb_new_R                     0.11691518635  0.25854835871
# asl_flag_Yes                    -0.49014890015 -0.34008932624
# CALIFORNIA_NORTH_AREA            0.03227915329  0.21834379124
# NORTH_FLORIDA_AREA               0.03533033337  0.25421401476
# NORTHWEST_R_ROCKY_MOUNTAIN_AREA  0.18609724774  0.40013393539
# SOUTH_FLORIDA_AREA               0.20486991660  0.44534681579
# SOUTHWEST_AREA                   0.02802905070  0.21307257604
# TENNESSEE_AREA                  -0.28855795418 -0.00037951850
# ethnic_C                        -1.62638729665 -0.41463638709
# ethnic_N                        -0.16011992642 -0.05113316320
# ethnic_O                         0.18296134088  0.40456612814
# ethnic_S                        -0.16555340351 -0.02127246984
# ethnic_U                        -0.17593905425 -0.02285978827
# ethnic_Z                        -0.47349191040 -0.23544472063
# hnd_price_79.98                 -0.20878751541 -0.05229959014
# hnd_price_105.08                -0.62693879846 -0.08309927105
# hnd_price_129.98                -0.27714933375 -0.12331085183
# hnd_price_149.98                -0.19107436468 -0.06034783995
# hnd_price_199.98                -0.43175076000 -0.25468519014
# hnd_price_249.98                -1.82152620148 -0.50033534813
# uniqsubs_2                       0.10496462873  0.20677043939
# uniqsubs_3                       0.06049701161  0.24018795788
# uniqsubs_4                       0.07338771637  0.35583431576
# uniqsubs_7                       0.01456121235  1.31541003834
# retdays_Dummy1                   0.59105497570  0.81691470035
# prizm_social_one_Dummy_T         0.05575741583  0.17991654419
# prizm_social_one_Dummy_R         0.00050864619  0.20752440295
# age1_Dummy_Mid_Age              -0.29334801664 -0.19695112743
# age1_Dummy_Senior               -0.35210603291 -0.17522125109
# age2_Dummy_Senior               -0.24935734672 -0.05594219332




#2. Validation of survey findings. a) Whether "cost and billing" and "network and service quality" are
#important factors influencing churn behaviour. b) Are data usage connectivity issues turning out to be
#costly? In other words, is it leading to churn? 

#"cost and billing" Factors
#totmrc_Mean            0.00143547738 
#totrev                 0.00005987964
#rev_Range              0.00093609033 
#ovrrev_Mean            0.00429720917 

#"network and service quality" factors
#mou_Mean               0.00093609033                   
#change_mou             0.00012201096
#drop_blk_Mean          0.00429720917                   
#owylis_vce_Range       0.00307453255
#drop_vce_Mean          0.08327860740  
#mou_opkv_Range         0.00014755872
#retdays_Dummy1         0.70437897085 
#A) Yes, the beta coefficient of variables is showing very important factor influencing churn increase/decrease.
#that is considering with customers join date from analyse day how he/she makes a call to retent. Customer chance of churn is high.
#the factor value helped customer need to be focused to give offers to retain.

#data usage factor
#complete_Mean_Dummy   0.00143547738
#and dat columns  like plcd_dat_Mean,datovr_Mean,drop_dat_Mean 

quantile(telecomChurn_Study_Cleaned$plcd_dat_Mean, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
quantile(telecomChurn_Study_Cleaned$plcd_Attempt_Mean_Dummy, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#B) No, The beta coefficients of variables is showing a very important factor for churn behaviour influence. 
#If we notice above mentioned are beta values and a unit increase is them is not impacting any on churn.
#It seems "cost and billing" Factors are not very important factor here to influence churn behaviour at Telecom Company  

#USeage for data is not affecting and looks anaysys have less data; 
#very less data useage predicted and Report also says data calls and internet useage is closed to 10 to 14% only
#Not Leading very much with abve data % interms to Churn

#3. Would you recommend rate plan migration as a proactive retention strategy?

#Yes for few not all
#Reason : Variable Ovrrev Mean is with 0.004 coEff; Its a Mean Overage Revenue in business
# Other word : sum of data and voice overrage Revenue, after Bill 
# CoEff not strong to show churn activitys, So we can consider for few set of customers who have some
# case basis validation success. But rate plan migration as a proactive retention strategy will not work for all.

#4. What would be your recommendation on how to use this churn model for prioritisation of customers for a
#proactive retention campaigns in the future?

gains(testing$churn, predict(logRegression_ModelStage2, type="response", newdata = testing), groups=10)

#Top 20% probability shows 29.4%, closely 30% are more likely to churn

#High Churn Rate prediction
testing$highChurnProbability <- predict(logRegression_ModelStage2,type="response", newdata = testing)
quantile(testing$highChurnProbability, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#10%       20%       30%       40%       50%       60%       70%       80%       90%      100% 
#0.1425492 0.1701428 0.1935445 0.2139201 0.2329689 0.2535021 0.2754767 0.3010562 0.3386616 0.7403609

#Above % shows 20% of the probability contains <from 80 to 100(20)> - 0.3010562 0.7403609 
#This notifies us it may highly likely Churn

##Predict a Customer Who will Churn
## Apply Cut-off

customerPrediction <- predict(logRegression_ModelStage2, type="response", newdata=testing)
customerPrediction <- ifelse(customerPrediction >= 0.3010562 , 1, 0)
table(customerPrediction, testing$churn)

#take upto <from 80 to 100(20)> as per quantile result 
#and consider churn value(0 or 1 : here 1) then to predict customer use unique vaiable to identify via Customer_ID
expectedPrediction <- testing[testing$highChurnProbability > 0.3010562 & 
                                testing$highChurnProbability <= 0.7403609 & 
                                testing$churn == "1","Customer_ID"]
expectedPrediction <- as.data.frame(expectedPrediction)
nrow(expectedPrediction) #1351 Customers

#Predicted Customers who likely to churn 
write.csv(expectedPrediction, "Predicted Customers who likely to churn.csv", row.names = T)

### Model logRegression_ModelStage2 is used to predict list of Telecom Company customers who highly likely churn.
# Unique list can be prepared using Customer_ID

# 5. What would be the target segments for proactive retention campaigns? Falling ARPU forecast is also a
# concern and therefore, Telecom Company would like to save their high revenue customers besides managing
# churn. Given a budget constraint of a contact list of 20% of the subscriber pool, which subscribers should
# prioritized if "revenue saves" is also a priority besides controlling churn. In other words, controlling churn
# is the primary objective and revenue saves is the secondary objective.


## Get the Probability Score and Revenue value rate

saveRevenueCustomers <- predict(logRegression_ModelStage2, type="response",newdata=testing)
testing$highRevenueCustomers <- predict(logRegression_ModelStage2,type="response",newdata=testing)
quantile(testing$highRevenueCustomers, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

savehighRevenueCustomers <- ifelse(saveRevenueCustomers < 0.20, "Low Probability", 
                                   ifelse(saveRevenueCustomers >= 0.20 & saveRevenueCustomers < 0.30, "Medium Probability", "High Probability"))

table(savehighRevenueCustomers, testing$churn)

str(testing$totrev)
quantile(testing$totrev, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
revenueValueRate <-  ifelse(testing$totrev < 670.660, "Low Revenue", 
                            ifelse(testing$totrev >= 670.660 & testing$totrev < 1133.930, "Medium Revenue", "High Revenue"))

table(savehighRevenueCustomers,revenueValueRate)

## This Result can help to select the level of customer need to be targeted 

## Extract List

summary(savehighRevenueCustomers)
summary(revenueValueRate)

#put it in testing Data Set
testing$ProbabilityRange <- ifelse(saveRevenueCustomers < 0.20, "Low Probability", 
                                   ifelse(saveRevenueCustomers >= 0.20 & saveRevenueCustomers < 0.30, "Medium Probability", "High Probability"))
testing$RevenueRange <-  ifelse(testing$totrev < 670.660, "Low Revenue", 
                                ifelse(testing$totrev >= 670.660 & testing$totrev < 1133.930, "Medium Revenue", "High Revenue"))


TargetedCustomersforHighRevenue <- testing[testing$ProbabilityRange == "High Probability" & testing$RevenueRange == "High Revenue","Customer_ID"]
TargetedCustomersforHighRevenue<- as.data.frame(TargetedCustomersforHighRevenue)
nrow(TargetedCustomersforHighRevenue) #1385 Customers

#Predicted Customers who likely to be targetted with offers and etc etc to retain for High Revenue 
write.csv(TargetedCustomersforHighRevenue, "Predicted Customers who likely to retain for High Revenue.csv", row.names = T)

