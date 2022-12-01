#The client is a large financial service organization, which is into disbursing loans to its customers.

#The dataset consists of customer information of ABPI financial services. It is a finance dataset, 
#which consists of customer's demographics, loan disbursed, asset cost being purchased, and the customer's previous 
#account and loan history. The dataset also consists of the state and branch id of ABPI from where the loan was disbursed, 
#the customer's account history. It also contains the CNS score and score description provided by the Credit Bureaus.

#Objective
#It is a challenge for any financial services to target the right people for disbursing the loan.
#You need to assess what data is available and perform some exploratory and descriptive analytics to identify interesting and 
#useful patterns, trends, and insights. Also, build appropriate predictive model/s on the data.

#Participants should be able to :
  
#Clean the data
#perform EDA and Vizualizations
#perform feature engineering
#build a model (logistic regression)
#evaluate the model
#provide business insights from the model

#The evaluation is based on AUC score

#File descriptions
#loan_details_train.csv - the training set
#loan_details_test.csv - the test set
#loan_details_sample.csv - a sample submission file in the correct format; but the predicted valued are randomly set to 0.5 probability each...i.e, incorrect.. :)

#Data fields
#ID - loan id
#disbursed_amount - Amount of Loan disbursed
#asset_cost - Cost of the Asset
#ltv - Loan to Value of the asset
#branch_id - Branch where the loan was disbursed
#Date.of.Birth- Date of birth of the customer
#Employment.Type- Employment Type of the customer (Salaried/Self Employed)
#DisbursalDate- Date of disbursement
#region- region of disbursement
#Employee_code_ID- Employee of the organization who logged the disbursement
#MobileNo_Avl_Flag - if Mobile no. was shared by the customer, then flagged as 1
#Aadhar_flag - if aadhar was shared by the customer then flagged as 1
#PAN_flag - if pan was shared by the customer then flagged as 1
#VoterID_flag - if voter was shared by the customer then flagged as 1
#Driving_flag - if DL was shared by the customer then flagged as 1
#Passport_flag- if passport was shared by the customer then flagged as 1
#PERFORM_CNS.SCORE - Bureau Score
#DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS - Number of delinquent accounts in the last six months
#CREDIT.HISTORY.LENGTH- Credit history in terms of years
#NO.OF_INQUIRIES- Number of inquiries made by the customer
#default - Payment default (flag) in the first EMI on due date

##Loading First Required Packages/library

library(gains)
library(dplyr)
library(ggplot2)
library(irr)
library(caret)

##Loading Data

setwd("C:\\Users\\USER\\Desktop\\IPBA WORKING\\Kagle Competitions\\city-bank-loan-default-prediction")
train<-read.csv('loan_details_train.csv',header=T,na.strings=c(""))

##Checking Data Characteristics
str(train)



#Checking Missing or NA value of Target Variable

colSums(is.na(train))

summary(train$default)

#Sanity Check and cleaning

table(train$default)#Lets make in two factor "Yes-Defaulter" and "No Defaulter"
table(train$disbursed_amount)#Integer is ok
table(train$asset_cost)#Integer is ok
table(train$ltv)#Numeric is ok
table(train$branch_id)#integer is ok
table(train$region)#Make it factor 4level
table(train$Date.of.Birth)???
table(train$Employment.Type)#Make it two level factor
table(train$MobileNo_Avl_Flag)#Make it two level factor
table(train$Aadhar_flag)#Make it two level factor
table(train$PAN_flag)#Make it two level factor
table(train$VoterID_flag)#Make it two level factor
table(train$Driving_flag)#Make it two level factor
table(train$Passport_flag)#Make it two level factor
table(train$PERFORM_CNS.SCORE)#let it be integer
table(train$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS)#let it be integer
table(train$NO.OF_INQUIRIES)
table(train$CREDIT.HISTORY.LENGTH)#convert into number/factor


#Lets implement the varibale convertion

train[train$default == 0,]$default <- "Yes_defaulter"
train[train$default == 1,]$default <- "No_defaulter"
train$default <- as.factor(train$default)

train$region <- as.factor(train$region)
train$Employment.Type <- as.factor(train$Employment.Type)

train$MobileNo_Avl_Flag <- as.factor(train$MobileNo_Avl_Flag)

train[train$Aadhar_flag == 0,]$Aadhar_flag <- "No_available"
train[train$Aadhar_flag == 1,]$Aadhar_flag <- "Yes_available"
train$Aadhar_flag <- as.factor(train$Aadhar_flag)

train[train$PAN_flag == 0,]$PAN_flag <- "No_available"
train[train$PAN_flag == 1,]$PAN_flag <- "Yes_available"
train$PAN_flag <- as.factor(train$PAN_flag)

train[train$VoterID_flag == 0,]$VoterID_flag <- "No_available"
train[train$VoterID_flag == 1,]$VoterID_flag <- "Yes_available"
train$VoterID_flag <- as.factor(train$VoterID_flag)

train[train$Driving_flag == 0,]$train$Driving_flag <- "No_available"
train[train$Driving_flag == 1,]$train$Driving_flag <- "Yes_available"
train$Driving_flag <- as.factor(train$Driving_flag)

train[train$Passport_flag == 0,]$train$Passport_flag <- "No_available"
train[train$Passport_flag == 1,]$train$Passport_flag <- "Yes_available"
train$Passport_flag <- as.factor(train$Passport_flag)

#Removed yrs & mon string from variables
train$CREDIT.HISTORY.LENGTH1 <- gsub("yrs","", train$CREDIT.HISTORY.LENGTH)
train$CREDIT.HISTORY.LENGTH1 <- gsub("mon","", train$CREDIT.HISTORY.LENGTH1)



train$CREDIT.HISTORY.LENGTH1<-as.factor(train$CREDIT.HISTORY.LENGTH1)

transform(train, CREDIT.HISTORY.LENGTH1 = as.numeric(CREDIT.HISTORY.LENGTH1))


class(train$CREDIT.HISTORY.LENGTH1)

train<-train[,-18]

str(train)


#Build the first model using all the variables 

model_1<-glm(default~CREDIT.HISTORY.LENGTH1,disbursed_amount,data=train,family="binomial")
summary(model_1)





#Need to check if any missing value
train$disbursed_amount<-is.na(train$disbursed_amount)
nrow(train$disbursed_amount)

train$asset_cost<-is.na(train$asset_cost)
nrow(train$asset_cost)
#Need to chechk if any missing value or zero vlaue


train$ltv<-is.na(train$ltv)
nrow(train$ltv)
#Need to chechk if any missing value or zero vlaue


train$branch_id<-is.na(train$branch_id)
nrow(train$branch_id)
#Need to chechk if any missing value or zero vlaue



train$region<-is.na(train$region)
nrow(train$region)


train$Employment.Type<-is.na(train$Employment.Type)
summary(train$Employment.Type)
nrow(train$Employment.Type)


train$MobileNo_Avl_Flag<-is.na(train$MobileNo_Avl_Flag)
nrow(train$MobileNo_Avl_Flag)
summary(train$MobileNo_Avl_Flag)

train$Aadhar_flag<-is.na(train$Aadhar_flag)
nrow(train$Aadhar_flag)

train$PAN_flag<-is.na(train$PAN_flag)
nrow(train$PAN_flag)

train$VoterID_flag<-is.na(train$VoterID_flag)
nrow(train$VoterID_flag)

train$Driving_flag<-is.na(train$Driving_flag)
nrow(train$Driving_flag)

train$Passport_flag<-is.na(train$Passport_flag)
nrow(train$Passport_flag)

train$PERFORM_CNS.SCORE<-is.na(train$PERFORM_CNS.SCORE)
nrow(train$PERFORM_CNS.SCORE)

#Need to check if any missing value or zero value

train$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS<-is.na(train$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS)
nrow(train$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS)


train$CREDIT.HISTORY.LENGTH<-is.na(train$CREDIT.HISTORY.LENGTH)
nrow(train$CREDIT.HISTORY.LENGTH)
#Need to chechk if any missing value or zero vlaue

table(train$NO.OF_INQUIRIES)
train$NO.OF_INQUIRIES<-is.na(train$NO.OF_INQUIRIES)
nrow(train$NO.OF_INQUIRIES)

table(train$default)
train$default<-is.na(train$default)
nrow(train$default)






train$Employment.Type<-ifelse(is.na(train$Employment.Type),"Missing",as.character(train$Employment.Type))

train$Employment.Type<-as.factor(train$Employment.Type)
summary(train$Employment.Type) #566 missing entries notified need to do missning value treatment

summary(train$NO.OF_INQUIRIES)
table(train$NO.OF_INQUIRIES)

summary(train$PERFORM_CNS.SCORE)
table(train$PERFORM_CNS.SCORE)

summary(train$ltv)
table(train$ltv)

summary(train$disbursed_amount)
table(train$disbursed_amount)

summary(train$asset_cost)

colSums(is.na(train))


#Treatment on Date & Character variables

train$Date.of.Birth<-as.Date(train$Date.of.Birth,"%d-%b-%y")
summary(train$Date.of.Birth)
train$DisbursalDate<-as.Date(train$DisbursalDate,"%d-%b-%y")
summary(train$DisbursalDate)

#branch_id
#region
#Date.of.Birth
#DisbursalDate
#CREDIT.HISTORY.LENGTH


str(train)

#Build the first model using all the variables 

model_1<-glm(default~.,data=train[,-1],family="binomial")
summary(model_1)


model_1<-glm(default~.,data=train,family="binomial")
summary(train)

mod<-glm(Target~.,data=dm[,-9],family="binomial")
summary(mod)


#---------------------------------------------------------------#Work Book

train%>%mutate(Group_docs=ifelse(Aadhar_flag,PAN_flag<0),1,0)->train

library(lubridate)

train$Date.of.Birth<-as.factor(train$Date.of.Birth)


train$DisbursalDate<-as.factor(train$DisbursalDate)

train$Date.of.Birth<-as.Date(train$Date.of.Birth,"%d-%b-%y")
head(months(train$Date.of.Birth))
unique(months(train$Date.of.Birth))

train$DisbursalDate<-as.Date(train$DisbursalDate,"%d-%b-%y")
train<-mutate(train,Age=DisbursalDate -  Date.of.Birth)

remove(train)

dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm


#train$Date.of.Birth(is.na(train$Date.of.Birth))
#train$Date.of.Birth[is.na(train$Date.of.Birth)] <- mean(train$Date.of.Birth,na.rm=T)

#install.packages("Amelia") #this package for finding Missing vlaue in map
#library(Amelia)
#missmap(train,main = "Missing values vs observed")

#library(Amelia)
#library(mlbench)
#missmap(train, col=c("blue", "red"), legend=FALSE)

head(train,25)

tail(train,25)

summary(train)

dim(train)

#Sanity Check and cleaning

train$Employment.Type<-ifelse(is.na(train$Employment.Type),"Missing",as.character(train$Employment.Type))

train$Employment.Type<-as.factor(train$Employment.Type)
summary(train$Employment.Type) #566 missing entries notified need to do missning value treatment

train$Employment.Type<-ifelse(is.na(train$Employment.Type),"Missing",as.character(train$Employment.Type))

train$Employment.Type<-as.factor(train$Employment.Type)
contrasts(train$Employment.Type)
summary(train$Employment.Type)


train$CREDIT.HISTORY.LENGTH<-as.factor(train$CREDIT.HISTORY.LENGTH)
contrasts(train$CREDIT.HISTORY.LENGTH)
summary(train$CREDIT.HISTORY.LENGTH) 

train$region<-as.factor(train$region)
summary(train$region)
contrasts(train$region)

#Merge all variables to one column as Scan_Docs MobileNo_Avl_Flag+Aadhar_flag+PAN_flag+VoterID_flag+Driving_flag+Passport_flag+Passport_flag

train <- merge(x=train$MobileNo_Avl_Flag, y=train$Aadhar_flag, by ="ID")

order_sales <- merge( x = orders, y = orderDetails_summary, by ="orderid")

library(tidyr)

unite(train, Group_docs, c(MobileNo_Avl_Flag, Aadhar_flag))

summary(train$NO.OF_INQUIRIES)
table(train$NO.OF_INQUIRIES)

summary(train$PERFORM_CNS.SCORE)
table(train$PERFORM_CNS.SCORE)

str(train)

#To find missing value
colSums(is.na(train))

#convert date of birth character to numeric "Age"
#train$Date.of.Birth<-as.Date(train$Date.of.Birth,"%d-%b-%y")
#str(train)
#To conver DOB into age lets download lbirtary (eepttols)
#install.packages(eeptools)
#library(eeptools)
#age_cal(train$Date.of.Birth, enddate = Sys.Date(),units = "years", precise = TRUE)
#b<-a
#train$Age<-train$Date.of.Birth - as.date("01-02-2022")
#summary(train$Date.of.Birth)
#train%>%filter(PERFORM_CNS.SCORE==0)%>%nrow()
#quantile(train$PERFORM_CNS.SCORE,p=c(1:100)/100)
#train%>%filter(default==0)%>%nrow()
#train%>%filter(default==1)%>%nrow()
#quantile(train$default,p=c(1:100)/100)

#Lets factor the integer variable into categorical variable
#train$default<-as.factor(dm$default)
#xtab(~default+Employment.Type, data=train)

#Lets put trial logistic model to find significant vairbales

train=glm(default~disbursed_amount+asset_cost+ltv+ branch_id+region+Date.of.Birth+Employment.Type+DisbursalDate+MobileNo_Avl_Flag+Aadhar_flag+PAN_flag+VoterID_flag +Driving_flag+Passport_flag+PERFORM_CNS.SCORE+DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS+CREDIT.HISTORY.LENGTH+NO.OF_INQUIRIES,family="binomial",data = train)
summary(train)

