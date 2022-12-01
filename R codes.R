#A real-estate startup has hired you- participants of the IPBA- to work on a pricing model for used houses.

#Objective
#To predict the Housing prices in Indore as a function of the different
#attributes of houses and locality

#Develop a pricing model that will make an estimate of the Price of the house. 
#This will help the company to reduce its cost and also streamline the process of selling used houses.


#CRIM: per capita crime rate
#ZN: the proportion of residential land zoned for lots larger than 25,000 sq.ft.
#INDUS: the proportion of non-retail business acres per town.
#RIVER_FLG: River Dummy Var (this is equal to 1 if tract bounds river; 0 otherwise)
#nitric oxides concentration: the nitric oxides concentration (parts per 10 million)
#rooms/dwelling: the average number of rooms per dwelling
#AGE: the proportion of owner-occupied units built prior to 1940
#DIS: the weighted distances to five employment centers of the town
#RAD: the index of accessibility to radial highways
#TAX: the full-value property-tax rate per INR 10 millions (1 crores)
#PTRATIO: the pupil-teacher ratio by area
#B: calculated as 1000(Bk - 0.63)², where Bk is the proportion of people living is slums in the area
#LSTAT: This is the percentage lower status and income of the population
#MEDV: This is the median value of owner-occupied homes in INR millions (10 lakhs)

#Participants should be able to :
#Clean the data
#perform EDA and Visualizations
#perform feature engineering
#build a model (linear regression)
#evaluate the model
#provide business insights from the model



library(dplyr)
library(ggplot2)
library(car)

"\IndoreHP_Test.csv"

setwd("C:\\Users\\USER\\Desktop\\IPBA WORKING\\Kagle Competitions\\indore-city-house-price-predection")

train<-read.csv("IndoreHP_Train.csv")

dim(train)
head(train)
tail(train)


summary(train)
str(train)


#To find missing value
colSums(is.na(train))

#MEDV - To Find outliers & variable distribution 
summary(train$MEDV)
hist(train$MEDV)
boxplot(train$MEDV)

#Histogram denoted - It seems this variable normally distributed and write side skewed maximum properties falls under 
#range of 15 to 30 millions

#Box plot Denoted - median properties price INR22 millions, 
#some of the properties in range of 50millions seen but cannot treat as outliers 

#CRIME - To Find outliers & variable distribution 
summary(train$CRIME)
qplot(train$CRIM, col = "pink")
boxplot(train$MEDV, train$CRIM, col ="red")

#Chart shows that 90% area under 0=>1 crime rate

#ZN - To Find outliers & variable distribution 
summary(train$ZN)
boxplot(train$ZN)
boxplot(train$MEDV, train$ZN, col ="blue")

#It seems there are average 15 residential land zone which larger than 25k sqft available. It seems shortage 

#INDUS - To Find outliers & variable distribution 
summary(train$INDUS)
plot(train$INDUS)
boxplot(train$INDUS)

#Around average 8acres of land available for non-retail business 

#RIVER_FLG - To Find outliers & variable distribution ??? Need work on this
summary(train$RIVER_FLG)
qplot(train$RIVER_FLG)

#Nitric - To Find outliers & variable distribution 
summary(train$nitric.oxides.concentration)
plot(train$nitric.oxides.concentration)
boxplot(train$nitric.oxides.concentration)

#Maximum area where nitric oxides content an average of 0.51 and 
#in 150areas its content goes upto 0.90 levels - pollution is concerned here


#Rooms/Dwelling - To Find outliers & variable distribution 
summary(train$X.rooms.dwelling)
plot(train$X.rooms.dwelling)
boxplot(train$X.rooms.dwelling)

#Average more than 6 rooms are available in Maximum areas.

#AGE - To Find outliers & variable distribution 
summary(train$AGE)
plot(train$AGE)
boxplot(train$AGE)

#Average of property 60+years available in Maximum areas. 
#But still 100 to 150 properties are available in age range of 100 years

#DIS - To Find outliers & variable distribution 
summary(train$DIS)
plot(train$DIS)
boxplot(train$DIS)

#Maximum properties are 3to6km away from five employment centers of town

#RAD - To Find outliers & variable distribution 
summary(train$RAD)
plot(train$RAD)
boxplot(train$RAD)

#Maximum properties has 2to5 index rating for accessibility to highway.  

#TAX - To Find outliers & variable distribution ?????
summary(train$TAX)
plot(train$TAX)
boxplot(train$TAX)



#PTRATIO - To Find outliers & variable distribution ????
summary(train$PTRATIO)
plot(train$PTRATIO)
boxplot(train$PTRATIO)

#B - To Find outliers & variable distribution ???
summary(train$B)
plot(train$B)
boxplot(train$B)


#LSTAT - To Find outliers & variable distribution ???
summary(train$LSTAT)
plot(train$LSTAT)
boxplot(train$LSTAT)

##Bivariate analysis 

#Correlations
cor(train$MEDV,train$CRIM)
cor(train$MEDV,train$ZN)
cor(train$MEDV,train$INDUS)
cor(train$MEDV,train$RIVER_FLG)
cor(train$MEDV,train$nitric.oxides.concentration)
cor(train$MEDV,train$X.rooms.dwelling)
cor(train$MEDV,train$AGE)
cor(train$MEDV,train$DIS)
cor(train$MEDV,train$RAD)
cor(train$MEDV,train$TAX)
cor(train$MEDV,train$PTRATIO)
cor(train$MEDV,train$B)
cor(train$MEDV,train$LSTAT)

#It seems that X.rooms.dwelling, LSTAT, PTRATIO variables are highly correlated with MEDV


#Viz for continuous variables
qplot(train$CRIM,train$MEDV)
boxplot(train$ZN,train$MEDV)
qplot(train$INDUS,train$MEDV)
qplot(train$RIVER_FLG,train$MEDV)
qplot(train$nitric.oxides.concentration,train$MEDV)
qplot(train$X.rooms.dwelling,train$MEDV)
qplot(train$AGE,train$MEDV)
qplot(train$DIS,train$MEDV)
qplot(train$RAD,train$MEDV)
qplot(train$TAX,train$MEDV)
qplot(train$PTRATIO,train$MEDV)
qplot(train$B,train$MEDV)
qplot(train$LSTAT,train$MEDV)


===========================#Models Building===================================

attach(train)

#SLRM No.1

Reg_SLRM_1<-lm(MEDV~CRIM,data=train)
summary(Reg_SLRM_1)

#SLRM No.2
Reg_SLRM_2<-lm(MEDV~ZN,data=train)
summary(Reg_SLRM_2)

#SLRM No.3
Reg_SLRM_3<-lm(MEDV~INDUS,data=train)
summary(Reg_SLRM_3)

#SLRM No.4
Reg_SLRM_4<-lm(MEDV~RIVER_FLG,data=train)
summary(Reg_SLRM_4)

#SLRM No.5
Reg_SLRM_5<-lm(MEDV~nitric.oxides.concentration,data=train)
summary(Reg_SLRM_5)

#SLRM No.6
Reg_SLRM_6<-lm(MEDV~X.rooms.dwelling,data=train)
summary(Reg_SLRM_6)

#SLRM No.7
Reg_SLRM_7<-lm(MEDV~AGE,data=train)
summary(Reg_SLRM_7)

#SLRM No.8
Reg_SLRM_8<-lm(MEDV~DIS,data=train)
summary(Reg_SLRM_8)

#SLRM No.9
Reg_SLRM_9<-lm(MEDV~RAD,data=train)
summary(Reg_SLRM_9)

#SLRM No.10
Reg_SLRM_10<-lm(MEDV~TAX,data=train)
summary(Reg_SLRM_10)

#SLRM No.11
Reg_SLRM_11<-lm(MEDV~PTRATIO,data=train)
summary(Reg_SLRM_11)

#SLRM No.12
Reg_SLRM_12<-lm(MEDV~B,data=train)
summary(Reg_SLRM_12)

#SLRM No.13
Reg_SLRM_13<-lm(MEDV~LSTAT,data=train)
summary(Reg_SLRM_13)


------------------------##Multivariate Regression Model----------------------

#MRMODEL-1
mrmodel_1<-lm(MEDV~CRIM+ZN+INDUS+RIVER_FLG+nitric.oxides.concentration+X.rooms.dwelling+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT,data=train)
summary(mrmodel_1)

#MRMODEL-2 (Drope INDUS)
mrmodel_2<-lm(MEDV~CRIM+ZN+RIVER_FLG+nitric.oxides.concentration+X.rooms.dwelling+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT,data=train)
summary(mrmodel_2)

#MRMODEL-3 (Drope INDUS+RIVER_FLG)
mrmodel_3<-lm(MEDV~CRIM+ZN+nitric.oxides.concentration+X.rooms.dwelling+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT,data=train)
summary(mrmodel_3)

#MRMODEL-4 (Drope INDUS+RIVER_FLG+RAD)
mrmodel_4<-lm(MEDV~CRIM+ZN+nitric.oxides.concentration+X.rooms.dwelling+AGE+DIS+TAX+PTRATIO+B+LSTAT,data=train)
summary(mrmodel_4)

-----------------#SELECTING MODEL#3-------------
#MRMODEL-3 (Drope INDUS+RIVER_FLG)
mrmodel_3<-lm(MEDV~CRIM+ZN+nitric.oxides.concentration+X.rooms.dwelling+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT,data=train)
summary(mrmodel_3)

-------------------------##Multicollinearity Test--------------------

vif(mrmodel_3)


##Getting predicted values
PredMEDV<-predict(mrmodel_3)
PredMEDV


##Finding Residuals
#Residuals are the errors which is Actual - Predicted 
ResMEDV<-resid(mrmodel_3)
ResMEDV

#checking for normality assumption
hist(ResMEDV)
# OR
hist(mrmodel_3$residuals)

#Quantile Quantile plot
qqPlot(mrmodel_3$residuals)



# ##Plotting Residuals vs Predicted Values
# ##Checking Heteroskedastcity - exists if there is a pattern between predicted values and error
# 
plot(PredMEDV,ResMEDV,abline(0,0))

##Plotting actual vs predicted values
#plot(mmix$NewVolSale,col="blue",type="l")
plot(train$MEDV[1:350],col="blue",type="l")
lines(PredMEDV,col="red",type="l")

#Testing Model on test data

library(dplyr)
library(ggplot2)
library(car)


setwd("C:\\Users\\USER\\Desktop\\IPBA WORKING\\Kagle Competitions\\indore-city-house-price-predection")

test<-read.csv("IndoreHP_Test.csv")

dim(test)
head(test)
tail(test)
summary(test)
str(test)

colSums(is.na(test))



submission_predictions<-predict(mrmodel_3,test)
submission_predictions

prediction_File<-tibble(ID=test$ID, MEDV=submission_predictions)
prediction_File

write.csv(prediction_File,"submission_Rajesh1.csv")





