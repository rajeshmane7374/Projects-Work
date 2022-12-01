#About
#The client is into the chemical industry business. They produce and distribute various types of chemicals to the market.

#The dataset consists of the daily average demand for one type of chemical (in Kilo Gallons) across multiple markets of a country.

#Objective
#The client would like to use the appropriate demand forecasting technique/s to help them predict the daily demand of the chemical, 
#with high accuracy. This demand prediction will help them efficiently manage their production capacities and distribution logistics.

#You need to assess what data is available and perform some exploratory and descriptive analytics to identify interesting and useful 
#patterns, trends, and insights. Also, build appropriate predictive model/s on the data.

#Participants should be able to :
  
#Clean the data
#perform EDA and Vizualizations
#build model/s (time seires)
#evaluate the model
#provide business insights from the model
#Note - Please refer to the data dictionary (in the data section) to get a better understanding of the variables

#File descriptions
#DemandTrain.csv - the training set
#DemandTest.csv - the test set
#DemandSubmission.csv - a sample submission file in the correct format; but the predicted demand values are randomly set to 
#25.5 each...i.e, incorrect.. :)
#Data fields
#ID - date
#mean_demand_kiloGallon - average demand of the chemical across various markets in the country

install.packages("fpp")

library(dplyr)
library(ggplot2)

ChemSales<-read.csv("C:\\Users\\rajes\\Desktop\\IPBA JS IIM Indore\\Kagle Competitions\\prediction-of-chemical-demand\\DemandTrain.csv")

view(ChemSales)
class(ChemSales)#Data is in data frame format need to convert into Time Series

str(ChemSales)#Dates seems in character format need to convert into date format
summary(ChemSales)


#Lets convert dats column character to date format 
library(lubridate)

ChemSales$date<-dmy(ChemSales$date)

class(ChemSales$date)
ChemSales <- ChemSales[order(ChemSales$date), ]
head(ChemSales$date)
tail(ChemSales$date)


#To Check any missing value

summary(ChemSales$mean_demand_kiloGallon)
class(ChemSales$mean_demand_kiloGallon)
table(ChemSales$mean_demand_kiloGallon)
colSums(is.na(ChemSales))


#Lets convert data frame into Time Series format  
library(tseries)

ChemSalesTS=ts(ChemSales$mean_demand_kiloGallon,start=c(2014,1),frequency = 365)
plot(ChemSalesTS)

#The plot chart indicates the product has seasonal cycle demand and constant trend, it seems sales start picking-up from Jan till mid of year,
#then start down trend after mid of year till end of year. It observed there is no specific trend straight liner trend. 

acf(ChemSalesTS)

#Lets Chechk Data stationery or Non-stationery. 
adf.test(ChemSalesTS,k=365)

#Data is non stationery since p vlaue is not close to -12and 0.01

#To make it stationery lets run first order difference

ChemSales_1<-diff(ChemSalesTS, differences = 1)

adf.test(ChemSales_1,k=365)

acf(ChemSales_1)
pacf(ChemSales_1)

#ADF test now close to -12 and p value is equal to 0.01

ChemSales_1<-diff(ChemSalesTS, differences = 2)

adf.test(ChemSales_1,k=365)

acf(ChemSales_1)
pacf(ChemSales_1)

#Lets select difference-2 in ARIMA model as order of "d"

#Run PACF test to select AR term or the p term - correlation between lags

pacf(ChemSales_1)

#To Select 7 as order "p"

#Run ACF test to select MA term or the q term

acf(ChemSales_1)

#To Select 3 as order "q"

#Lets fit the ARIMA model

ChemSalesARIMA<-arima(ChemSalesTS,order = c(4,1,2))
ChemSalesARIMA

#To Validtae the model

plot.ts(ChemSalesARIMA$residuals)

acf(ChemSalesARIMA$residuals)

hist(ChemSalesARIMA$residuals)

#To forecast the model 
install.packages("forcats")

library(forecast)

mychemSalesforecast=forecast(ChemSalesARIMA,h=113)

mychemSalesforecast

plot(mychemSalesforecast)#In plot trend line flat indicating no trend, however   

Box.test(mychemSalesforecast$resid, lag=5, type= "Ljung-Box")
Box.test(mychemSalesforecast$resid, lag=15, type= "Ljung-Box")
Box.test(mychemSalesforecast$resid, lag=25, type= "Ljung-Box")

accuracy(mychemSalesforecast)

#The test shows us forecasts do not show any autocorrelation or not significantly correlated.


#Testing Model on test data

library(dplyr)
library(ggplot2)

ChemSales_Demand<-read.csv("C:\\Users\\rajes\\Desktop\\IPBA JS IIM Indore\\Kagle Competitions\\prediction-of-chemical-demand\\DemandTest.csv")

view(ChemSales_Demand)
class(ChemSales_Demand)#Data is in data frame format need to convert into Time Series

str(ChemSales_Demand)#Dates seems in character format need to convert into date format
summary(ChemSales_Demand)


#Lets convert dats column character to date format 
class(ChemSales_Demand$date)

library(lubridate)

ChemSales_Demand$date<-dmy(ChemSales_Demand$date)

class(ChemSales_Demand$date)

submission_predictions<-predict(mychemSalesforecast,ChemSales_Demand$date)
submission_predictions


prediction_File<-tibble(date=ChemSales_Demand$date, ChemSalesDemand=submission_predictions$mean)
prediction_File


write.csv(prediction_File,"submission_ChemSales4.csv",row.names = FALSE)

#MOst Important - I faced major problem becuase of final write file was having index number 
#due to which third column of index number was appearing and in kaggle competition not able to submit the file, 
#Well now the problem has been receified by myslef putting last single command as "row.names=FALSE" this command
#help to write file without index number.





#To Decompose the components  

ChemsSalesTS_decompose=decompose(ChemSalesTS,type="additive")

plot(as.ts(ChemsSalesTS_decompose$seasonal))
plot(as.ts(ChemsSalesTS_decompose$trend))
plot(as.ts(ChemsSalesTS_decompose$random))

plot(ChemsSalesTS_decompose)

acf(ChemsSalesTS_decompose)

#To Box-cox log transformation 




#lets use Box-cox log transformation
Box.test(ChemSales2,lag=20,type="Ljung-Box")

acf(ChemSales2)

library(forecast)

Fit<-ets(ChemSales2)
Summary(Fit)


rm(list=ls())

str(ChemSales)

#To plot the time series.
library(ggplot2)
plot.ts(ChemSales)
autoplot(ChemSales)









