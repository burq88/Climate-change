################################################################################
#Code Used to Remove Outliers for Deliverable 4
################################################################################

################################################################################
#Import dataset
project_data <- read.csv("C:/Users/590031/Desktop/Penn State/DAAN 881/20210304_Project_Data_Draft.csv")

################################################################################
#Find the first and third quartile yields and IQR for each crop
library(dplyr)
project_data %>%
  group_by(Commodity) %>%
  summarize(first_quart_yield = quantile(Total_Yield,0.25))
project_data %>%
  group_by(Commodity) %>%
  summarize(third_quart_yield = quantile(Total_Yield,0.75))
project_data %>%
  group_by(Commodity) %>%
  summarize(IQR_yield = quantile(Total_Yield,0.75)-quantile(Total_Yield,0.25))

################################################################################
#Find Q3+1.5*IQR (upper outlier bound) and Q1-1.5*IQR (lower outlier bound) for
#each crop using the values from above
upper_bound_barley = 48+(1.5*24)
lower_bound_barley = 24-(1.5*24)
upper_bound_corn = 92+(1.5*65.2)
lower_bound_corn = 26.8-(1.5*65.2)
upper_bound_oats = 50+(1.5*22.5)
lower_bound_oats = 27.5-(1.5*22.5)
upper_bound_soybeans = 31+(1.5*15)
lower_bound_soybeans = 16-(1.5*15)
upper_bound_wheat = 38+(1.5*21.5)
lower_bound_wheat = 16.5-(1.5*21.5)

#Add new column to determine if a value is an upper outlier
project_data$Outlier[project_data$Commodity == "BARLEY" & project_data$Total_Yield > upper_bound_barley] <- "Outlier"
project_data$Outlier[project_data$Commodity == "CORN" & project_data$Total_Yield > upper_bound_corn] <- "Outlier"
project_data$Outlier[project_data$Commodity == "OATS" & project_data$Total_Yield > upper_bound_oats] <- "Outlier"
project_data$Outlier[project_data$Commodity == "SOYBEANS" & project_data$Total_Yield > upper_bound_soybeans] <- "Outlier"
project_data$Outlier[project_data$Commodity == "WHEAT" & project_data$Total_Yield > upper_bound_wheat] <- "Outlier"
project_data$Outlier <- ifelse(project_data$Outlier == "Outlier","Outlier","No")

################################################################################
#Count number of outliers in data set, remove outliers, determine number of 
#resulting rows after removing them
table(project_data$Outlier)
project_data <- project_data[project_data$Outlier != "Outlier", ]
nrow(project_data)

################################################################################
#Export data set
write.csv(project_data,"C:/Users/590031/Desktop/Penn State/DAAN 881/20210310_Project_Data_Draft.csv", row.names = FALSE)

################################################################################
#Code Used to Convert Commodities into Dummy Variables After Removing Outliers 
#For Deliverable 4
################################################################################

################################################################################
#Import data and libraries
library(readxl)
project_data <- read_excel("20210312_Project_Data_Draft.xlsx")
head(project_data)
colnames(project_data)

library(fastDummies)

################################################################################
#Convert state and commodity variables into factors
project_data$State_factor <- as.numeric(factor(project_data$State))
project_data$Commodity_factor <- as.numeric(factor(project_data$Commodity))
head(project_data)
str(project_data)

################################################################################
#Convert commodity variables into dummy variables
library(openxlsx)
project_data_1 <- fastDummies::dummy_cols(project_data, select_columns = "Commodity", remove_first_dummy = TRUE)
str(project_data_1)

#Export file
write.xlsx(project_data_1, file = "20210323_Project_Data_w_dummies.xlsx", asTable = FALSE)

################################################################################
#Multiple Linear Regression (Assumptions + Model) Code for Deliverables 5 & 6
################################################################################

################################################################################
#Import dataset, libraries, create data subsets

#Import dataset 
install.packages("readxl")
library("readxl")
project_data <- read_excel("C:/Users/590031/Desktop/Penn State/DAAN 881/20210323_Project_Data_w_dummies.xlsx")

#Import libraries
library('ggplot2')
library('magrittr')
install.packages("caret")
library('caret')

#Create data subsets for each crop
project_data_barley <- project_data[project_data$Commodity == "BARLEY", ]
project_data_corn <- project_data[project_data$Commodity == "CORN", ]
project_data_oats <- project_data[project_data$Commodity == "OATS", ]
project_data_soybeans <- project_data[project_data$Commodity == "SOYBEANS", ]
project_data_wheat <- project_data[project_data$Commodity == "WHEAT", ]

################################################################################
#Build multiple linear regression models
lin_reg_barley <- lm(Total_Yield~Annual_Percipitation + Average_Annual_Temperature, data=project_data_barley)
lin_reg_corn <- lm(Total_Yield~Annual_Percipitation + Average_Annual_Temperature, data=project_data_corn)
lin_reg_oats <- lm(Total_Yield~Annual_Percipitation + Average_Annual_Temperature, data=project_data_oats)
lin_reg_soybeans <- lm(Total_Yield~Annual_Percipitation + Average_Annual_Temperature, data=project_data_soybeans)
lin_reg_wheat <- lm(Total_Yield~Annual_Percipitation + Average_Annual_Temperature, data=project_data_wheat)

################################################################################
#Verify assumptions for MLR model

#Check if mean of residuals is zero
mean(lin_reg_barley$residuals)
mean(lin_reg_corn$residuals)
mean(lin_reg_oats$residuals)
mean(lin_reg_soybeans$residuals)
mean(lin_reg_wheat$residuals)

#Check for homoscedasticity for each crop
par(mfrow=c(2,2))
plot(lin_reg_barley)
par(mfrow=c(2,2))
plot(lin_reg_corn)
par(mfrow=c(2,2))
plot(lin_reg_oats)
par(mfrow=c(2,2))
plot(lin_reg_soybeans)
par(mfrow=c(2,2))
plot(lin_reg_wheat)

#Check for autocorrelation
lmtest::dwtest(lin_reg_barley)
lmtest::dwtest(lin_reg_corn)
lmtest::dwtest(lin_reg_oats)
lmtest::dwtest(lin_reg_soybeans)
lmtest::dwtest(lin_reg_wheat)

#Check variance inflation factors (VIFs)
install.packages("car")
library(car)
vif(lin_reg_barley)
vif(lin_reg_corn)
vif(lin_reg_oats)
vif(lin_reg_soybeans)
vif(lin_reg_wheat)

################################################################################
#Define mean squared error (MSE) and mean absolute error (MAE) functions to 
#test accuracy for each model - y_p is the predicted value and y is the real value

mse <- function(y_p, y) {
  return(mean((y-y_p)^2))
}
mae <- function(y_p, y) {
  return(mean(abs(y-y_p)))
}
################################################################################
#Test barley accuracy

#Check r^2
summary(lin_reg_barley)

#Split barley data into train/test data (80:20 split) 
set.seed(123)
trnind_barley = createDataPartition(project_data_barley$Total_Yield, p=0.8, list=FALSE)
train_data_barley = project_data_barley[trnind_barley,]
test_data_barley = project_data_barley[-trnind_barley,]

#Evaluate train and test data for barley with MSE & MAE
#Evaluate barley train data
barley_train_pred <- predict(lin_reg_barley, train_data_barley)
mse(barley_train_pred, train_data_barley$Total_Yield)
mae(barley_train_pred, train_data_barley$Total_Yield)
#Evaluate barley test data
barley_test_pred <- predict(lin_reg_barley, test_data_barley)
mse(barley_test_pred, test_data_barley$Total_Yield)
mae(barley_test_pred, test_data_barley$Total_Yield)

################################################################################
#Test corn accuracy

#Check r^2
summary(lin_reg_corn)

#Split corn data into train/test data (80:20 split) 
set.seed(123)
trnind_corn = createDataPartition(project_data_corn$Total_Yield, p=0.8, list=FALSE)
train_data_corn = project_data_corn[trnind_corn,]
test_data_corn = project_data_corn[-trnind_corn,]

#Evaluate train and test data for corn
#Evaluate corn train data
corn_train_pred <- predict(lin_reg_corn, train_data_corn)
mse(corn_train_pred, train_data_corn$Total_Yield)
mae(corn_train_pred, train_data_corn$Total_Yield)
#Evaluate corn test data
corn_test_pred <- predict(lin_reg_corn, test_data_corn)
mse(corn_test_pred, test_data_corn$Total_Yield)
mae(corn_test_pred, test_data_corn$Total_Yield)

################################################################################
#Test oats accuracy

#Check r^2
summary(lin_reg_oats)

#Split oats data into train/test data (80:20 split) 
set.seed(123)
trnind_oats = createDataPartition(project_data_oats$Total_Yield, p=0.8, list=FALSE)
train_data_oats = project_data_oats[trnind_oats,]
test_data_oats = project_data_oats[-trnind_oats,]

#Evaluate train and test data for oats
#Evaluate oats train data
oats_train_pred <- predict(lin_reg_oats, train_data_oats)
mse(oats_train_pred, train_data_oats$Total_Yield)
mae(oats_train_pred, train_data_oats$Total_Yield)
#Evaluate oats train data
oats_test_pred <- predict(lin_reg_oats, test_data_oats)
mse(oats_test_pred, test_data_oats$Total_Yield)
mae(oats_test_pred, test_data_oats$Total_Yield)

################################################################################
#Test soybeans accuracy

#Check r^2
summary(lin_reg_soybeans)

#Split soybeans data into train/test data (80:20 split) 
set.seed(123)
trnind_soybeans = createDataPartition(project_data_soybeans$Total_Yield, p=0.8, list=FALSE)
train_data_soybeans = project_data_soybeans[trnind_soybeans,]
test_data_soybeans = project_data_soybeans[-trnind_soybeans,]

#Evaluate train and test data for soybeans
#Evaluate soybeans train data
soybeans_train_pred <- predict(lin_reg_soybeans, train_data_soybeans)
mse(soybeans_train_pred, train_data_soybeans$Total_Yield)
mae(soybeans_train_pred, train_data_soybeans$Total_Yield)
#Evaluate soybeans test data
soybeans_test_pred <- predict(lin_reg_soybeans, test_data_soybeans)
mse(soybeans_test_pred, test_data_soybeans$Total_Yield)
mae(soybeans_test_pred, test_data_soybeans$Total_Yield)

################################################################################
#Test wheat accuracy

#Check r^2
summary(lin_reg_wheat)

#Split wheat data into train/test data (80:20 split) 
set.seed(123)
trnind_wheat = createDataPartition(project_data_wheat$Total_Yield, p=0.8, list=FALSE)
train_data_wheat = project_data_wheat[trnind_wheat,]
test_data_wheat = project_data_wheat[-trnind_wheat,]

#Evaluate train and test data for wheat
#Evaluate wheat train data
wheat_train_pred <- predict(lin_reg_wheat, train_data_wheat)
mse(wheat_train_pred, train_data_wheat$Total_Yield)
mae(wheat_train_pred, train_data_wheat$Total_Yield)
#Evaluate wheat test data
wheat_test_pred <- predict(lin_reg_wheat, test_data_wheat)
mse(wheat_test_pred, test_data_wheat$Total_Yield)
mae(wheat_test_pred, test_data_wheat$Total_Yield)

################################################################################
#Time Series Code (Assumptions, Model, Residual Plots) for Deliverables 5 & 6
################################################################################

################################################################################
#Read the dataset after downloading it from Canvas.  
#Note: You'll need to modify the path provided in the first line of code below, 
#based on where you saved the dataset09.csv (Example: "C://Users//[your PSU user ID]//Desktop//dataset09.csv")

data <- X20210312_Project_Data_Draft     # enter the path where you saved the dataset file

################################################################################
#Summary of data, transform, create crop subsets

#Get the summary of the dataset
summary(data)

#Transform
data2 <- data %>%
  group_by(Commodity,Year) %>%
  summarise(Total_Yield = sum(Total_Yield))

#subset crops 
barley <- subset(data2, data2$Commodity == "BARLEY")
corn <- subset(data2, data2$Commodity == "CORN")
oats <- subset(data2, data2$Commodity == "OATS")
soybeans <- subset(data2, data2$Commodity == "SOYBEANS")
wheat <- subset(data2, data2$Commodity == "WHEAT")

################################################################################
#Turn into time series 

data_ts_barley <- ts(barley$Total_Yield, start=c(1900, 1), freq=1)
data_ts_corn <- ts(corn$Total_Yield, start=c(1900, 1), freq=1)
data_ts_oats <- ts(oats$Total_Yield, start=c(1900, 1), freq=1)
data_ts_soybeans <- ts(soybeans$Total_Yield, start=c(1924, 1), freq=1)
data_ts_wheat <- ts(wheat$Total_Yield, start=c(1900, 1), freq=1)

################################################################################
#Fit a regression line on this dataset

plot(data_ts_barley)
abline(reg=lm(data_ts_barley~time(data_ts_barley)))
plot(data_ts_corn)
abline(reg=lm(data_ts_corn~time(data_ts_corn)))
plot(data_ts_oats)
abline(reg=lm(data_ts_oats~time(data_ts_oats)))
plot(data_ts_soybeans)
abline(reg=lm(data_ts_soybeans~time(data_ts_soybeans)))
plot(data_ts_wheat)
abline(reg=lm(data_ts_wheat~time(data_ts_wheat)))

################################################################################
#Verifying time series seasonal effects, Dickey-Fuller Test, plot ACF & PACF plots

#Look in to seasonal effects - not relevant here because annual data can't tell seasons

boxplot(data_ts_barley~cycle(data_ts_barley))
boxplot(data_ts_corn~cycle(data_ts_corn))
boxplot(data_ts_oats~cycle(data_ts_oats))
boxplot(data_ts_soybeans~cycle(data_ts_soybeans))
boxplot(data_ts_wheat~cycle(data_ts_wheat))


library(tseries)

# Now perform the Augmented Dickey-Fuller Test
adf.test(diff(log(data_ts_barley)), alternative="stationary", k=0)
adf.test(diff(log(data_ts_corn)), alternative="stationary", k=0)
adf.test(diff(log(data_ts_oats)), alternative="stationary", k=0)
adf.test(diff(log(data_ts_soybeans)), alternative="stationary", k=0)
adf.test(diff(log(data_ts_wheat)), alternative="stationary", k=0)

# Let us now plot the series
plot(diff(log(data_ts_barley)))
plot(diff(log(data_ts_corn)))
plot(diff(log(data_ts_oats)))
plot(diff(log(data_ts_soybeans)))
plot(diff(log(data_ts_wheat)))

#Here after 1 diff the series became stationary (see the figure below)

#Look into the ACF and PACF charts
acf(diff(log(data_ts_barley)))
pacf(diff(log(data_ts_barley)))

acf(diff(log(data_ts_corn)))
pacf(diff(log(data_ts_corn)))

acf(diff(log(data_ts_oats)))
pacf(diff(log(data_ts_oats)))

acf(diff(log(data_ts_soybeans)))
pacf(diff(log(data_ts_soybeans)))

acf(diff(log(data_ts_wheat)))
pacf(diff(log(data_ts_wheat)))

################################################################################
#Build time series model

#Train data
set.seed(123)
barley_train = data_ts_barley[1:92]
corn_train = data_ts_corn[1:92]
oats_train = data_ts_oats[1:92]
soybeans_train = data_ts_soybeans[1:72]
wheat_train = data_ts_wheat[1:92]


#Test data
barley_test <- data_ts_barley[93:114]
corn_test <- data_ts_corn[93:114]
oats_test <- data_ts_oats[93:114]
soybeans_test <- data_ts_soybeans[73:90]
wheat_test <- data_ts_wheat[93:114]


#Fit ARIMA(1,1,0)
library(forecast)

#Manual ARIMA model
(fit_barley <- arima(barley_train, c(0, 1, 1)))
(fit_corn <- arima(corn_train, c(0, 1, 1)))
(fit_oats <- arima(oats_train, c(0, 1, 1)))
(fit_soybeans <- arima(soybeans_train, c(0, 1, 1)))
(fit_wheat <- arima(wheat_train, c(0, 1, 1)))

fit_barley
fit_corn
fit_oats
fit_soybeans
fit_wheat

#Time series prediction for the next 50 years
pred_barley <- forecast(fit_barley,50)
pred_corn <- forecast(fit_corn, 50)
pred_oats <- forecast(fit_oats, 50)
pred_soybeans <- forecast(fit_soybeans, 50)
pred_wheat <- forecast(fit_wheat, 50)

#Plot predictions
plot(pred_barley, main = "Barley ARIMA (0,1,1) Forecast")
plot(pred_corn, main = "Corn ARIMA (0,1,1) Forecast")
plot(pred_oats, main = "Oats ARIMA (0,1,1) Forecast")
plot(pred_soybeans, main = "Soybeans ARIMA (0,1,1) Forecast")
plot(pred_wheat, main = "Wheat ARIMA (0,1,1) Forecast")

#Additional forecasting used for accuracy measures 
forecast_barley = predict(fit_barley,22)
forecast_corn = predict(fit_corn,22)
forecast_oats = predict(fit_oats,22)
forecast_soybeans = predict(fit_soybeans,18)
forecast_wheat = predict(fit_wheat,22)

#Auto arima model 
autoarima_barley=auto.arima(barley_train)
autoarima_corn=auto.arima(corn_train)
autoarima_oats=auto.arima(oats_train)
autoarima_soybeans=auto.arima(soybeans_train, allowdrift=F)
autoarima_wheat=auto.arima(wheat_train, allowdrift=F)

#Predicting next 50 years
pred_barley3 <- forecast(autoarima_barley, 50)
pred_corn3 <- forecast(autoarima_corn, 50)
pred_oats3 <- forecast(autoarima_oats, 50)
pred_soybeans3 <- forecast(autoarima_soybeans, 50)
pred_wheat3 <- forecast(autoarima_wheat, 50)

#Plot prediction
plot(pred_barley3, sub = "Barley")
plot(pred_corn3, sub = "Corn")
plot(pred_oats3, sub ="Oats")
plot(pred_soybeans3, sub = "Soybeans")
plot(pred_wheat3, sub = "Wheat")

#Additonal forecasting for accuracy measures
forecast_barley2 = predict(autoarima_barley,22)
forecast_corn2 = predict(autoarima_corn,22)
forecast_oats2 = predict(autoarima_oats,22)
forecast_soybeans2 = predict(autoarima_soybeans,18)
forecast_wheat2 = predict(autoarima_wheat,22)

################################################################################
#Compare time series models and determine accuracy measures
#Compare models
url <- "http://cran.r-project.org/src/contrib/Archive/DMwR/DMwR_0.4.1.tar.gz"
pkgFile <- "DMwR_0.4.1.tar.gz"
download.file(url = url, destfile = pkgFile)
install.packages(pkgs=pkgFile, type="source", repos=NULL)
library(DMwR)

#Accuracy measures 
accmeasures1_barley=regr.eval(barley_test, forecast_barley$pred)
accmeasures2_barley=regr.eval(barley_test, forecast_barley2$pred)
accMeasure_barley=rbind(accmeasures1_barley,accmeasures2_barley)
print(accMeasure_barley)

accmeasures1_corn=regr.eval(corn_test, forecast_corn$pred)
accmeasures2_corn=regr.eval(corn_test, forecast_corn2$pred)
accMeasure_corn=rbind(accmeasures1_corn,accmeasures2_corn)
print(accMeasure_corn)

accmeasures1_oats=regr.eval(oats_test, forecast_oats$pred)
accmeasures2_oats=regr.eval(oats_test, forecast_oats2$pred)
accMeasure_oats=rbind(accmeasures1_oats,accmeasures2_oats)
print(accMeasure_oats)

accmeasures1_soybeans=regr.eval(soybeans_test, forecast_soybeans$pred)
accmeasures2_soybeans=regr.eval(soybeans_test, forecast_soybeans2$pred)
accMeasure_soybeans=rbind(accmeasures1_soybeans,accmeasures2_soybeans)
print(accMeasure_soybeans)

accmeasures1_wheat=regr.eval(wheat_test, forecast_wheat$pred)
accmeasures2_wheat=regr.eval(wheat_test, forecast_wheat2$pred)
accMeasure_wheat=rbind(accmeasures1_wheat,accmeasures2_wheat)
print(accMeasure_wheat)

################################################################################
#Check residual plots to see goodness of fit for the models

tsdisplay(residuals(fit_barley), lag.max = 15, main = "Trained Barley model")
tsdisplay(residuals(fit_corn), lag.max = 15, main = "Trained Corn model")
tsdisplay(residuals(fit_wheat), lag.max = 15, main = "Trained Wheat model")
tsdisplay(residuals(fit_soybeans), lag.max = 15, main = "Trained Soybeans model")
tsdisplay(residuals(fit_oats), lag.max = 15, main = "Trained Oats model")

tsdisplay(residuals(autoarima_barley), lag.max = 15, main = "Trained Auto-Barley model")
tsdisplay(residuals(autoarima_corn), lag.max = 15, main = "Trained Auto-Corn model")
tsdisplay(residuals(autoarima_wheat), lag.max = 15, main = "Trained Auto-Wheat model")
tsdisplay(residuals(autoarima_soybeans), lag.max = 15, main = "Trained Auto-Soybeans model")
tsdisplay(residuals(autoarima_oats), lag.max = 15, main = "Trained Auto-Oats model")