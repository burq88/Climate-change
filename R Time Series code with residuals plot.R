# Read the dataset after downloading it from Canvas.  Note: You'll need to modify the path provided in the first line of code below, based on where you saved the dataset09.csv (Example: "C://Users//[your PSU user ID]//Desktop//dataset09.csv")
library("readxl")
data <- read_excel("20210312_Project_Data_Draft.xlsx")    # enter the path where you saved the dataset file

# Get the summary of the dataset

summary(data)

#Transform
library(dplyr)
data2 <- data %>%
  group_by(Commodity,Year) %>%
  summarise(Total_Yield = sum(Total_Yield))


#subset crops 
barley <- subset(data2, data2$Commodity == "BARLEY")
corn <- subset(data2, data2$Commodity == "CORN")
oats <- subset(data2, data2$Commodity == "OATS")
soybeans <- subset(data2, data2$Commodity == "SOYBEANS")
wheat <- subset(data2, data2$Commodity == "WHEAT")

# Turn into time series 

library(forecast)
data_ts_barley <- ts(barley$Total_Yield, start=c(1900, 1), freq=1)
data_ts_corn <- ts(corn$Total_Yield, start=c(1900, 1), freq=1)
data_ts_oats <- ts(oats$Total_Yield, start=c(1900, 1), freq=1)
data_ts_soybeans <- ts(soybeans$Total_Yield, start=c(1924, 1), freq=1)
data_ts_wheat <- ts(wheat$Total_Yield, start=c(1900, 1), freq=1)


# Let us fit a regression line on this dataset

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



# Let’s look in to the seasonal effects
#not relevant here because annual data can't tell seasons

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

# Here after 1 diff the series became stationary (see the figure below)

# Now let us look into the ACF and PACF charts

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


#model
#train data
set.seed(123)
barley_train = data_ts_barley[1:92]
corn_train = data_ts_corn[1:92]
oats_train = data_ts_oats[1:92]
soybeans_train = data_ts_soybeans[1:72]
wheat_train = data_ts_wheat[1:92]


# test data
barley_test <- data_ts_barley[93:114]
corn_test <- data_ts_corn[93:114]
oats_test <- data_ts_oats[93:114]
soybeans_test <- data_ts_soybeans[73:90]
wheat_test <- data_ts_wheat[93:114]


# Let us now fit the ARIMA(1,1,0)
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

# Now let us do some prediction for the next 50 years

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


#auto arima model 
autoarima_barley=auto.arima(barley_train)
autoarima_corn=auto.arima(corn_train)
autoarima_oats=auto.arima(oats_train)
autoarima_soybeans=auto.arima(soybeans_train, allowdrift=F)
autoarima_wheat=auto.arima(wheat_train, allowdrift=F)

#predicting next 50 years
pred_barley3 <- forecast(autoarima_barley, 50)
pred_corn3 <- forecast(autoarima_corn, 50)
pred_oats3 <- forecast(autoarima_oats, 50)
pred_soybeans3 <- forecast(autoarima_soybeans, 50)
pred_wheat3 <- forecast(autoarima_wheat, 50)

#plot prediction
plot(pred_barley3, sub = "Barley")
plot(pred_corn3, sub = "Corn")
plot(pred_oats3, sub ="Oats")
plot(pred_soybeans3, sub = "Soybeans")
plot(pred_wheat3, sub = "Wheat")

#additonal forecasting for accuracy measures
forecast_barley2 = predict(autoarima_barley,22)
forecast_corn2 = predict(autoarima_corn,22)
forecast_oats2 = predict(autoarima_oats,22)
forecast_soybeans2 = predict(autoarima_soybeans,18)
forecast_wheat2 = predict(autoarima_wheat,22)


#compare models

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

#checking residuals to see goodness of fit for the models

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
