#########################################################################
####### Case Study - Time Series - Retail-Giant Sales Forecasting #######
#                                                                       #
# 1. Business Understanding                                             #
# 2. Data Understanding                                                 #
# 3. Data Preparation                                                   #
# 4. Model Building and Analysis                                        #
# 5. Model Evaluation                                                   #
# 6. Conclusion                                                         #
#                                                                       #
#########################################################################

# 1. Business Understanding: 

#################################################################################################################
#                                                                                                               #
# "Global Mart" is an online store super giant having worldwide operations. It takes orders and       	        # 	
# delivers across the globe and deals with all the major product categories - consumer, corporate & home office #
# 										                                                                                  				#
# Now as a sales/operations manager, you want to finalise the plan for the next 6 months.  		                	#
# So, you want to forecast the sales and the demand for the next 6 months, that would help you manage 	      	#
# the revenue and inventory accordingly.                                                      									#	
# 														                                                                                  #
# The store caters to 7 different market segments and in 3 major categories. You want to forecast at this     	#
# granular level, so you subset your data into 21 (7*3) buckets before analysing these data.		              	#
# 													                                                                                  	#
# But not all of these 21 market buckets are important from the store's point of view. So you need to find out	#
# 2 most profitable (and consistent) segment from these 21 and forecast the sales and demand for these segments #
#                                                                                                               #
#################################################################################################################


# 2. Data Understanding: 

#########################################################################
#                                                                       #
# Global Superstore Dataset                                             #
# Number of Instances in Complete Dataset : 51,290                      #
# Number of Attributes : 24                                             #
#                                                                       #
# Data Dictionary                                                       #
# ______________________________________________________________________#
# Attribute_Name| Description                                           #
# ______________  ______________________________________________________#
# Row ID	      : Unique ID of each data point/record in the dataset    #
# Order ID	    : Unique ID of the transaction                          #
# Order Date	  : Date on which the order was placed                    #
# Ship Date	    : Date on which the shipment was made                   #
# Ship Mode	    : The mode of shipment (category)                       #
# Customer ID	  : The unique ID of the customer                         #
# Customer Name	: Name of the customer                                  #
# Segment	      : The market segment to which the product belongs       #
# City	        : City of the delivery address                          #
# State	        : State of the delivery address                         #
# Country	      : Country of the delivery address                       #
# Postal Code	  : Postal code of the delivery address                   #
# Market	      : Market segment to which the customer belongs          #
# Region	      : Geographical region of the customer                   #
# Product ID	  : Unique ID of the product                              #
# Category	    : Category of the product                               #
# Sub-Category	: Sub-category of the product                           #
# Product Name	: Name of the product                                   #
# Sales	        : Total sales value of the transaction                  #
# Quantity	    : Quantity of the product ordered                       #
# Discount	    : Discount percentage offered on the product            #
# Profit	      : Profit made on the transaction                        #
# Shipping Cost : Shipping cost incured on the transaction              #
# Order Priority:	Priority assigned to the order                        #
#                                                                       #
#########################################################################
# 3. Data Preparation: 

#install.packages("ggplot2")
#install.packages("graphics")
#install.packages("forecast")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("tseries")

#Loading necessary libraries

library(ggplot2)
library(graphics)
library(forecast)
library(lubridate)
library(dplyr)
library(tseries)

#Loading Data

global_superstore_data <- read.csv("Global Superstore.csv", stringsAsFactors = F)

#Understanding data

dim(global_superstore_data)

str(global_superstore_data)

head(global_superstore_data)

#Check for missing values

sum(is.na(global_superstore_data))

#Check which all attribute has missing values
colSums(is.na(global_superstore_data))
#There is a total of 41296 missing values in Postal.Code
#Since postal code is not going to be the 
#part of the analysis there is no need to treat missing values

#Selecting required fields for analysis
global_superstore <- global_superstore_data[c("Order.Date", "Segment", "Market", "Sales", "Quantity", "Profit")]


str(global_superstore$Order.Date)
#This is stored as character. Converting this to date format

global_superstore$Order.Date <- dmy(global_superstore$Order.Date)
str(global_superstore$Order.Date)

#Replace days by first day of the month
mday(global_superstore$Order.Date) <- 01

#To aggregate the 3 attributes  - Sales, Quantity & Profit, 
#over the Order Date to arrive at monthly values for these attributes

GSS_aggregate <- group_by(global_superstore, Segment, Market, Order.Date) %>% summarise(Agg_Sales = sum(Sales),
                                                                                        Agg_Qty = sum(Quantity),
                                                                                        Agg_Prof = sum(Profit))

#Finding coefficient of variable (CV) for aggregated profit and sorting as per CV

GSS_Profitable <- group_by(GSS_aggregate, Segment, Market) %>% summarise(CV = sd(Agg_Prof)/mean(Agg_Prof)) %>% arrange(CV)
View(GSS_Profitable)

#Visualization of monthly profits for different markets

ggplot(GSS_Profitable, aes(x = factor(Market), y = CV, fill = factor(Segment))) + geom_bar(stat = "identity", 
                                                                                           position = "dodge") +
  xlab("Market") + ylab(" CV of Monthly Profit") + ggtitle("CV of Monthly Profit v Market Segment") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label = round(CV,2)), vjust = 1.5, color = "black", 
                                                            position = position_dodge(0.9), size = 3)

#Based on the results the two most profitable and consistently profitable matkets are 
#'EU Consumer' & 'APAC Consumer'

#Extracting data for EU Consumer
GSS_EU_Consumer <- filter(GSS_aggregate, Segment == "Consumer", Market == "EU") %>% arrange(Order.Date)

#Extracting data for APAC Consumer
GSS_APAC_Consumer <- filter(GSS_aggregate, Segment == "Consumer", Market == "APAC") %>% arrange(Order.Date)

nrow(GSS_EU_Consumer)
nrow(GSS_APAC_Consumer)
#Above both have 48 data points

#Creating Monthly sequence
Month = 1:nrow(GSS_APAC_Consumer)

#Splitting training and test data for both markets
GSS_EU_Consumer <- cbind(GSS_EU_Consumer, Month = Month)
GSS_APAC_Consumer<- cbind(GSS_APAC_Consumer, Month = Month)

GSS_EU_Consumer_IN <- GSS_EU_Consumer[1:42,]
GSS_EU_Consumer_OUT <- GSS_EU_Consumer[43:48,]

GSS_APAC_Consumer_IN <- GSS_APAC_Consumer[1:42,]
GSS_APAC_Consumer_OUT <- GSS_APAC_Consumer[43:48,]

##### Time Series Analysis for EU Consumer Sales (ARMA) #####

Total_Sales_EU_TS <- ts(GSS_EU_Consumer$Agg_Sales)
plot(Total_Sales_EU_TS)

EU_Sales_IN_TS <- ts(GSS_EU_Consumer_IN$Agg_Sales)
plot(EU_Sales_IN_TS)

#Applying Moving Average Smoothing

w <- 1
smoothedseries <- stats::filter(EU_Sales_IN_TS, filter = rep(1/(2*w+1),(2*w+1)), method = "convolution", 
                                sides = 2)

#Smoothing left end of Time Series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for(i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the Time Series

n <- length(EU_Sales_IN_TS)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for(i in seq(n-w+1,n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_IN <- GSS_EU_Consumer_IN$Month
lines(smoothedseries, col = "blue", lwd = 2)

#Amplitude of the seasonal curve does not show significant increase with time. 
#Hence we will try additive model

smootheddf <- as.data.frame(cbind(timevals_IN, as.vector(smoothedseries)))
colnames(smootheddf) <- c("Month", "Sales")

lmfit1 <- lm(Sales~ sin(0.5*Month) + cos(0.5*Month) + Month, data = smootheddf)
summary(lmfit1)
#Multiple R-squared:  0.7269,	Adjusted R-squared:  0.7053
globalpred_1 <- predict(lmfit1, Month = timevals_IN)

plot(EU_Sales_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_1, col = "red", lwd = 2)


lmfit2 <- lm(Sales~ sin(0.5*Month) * poly(Month, 2) + cos(0.5*Month) * poly(Month,2) +Month, data = smootheddf)
summary(lmfit2)
#Multiple R-squared:  0.7952,	Adjusted R-squared:  0.7456
globalpred_2 <- predict(lmfit2, Month = timevals_IN)

plot(EU_Sales_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_2, col = "red", lwd = 2)


lmfit3 <- lm(Sales~ sin(0.5*Month) * poly(Month, 3) + cos(0.5*Month) * poly(Month,3) +Month, data = smootheddf)
summary(lmfit3)
#Multiple R-squared:  0.8793,	Adjusted R-squared:  0.835
globalpred_3 <- predict(lmfit3, Month = timevals_IN)

plot(EU_Sales_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_3, col = "red", lwd = 2)

lmfit4 <- lm(Sales~ sin(0.5*Month) * poly(Month, 4) + cos(0.5*Month) * poly(Month, 4) +Month, data = smootheddf)
summary(lmfit4)
#Multiple R-squared:  0.8821,	Adjusted R-squared:  0.8209
globalpred_4 <- predict(lmfit4, Month = timevals_IN)

plot(EU_Sales_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_4, col = "red", lwd = 2)

#Adjusted R-Squared is best for polynomial degree 3.
#We will use lmfit3 for global component

#Checking Locally predictable series using ARMA

localpred <- EU_Sales_IN_TS - globalpred_3
plot(localpred, col = "red", type = "l", lwd = 2)
acf(localpred)
acf(localpred, type = "partial")
armafit <- auto.arima(localpred)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit

#Checking if residual series is white noise

resi <- localpred - fitted(armafit)
adf.test(resi, alternative = "stationary")
#Dickey-Fuller = -7.3166, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
#Pvalue = 0.01 (< 0.05)
kpss.test(resi)
#KPSS Level = 0.017855, Truncation lag parameter = 1, p-value = 0.1
#PValue = 0.1 (> 0.05)

#Predicting last 6 month values

outdata <- GSS_EU_Consumer_OUT
timevals_OUT <- outdata$Month
globalpred_OUT <- predict(lmfit3, data.frame(Month = timevals_OUT))

fcast <- globalpred_OUT

#Comparing predictions using MAPE

Mape_1 <- accuracy(fcast, outdata$Agg_Sales)[5]
Mape_1
# = 92.95788

#Plotting predictions with actual values
prediction_1 <- c(ts(globalpred_3), ts(globalpred_OUT))
plot(Total_Sales_EU_TS, col = "black", lwd = 2)
lines(prediction_1, col = "red", lwd = 2)

##### Time Series Analysis for EU Consumer Sales (ARIMA) #####

autoarima <- GSS_EU_Consumer_IN$Agg_Sales%>%auto.arima()
autoarima 
par(mar=c(1,1,1,1))
tsdiag(autoarima)
par(mar=c(5.1,4.1,4.1,2.1))
plot(autoarima$x, col="black")

#Checking if residual series is white noise
timeser <- ts(GSS_EU_Consumer_IN$Agg_Sales)
residual_autoarima <- timeser-fitted(autoarima)

adf.test(residual_autoarima, alternative = "stationary")
#P Value = 0.01 viz, less than 0.05
kpss.test(residual_autoarima)
#P Value = 0.01 viz, less than 0.05
#Conclusion: Series is stationary

#Evaluating using MAPE

fcast_autoarima <- predict(autoarima, n.ahead = 6)
MAPE_autoarima <- accuracy(fcast_autoarima$pred, GSS_EU_Consumer_OUT$Agg_Sales)[5]
MAPE_autoarima
# is equal to 28.9226

#MAPE value for ARMA (manual) is 92.957 and for Auto Arima it is 28.922
#Hence Auto Arima is better for forecasting in this case

GSS_EU_Consumer$Agg_Sales%>%auto.arima()%>%predict(n.ahead = 6)

#Time Series:
#  Start = 49 
#End = 54 
#Frequency = 1 
#[1] 49358.71 58063.62 59714.33 54191.79 56811.55 58010.84

############Time Series Analysis for EU consumer Quantity (ARMA)

Total_Quantity_EU_TS <- ts(GSS_EU_Consumer$Agg_Qty)
plot(Total_Quantity_EU_TS)

EU_Quantity_IN_TS <- ts(GSS_EU_Consumer_IN$Agg_Qty)
plot(EU_Quantity_IN_TS)

#Applying moving average smoothing

w <- 1
smoothedseries <- stats::filter(EU_Quantity_IN_TS, filter = rep(1/(2*w+1), (2*w+1)), 
                                method = "convolution", sides = 2)

#Smoothing Left end of the Time Series
diff <- smoothedseries[w+2] - smoothedseries[w+1] 
for(i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing Right end of the Time Series
n <- length(EU_Quantity_IN_TS)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for(i in seq(n-w+1,n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plotting the smoothed time Series

timevals_IN <- GSS_EU_Consumer_IN$Month
lines(smoothedseries, col = "blue", lwd = 2)

smootheddf <- as.data.frame(cbind(timevals_IN, as.vector(smoothedseries)))
colnames(smootheddf) <- c("Month", "Sales")

#Modelling Seasonality using a sinusoidal function

lmfit1 <- lm(Sales~ sin(0.5*Month) + cos(0.5*Month) + Month, data = smootheddf)
summary(lmfit1)
#Multiple R-squared:  0.851,	Adjusted R-squared:  0.8393
globalpred_1 <- predict(lmfit1, Month = timevals_IN)
summary(globalpred_1)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_1, col = "red", lwd = 2)


lmfit2 <- lm(Sales~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2) + Month, data = smootheddf)
summary(lmfit2)
#Multiple R-squared:  0.8927,	Adjusted R-squared:  0.8666  
globalpred_2 <- predict(lmfit2, Month = timevals_IN)
summary(globalpred_2)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_1, col = "red", lwd = 2)

lmfit3 <- lm(Sales~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3) + Month, data = smootheddf)
summary(lmfit3)
#Multiple R-squared:  0.9132,	Adjusted R-squared:  0.8814 
globalpred_3 <- predict(lmfit3, Month = timevals_IN)
summary(globalpred_3)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_1, col = "red", lwd = 2)

lmfit4 <- lm(Sales~ sin(0.5*Month) * poly(Month,4) + cos(0.5*Month) * poly(Month,4) + Month, data = smootheddf)
summary(lmfit4)
#Multiple R-squared:  0.9164,	Adjusted R-squared:  0.8731  
globalpred_4 <- predict(lmfit4, Month = timevals_IN)
summary(globalpred_4)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_1, col = "red", lwd = 2)

#Adjusted R-Squared is best for polynomial degree 3.
#We will use lmfit3 for global component

#Checking Locally predictable series using ARMA

local_pred <- EU_Quantity_IN_TS - globalpred_3
plot(local_pred, col = "red", type = "l")
acf(local_pred)
acf(local_pred, type = "partial")
armafit <- auto.arima(local_pred)
par(mar = c(1,1,1,1))
tsdiag(armafit)
par(mar = c(5.1,4.1,4.1,2.1))
armafit

#Checking if residual series is white noise

resi <- local_pred - fitted(armafit)
adf.test(resi, alternative = "stationary")
#data:  resi
#Dickey-Fuller = -6.6825, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
#kpss.test(resi)
#data:  resi
#KPSS Level = 0.023531, Truncation lag parameter = 1, p-value = 0.1
#SERIES IS STATIONARY

#Evaluating using MAPE

outdata <- GSS_EU_Consumer_OUT
timevals_OUT <- outdata$Month
globalpred_OUT <- predict(lmfit3, data.frame(Month = timevals_OUT))

localpred_OUT <- predict(armafit, n.ahead = 6)
fcast <- globalpred_OUT+localpred_OUT$pred

Mape_1 <- accuracy(fcast, outdata$Agg_Qty)[5]
Mape_1
# 31.45475
#Plotting predictions with actual values

prediction_1 <- c(ts(globalpred_3), ts(globalpred_OUT))
plot(Total_Quantity_EU_TS, col = "black", lwd = 2)
lines(prediction_1, col = "red", lwd = 2)

##### Time Series Analysis for EU Consumer Quantity (Auto ARIMA) #####

autoarima <- GSS_EU_Consumer_IN$Agg_Qty%>%auto.arima()
autoarima
#ARIMA(2,1,0) 

#Coefficients:
#  ar1      ar2
#-0.7359  -0.5879
#s.e.   0.1224   0.1185

#sigma^2 estimated as 21185:  log likelihood=-261.9
#AIC=529.8   AICc=530.44   BIC=534.94

par(mar=c(1,1,1,1))
tsdiag(autoarima)
par(mar=c(5.1,4.1,4.1,2.1))
plot(autoarima$x, col = "black")
lines(fitted(autoarima), col = "red")

#Checking is residual series is white noise
timeser <- ts(GSS_EU_Consumer_IN$Agg_Qty)
residual_autoarima <- timeser - fitted(autoarima)
adf.test(residual_autoarima, alternative = "stationary")
#data:  residual_autoarima
#Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
#alternative hypothesis: stationary

kpss.test(residual_autoarima)
#data:  residual_autoarima
#KPSS Level = 0.047939, Truncation lag parameter = 1, p-value = 0.1

#Evaluating the model using MAPE

fcast_autoarima <- predict(autoarima, n.ahead = 6)
MAPE_autoarima <- accuracy(fcast_autoarima$pred, GSS_EU_Consumer_OUT$Agg_Qty)[5]
MAPE_autoarima
# 30.13319

#MAPE value for ARMA (manual) is 31.454 and for Auto Arima it is 30.133
#Hence Auto Arima is better for forecasting in this case

GSS_EU_Consumer$Agg_Qty%>%auto.arima()%>%predict(n.ahead = 6)

#Time Series:
#  Start = 49 
#End = 54 
#Frequency = 1 
#[1] 626.2009 786.6056 842.9179 704.8258 768.6274 807.6497

##### Time Series Analysis for APAC Consumer Sales (ARMA) #####

Total_Consumer_Sales_APAC_TS <- ts(GSS_APAC_Consumer$Agg_Sales)
plot(Total_Consumer_Sales_APAC_TS)

APAC_Sales_IN_TS <- ts(GSS_APAC_Consumer_IN$Agg_Sales)
plot(APAC_Sales_IN_TS)

# Moving average smoothing

w <- 1
smoothedseries <- stats::filter(APAC_Sales_IN_TS, filter = rep(1/(2*w+1), (2*w+1)), 
                                method = "convolution", sides = 2)

#Smoothing left side of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for(i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing Right side of the Time series

n <- length(APAC_Sales_IN_TS) 
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for(i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plotting
timevals_IN <- GSS_APAC_Consumer_IN$Month
lines(smoothedseries, col = "blue", lwd = 2)

smootheddf <- as.data.frame(cbind(timevals_IN, as.vector(smoothedseries)))
colnames(smootheddf) <- c("Month", "Sales")

#Modelling seasonality using sinusoidal function

lmfit1 <- lm(Sales~ sin(0.5*Month) + cos(0.5*Month) + Month, data = smootheddf)
summary(lmfit1)
#Multiple R-squared:  0.7386,	Adjusted R-squared:  0.718 
globalpred_1 <- predict(lmfit1, Month = timevals_IN)
summary(globalpred_1)
plot(APAC_Sales_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_1, col = "red", lwd = 2)

lmfit2 <- lm(Sales~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2) + Month, data = smootheddf)
summary(lmfit2)
#Multiple R-squared:  0.8356,	Adjusted R-squared:  0.7958
globalpred_2 <- predict(lmfit2, Month = timevals_IN)
summary(globalpred_2)
plot(APAC_Sales_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_2, col = "red", lwd = 2)

lmfit3 <- lm(Sales~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3) + Month, data = smootheddf)
summary(lmfit3)
#Multiple R-squared:  0.8654,	Adjusted R-squared:  0.816
globalpred_3 <- predict(lmfit3, Month = timevals_IN)
summary(globalpred_3)
plot(APAC_Sales_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_3, col = "red", lwd = 2)

lmfit4 <- lm(Sales~ sin(0.5*Month) * poly(Month,4) + cos(0.5*Month) * poly(Month,4) + Month, data = smootheddf)
summary(lmfit4)
#Multiple R-squared:  0.8729,	Adjusted R-squared:  0.8071
globalpred_4 <- predict(lmfit4, Month = timevals_IN)
summary(globalpred_4)
plot(APAC_Sales_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_4, col = "red", lwd = 2)

#Adjusted R-Squared is best for polynomial degree 3.
#We will use lmfit3 for global component

#Checking for locally predicatable series

local_pred <- APAC_Sales_IN_TS - globalpred_3
plot(local_pred, col = "red", type = "l")
acf(local_pred)
acf(local_pred, type = "partial")
armafit <- auto.arima(local_pred)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit
#Series: local_pred 
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 8.8e+07:  log likelihood=-443.75
#AIC=889.49   AICc=889.59   BIC=891.23

#Checking if residual series is white noise
resi <- local_pred - fitted(armafit)
adf.test(resi, alternative = "stationary")
#data:  resi
#Dickey-Fuller = -6.8673, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
kpss.test(resi)
#data:  resi
#KPSS Level = 0.021731, Truncation lag parameter = 1, p-value = 0.1

# Residual series is stationary

#Evaluating MAPE

outdata <- GSS_APAC_Consumer_OUT
timevals_OUT <- outdata$Month
globalpred_OUT <- predict(lmfit3, data.frame(Month = timevals_OUT))
fcast <- globalpred_OUT

Mape_1 <- accuracy(fcast, outdata$Agg_Sales)[5]
Mape_1
# 31.07429

#Plotting prediction along with original values

prediction_1 <- c(ts(globalpred_3), ts(globalpred_OUT))
plot(Total_Consumer_Sales_APAC_TS, col = "black")
lines(prediction_1, col = "red")

##### Time Series Analysis for APAC Consumer Sales (Auto ARIMA) #####

autoarima <- GSS_APAC_Consumer_IN$Agg_Sales%>%auto.arima()
autoarima
par(mar=c(1,1,1,1))
tsdiag(autoarima)
par(mar=c(5.1,4.1,4.1,2.1))
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Checking if residual series is white noise
timeser <- ts(GSS_APAC_Consumer_IN$Agg_Sales)
residual_autoarima <- timeser - fitted(autoarima)
adf.test(residual_autoarima, alternative = "stationary")
#data:  residual_autoarima
#Dickey-Fuller = -4.2563, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
kpss.test(residual_autoarima)
#data:  residual_autoarima
#KPSS Level = 0.042734, Truncation lag parameter = 1, p-value = 0.1

#Residual Time Series is Stationary
#Evaluating model using MAPE
fcast_autoarima <- predict(autoarima, n.ahead = 6)
MAPE_autoarima <- accuracy(fcast_autoarima$pred, GSS_APAC_Consumer_OUT$Agg_Sales)[5]
MAPE_autoarima
# 27.68952

#MAPE value for ARMA (manual) is 31.07 and for Auto Arima it is 27.68
#Hence Auto Arima is better for forecasting in this case

#Forecasting for 6 months
GSS_APAC_Consumer$Agg_Sales%>%arima(order = c(2,1,0))%>%predict(n.ahead = 6)
#Time Series:
#  Start = 49 
#End = 54 
#Frequency = 1 
#[1] 71649.02 69216.51 68565.57 69356.25 69031.66 69072.60

##### Time Series Analysis for APAC Consumer Quantity (ARMA) #####

Total_Consumer_Quantity_APAC_TS <- ts(GSS_APAC_Consumer$Agg_Qty)
plot(Total_Consumer_Quantity_APAC_TS)

APAC_Quantity_IN_TS <- ts(GSS_APAC_Consumer_IN$Agg_Qty)
plot(APAC_Quantity_IN_TS)

#Moving average smoothing
w <- 1
smoothedseries <- stats::filter(APAC_Quantity_IN_TS, filter = rep(1/(2*w+1), (2*w+1)), 
                                method = "convolution", sides = 2)

#Smoothing left side of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for(i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing Right side of the Time series

n <- length(APAC_Quantity_IN_TS) 
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for(i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plotting
timevals_IN <- GSS_APAC_Consumer_IN$Month
lines(smoothedseries, col = "blue", lwd = 2)

smootheddf <- as.data.frame(cbind(timevals_IN, as.vector(smoothedseries)))
colnames(smootheddf) <- c("Month", "Quantity")

#Modelling seasonality using sinusoidal function
lmfit1 <- lm(Quantity~ sin(0.5*Month) + cos(0.5*Month) + Month, data = smootheddf)
summary(lmfit1)
#Multiple R-squared:  0.7244,	Adjusted R-squared:  0.7027
globalpred_1 <- predict(lmfit1, Month = timevals_IN)
summary(globalpred_1)
plot(APAC_Quantity_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_1, col = "red", lwd = 2)

lmfit2 <- lm(Quantity~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2) + Month, data = smootheddf)
summary(lmfit2)
#Multiple R-squared:  0.8544,	Adjusted R-squared:  0.819 
globalpred_2 <- predict(lmfit2, Month = timevals_IN)
summary(globalpred_2)
plot(APAC_Quantity_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_2, col = "red", lwd = 2)

lmfit3 <- lm(Quantity~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3) + Month, data = smootheddf)
summary(lmfit3)
#Multiple R-squared:  0.8678,	Adjusted R-squared:  0.8193 
globalpred_3 <- predict(lmfit3, Month = timevals_IN)
summary(globalpred_3)
plot(APAC_Quantity_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_3, col = "red", lwd = 2)

lmfit4 <- lm(Quantity~ sin(0.5*Month) * poly(Month,4) + cos(0.5*Month) * poly(Month,4) + Month, data = smootheddf)
summary(lmfit4)
#Multiple R-squared:  0.8825,	Adjusted R-squared:  0.8216 
globalpred_4 <- predict(lmfit4, Month = timevals_IN)
summary(globalpred_4)
plot(APAC_Quantity_IN_TS)
lines(smoothedseries, col = "blue", lwd = 2)
lines(timevals_IN, globalpred_4, col = "red", lwd = 2)

#Looking at the RSquarred values and model complexity. lmfit2 seems to be most appropriate model in this case
# Checking for locally predictabke series

local_pred <- APAC_Quantity_IN_TS - globalpred_2
plot(local_pred, col = "red", type = "l")
acf(local_pred)
acf(local_pred, type = "partial")
armafit <- auto.arima(local_pred)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit

#Checking if residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary") 
#data:  resi
#Dickey-Fuller = -7.1589, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
kpss.test(resi)
#data:  resi
#KPSS Level = 0.019377, Truncation lag parameter = 1, p-value = 0.1

#Residual Series is Stationary
#evaluating using MAPE

outdata <- GSS_APAC_Consumer_OUT
timevals_OUT <- outdata$Month
globalpred_OUT <- predict(lmfit2, data.frame(Month = timevals_OUT))
fcast <- globalpred_OUT

Mape_1 <- accuracy(fcast, outdata$Agg_Qty)[5]
Mape_1
# 33.79438

#Plotting prediction along with original values

prediction_1 <- c(ts(globalpred_2), ts(globalpred_OUT))
plot(Total_Consumer_Quantity_APAC_TS, col = "black")
lines(prediction_1, col = "red")

##### Time Series Analysis for APAC Consumer Quantity (Auto ARIMA) #####

autoarima <- GSS_APAC_Consumer_IN$Agg_Qty%>%auto.arima()
autoarima
par(mar=c(1,1,1,1))
tsdiag(autoarima)
par(mar=c(5.1,4.1,4.1,2.1))
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Checking if residual series is white noise
timeser <- ts(GSS_APAC_Consumer_IN$Agg_Qty)
residual_autoarima <- timeser - fitted(autoarima)
adf.test(residual_autoarima, alternative = "stationary")
#data:  residual_autoarima
#Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
kpss.test(residual_autoarima)
#data:  residual_autoarima
#KPSS Level = 0.031535, Truncation lag parameter = 1, p-value = 0.1

#Residual Time Series is Stationary
#Evaluating model using MAPE
fcast_autoarima <- predict(autoarima, n.ahead = 6)
MAPE_autoarima <- accuracy(fcast_autoarima$pred, GSS_APAC_Consumer_OUT$Agg_Qty)[5]
MAPE_autoarima
# 26.24458

#MAPE value for ARMA (manual) is 33.79438 and for Auto Arima it is 26.24458
#Hence Auto Arima is better for forecasting in this case

#Forecasting for next 6 months
GSS_APAC_Consumer$Agg_Qty %>% arima(order=c(2,1,0)) %>% predict( n.ahead = 6)
#Time Series:
#  Start = 49 
#End = 54 
#Frequency = 1 
#[1] 842.6532 837.2847 837.8234 838.6757 838.1635 838.2291

