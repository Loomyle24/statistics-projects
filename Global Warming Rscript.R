#############################################MATH 456 project R code####################################################################

###########preliminary work###########

#setting the correct working directory
setwd("C:/Users/loomy/Desktop/global warming forecasting")

#loading the Excel file and visualizing the data
install.packages("readxl")
library("readxl")
pdata<-read_excel("pdata.xlsx")
View(pdata)


#Converting the data into a time series
pdata_ts <-ts(pdata$Anomaly,start = min(pdata$Year),end = max(pdata$Year),frequency=1)

#plot of the time series
plot(pdata_ts, xlab = "Year", ylab="T-anomalies", main = "Temperature anomalies from 1923-2022")

#adding a significant variable to the model
ys <- pdata$Year^2

#Estimation and elimination of trend via least squares estimate

# Fit a linear regression model to capture the trend
model <- lm(Anomaly ~ pdata$Year+ys, data = pdata)
summary(model)

# Call:
#   lm(formula = Anomaly ~ pdata$Year + ys, data = pdata)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.219041 -0.082482 -0.002362  0.073538  0.314220 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.904e+02  5.752e+01   8.525 2.03e-13 ***
#   pdata$Year  -5.077e-01  5.833e-02  -8.703 8.41e-14 ***
#   ys           1.314e-04  1.479e-05   8.885 3.42e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1102 on 97 degrees of freedom
# Multiple R-squared:  0.8972,	Adjusted R-squared:  0.8951 
# F-statistic: 423.2 on 2 and 97 DF,  p-value: < 2.2e-16



#estimation the trend
trend <- fitted(model)
#Removing the trend from the time series
detrended_ts <- pdata_ts - trend

#Plot of the original time series and the detrended series
par(mfrow=c(1,2))
plot(pdata_ts, xlab = "Year", ylab="T-anomalies", main = "Temperature anomalies from 1923-2022",type ="l")
#abline(a = coef(model)[1], b = coef(model)[2], col ="red")#Trend line
lines(ts(predict(model), start=start(pdata_ts), frequency=frequency(pdata_ts)), col="red")
plot(detrended_ts, xlab = "Year", ylab="T-anomalies", main = "Detrended Temperature anomalies from 1923-2022",type ="l",col ="blue")

#Assessing the residuals of the regression
#obtaining the model residuals
m_residuals <- resid(model)


checkresiduals(detrended_ts)
# Ljung-Box test
# 
# data:  Residuals
# Q* = 48.633, df = 10, p-value = 4.755e-07
# 
# Model df: 0.   Total lags used: 10


#ACF plot of the residuals
acf(detrended_ts,lag.max=50, main="ACF plot of the residuals")

### Least squares residuals normality
library("nortest")
library("forecast")
library("randtests")
ad.test(detrended_ts) #Anderson-Darling test
# Anderson-Darling normality test
# 
# data:  detrended_ts
# A = 0.47803, p-value = 0.2314

shapiro.test(detrended_ts) #Shapiro-Wilk test
# Shapiro-Wilk normality test
# 
# data:  detrended_ts
# W = 0.97973, p-value = 0.1267

par(mfrow=c(1,2))
qqnorm(detrended_ts) #Q-Q plot
qqline(detrended_ts,col="red")
hist(detrended_ts, freq=FALSE, breaks=20)




#Least squares method din't achieve stationarity we will use differencing
#Removing trend and seasonality through differencing to achieve stationarity
diff1 = diff(pdata_ts, lag = 1)
diff2 = diff(diff1,lag = 1)

#Differenced time series plot showing the first and second difference
par(mfrow = c(1, 2))
plot(diff1, xlab = "Year", ylab = "First Differenced T-anomalies", 
     main = "First Differenced Temperature Anomalies from 1923-2022", type = "l")
plot(diff2, xlab = "Year", ylab = "Second Differenced T-anomalies", 
     main = "Second Differenced Temperature Anomalies from 1923-2022", type = "l")

#ACF and PACF plots
par(mfrow=c(1,2))
acf(diff2, lag.max=30)
pacf(diff2, lag.max=30)


############################ARMA model########################################3
### Models based on ARMA(p,q) with different information criteria.
### allowmean = FALSE means that the mean of the process is assumed to be zero.

arma_aicc=auto.arima(pdata_ts, d=2, D=0, max.p=5, max.q=5, 
                     max.P=0, max.Q=0,  stationary=TRUE, seasonal=FALSE, ic="aicc", allowmean=FALSE)
arma_aicc
# arma_aicc
# Series: pdata_ts 
# ARIMA(2,0,1) with zero mean 
# 
# Coefficients:
#   ar1     ar2     ma1
# 0.0223  0.9381  0.8858
# s.e.  0.0469  0.0455  0.0653
# 
# sigma^2 = 0.01134:  log likelihood = 81.9
# AIC=-155.8   AICc=-155.38   BIC=-145.38


arma_aic=auto.arima(pdata_ts, d=2, D=0, max.p=5, max.q=5, 
                    max.P=0, max.Q=0,  stationary=TRUE, seasonal=FALSE, ic="aic", allowmean=FALSE)
arma_aic

# arma_aic
# Series: pdata_ts 
# ARIMA(2,0,1) with zero mean 
# 
# Coefficients:
#   ar1     ar2     ma1
# 0.0223  0.9381  0.8858
# s.e.  0.0469  0.0455  0.0653
# 
# sigma^2 = 0.01134:  log likelihood = 81.9
# AIC=-155.8   AICc=-155.38   BIC=-145.38


arma_bic=auto.arima(pdata_ts, d=2, D=0, max.p=5, max.q=5, 
                    max.P=0, max.Q=0,  stationary=TRUE, seasonal=FALSE, ic="bic", allowmean=FALSE)
arma_bic
# arma_bic
# Series: pdata_ts 
# ARIMA(1,0,0) with zero mean 
# 
# Coefficients:
#   ar1
# 0.9758
# s.e.  0.0213
# 
# sigma^2 = 0.01188:  log likelihood = 78.75
# AIC=-153.51   AICc=-153.39   BIC=-148.3

#Based on the model estimation, we notice that all the models perform similarly
#I will be choosing arma bic because its the simpliest model


### Residuals diagnostics with ARMA BIC as the model
checkresiduals(arma_bic)

# Ljung-Box test
# 
# data:  Residuals from ARIMA(1,0,0) with zero mean
# Q* = 45.081, df = 9, p-value = 8.912e-07
# 
# Model df: 1.   Total lags used: 10

arma_res=arma_bic$residuals
arma_res


###Arma model residual diagnostic

library("nortest")
library("forecast")
library("randtests")

par(mfrow=c(1,2))
acf(arma_res, lag.max=30)
pacf(arma_res, lag.max=30)

### Portmanteau test
pvalues = double(10)
for(lag in 1:10){
  pvalues[lag] = Box.test(arma_res, lag=lag)$p.value
}
par(mfrow=c(1,1))
plot(pvalues, main="P-Values of the Portmanteau test", xlab="Lag", ylab="P-Value",ylim=c(0,1))
abline(h=0.05, lty=2, col="blue")

### Rank test
rank_test <- rank.test(arma_res, alternative="two.sided")
rank_test

# Mann-Kendall Rank Test
# 
# data:  arma_res
# statistic = 1.2925, n = 100, p-value = 0.1962
# alternative hypothesis: trend


### Normality
ad.test(arma_res) #Anderson-Darling test
# Anderson-Darling normality test
# 
# data:  arma_res
# A = 0.3948, p-value = 0.3667


shapiro.test(arma_res) #Shapiro-Wilk test
# Shapiro-Wilk normality test
# 
# data:  arma_res
# W = 0.98481, p-value = 0.3075

#Normal probability plots
par(mfrow=c(1,2))
qqnorm(arma_res) #Q-Q plot
qqline(arma_res)
hist(arma_res, freq=FALSE, breaks=20)

##########ARMA model forecasting
### Forecasting (1 to 12 steps ahead)
ar_forecasts=predict(arma_bic, n.ahead=12)
ar_forecasts$pred#Obtaining the predicted values

# Time Series:
#   Start = 2023 
# End = 2032 
# Frequency = 1 
# [1] 0.8879744 0.8664819 0.8455096 0.8250449 0.8050755 0.7855895 0.7665751 0.7480210 0.7299159
# [10] 0.7122490

#plot of the forecasted time series.
forecasts=predict(arma_bic)
autoplot(forecast(arma_bic, h=12))





###################Singular Spectrum Analysis model##################

library("Rssa")

#####Singular spectrum analysis
n =length(pdata$Anomaly)

# Decomposition stage
ssa_pdata= ssa(pdata_ts, kind="1d-ssa") 

# Diagnostics for grouping
plot(ssa_pdata) # Eigenvalues
plot(ssa_pdata, type = "vectors") # Eigenvectors
plot(ssa_pdata, type = "paired") # Pairs of eigenvectors
plot(wcor(ssa_pdata)) # w-correlation matrix plot

# A list containing all the groups
group_pdata = list(c(1,2,3,4,9), c(5,6),c(7,8)) #Trend + Seasonalities with two different periods
num_components = length(group_pdata)
recon_pdata = reconstruct(ssa_pdata, groups = group_pdata)

#Finding periodicities
parestimate(ssa_pdata, list(c(5,6)), method = "pairs")$periods #8.476165
parestimate(ssa_pdata, list(c(7,8)), method = "pairs")$periods #3.623777

#Reconstruction stage
par(mfrow=c(1,3))
main_vector =c("Trend", "Periodicities #1", "Periodicities #2")
for(i in 1:num_components){
  plot(recon_pdata[[i]], main = main_vector[i])
}

# Reconstructed deterministic pattern
deterministic = rep(0,n)
for(i in 1:num_components){
  deterministic = deterministic + recon_pdata[[i]]
}

#Plot of original time series and the deterministic patterns
plot(cbind(pdata_ts, deterministic), plot.type ="single", col=c("black","red"),
     main="GLobal T-anomaly data", lwd=2, xlab="Year", ylab="Temperature anomalies")
legend("topleft", legend=c("Original","Deterministic"),
       col=c("black", "red"), lwd=2, cex=1.2, bg="lightblue")


# Forecasting for the rssa
n_ahead = 12 # Number of forecasts. 
for_pdata_ssa = rforecast(ssa_pdata, groups = group_pdata, len = n_ahead, only.new = FALSE)
for_pdata = rep(0,(n+n_ahead))
for(i in 1:num_components){
  for_pdata = for_pdata + for_pdata_ssa[[i]]
}

#plot of the forecast
plot(cbind(pdata_ts,for_pdata),plot.type ="single", col=c("black","red"),
     main="Forecasted T_anomalies based on SSA",lwd=2, xlab="Year", ylab="Temperature Anomalies")
legend("topleft",legend = c("Original", "Deterministic"),
       col=c("black","red"),lwd=2, cex=1.2, bg="lightblue")

#Theactual forecast values of the ssa model
for_pdata[(n+1):(n+n_ahead)]
# 
# 1.0015505 0.9453171 0.9674782 0.9940933 0.9784838 0.8714873 0.8937903 0.8898035 0.8565798
# 0.8183558 0.8912250 0.8577907


#SSA model residuals diagnostic
library("nortest")
library("forecast")
library("randtests")

#Obtaining the ssa residuals
res_ssa <-pdata_ts-deterministic

#ACF and PACF plot of ssa residuals
par(mfrow=c(1,2))
acf(res_ssa,lag.max = 30)
pacf(res_ssa,lag.max = 30)

### Portmanteau test
pvalues = double(10)
for(lag in 1:10){
  pvalues[lag] = Box.test(res_ssa, lag=lag)$p.value
}
par(mfrow=c(1,1))
plot(pvalues, main="P-Values of the Portmanteau test", xlab="Lag", ylab="P-Value",ylim=c(0,1))
abline(h=0.05, lty=2, col="blue")

### Rank test
rank_test <- rank.test(res_ssa, alternative="two.sided")
rank_test

# Mann-Kendall Rank Test
# 
# data:  res_ssa
# statistic = 0.25612, n = 100, p-value = 0.7979
# alternative hypothesis: trend

### Normality
ad.test(res_ssa) #Anderson-Darling test

# Anderson-Darling normality test
# 
# data:  res_ssa
# A = 0.49122, p-value = 0.2147


shapiro.test(res_ssa) #Shapiro-Wilk test

# Shapiro-Wilk normality test
# 
# data:  res_ssa
# W = 0.98803, p-value = 0.5106

#Normal probability plots
par(mfrow=c(1,2))
qqnorm(res_ssa) #Q-Q plot
qqline(res_ssa)
hist(res_ssa, freq=FALSE, breaks=20)

#Residuals checking
checkresiduals(res_ssa)

#########Forecasting evaluation
#Theactual forecast values of the ssa model
for_pdata[(n+1):(n+n_ahead)]

ar_forecasts





# Forecast validation with sliding window approach (with 50 windows)
# Root mean square errors (RMSEs) for point forecasts


num_window = 12
combined_models_error = matrix(NA, ncol=n_ahead, nrow=num_window)

#I might need to change pdata_ts with pdata$Anomaly in line 407
for(w in 1:num_window){
  pdata_window = pdata_ts[w:(w+n-1)]
  window_ssa = ssa(pdata_window, kind="1d-ssa") 	#SSA model fitting
  recon_pdata_window = reconstruct(window_ssa, groups = group_pdata)
  
  # Reconstructed deterministic pattern
  deterc_window = rep(0,n)
  for(i in 1:num_components){
    deterc_window = deterc_window + recon_pdata_window[[i]]
  }
  
  # Forecast of the deterministic pattern
  forecast_window_ssa = rforecast(window_ssa, groups = group_pdata, len = n_ahead, only.new = FALSE)
  forecast_window = rep(0,(n+n_ahead))
  for(i in 1:num_components){
    forecast_window = forecast_window + forecast_window_ssa[[i]]
  }
  
  # Residuals from the deterministic pattern
  res_det_window = pdata_window - deterc_window	
  
  ar_bic_window = Arima(res_det_window, order=c(1,0,0), seasonal=c(0,0,0), include.mean=FALSE, method="CSS", lambda=NULL)
  
  ar_res_window=ar_bic_window$residuals
  
  ### Forecasting (1 to 12 steps ahead)
  forecasts_window=predict(ar_bic_window, n.ahead=n_ahead)
  
  ### point forecasts (by adding the SSA and ARMA part) on the log scale
  point_forecasts_window = c(forecasts_window$pred) + forecast_window[(n+1):(n+n_ahead)]
  
  ### For RMSE calculation
  actual_window = pdata_ts[(w+n):(w+n-1+n_ahead)] #might need to replace wiht pdata$anomaly
  combined_models_error[w,]=point_forecasts_window - actual_window
}

### RMSE values for 1- to 12-step ahead forecasts on the log scale
RMSEs_model = sqrt(colMeans(combined_models_error^2))
RMSEs_model

#########calculating the RMSE

rmse_arma =sqrt(mean((pdata_ts -fitted(arma_bic))^2))

 rmse_arma #0.1084263

rmse_ssa = sqrt(mean((pdata_ts-deterministic)^2))

rmse_ssa #0.06382866




#Using the RMSE our ssa model is the best performing lewts use the MAriano Diebold terst to verify that
res_ssa #ssa forecast error
arma_res #arma model error


dm_test =dm.test(res_ssa,arma_res, alternative = "two.sided", h=12)
dm_test

# Diebold-Mariano Test
# 
# data:  res_ssaarma_res
# DM = -4.3257, Forecast horizon = 12, Loss function power = 2, p-value = 3.634e-05
# alternative hypothesis: two.sided

# There is enough statistical evidence to conclude that the Singular Spectrum Analysis (SSA) model provides
# significantly better forecast accuracy than the AutoRegressive Moving Average (ARMA) model over a 12-time point horizon