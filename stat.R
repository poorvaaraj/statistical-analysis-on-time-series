library(quantmod)
library(stats)
library(forecast)
library(nortest)
library(xts)
library(data.table)

#task1: Downloading data 
getSymbols("^GSPC", from ="2009-01-01", to="2020-12-31")

head(GSPC)
tail(GSPC)

Open <- Op(GSPC)
High <- Hi(GSPC)
Low <- Lo(GSPC)
Close <- Cl(GSPC)
Volume <- Vo(GSPC)
AdjClose <- Ad(GSPC)

#task2: Transforming series into log returns
ret_a = quantmod::periodReturn(GSPC$GSPC.Adjusted,period="daily", type="log")
head(ret_a)


#task3 Examining Acf and PACF function
acf(ret_a)

pacf(ret_a)

#task4 Performing the Ljung-Box test and describing the test-hypothesis 
Box.test(ret_a, type = "Ljung-Box") 
#pvalue should be more than 0.05 
##(P_value is 7.772e-16)

#task5 stationarity testing
#The null hypothesis for adf tests is that the data are non-stationary. We want to REJECT the null hypothesis for this test, so we want a p-value of less that 0.05 (or smaller).

tseries::adf.test(ret_a)
#p value <0.05 means its a stationary data

#task6
#normality testing
lillie.test((ret_a))
ad.test(as.numeric((ret_a)))
sf.test(ret_a)



GSPC_arm1 <- auto.arima(ret_a)
GSPC_arm1
#sigma value should be less - (0.0001329)
#and AIC value should be lower (-18381.11), less parameters ARIMA(2,0,0)
GSPC_arm1$coef

confint(GSPC_arm1, level=0.98)
#confint values intercept or crosses 0; count(1)
#GSPC_arm2 <- auto.arima(GSPC$GSPC.Adjusted) #bad fit
#GSPC_arm2
checkresiduals(GSPC_arm1)
pacf(ret_a)
#more than 0.05 ; its 7.464 and lines were u blue lines
GSPC_ARM210 <- arima(ret_a, order = c(2,1,0))
GSPC_ARM210 
#sigma-(0.0001772) and AIC -17504; ARIMA(2,1,0)
confint(GSPC_ARM210, level=0.98)
#Confint does not cross 0
checkresiduals(GSPC_ARM210)
#lines were above to blue line and p value was 2.2e
GSPC_ARM100 <- arima(ret_a, order = c(1,0,0))
GSPC_ARM100
#sigma value-(0.0001335) and aic- (-18366.82)
GSPC_ARM100$coef
confint(GSPC_ARM100, level=0.98)
#confint value doesnt cross 
checkresiduals(GSPC_ARM100)
#p value  lines were similarly under blue line like 2,0,0 , pval 8.771e

GSPC_ARM111 <- arima(ret_a, order = c(1,1,1))
GSPC_ARM111 #s-(0.0001335, AIC- (-18351.45))
confint(GSPC_ARM111, level=0.98)
#doesnt intercept
checkresiduals(GSPC_ARM111)
#pvalue(8.771e)

GSPC_ARM011 <- arima(ret_a, order = c(0,1,1))
GSPC_ARM011
confint(GSPC_ARM011, level=0.98)
checkresiduals(GSPC_ARM100)

#Best suited yet 1,0,0

