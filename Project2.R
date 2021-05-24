#Assignment 2
library(forecast)
library(fpp)
#------2	Plot Original Time Series Data
data<-read.table("/Users/sampsonliu/Desktop/drug.txt",header=TRUE,sep=",") 
data<- ts(data$value,start=c(1991,7),frequency = 12)
plot.ts(data,xlab="Year",ylab="Sales",main="Monthly anti-diabetic drug sales") #plot original time series data

#------3	Box-Cox Transformation
lambda <- BoxCox.lambda(data) # conduct transformation 
BoxCox_data<-BoxCox(data,lambda)
plot.ts(BoxCox_data,xlab="Year",ylab="Sales",main="Monthly anti-diabetic drug sales after Box-Cox transformation") 

#------4	Seasonal Differencing
d_data=diff(BoxCox_data,lag=12)
length(d_data)
plot(d_data,xlab="Year",ylab="Sales",main="Monthly anti-diabetic drug sales after seasonal differencing")
lines(d_data,type="l")
#---ACF
acf(d_data,lag.max = 50, xlab="Year",ylab="Sales",main="Monthly anti-diabetic drug sales after ACF")

#------5	Unit Root Tests
library(tseries)
adf.test(data) #p-value = 0.01, non-stationary 
adf.test(d_data)#p-value = 0.01776, stationary 
kpss.test(data)#p-value = 0.01, non-stationary
kpss.test(d_data) #p-value = 0.1, level stationary

#Preprocessing 
#build a function to split data 
splitTrainXvat <- function(tser, perc_train)
{
  ntrain <- floor(length(as.vector(tser)) * perc_train)
  nval <- length(as.vector(tser)) - ntrain
  
  ttrain <- ts(as.vector(tser[1:ntrain]), start = start(tser), frequency = frequency(tser))
  tval <- ts(as.vector(tser[ntrain + 1:nval]), start = end(ttrain) + deltat(tser), 
             frequency = frequency(tser))
  
  stopifnot(length(ttrain) == ntrain)
  stopifnot(length(tval) == nval)
  
  list(ttrain, tval)
}
datasplit <- splitTrainXvat(data, 0.80)
train <- datasplit[[1]]
test<- datasplit[[2]]
lambda_train <- BoxCox.lambda(train) 
BoxCox_train<-BoxCox(train,lambda_train)
#------6	Fitting in Different Models
#---6.1	Linear regression model with seasonal and trend features
regression <- tslm(train ~ trend + season, lambda=lambda_train)

#---6.2	Holt-Winters model with additive seasonal components 
HW_additive<- HoltWinters(BoxCox_train, seasonal = "add")

#---6.3	Holt-Winters model with multiplicative seasonal components
HW_multiplicative<- HoltWinters(BoxCox_train, seasonal = "mul")

#---6.4 Seasonal autoregressive integrated moving average model 
auto_arima <- auto.arima(train, max.p=5, max.q=5,max.P=5, max.Q=5, d=0, D=1, allowdrift = TRUE,stepwise=FALSE, approximation=FALSE, lambda=0)
tsdiag(auto_arima) #diagnostic checking

#------Prediction 
pred.regression<- forecast(regression, h = length(test))$mean
pred.HW_additive <- InvBoxCox(forecast(HW_additive, h = length(test))$mean, lambda_train)
pred.HW_multiplicative <- InvBoxCox(forecast(HW_multiplicative, h = length(test))$mean, lambda_train)
pred.auto_arima <- forecast(auto_arima, h =length(test))$mean

#---Linear regression model with seasonal and trend features
plot(train,xlab="Year",ylab="Sales", xlim=c(1992,2008), ylim =c(0,40),type="l" ,main=" Monthly anti-diabetic drug sales prediction " )
lines(fitted(forecast(regression, h = length(test))), col="red", lty=2) #pred.regression
lines(test,col="blue")
lines(pred.regression, col="red")
legend("topleft", lty=c(1,1,2), col=c("black","blue","red"),c("Taining Data","Testing Data","Linear regression model with seasonal and trend features"))

#---Holt-Winters model with additive seasonal components 
plot(train,xlab="Year",ylab="Sales", xlim=c(1992,2008), ylim =c(0,40),type="l" ,main=" Monthly anti-diabetic drug sales prediction " )
lines(InvBoxCox(fitted(forecast(HW_additive, h = length(test))), lambda_train), col="green", lty=2) #pred.regression
lines(test,col="blue")
lines(pred.HW_additive, col="green")
legend("topleft", lty=c(1,1,2), col=c("black","blue","green"),c("Taining Data","Testing Data","Holt-Winters model with additive seasonal components "))

#---Holt-Winters model with multiplicative seasonal components
plot(train,xlab="Year",ylab="Sales", xlim=c(1992,2008), ylim =c(0,40),type="l" ,main=" Monthly anti-diabetic drug sales prediction " )
lines(InvBoxCox(fitted(forecast(HW_multiplicative, h = length(test))), lambda_train), col="orange", lty=2) #pred.regression
lines(test,col="blue")
lines(pred.HW_multiplicative, col="orange")
legend("topleft", lty=c(1,1,2), col=c("black","blue","orange"),c("Taining Data","Testing Data","Holt-Winters model with multiplicative seasonal components "))

#---Seasonal autoregressive integrated moving average model 
plot(train,xlab="Year",ylab="Sales", xlim=c(1992,2008), ylim =c(0,40),type="l" ,main=" Monthly anti-diabetic drug sales prediction " )
lines(fitted(forecast(auto_arima, h =length(test))), col="violet", lty=2) #pred.regression
lines(test,col="blue")
lines(pred.HW_multiplicative, col="violet")
legend("topleft", lty=c(1,1,2), col=c("black","blue","violet"),c("Taining Data","Testing Data","Seasonal autoregressive integrated moving average model"))

#------7  Evaluation 
eval_table<-rbind(accuracy(pred.regression,data),
                  accuracy(pred.HW_additive,data),
                  accuracy(pred.HW_multiplicative,data),
                  accuracy(pred.auto_arima,data))
row.names(eval_table) <- c("Linear regression model with seasonal and trend features",
                    "Holt-Winters model with additive seasonal components",
                    "Holt-Winters model with multiplicative seasonal components",
                    "Seasonal autoregressive integrated moving average model ")
eval_table<- data.frame(eval_table)
eval_table <- eval_table[,(names(eval_table) %in% c("RMSE", "MAE", "MAPE", "Theil.s.U"))]
round(eval_table[order(eval_table$RMSE),], 5)


