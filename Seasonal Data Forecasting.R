#Assignment 2
library(forecast)
library(fpp)
library(ggplot2)
library(gridExtra)
#plot the time series data
ts_all <- a10
ggplot(ts_all, aes(as.Date(ts_all), as.matrix(ts_all))) + 
  geom_line(colour = "red")+
  ylab("Sales") + 
  xlab("Date") + 
  ggtitle("Monthly anti-diabetic drug sales")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), axis.title = element_text(colour = 'black'), legend.text=element_text(), legend.title=element_text(), legend.key = element_rect(colour = "black"))


temp <- decompose(ts_all)
tst <- data.frame(actual = as.matrix(temp$x), date = as.Date(temp$x), seasonal = as.matrix(temp$seasonal),
                  trend = as.matrix(temp$trend), random = as.matrix(temp$random), type = temp$type)

a <- ggplot(tst, aes(tst[,2], tst[,1]))+
  geom_line()+
  ylab("observed") + 
  xlab("") + 
  ggtitle(paste("Decomposition of",  tst$type, "time series", sep=" "))+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"), 
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))



temp <- decompose(ts_all)
tst <- data.frame(actual = as.matrix(temp$x), date = as.Date(temp$x), seasonal = as.matrix(temp$seasonal),
                  trend = as.matrix(temp$trend), random = as.matrix(temp$random), type = temp$type)

a <- ggplot(tst, aes(tst[,2], tst[,1]))+
  geom_line()+
  ylab("observed") + 
  xlab("") + 
  ggtitle(paste("Decomposition of",  tst$type, "time series", sep=" "))+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"), 
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))

b <- ggplot(tst, aes(tst[,2], tst[,3]))+
  geom_line()+
  ylab("seasonal") + 
  xlab("") + 
  ggtitle("")+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"),
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))

c <- ggplot(tst, aes(tst[,2], tst[,4]))+
  geom_line()+
  ylab("trend") + 
  xlab("") + 
  ggtitle("")+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"),
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))

d <- ggplot(tst, aes(tst[,2], tst[,5]))+
  geom_line()+
  ylab("random") + 
  xlab("") + 
  ggtitle("")+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"),
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))

grid.arrange(a,b,c,d, ncol = 1)


tst1 <- data.frame(cbind(as.matrix(ts_all), as.matrix(cycle(ts_all))))

ggplot(tst, aes(as.factor(tst[,2]), tst[,1])) +
  ylab("Sales") + 
  xlab("Month") + 
  ggtitle("Sales for each month") + 
  stat_boxplot(geom ='errorbar')+ 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(aes(colour=tst[,1]), position = position_jitter(width = 0.2)) +
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Sales")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), axis.title = element_text(colour = 'black'), legend.text=element_text(), legend.title=element_text(), legend.key = element_rect(colour = "black"))

tst <- data.frame(cbind(as.matrix(ts_all), as.matrix(cycle(ts_all))))
splitTrainXvat <- function(tser, perc_train){
  ntrain <- floor(length(as.vector(tser)) * perc_train)
  nval <- length(as.vector(tser)) - ntrain
  
  ttrain <- ts(as.vector(tser[1:ntrain]), start = start(tser), frequency = frequency(tser))
  tval <- ts(as.vector(tser[ntrain + 1:nval]), start = end(ttrain) + deltat(tser), 
             frequency = frequency(tser))
  
  stopifnot(length(ttrain) == ntrain)
  stopifnot(length(tval) == nval)
  
  list(ttrain, tval)
}

data <- splitTrainXvat(ts_all, 0.80)
ts_train <- data[[1]]
ts_val <- data[[2]]


#see model parameters
mod.ar <- ar(ts_train, method = "yule-walker", lambda=0)
mod.reg <- tslm(ts_train ~ trend + season, lambda=0)
mod.hw.mul <- HoltWinters(log(ts_train), seasonal = "mul")
mod.hw.add <- HoltWinters(log(ts_train), seasonal = "add")
mod.arima <- auto.arima(ts_train, max.p=2, max.q=2,
                        max.P=2, max.Q=2, max.d=2, max.D=2, allowdrift = TRUE,
                        stepwise=FALSE, approximation=FALSE)
mod.arima.boxcox <- auto.arima(ts_train, max.p=2, max.q=2,
                               max.P=2, max.Q=2, max.d=2, max.D=2, allowdrift = TRUE,
                               stepwise=FALSE, approximation=FALSE, lambda=0)
mod.ets <- ets(ts_train, model="ZZZ", lambda=0)
mod.tbats <- tbats(ts_train, use.box.cox=TRUE)

