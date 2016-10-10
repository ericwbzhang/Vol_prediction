
rm(list=ls())

library(lubridate)
setwd('/Users/Eric/Documents/Vol_prediction')
SPXdata<- read.csv('SPX_rvol.csv')
rownames(SPXdata)<- ymd( SPXdata$DATE)
SPXdata$SPX2.rvol<- sqrt(SPXdata$SPX2.rv)

## preliminary analysis for SPX2.rv 
acf(SPXdata$SPX2.rvol)
acf(SPXdata$SPX2.r)
library(tseries)
adf.test(SPXdata$SPX2.rvol)
kpss.test(SPXdata$SPX2.rvol, null = 'Level')
# ADF and KPSS test get both rejected. It is a flag of LRD.
plot(SPXdata$DATE, SPXdata$SPX2.rvol, type = 'n')
lines( SPXdata$DATE, SPXdata$SPX2.rvol)


## Fact:
## 1. SPX2.r shows week auto corr and SPX2.rv shows strong LRD
## 2. SPX2.r accepts the hypo of stationary 
Box.test(SPXdata$SPX2.r, lag =1 , type= 'Ljung-Box')
shapiro.test(SPXdata$SPX2.r)
plot(density(SPXdata$SPX2.r))
library(moments)
kurtosis(SPXdata$SPX2.r)
## model SPX2.r by t dist
library(MASS)
t.pars<-fitdistr(SPXdata$SPX2.r, densfun = 't', start= list(m=0,s= 0.01 ,df= 1))
plot(density(SPXdata$SPX2.r), xlim= c(-.1,.1), ylim=c(-1, 55) )
par(new=TRUE)
curve( dt( (x- t.pars$estimate[1])/t.pars$estimate[2], 
           df= t.pars$estimate[3])/ t.pars$estimate[2],
       from= -.1,
       to= .1, xlim= c(-.1,.1), 
       ylim=c (-1, 55),
       col= 'green')

library(forecast)
auto.arima(SPXdata$SPX2.r)
## auto.arima indicates ARMA(2,0) model for SPX2.r
## So apply standard ARMA(2,0)-GARCH(1,1) model for SPX2.r
library(rugarch)
sgarch<- ugarchspec(mean.model = list( armaOrder= c(2,0),
                                       include.mean= TRUE),
                    distribution.model = 'std')
sgarch_fitted<- ugarchfit(sgarch, data =  SPXdata$SPX2.r)
whole_len<- length( SPXdata$SPX2.r)
burning_len<- 1000
forecast_len<- whole_len- burning_len
ret<- data.frame(SPX2.r= SPXdata$SPX2.r)
rownames(ret)<- rownames(SPXdata)
# rolling estimation and forecasting
sgarch_roll<- ugarchroll(spec = sgarch, 
                         data= ret,
                         n.ahead = 1,
                         forecast.length =forecast_len,
                         refit.every = 5)

## plot the predicted vol and realized vol(5-min estimator by OxManLab)
library(ggplot2)
library(reshape2)
x<- tail( ymd( SPXdata$DATE), forecast_len)
realized_vol<- sqrt(tail( SPXdata$SPX2.rv, forecast_len))
sgarch.predicted_vol<- sgarch_roll@forecast$density[,'Sigma']
tmp_df<- data.frame(x, realized_vol, sgarch.predicted_vol)
sgarch.g<- ggplot(melt(tmp_df, id.var= 'x'), aes(x=x, y= value))+
  geom_line(aes(colour= variable, group= variable))+
  scale_color_manual(values = c('grey', 'red'))+
  ylab('daily volatility')+
  xlab('date index')+
  theme(legend.title= element_blank())+
  ggtitle('ARMA(2,0)-GARCH(1,1) vol prediction')

jpeg('ARMAGARCH.jpeg')
sgarch.g
dev.off()


## compute the squared error
sgarch.MSE<- mean(( realized_vol- sgarch.predicted_vol)^2)
summary( (realized_vol-sgarch.predicted_vol)^2)
sgarch.MSE
cor( realized_vol, sgarch.predicted_vol)
summary( lm( realized_vol~ sgarch.predicted_vol))

## save sgarch model
sgarch_model<- list()
sgarch_model$spec<- sgarch
sgarch_model$roll<- sgarch_roll
sgarch_model$plot<- sgarch.g
sgarch_model$MSE<- sgarch.MSE
sgarch_model$roll.pred<- tmp_df
save(sgarch_model, file = 'sgarch_model')

## EGARCH estimation
egarch<- ugarchspec(variance.model = list( model='eGARCH'),
                    mean.model = list( armaOrder= c(2,0)),
                    distribution.model = 'std')
egarch_fitted<- ugarchfit(egarch, data= SPXdata$SPX2.r)
egarch_roll<- ugarchroll( spec = egarch,
                          data= ret,
                          n.ahead = 1,
                          forecast.length = forecast_len,
                          refit.every = 5)

x<- tail( ymd( SPXdata$DATE), forecast_len)
realized_vol<- sqrt(tail( SPXdata$SPX2.rv, forecast_len))
egarch.predicted_vol<- egarch_roll@forecast$density[,'Sigma']
tmp_df<- data.frame(x, realized_vol, egarch.predicted_vol)
egarch.g<- ggplot(melt(tmp_df, id.var= 'x'), aes(x=x, y= value))+
  geom_line(aes(colour= variable, group= variable))+
  scale_color_manual(values = c('grey', 'blue'))+
  ylab('daily volatility')+
  xlab('date index')+
  theme(legend.title= element_blank())+
  ggtitle('ARMA(2,0)-EGARCH(1,1) vol prediction')
jpeg('ARMAEGARCH.jpeg')
egarch.g
dev.off()

egarch.MSE<- mean(( realized_vol- egarch.predicted_vol)^2)
summary( (realized_vol-egarch.predicted_vol)^2)
egarch.MSE
cor( realized_vol, egarch.predicted_vol)
summary( lm( realized_vol~ egarch.predicted_vol))

egarch_model<- list()
egarch_model$spec<- egarch
egarch_model$roll<- egarch_roll
egarch_model$plot<- egarch.g
egarch_model$MSE<- egarch.MSE
egarch_model$roll.pred<- tmp_df
save(egarch_model, file= 'egarch_model')

