rm(list=ls())

setwd('/Users/Eric/Desktop/Vol_prediction')
SPXdata<- read.csv('SPX_rvol.csv')
library(lubridate)
rownames(SPXdata)<- ymd(SPXdata$DATE)
SPXdata$SPX2.rvol<- sqrt(SPXdata$SPX2.rv)

acf( SPXdata$SPX2.rvol, 1000)
library(tseries)
library(pracma)
hurstexp(SPXdata$SPX2.rvol)
adf.test(SPXdata$SPX2.rvol)
kpss.test(SPXdata$SPX2.rvol)
## acf of SPX2.rv decays slowly. It indicates LRD. 
## Model SPX2.rv by fractional ARIMA
library(rugarch)


## armaOrder should be (0,0) 
# pq<- c()
# aic<- c()
# for (p in 0:1){
#   for (q in 0:1){
#     arfima.model<- ugarchspec(mean.model = list(armaOrder= c(p, q),
#                                                 arfima=TRUE))
#     arfima.fitted<- ugarchfit( spec= arfima.model, 
#                                data= SPXdata$SPX2.rv)
#     pq<- c(pq, paste(c(p,q), collapse = ','))
#     aic<- c(aic, infocriteria(arfima.fitted)[1])
#   }
# }
# tmp.df<- data.frame(pq= pq, aic= aic)
# tmp.df

rVol<- data.frame(rvol= SPXdata$SPX2.rvol)
rownames(rVol)<- ymd(SPXdata$DATE)
arfima_egarch.model<- ugarchspec(mean.model = list(armaOrder= c(0,0),
                                            arfima=TRUE),
                          variance.model = list(model='eGARCH'))
arfima_egarch.fitted<- ugarchfit(spec = arfima_egarch.model, 
                          data= rVol)
arfima_egarch.fitted
library(ggplot2)
library(lubridate)
library(reshape2)

tmp_df<- data.frame(x= rownames(rVol),
                    realized= rVol$rvol, 
                    arfima_egarch.fitted= arfima_egarch.fitted@fit$fitted.values )
# g<- ggplot(melt(tmp_df, id.vars = 'x'), aes(x=x, y= value))+
#   geom_line(aes(colour= variable, group= variable))+ 
#   scale_color_manual(values= c('grey', 'red'))+
#   ylab('daily volatility')+
#   xlab('date index')+
#   theme(legend.title= element_blank())
# g
cor( tmp_df$realized, tmp_df$arfima_egarch.fitted)
summary( lm( tmp_df$realized~ tmp_df$arfima_egarch.fitted))

##residual analysis
resi<- arfima_egarch.fitted@fit$residuals
plot(resi, type= 'l')
acf(resi, 30)
adf.test(resi)
kpss.test(resi)
library(lmtest)

## rolling prediction
whole_len<- dim(rVol)[1]
burning_len<- 1000
forecast_len<- whole_len- burning_len
arfima_egarch.roll<- ugarchroll(arfima_egarch.model, 
                         data = rVol, 
                         n.ahead = 1,
                         forecast.length = forecast_len,
                         refit.every = 5)
library(ggplot2)
library(reshape2)
tmp_df<- data.frame(x= tail( ymd( SPXdata$DATE), forecast_len),
                    realized_vol= tail(rVol$rvol, forecast_len),
                    arfima_egarch.predicted_vol= arfima_egarch.roll@forecast$density[,'Mu'])
arfima_egarch.g<- ggplot(melt(tmp_df, id.var= 'x'), aes(x=x, y= value))+
  geom_line(aes(colour= variable, group= variable))+
  scale_color_manual(values = c('grey', 'purple'))+
  ylab('daily volatility')+
  xlab('date index')+
  theme(legend.title= element_blank())+
  ggtitle('Realized Vol ARFIMA(0,d,0)_EGARCH(1,1) rolling prediction')

jpeg('rVolARFIMAEGARCH.jpeg')
arfima_egarch.g
dev.off()

## compute the squared error
arfima_egarch.MSE<- mean(( tmp_df$realized_vol- tmp_df$arfima_egarch.predicted_vol)^2)
summary( (tmp_df$realized_vol-tmp_df$arfima_egarch.predicted_vol)^2)
arfima_egarch.MSE
cor( tmp_df$realized_vol, tmp_df$arfima_egarch.predicted_vol)
summary( lm( tmp_df$realized_vol~ tmp_df$arfima_egarch.predicted_vol))

arfima_egarch_model<- list()
arfima_egarch_model$spec<- arfima_egarch.model
arfima_egarch_model$roll<- arfima_egarch.roll
arfima_egarch_model$plot<- arfima_egarch.g
arfima_egarch_model$MSE<- arfima_egarch.MSE
arfima_egarch_model$roll.pred<- tmp_df
save(arfima_egarch_model, file='arfima_egarch_model')

