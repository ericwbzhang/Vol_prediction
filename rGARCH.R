
rm(list=ls())
setwd('/Users/Eric/Desktop/Vol_prediction')

library(lubridate)
setwd('/Users/Eric/Desktop/Vol_prediction')
SPXdata<- read.csv('SPX_rvol.csv')
rownames(SPXdata)<- ymd( SPXdata$DATE)
SPXdata$SPX2.rvol<- sqrt(SPXdata$SPX2.rv)

library(rugarch)
library(xts)
rgarch.model<- ugarchspec(mean.model = list(armaOrder= c(2,0)),
                    variance.model = list(model= 'realGARCH',
                                          garchOrder= c(2,1)))
setbounds(rgarch.model)<- list(alpha2=c (-1,1))
SPXdata.xts<- SPXdata
SPXdata.xts$DATE<- NULL
SPXdata.xts<- as.xts(SPXdata.xts)
rgarch.fit<- ugarchfit(spec = rgarch.model, 
                       data= SPXdata.xts$SPX2.r, 
                       solver= 'hybrid', 
                       realizedVol= SPXdata.xts$SPX2.rvol)

whole_len<- dim(SPXdata.xts)[1]
burning<- 1000
forecast_len<- whole_len- burning
rgarch.roll<- ugarchroll(spec = rgarch.model,
                         data= SPXdata.xts$SPX2.r,
                         n.ahead = 1, 
                         forecast.length = forecast_len, 
                         refit.every = 5,
                         solver= 'hybrid',
                         realizedVol= SPXdata.xts$SPX2.rvol
                         )

library(ggplot2)
library(reshape2)
tmp_df<- data.frame(x= tail( ymd(index(SPXdata.xts)) ,forecast_len),
                    realized_vol= tail( SPXdata$SPX2.rvol, forecast_len),
                    rgarch.prediction_vol= rgarch.roll@forecast$density$RVolForecast)
rgarch.g<- ggplot( data= melt( tmp_df, id.var= 'x'), aes( x=x, y= value))+
  geom_line(aes(colour= variable, group= variable))+
  scale_colour_manual(values= c( 'grey', 'orange'))+
  ylab('daily volatility')+
  xlab( 'date')+
  theme( legend.title= element_blank())+
  ggtitle( 'realizedGARCH(ARMA(2,0)-rGARCH(2,1)) rolling prediction')
jpeg( 'rGARCH.jpeg')
rgarch.g
dev.off()

rgarch.MSE<- mean( (tmp_df$realized_vol- tmp_df$rgarch.prediction_vol)^2)
summary( (tmp_df$realized_vol- tmp_df$rgarch.prediction_vol)^2)
rgarch.MSE
cor( tmp_df$realized_vol, tmp_df$rgarch.prediction_vol)
summary( lm( tmp_df$realized_vol~ tmp_df$rgarch.prediction_vol))

rgarch_model<- list()
rgarch_model$spec<- rgarch.model
rgarch_model$roll<- rgarch.roll
rgarch_model$plot<- rgarch.g
rgarch_model$MSE<- rgarch.MSE
rgarch_model$roll.pred<- tmp_df
save(rgarch_model, file = 'rgarch_model')




