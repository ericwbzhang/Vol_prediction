rm(list=ls())

setwd('/Users/Eric/Desktop/Vol_prediction')
load('egarch_model')
load('arfima_egarch_model')
load('rgarch_model')
library(lubridate)
library(xts)
library(zoo)
SPXdata<- read.csv('SPX_rvol.csv')
SPXdata$SPX2.rvol<- sqrt( SPXdata$SPX2.rv)
rownames(SPXdata)<- ymd( SPXdata$DATE)

## VX_lag1 contains the information of implied vol with 1 day lag
VX<- read.csv('VX.csv')
VX$DATE<- ymd(VX$DATE)
rownames(VX)<- ymd(VX$DATE)
VX$DATE<- NULL
VX<- as.xts(VX)
VX_lag1<- lag( VX, 1, na.pad = TRUE)
VX_lag1<- as.data.frame(VX_lag1)
VX_lag1$DATE<- ymd(rownames(VX_lag1))
VX_lag1<- VX_lag1[ complete.cases(VX_lag1), ]

## forecasting is the time series 1 day ahead forecasting
forecasting<- data.frame(DATE=ymd(egarch_model$roll.pred$x),
                         real= egarch_model$roll.pred$realized_vol,
                         egarchPred= egarch_model$roll.pred$egarch.predicted_vol,
                         rgarchPred= rgarch_model$roll.pred$rgarch.prediction_vol,
                         arfima_egarchPred= arfima_egarch_model$roll.pred$arfima_egarch.predicted_vol)
## combine the VX_lag1 and forecasting 
fore_vx<- merge( VX_lag1, forecasting, by='DATE')
fore_vx<- fore_vx[complete.cases(fore_vx),]
rownames(fore_vx)<- ymd(fore_vx$DATE)
fore_vx$DATE<- NULL
fore_vx<- as.xts( fore_vx)

tmp<- list()
tmp$rvol<- fore_vx$real
fore_vx$real<- NULL
tmp$x<- fore_vx
fore_vx<- tmp


## rf ensemble to get an average forecasting ( not successful)
library(randomForest)
# # cv<- rfcv(trainx = fore_vx$x, trainy = fore_vx$rvol, cv.fold = 5)
# 
# ensemble_rf<- randomForest(x = fore_vx$x ,
#                            y= fore_vx$rvol,
#                            ntree= 1000,
#                            importance= TRUE)
# 
# plot(ensemble_rf)
# varImpPlot(ensemble_rf)
# var_selection<- c( 'rgarchPred',
#                    'arfima_egarchPred',
#                    'VIX.Close',
#                    'egarchPred',
#                    'VIX.Open',
#                    'VX.C5_2',
#                    'VX.C2_1',
#                    'VX.C1_0',
#                    'VX4.OpenInt_lag1',
#                    'VX1.Settle')
# tmp<- fore_vx$x[,  var_selection]
# 
# ensemble_rf<- randomForest(x = tmp ,
#                            y= fore_vx$rvol,
#                            ntree= 1000,
#                            importance= TRUE)
# 


## rf ensemble the time series forecasting to get an average
rownames(forecasting)<- forecasting$DATE
forecasting$DATE<- NULL

ensemble_rf<- randomForest(real~.,
                           data = forecasting,
                           ntree= 1000, 
                          mtry= 2,
                           importance= TRUE)

ensemble_rf
plot(ensemble_rf)
ensemble_rf$mse
ensemble_rf$rsq
library(ggplot2)
library(reshape2)
tmp<- data.frame( x= ymd( egarch_model$roll.pred$x),
                  reallized_vol= forecasting$real,
                  rf.predicted_vol= ensemble_rf$predicted)

rf.g<- ggplot(melt(data =  tmp, id.var='x'), aes( x=x, y= value))+
  geom_line(aes(colour= variable, group= variable))+
  scale_color_manual(values = c('grey', 'dark green'))+
  ylab('daily volatility')+
  xlab('date index')+
  theme(legend.title= element_blank())+
  ggtitle('RandomForest ensemble prediction')

jpeg('RF_ENSEMBLE.jpeg')
rf.g
dev.off()

rf<- list()
rf$model<- ensemble_rf
rf$plot<- rf.g
rf$MSE<- mean( ensemble_rf$mse)
rf$roll.pred<- tmp

save(rf, file='rf')
