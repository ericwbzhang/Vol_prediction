rm(list=ls())
setwd('/Users/Eric/Desktop/Vol_prediction')
# SPXdata<- read.csv('SPX_rvol.csv')
# SPXdata$SPX2.rvol<- sqrt( SPXdata$SPX2.rv)
# VX<- read.csv('VX.csv')
# 
# ## bind SPXdata and VX
# SPXVX<- merge(SPXdata, VX, by='DATE', all.x = TRUE)
# SPXVX<- SPXVX[ complete.cases(SPXVX),]
# write.csv(SPXVX, file='SPXVX.csv',row.names = FALSE)
SPXVX<- read.csv('SPXVX.csv')
## liner model
library(lubridate)
library(zoo)
library(quantmod)
SPXVX_lag1<- read.zoo(SPXVX)
SPXVX_lag1<-( lag( SPXVX_lag1, -1, na.pad = TRUE))
colnames(SPXVX_lag1)<- paste0(colnames(SPXVX_lag1), '_lag1')
SPXVX_lag1$SPX2.rvol<- SPXVX$SPX2.rvol
SPXVX_lag1<- SPXVX_lag1[complete.cases(SPXVX_lag1), ]
summary( lm( SPX2.rvol~ ., data = SPXVX_lag1))

library(glmnet)
lm.lasso<- (glmnet(x= SPXVX_lag1[, 1: (dim(SPXVX_lag1)[2]-1)], 
               y= SPXVX_lag1$SPX2.rvol,
               alpha = 1,
               lambda = .001))
lm.lasso$beta

lm.lasso.cv<-cv.glmnet(x= as.matrix(SPXVX_lag1[, 1: (dim(SPXVX_lag1)[2]-1)]), 
          y= as.matrix(SPXVX_lag1$SPX2.rvol),
          alpha = 1)
plot(lm.lasso.cv)
## lasso variable selection implies that there is no linear significance in VX varibles
## It indicates that VX volatility measure affects the realized vol in a more 
## sophisicated way, if there exists. 


## introduce VX to ARMA EGARCH model
library(rugarch)

## The lasso reg select variables:
## VIX.Close_lag1
## VX.C1_0_lag1
## VX.C2_1_lag1
## Skew_lag1
## VIX_daily_Range_lag1
## VIX.Open_lag1
## VIX.Close_lag2
## VX1.OpenInt_lag1_lag1
## SPX2.highlow_lag1

external.regressors<- c('VIX.Close_lag1',
                        'VX.C1_0_lag1',
                        'VX.C2_1_lag1',
                        'Skew_lag1',
                        'VIX.daily_Range_lag1',
                        'VIX.Open_lag1',
                        'VX1.OpenInt_lag1_lag1',
                        'SPX2.highlow_lag1'
                        )
external.regressors<- data.frame(SPXVX_lag1[, external.regressors])
rVol<- data.frame(SPXVX_lag1$SPX2.rvol)
ret<- data.frame(SPX2.r= SPXVX$SPX2.r, DATE= SPXVX$DATE)[-1, ]
rownames(ret)<- ymd( ret$DATE)
ret$DATE<- NULL
egarch_vx<- ugarchspec(variance.model = list( model='eGARCH',
                                              external.regressors= as.matrix(external.regressors)),
                    mean.model = list( armaOrder= c(2,0)),
                    distribution.model = 'std')
# egarch_vx_fitted<- ugarchfit(egarch_vx, data= ret)

whole_len<- dim(ret)[1]
burning<- 500
forecast_len<- whole_len- burning

egarch_vx_roll<- ugarchroll( spec = egarch_vx,
                          data= ret,
                          n.ahead = 1,
                          forecast.length = forecast_len,
                          refit.every = 5)


tmp_df<- data.frame(x=tail(ymd(rownames(ret)), forecast_len),
                    realized_vol= tail(ret[,1], forecast_len),
                    egarch_vx.predicted_vol= egarch_vx_roll@forecast$density[,'Sigma'] )
egarch_vx.g<- ggplot(melt(tmp_df, id.var= 'x'), aes(x=x, y= value))+
  geom_line(aes(colour= variable, group= variable))+
  scale_color_manual(values = c('grey', 'green'))+
  ylab('daily volatility')+
  xlab('date index')+
  theme(legend.title= element_blank())+
  ggtitle('ARMA(2,0)-EGARCH(1,1) with extReg vol prediction')
jpeg('ARMAEGARCH_VX.jpeg')
egarch_vx.g
dev.off()

egarch_vx.MSE<- mean(( tmp_df$realized_vol- tmp_df$egarch_vx.predicted_vol)^2)
summary( (tmp_df$realized_vol-tmp_df$egarch_vx.predicted_vol)^2)
egarch_vx.MSE
cor( tmp_df$realized_vol, tmp_df$egarch_vx.predicted_vol)
summary( lm( tmp_df$realized_vol~ tmp_df$egarch_vx.predicted_vol))

egarch_vx_model<- list()
egarch_vx_model$spec<- egarch_vx
egarch_vx_model$roll<- egarch_vx_roll
egarch_vx_model$plot<- egarch_vx.g
egarch_vx_model$MSE<- egarch_vx.MSE
egarch_vx_model$roll.pred<- tmp_df
save(egarch_vx_model, file= 'egarch_vx_model')

