rm(list=ls())

## Data Cleaning 
library(lubridate)
setwd('/Users/Eric/Desktop/Vol_prediction')
tmp<- readLines('OxRealizedVol_SPX.csv')
mktData<- read.csv( textConnection(tmp[-(1:2)]))
mktData$SPX2.rcto<-NULL
mktData<- mktData[complete.cases(mktData),]
tmp<- c('DateID',
        'SPX2.rv',
        'SPX2.r',
        'SPX2.rs',
        'SPX2.nobs',
        'SPX2.open',
        'SPX2.highlow',
        'SPX2.highopen',
        'SPX2.openprice',
        'SPX2.closeprice')
mktData<- mktData[, names(mktData) %in% tmp]
mktData$DATE<-ymd( mktData$DateID)
mktData$DateID<- NULL
write.csv(mktData, file = 'SPX_rvol.csv', row.names = FALSE)