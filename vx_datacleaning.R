rm(list=ls())
setwd('/Users/Eric/Desktop/Vol_prediction')
dat<- list()
# start_date<- ymd(20070401)
# end_date<- ymd(20161006)

## Collect VX future data
for ( i in 1:5){
  filename<- paste0('CHRIS-CBOE_VX', toString(i), '.csv')
  #print(filename)
  tmp<- read.csv(filename)
  DATE<- rev(tmp$Trade.Date)
  vx<- data.frame(Settle= tmp$Settle, 
                  daily_Range_pct= (tmp$High- tmp$Low)/tmp$Settle,
                  Volume= tmp$Total.Volume,
                  OpenInt_lag1= tmp$Prev..Day.Open.Interest)
  vx<- vx[rev(rownames(vx)), ]
  colnames(vx)<- paste0( paste0('VX',toString(i),'.'), colnames(vx))
  vx$DATE<- DATE
  cmmd<- paste0('dat$vx', toString(i), '<-vx')
  eval(parse(text= cmmd))
  #print(cmmd)
  print( dim(vx))
}

## Collect VIX data
vix<- read.csv('CBOE-VIX.csv')
vix<- vix[rev(rownames(vix)), ]
vix$VIX.daily_Range<- vix$VIX.High- vix$VIX.Low
tmp<- data.frame(VIX.daily_Range= (vix$VIX.High- vix$VIX.Low)/ vix$VIX.Close,
                 VIX.Close= vix$VIX.Close,
                 VIX.Open= vix$VIX.Open)
rownames(tmp)<- vix$Date
vix<-tmp
vix$DATE<- rownames(vix)

## Collect Skew data
skew<- read.csv('CBOE-SKEW.csv')
skew<- skew[rev(rownames(skew)), ]
rownames(skew)<- skew$Date
skew$Date<- NULL
colnames(skew)<- c('Skew')
skew$DATE<- rownames(skew)

## Bind all VIX realated data to VX
VX<- skew
VX<- merge( VX, vix, by='DATE', all.y = TRUE)

for (name in names(dat)){
  VX<- merge( VX, dat[[name]], by='DATE', all.y = TRUE)
}
## Compute Contango metrixs
VX$VX.C1_0<-(VX$VX1.Settle- VX$VIX.Close)/ VX$VIX.Close
VX$VX.C2_1<- (VX$VX2.Settle- VX$VX1.Settle)/ VX$VX1.Settle
VX$VX.C5_2<- (VX$VX5.Settle- VX$VX2.Settle)/ VX$VX2.Settle/3.0

## store VX to file 
write.csv(VX, file = 'VX.csv', row.names = FALSE)