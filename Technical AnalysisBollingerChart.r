#---------------------------------------------------------------------------------
# Title: Technical Analysis Using R

# Google "bse stock price historical data" and download the stock price data for, say, BSE stock LT
# copy the .csv file from 'downloads' directory of the computer & save it at any convenient 
# directory location

# Copy the full R code given below and paste it in the source code window of R Studio

# module A: Data Input 
# module A1: SMA
# module A2; EMA
# module A3: Bollinger Bands
# module A4: MACD
# module A5: RSI

#---------------------------------------------------------------------------------

# module A: Data Input

setwd("C://Users//pjawaria//Desktop//NSE Stock Research")

getwd()

input <- read.csv("LT Apr-May-Jun.csv" , sep=",", header=T)
head(input)


datadf<-data.frame(TrDate=as.Date(input$Date,format="%d-%b-%Y"), 
                   OpenPrice=input$Open.Price, 
                   HighPrice=input$High.Price, 
                   LowPrice=input$Low.Price, 
                   ClosePrice=input$Close.Price,
                   volume=input$Total.Traded.Quantity
)

abc<-datadf[order(datadf$TrDate, decreasing=F),]
rownames(abc)<-c(1:nrow(datadf))
str(abc)
head(abc)
abc$TrDate<-as.character(abc$TrDate, format="%d-%b-%y")
head(abc)
tail(abc)
library(TTR)

# module A: Data Input End

#---------------------------------------------------------------------------------

# module A1
# Technical Analysis: SMA

sma<-SMA(abc$ClosePrice, n=20)
xyz<-data.frame(TrDate=abc$TrDate, ClosePrice=abc$ClosePrice, volume=abc$volume, sma)
head(xyz, 24)
layout(matrix(c(1,1,1,2,2), nrow=5, ncol=1))
plot(abc$ClosePrice, xaxt="n", type="l", col=2, xlab="Date", ylim=c(min(abc$ClosePrice)*0.95, max(abc$ClosePrice)*1.05), ylab= "Close Price", main="SMA20", lwd=2)
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

lines(xyz$sma, type="l", col='blue')
legend("topleft", lty=c(1,1), col=c('red', 'blue'), legend=c("Close Price", "SMA20"), bty="n", cex=0.6)

plot(abc$volume, type="h", xaxt="n", col="green", xlab="", main="", sub="", ylab="volume")
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

#---SMA20 and volume plot---

# module A1 End

#---------------------------------------------------------------------------------

# module A2
# Technical Analysis: EMA

ema<-EMA(abc$ClosePrice, n=20)
xyz<-data.frame(TrDate=abc$TrDate, ClosePrice=abc$ClosePrice, volume=abc$volume, ema)
head(xyz,24)
layout(matrix(c(1,1,1,2,2), nrow=5, ncol=1))
plot(abc$ClosePrice, xaxt="n", type="l", col=2, xlab="Date", ylim=c(min(abc$ClosePrice)*0.95, max(abc$ClosePrice)*1.05), ylab="Close Price", main="EMA20")
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

lines(xyz$ema, type="l", col='blue')
legend("topleft", lty=c(1,1), col=c('red', 'blue'), legend=c("Close Price", "SMA"), bty="n", cex=0.6)

plot(abc$volume, type="h", xaxt="n", col="green", xlab="", main="", sub="", ylab="volume")
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

#---EMA20 and volume plot---

# module A2 End

#---------------------------------------------------------------------------------

# module A3
# Technical Analysis: Bollinger Bands

bb20<-BBands(abc$ClosePrice, sd=2)
xyz<-data.frame(TrDate=abc$TrDate, ClosePrice=abc$ClosePrice, volume=abc$volume, bb20)
head(xyz,24)
layout(matrix(c(1,1,1,2,2), nrow=5, ncol=1))
plot(abc$ClosePrice, xaxt="n", type="l", col=2, ylim=c(min(abc$ClosePrice)*0.95, max(abc$ClosePrice)*1.05), xlab="Date", main="", sub="", ylab="Close Price", cex=0.6)
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

lines(xyz$up, type="l", col='purple')
lines(xyz$dn, type="l", col='purple')
lines(xyz$mavg, type="l", col='blue')
legend("topleft", col=c('red', 'purple', 'purple', 'blue'), lty=c(1,1,1,1), legend=c('Close Price', 'up', 'dn', 'mavg'), bty="n", cex=0.6)

plot(abc$volume, type="h", xaxt="n", col="green", xlab="", main="", sub="", ylab="volume")
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

#---Bollinger bands plot---

# module A3 End

#---------------------------------------------------------------------------------


# module A4
#Technical Analysis: MACD

# Let us calculate standard(12,26,9) MACD

macd<-MACD(abc$ClosePrice, nFast=12, nSlow=26, nSig=9, maType=SMA)
xyz<-data.frame(TrDate=abc$TrDate, ClosePrice=abc$ClosePrice, volume=abc$volume, macd)
head(xyz,40)
layout(matrix(c(1,1,1,2,2), nrow=5, ncol=1))
plot(abc$ClosePrice, xaxt="n", type="l", col=2, xlab="Date", ylim=c(min(abc$ClosePrice)*0.95, max(abc$ClosePrice)*1.05), ylab="Close Price", mai=c(0.1,0.5,0.1,0.1), main=NA, sub=NA)
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

legend("topleft", col='red', lty=1, legend=c('Close Price'), bty="n", cex=0.6)

plot(xyz$macd, xaxt="n", type="l", col="blue", ylab="MACD/Signal", mai=c(0.1,0.5,0.1,0.1), xlab=NA, main=NA, sub=NA)
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

lines(xyz$signal, type="l", col='brown')
abline(h=0, col='purple')
legend("topleft", col=c('blue', 'brown'), lty=c(1,1), legend=c('macd', 'signal'), bty="n", cex=0.6)


#---MACD plot---

# module A4 End

#---------------------------------------------------------------------------------

# module A5
# Technical Analysis: RSI, Relative Strength Index

rsi14<-RSI(abc$ClosePrice, n=14)
xyz<-data.frame(TrDate=abc$TrDate, ClosePrice=abc$ClosePrice, volume=abc$volume, rsi14)
head(xyz,20)
layout(matrix(c(1,1,1,2,2), nrow=5, ncol=1))
plot(abc$ClosePrice, xaxt="n", type="l", col=2, xlab="Date", main="", sub="", ylim=c(min(abc$ClosePrice)*0.95, max(abc$ClosePrice)*1.05), ylab="Close Price", mai=c(0.1,1,0.1,0.5))
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

plot(xyz$rsi14, xaxt="n", type="l", col="blue", ylim=c(0,100), , xlab="", main="", sub="",  ylab="RSI", mai=c(0.1,1,0.1,0.5))
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

#---RSI plot---

# module A5 End

#----------------------------------------------------------------------------------------------

