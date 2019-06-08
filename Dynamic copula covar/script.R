.libPaths("C:/Temp/win-library")
setwd("C:/Temp")
Sys.setlocale("LC_TIME", "English")   #change to English

#
library(TTR)
library(quantmod)
library(PerformanceAnalytics)
library(rugarch)
library(TSA)
library(rmgarch)
library(parallel)
library(dgof)
library(fGarch)
library(aTSA)
library(VineCopula)
library(psych)
library(Matrix)

#data from 2007/1/1 to 2016/1/1
#getequity=function(symbol){
#  getSymbols(symbol,src="yahoo",from="2007-01-01",to="2016-01-01",auto.assign = FALSE)
#}

getequity=function(symbol){
  getSymbols(symbol,src="yahoo",from="2008-01-01",to="2009-05-01",auto.assign = FALSE)
}

SPY <- getequity("SPY")

XLY <- getequity("XLY")
XLP <- getequity("XLP")
XLE <- getequity("XLE")
XLF <- getequity("XLF")
XLV <- getequity("XLV")
XLI <- getequity("XLI")
XLB <- getequity("XLB")
XLK <- getequity("XLK")
XLU <- getequity("XLU")
IYZ<-getequity("IYZ")

#
SPY <- SPY$SPY.Adjusted

XLY <- XLY$XLY.Adjusted
XLP <- XLP$XLP.Adjusted
XLE <- XLE$XLE.Adjusted
XLF <- XLF$XLF.Adjusted
XLV <- XLV$XLV.Adjusted
XLI <- XLI$XLI.Adjusted
XLB <- XLB$XLB.Adjusted
XLK <- XLK$XLK.Adjusted
XLU <- XLU$XLU.Adjusted
IYZ<-IYZ$IYZ.Adjusted

ETF<-cbind(XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IYZ,SPY)
ETF<-na.omit(ETF)
ETF<-matrix(ETF,,11)

#
SPY.R <- diff(log(SPY), lag=1)
XLY.R <- diff(log(XLY), lag=1)
XLP.R <- diff(log(XLP), lag=1)
XLE.R <- diff(log(XLE), lag=1)
XLF.R <- diff(log(XLF), lag=1)
XLV.R <- diff(log(XLV), lag=1)
XLI.R <- diff(log(XLI), lag=1)
XLB.R <- diff(log(XLB), lag=1)
XLK.R <- diff(log(XLK), lag=1)
XLU.R <- diff(log(XLU), lag=1)
IYZ.R <- diff(log(IYZ), lag=1)
#SPY.R[1] <- XLY.R[1] <- XLP.R[1] <- XLE.R[1] <- XLF.R[1] <- XLV.R[1] <- XLI.R[1] <- XLB.R[1] <- XLK.R[1] <- XLU.R[1] <- IYZ.R[1] <- 0

XLY.R<-na.omit(XLY.R) 
XLP.R<-na.omit(XLP.R) 
XLE.R<-na.omit(XLE.R) 
XLF.R<-na.omit(XLF.R) 
XLV.R<-na.omit(XLV.R) 
XLI.R<-na.omit(XLI.R) 
XLB.R<-na.omit(XLB.R) 
XLK.R<-na.omit(XLK.R) 
XLU.R<-na.omit(XLU.R) 
XLZ.R<-na.omit(IYZ.R) 
IYZ.R<-na.omit(XLY.R) 
SPY.R<-na.omit(SPY.R)

ETFDR<-cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)
ETFDR<-na.omit(ETFDR)   #daily return

summary(ETFDR)

t=McLeod.Li.test(y=ETFDR)

#GARCH model
#XLF
#EGARCH sstd
#------
#model=ugarchspec( variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
#                  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
#                  distribution.model = "sstd" )
#modelfit=ugarchfit(spec=model,data=XLF.R)

#residu=residuals(modelfit)
#t=McLeod.Li.test(y=residu)

#arma11,garch22
model=ugarchspec( variance.model = list(model = "fGARCH", garchOrder = c(2,2), 
                                        submodel = "GARCH"), 
                  mean.model = list(armaOrder = c(1,1), include.mean = TRUE), 
                  distribution.model = "sstd" ) 

model=ugarchspec( variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), 
                  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                  distribution.model = "sstd" ) 

m11<-ugarchfit(spec=model,data=XLY.R)
m12<-ugarchfit(spec=model,data=XLP.R)
m13<-ugarchfit(spec=model,data=XLE.R)
m14<-ugarchfit(spec=model,data=XLF.R)
m15<-ugarchfit(spec=model,data=XLV.R)
m16<-ugarchfit(spec=model,data=XLI.R)
m17<-ugarchfit(spec=model,data=XLB.R)
m18<-ugarchfit(spec=model,data=XLK.R)
m19<-ugarchfit(spec=model,data=XLU.R)
m110<-ugarchfit(spec=model,data=IYZ.R)
m111<-ugarchfit(spec=model,data=SPY.R)

par(mfrow=c(2,3))
resi1<-residuals(m11,standardize=T);t=McLeod.Li.test(y=resi1)
resi2<-residuals(m12,standardize=T);t=McLeod.Li.test(y=resi2)
resi3<-residuals(m13,standardize=T);t=McLeod.Li.test(y=resi3)
resi4<-residuals(m14,standardize=T);t=McLeod.Li.test(y=resi4)
resi5<-residuals(m15,standardize=T);t=McLeod.Li.test(y=resi5)
resi6<-residuals(m16,standardize=T);t=McLeod.Li.test(y=resi6)
resi7<-residuals(m17,standardize=T);t=McLeod.Li.test(y=resi7)
resi8<-residuals(m18,standardize=T);t=McLeod.Li.test(y=resi8)
resi9<-residuals(m19,standardize=T);t=McLeod.Li.test(y=resi9)
resi10<-residuals(m110,standardize=T);t=McLeod.Li.test(y=resi10)
resi11<-residuals(m111,standardize=T);t=McLeod.Li.test(y=resi11)

coef1<-coef(m11) 
coef2<-coef(m12)
coef3<-coef(m13)
coef4<-coef(m14) 
coef5<-coef(m15)
coef6<-coef(m16)
coef7<-coef(m17) 
coef8<-coef(m18)
coef9<-coef(m19)
coef10<-coef(m110)
coef11<-coef(m111)

coef<-cbind(coef1,coef2,coef3,coef4,coef5,coef6,coef7,coef8,coef9,coef10,coef11)

setwd("C:/Temp")
write.csv(coef, file = "C:/Temp/1.csv", row.names = FALSE)

#m11@fit$sigma   #time-varying standard deviation
#head(m11@fit$z)   # standardized N(0,1) ARMA(1,1) disturbances:
#str(m11)

#t[["p.values"]]
#------

#DCC&GARCH
#------
#DCC GARCH p=0
garch11.spec = ugarchspec(mean.model= list(armaOrder= c(1,1)), 
                          variance.model= list(garchOrder= c(1,1), 
                                               model = "eGARCH"), 
                          distribution.model="sstd")
dcc.garch11.spec=dccspec(uspec = multispec(replicate(11,garch11.spec)),
                         dccOrder = c(1,1),
                         distribution = 'mvnorm')
dcc.fit=dccfit(dcc.garch11.spec,data=ETFDR)

ran<-seq(1:2266)

residu=residuals(dcc.fit)
r1<-residu[,1]
sd<-StdDev(r1)

r12<-r1
i=1
for (i in ran){
  r12[i]<-r1[i]/sd
  i=i+1
}

t=McLeod.Li.test(y=r12)   #test XLF
Box.test(r12^2,lag=10,type='Ljung')

r12a<-arima(r12)
arch.test(r12a)
#------

#GARCH(2,1)
#t=McLeod.Li.test(y=XLY.R)
#------
m1<-garchFit(~1+garch(2,1),data=XLY.R,trace=F)
m2<-garchFit(~1+garch(2,1),data=XLP.R,trace=F)
m3<-garchFit(~1+garch(2,1),data=XLE.R,trace=F)
m4<-garchFit(~1+garch(2,1),data=XLF.R,trace=F)
m5<-garchFit(~1+garch(2,1),data=XLV.R,trace=F)
m6<-garchFit(~1+garch(2,1),data=XLI.R,trace=F)
m7<-garchFit(~1+garch(2,1),data=XLB.R,trace=F)
m8<-garchFit(~1+garch(2,1),data=XLK.R,trace=F)
m9<-garchFit(~1+garch(2,1),data=XLU.R,trace=F)
m10<-garchFit(~1+garch(2,1),data=IYZ.R,trace=F)

#garch22
#------

#GARCH£¨2£¬2£©
m1<-garchFit(~arma(1,1)+garch(2,2),data=XLY.R,trace=F)
m2<-garchFit(~arma(1,1)+garch(2,2),data=XLP.R,trace=F)
m3<-garchFit(~arma(1,1)+garch(2,2),data=XLE.R,trace=F)
m4<-garchFit(~arma(1,1)+garch(2,2),data=XLF.R,trace=F)
m5<-garchFit(~arma(1,1)+garch(2,2),data=XLV.R,trace=F)
m6<-garchFit(~arma(1,1)+garch(2,2),data=XLI.R,trace=F)
m7<-garchFit(~arma(1,1)+garch(2,2),data=XLB.R,trace=F)
m8<-garchFit(~arma(1,1)+garch(2,2),data=XLK.R,trace=F)
m9<-garchFit(~arma(1,1)+garch(2,2),data=XLU.R,trace=F)
m10<-garchFit(~arma(1,1)+garch(2,2),data=IYZ.R,trace=F)

par(mfrow=c(2,3))
resi1<-residuals(m1,standardize=T);t=McLeod.Li.test(y=resi1)
resi2<-residuals(m2,standardize=T);t=McLeod.Li.test(y=resi2)
resi3<-residuals(m3,standardize=T);t=McLeod.Li.test(y=resi3)
resi4<-residuals(m4,standardize=T);t=McLeod.Li.test(y=resi4)
resi5<-residuals(m5,standardize=T);t=McLeod.Li.test(y=resi5)
resi6<-residuals(m6,standardize=T);t=McLeod.Li.test(y=resi6)
resi7<-residuals(m7,standardize=T);t=McLeod.Li.test(y=resi7)
resi8<-residuals(m8,standardize=T);t=McLeod.Li.test(y=resi8)
resi9<-residuals(m9,standardize=T);t=McLeod.Li.test(y=resi9)
resi10<-residuals(m10,standardize=T);t=McLeod.Li.test(y=resi10)
#------
#rxly<-fitted(m1)
#t=McLeod.Li.test(y=rxly)

XLY.R<-resi1 
XLP.R<-resi2
XLE.R<-resi3 
XLF.R<-resi4 
XLV.R<-resi5 
XLI.R<-resi6 
XLB.R<-resi7 
XLK.R<-resi8 
XLU.R<-resi9 
IYZ.R<-resi10 
SPY.R<-resi11

#------

#Box.test(resi^2,lag=10,type='Ljung')

#r12a<-arima(resi)
#arch.test(r12a)

#ACF
#------
res<-ts(resi,frequency=250,start=c(2000)) 

plot(res,xlab='Date',ylab='st.resi',type='l')

par(mfcol=c(2,2))

acf(resi,lag=24)

acf(resi^2,lag=24)

pacf(resi,lag=24)

pacf(resi^2,lag=24)
#------

#R VINE
r1 <- pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))[,1]
r2 <- pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))[,2]
r3<- pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))[,3]
r4<- pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))[,4]
r5<- pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))[,5]
r6 <- pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))[,6]
r7 <- pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))[,7]
r8<- pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))[,8]
r9<- pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))[,9]
r10<- pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))[,10]
r11<- pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))[,11]

daxreturns<-pobs(as.matrix(cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)))

RVM <- RVineStructureSelect(daxreturns, c(1:10), progress = TRUE)
#str(RVM)
summary(RVM)

par(mfrow=c(2,3))
plot(RVM)
#contour(RVM)

ETFresi<-cbind(XLY.R,XLP.R,XLE.R,XLF.R,XLV.R,XLI.R,XLB.R,XLK.R,XLU.R,IYZ.R,SPY.R)
cor<-cov(ETFresi)
pairs.panels(cor)

#rsiSIM<-RVineSim(100,RVM)
#plot(rsiSIM[,1],type='l')

sim1<-matrix(0,1000,11)  #1000 paths
i=1
for (i in seq(11)){
  sim1[,i]<-RVineSim(1000,RVM)[,1]
  i=i+1
}

#i=1
#sim11<-matrix(0,100,1)
#for (i in seq(100)){
#  sim11[i,1]<-mean(sim1[i,])
#  i=i+1
#}

sim<-sim1[500,]
r1<-matrix(0,1,11)
i=1
q<-seq(11)
for (i in q){
  r1[1,i]<-coef[1,i]+coef[2,i]*r1[i]+coef[3,i]*sim[i]+sim[i]
  i=i+1
}

par(mfrow=c(2,1))
plot(t(r1),type='l')

#
mod1 = ugarchroll(spec=model, data = ETFDR[,1], n.ahead = 1, 
                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
#mod2 = ugarchroll(spec=model, data = ETFDR[,2], n.ahead = 1, 
#                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
#                  solver = "hybrid", fit.control = list(),
#                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
#                  keep.coef = TRUE)
mod3 = ugarchroll(spec=model, data = ETFDR[,3], n.ahead = 1, 
                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
mod4 = ugarchroll(spec=model, data = ETFDR[,4], n.ahead = 1, 
                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
mod5 = ugarchroll(spec=model, data = ETFDR[,5], n.ahead = 1, 
                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
mod6 = ugarchroll(spec=model, data = ETFDR[,6], n.ahead = 1, 
                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
mod7 = ugarchroll(spec=model, data = ETFDR[,7], n.ahead = 1, 
                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
mod8 = ugarchroll(spec=model, data = ETFDR[,8], n.ahead = 1, 
                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
#mod9 = ugarchroll(spec=model, data = ETFDR[,9], n.ahead = 1, 
#                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
#                  solver = "hybrid", fit.control = list(),
#                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
#                  keep.coef = TRUE)
#mod10 = ugarchroll(spec=model, data = ETFDR[,10], n.ahead = 1, 
#                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
#                  solver = "hybrid", fit.control = list(),
#                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
#                  keep.coef = TRUE)
mod11 = ugarchroll(spec=model, data = ETFDR[,11], n.ahead = 1, 
                   n.start = 200,  refit.every = 10, refit.window = "recursive", 
                   solver = "hybrid", fit.control = list(),
                   calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                   keep.coef = TRUE)
4
###
#forecast
par(mfrow=c(2,2))
mod1 = ugarchroll(spec=model, data = ETFDR[,1], n.ahead = 1, 
                 n.start = 400,  refit.every = 400, refit.window = "recursive", 
                 solver = "hybrid", fit.control = list(),
                 calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                 keep.coef = TRUE)
#mod2 = ugarchroll(spec=model, data = ETFDR[,2], n.ahead = 1, 
#                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
#                  solver = "hybrid", fit.control = list(),
#                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
#                  keep.coef = TRUE)
mod3 = ugarchroll(spec=model, data = ETFDR[,3], n.ahead = 1, 
                  n.start = 400,  refit.every = 400, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
mod4 = ugarchroll(spec=model, data = ETFDR[,4], n.ahead = 1, 
                  n.start = 400,  refit.every = 400, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
mod5 = ugarchroll(spec=model, data = ETFDR[,5], n.ahead = 1, 
                  n.start = 400,  refit.every = 400, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
mod6 = ugarchroll(spec=model, data = ETFDR[,6], n.ahead = 1, 
                  n.start = 400,  refit.every = 400, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
mod7 = ugarchroll(spec=model, data = ETFDR[,7], n.ahead = 1, 
                  n.start = 400,  refit.every = 400, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
mod8 = ugarchroll(spec=model, data = ETFDR[,8], n.ahead = 1, 
                  n.start = 400,  refit.every = 400, refit.window = "recursive", 
                  solver = "hybrid", fit.control = list(),
                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                  keep.coef = TRUE)
#mod9 = ugarchroll(spec=model, data = ETFDR[,9], n.ahead = 1, 
#                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
#                  solver = "hybrid", fit.control = list(),
#                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
#                  keep.coef = TRUE)
#mod10 = ugarchroll(spec=model, data = ETFDR[,10], n.ahead = 1, 
#                  n.start = 200,  refit.every = 10, refit.window = "recursive", 
#                  solver = "hybrid", fit.control = list(),
#                  calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
#                  keep.coef = TRUE)
mod11 = ugarchroll(spec=model, data = ETFDR[,11], n.ahead = 1, 
                   n.start = 400,  refit.every = 400, refit.window = "recursive", 
                   solver = "hybrid", fit.control = list(),
                   calculate.VaR = TRUE, VaR.alpha = c(0.01,0.025,0.05),
                   keep.coef = TRUE)

#
report(mod, type="VaR", VaR.alpha = 0.01, conf.level = 0.95) 
report(mod, type="fpm")

par(mfrow=c(2,2))
plot(mod1,VaR.alpha=0.05)
#plot(mod2,VaR.alpha=0.05)
plot(mod3,VaR.alpha=0.05)
plot(mod4,VaR.alpha=0.05)
plot(mod5,VaR.alpha=0.05)
plot(mod6,VaR.alpha=0.05)
plot(mod7,VaR.alpha=0.05)
plot(mod8,VaR.alpha=0.05)
#plot(mod9,VaR.alpha=0.05)
#plot(mod10,VaR.alpha=0.05)
plot(mod11,VaR.alpha=0.05)


#
par(mfrow=c(1,1))
VaR1=mod1@forecast$VaR[,"alpha(5%)"]   #Y
plot(VaR1,pch=0,col=493,ylim=c(-0.1,0))
#par(new=TRUE);VaR2=mod2@forecast$VaR[,"alpha(5%)"]   #P
#plot(VaR2,type='l',ylim=c(-0.6,0))
par(new=TRUE);VaR3=mod3@forecast$VaR[,"alpha(5%)"]   #E
plot(VaR3,col=84,pch='.',ylim=c(-0.2,0))
lines(VaR3,col=84,ylim=c(-0.2,0))
par(new=TRUE);VaR4=mod4@forecast$VaR[,"alpha(5%)"]   #F
plot(VaR4,pch='.',ylim=c(-0.2,0))
lines(VaR4,ylim=c(-0.2,0))
par(new=TRUE);VaR5=mod5@forecast$VaR[,"alpha(5%)"]   #V
plot(VaR5,pch='.',ylim=c(-0.2,0))
par(new=TRUE);VaR6=mod6@forecast$VaR[,"alpha(5%)"]   #I
plot(VaR6,pch=4,ylim=c(-0.2,0))
par(new=TRUE);VaR7=mod7@forecast$VaR[,"alpha(5%)"]   #B???
plot(VaR7,pch=5,col='red',ylim=c(-0.2,0))
par(new=TRUE);VaR8=mod8@forecast$VaR[,"alpha(5%)"]   #K
plot(VaR8,pch='.',col='green',ylim=c(-0.2,0))
lines(VaR8,ylim=c(-0.2,0),col='green')
#par(new=TRUE);VaR9=mod9@forecast$VaR[,"alpha(5%)"]   #U
#plot(VaR9,type='l',ylim=c(-0.6,0))
#par(new=TRUE);VaR10=mod10@forecast$VaR[,"alpha(5%)"]   #IYZ
#plot(VaR10,type='l',ylim=c(-0.6,0))
par(new=TRUE);VaR11=mod11@forecast$VaR[,"alpha(5%)"]   #SPY
#plot(VaR11,type='l',col='blue',ylim=c(-0.6,0))

#SPY
par(new=TRUE)
plot(VaR11,pch=7,ylim=c(-0.15,0),col='blue',axes=FALSE)
lines(VaR11,ylim=c(-0.15,0),col='blue')

legend("bottomright", legend=c("XLY", 'XLE','XLF','XLV',
                               'XLI','XLB','XLK','SPY'),
       col=c(493,84,'black','black','black','red','green','blue'),pch=0:7 ,cex=0.8)

legend("bottomright", legend=c('XLF',
                               'XLK','XLE'),
       col=c('black','green','blue'),pch='¡ª¡ª' ,cex=0.8)

#
getequity=function(symbol){
  getSymbols(symbol,src="yahoo",from="2008-01-01",to="2015-01-01",auto.assign = FALSE)
}
VIX<-getequity("^VIX")
plot(VIX)
