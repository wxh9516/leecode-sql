library(quantmod)

ticker<-c('SPY','XLB','XLE','XLF','XLI','XLK','XLP','XLU','XLY','XLV')
getSymbols(ticker,scr='google',from='2007-1-1',to='2017-12-31')

library(PerformanceAnalytics)

close1<-SPY$SPY.Close
close2<-XLB$XLB.Close
close3<-XLE$XLE.Close
close4<-XLF$XLF.Close
close5<-XLI$XLI.Close
close6<-XLK$XLK.Close
close7<-XLP$XLP.Close
close8<-XLU$XLU.Close
close9<-XLY$XLY.Close
close10<-XLV$XLV.Close

lr1<-diff(log(close1))
lr2<-diff(log(close2))
lr3<-diff(log(close3))
lr4<-diff(log(close4))
lr5<-diff(log(close5))
lr6<-diff(log(close6))
lr7<-diff(log(close7))
lr8<-diff(log(close8))
lr9<-diff(log(close9))
lr10<-diff(log(close10))

library(zoo)

par(mfrow = c(1,1))
rwv1<-apply.rolling(lr1,gap=60,width=60,FUN='mean')   #return SPY
rwv2<-apply.rolling(lr2,gap=60,width=60,FUN='mean')
rwv3<-apply.rolling(lr3,gap=60,width=60,FUN='mean')
rwv4<-apply.rolling(lr4,gap=60,width=60,FUN='mean')
rwv5<-apply.rolling(lr5,gap=60,width=60,FUN='mean')
rwv6<-apply.rolling(lr6,gap=60,width=60,FUN='mean')
rwv7<-apply.rolling(lr7,gap=60,width=60,FUN='mean')
rwv8<-apply.rolling(lr8,gap=60,width=60,FUN='mean')
rwv9<-apply.rolling(lr9,gap=60,width=60,FUN='mean')
rwv10<-apply.rolling(lr10,gap=60,width=60,FUN='mean')

var1<-apply.rolling(lr1,gap=60,width=60,FUN='VaR')   #var SPY
var2<-apply.rolling(lr2,gap=60,width=60,FUN='VaR')
var3<-apply.rolling(lr3,gap=60,width=60,FUN='VaR')
var4<-apply.rolling(lr4,gap=60,width=60,FUN='VaR')
var5<-apply.rolling(lr5,gap=60,width=60,FUN='VaR')
var6<-apply.rolling(lr6,gap=60,width=60,FUN='VaR')
var7<-apply.rolling(lr7,gap=60,width=60,FUN='VaR')
var8<-apply.rolling(lr8,gap=60,width=60,FUN='VaR')
var9<-apply.rolling(lr9,gap=60,width=60,FUN='VaR')
var10<-apply.rolling(lr10,gap=60,width=60,FUN='VaR')

plot(var1)

#3
library(igraph)
var<-cbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10)
var<-var[-c(0:60)]
var0<-cor(var)

g10 <- graph.adjacency(var0,weighted=TRUE,diag=F,mode='directed')
plot(g10,edge.arrow.size=0.1)

#4
pc<-princomp(var0,cor=TRUE,scor=TRUE)
summary(pc)

pca<-prcomp(var0,cor=F)
evec = pca$rotation[] 
eval <- pca$sdev^2
barplot(eval,col=colors)

#----------
library(quantreg)
a2<-rwv2$calcs
a2<-data.frame(a2)
write.table (a2, file ="a2.csv")

data <- read.csv("fe655.csv",head=FALSE)
v1<-data$V2
v2<-data$V3
d<-data.frame(v1,v2)
rq1<-rq(v1~v2,tau=.5,d)
c1 = coef(rq1)

#4
library(copula)
library(scatterplot3d)
library(ggplot2)
library(grid)

lr<-cbind(lr1,lr2,lr3,lr4,lr5,lr6,lr7,lr8,lr9,lr10)
lr<-lr[3:63,]

lr<-cor(lr, method = "kendall")

x1_mean <- mean(lr$SPY.Close)
x1_var <- var(lr$SPY.Close)
x1_rate <- x1_mean / x1_var
x1_shape <- ( (x1_mean)^2 ) / x1_var

hist(lr$SPY.Close, breaks = 20, col = "green", density = 20)
hist(rgamma( nrow(lr), rate = x1_rate, shape = x1_shape), breaks = 20,col = "blue", add = T, density = 20, angle = -45)

x2_mean <- mean(lr$XLB.Close)
x2_var <- var(lr$XLB.Close)
x2_rate <- x2_mean / x2_var
x2_shape <- ( (x2_mean)^2 ) / x2_var

x3_mean <- mean(lr$XLE.Close)
x3_var <- var(lr$XLE.Close)
x3_rate <- x3_mean / x3_var
x3_shape <- ( (x3_mean)^2 ) / x3_var

x4_mean <- mean(lr$XLF.Close)
x4_var <- var(lr$XLF.Close)
x4_rate <- x4_mean / x4_var
x4_shape <- ( (x4_mean)^2 ) / x4_var

x5_mean <- mean(lr$XLI.Close)
x5_var <- var(lr$XLI.Close)
x5_rate <- x5_mean / x5_var
x5_shape <- ( (x5_mean)^2 ) / x5_var

x6_mean <- mean(lr$XLK.Close)
x6_var <- var(lr$XLK.Close)
x6_rate <- x6_mean / x6_var
x6_shape <- ( (x6_mean)^2 ) / x6_var

x7_mean <- mean(lr$XLP.Close)
x7_var <- var(lr$XLP.Close)
x7_rate <- x7_mean / x7_var
x7_shape <- ( (x7_mean)^2 ) / x7_var

x8_mean <- mean(lr$XLU.Close)
x8_var <- var(lr$XLU.Close)
x8_rate <- x8_mean / x8_var
x8_shape <- ( (x8_mean)^2 ) / x8_var

x9_mean <- mean(lr$XLY.Close)
x9_var <- var(lr$XLY.Close)
x9_rate <- x9_mean / x9_var
x9_shape <- ( (x9_mean)^2 ) / x9_var

x10_mean <- mean(lr$XLV.Close)
x10_var <- var(lr$XLV.Close)
x10_rate <- x10_mean / x10_var
x10_shape <- ( (x10_mean)^2 ) / x10_var

cop_model<-claytonCopula(dim=10)
m <- pobs(as.matrix(lr))
fit <- fitCopula(cop_model, m, method = 'ml')
coef(fit)
tau(claytonCopula(param = coef(fit)))

cop<-claytonCopula(param = 1.38,dim=10)
persp(clayton,dCopula)

u = rCopula(1500,cop)
pdf = cCopula(u, cop)
cdf = pCopula(u,cop)

par(mfrow=c(1,3))
my_dist <- mvdc(claytonCopula(param = 1.38, dim = 10), margins = c("gamma","gamma","gamma","gamma","gamma","gamma","gamma","gamma","gamma","gamma"), paramMargins = list(list(shape = x1_shape, rate = x1_rate), list(shape = x2_shape, rate = x2_rate), list(shape = x3_shape, rate = x3_rate), list(shape = x4_shape, rate = x4_rate), list(shape = x5_shape, rate = x5_rate), list(shape = x6_shape, rate = x6_rate), list(shape = x7_shape, rate = x7_rate), list(shape = x8_shape, rate = x8_rate), list(shape = x9_shape, rate = x9_rate), list(shape = x10_shape, rate = x10_rate)))
samples = rMvdc(10000, my_dist)
scatterplot3d(samples[,1],samples[,2],pch=".")
scatterplot3d(samples[,1],samples[,3],pch=".")
scatterplot3d(samples[,1],samples[,5],pch=".")
scatterplot3d(samples[,1],samples[,7],pch=".")
scatterplot3d(samples[,1],samples[,8],pch=".")
scatterplot3d(samples[,1],samples[,9],pch=".")
scatterplot3d(samples[,1],samples[,10],pch=".")

#-----
# Generate random sample observations from the multivariate distribution
v <- rMvdc(5000, my_dist)
# Compute the density
pdf_mvd <- dMvdc(v, my_dist)
# Compute the CDF
cdf_mvd <- pMvdc(v, my_dist)

par(mfrow = c(1, 2))

contour(myMvd1, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3))


scatterplot3d(v[,1],v[,2], pdf_mvd, color="red", main="Density", xlab = "u1", ylab="u2", zlab="pMvdc",pch=".")
scatterplot3d(v[,1],v[,2], cdf_mvd, color="red", main="CDF", xlab = "u1", ylab="u2", zlab="pMvdc",pch=".")
persp(my_dist, dMvdc, xlim = c(-4, 4), ylim=c(0, 2), main = "Density")
contour(my_dist, dMvdc, xlim = c(-4, 4), ylim=c(0, 2), main = "Contour plot")
persp(my_dist, pMvdc, xlim = c(-4, 4), ylim=c(0, 2), main = "CDF")

contour(my_dist, pMvdc, xlim = c(-10, 10), ylim=c(0, 20), main = "Contour plot")

library(psych)
pairs.panels(lr)


#-----
c<-cbind(lr1,lr3)
u <- pobs(as.matrix(c))[,1]
v <- pobs(as.matrix(c))[,2]
selectedCopula <- BiCopSelect(u,v,familyset=NA)
selectedCopula

t.cop <- tCopula(dim=2)
set.seed(500)
m <- pobs(as.matrix(c))
persp(tCopula(dim=2,.77,df=.56),dCopula)

library(VineCopula)
u1 <- pobs(as.matrix(lr))[,1]
u2 <- pobs(as.matrix(lr))[,2]
u3 <- pobs(as.matrix(lr))[,3]
u4 <- pobs(as.matrix(lr))[,4]
u5 <- pobs(as.matrix(lr))[,5]
u6 <- pobs(as.matrix(lr))[,6]
u7 <- pobs(as.matrix(lr))[,7]
u8 <- pobs(as.matrix(lr))[,8]
u9 <- pobs(as.matrix(lr))[,9]
u10 <- pobs(as.matrix(lr))[,10]

selectedCopula <- BiCopSelect(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,familyset=NA)
selectedCopula

t.cop <- tCopula(dim=10)

m <- pobs(as.matrix(lr))
persp(claytonCopula(dim=10,0.41),dCopula)

u <- rCopula(m, 4)

contour(my_dist, dMvdc, xlim = c(-3, 3), ylim = c(-3, 3))
