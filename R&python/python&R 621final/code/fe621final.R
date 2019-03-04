library(Sim.DiffProc)
library(xts)

data=read.csv("fe621 final risk.csv",header =TRUE)
head(data)

s1<-data$stock1
s1<-ts(s1)

s2<-data$stock2
s2<-ts(s2)

s3<-data$stock3
s3<-ts(s3)

s4<-data$stock4
s4<-ts(s4)

s5<-data$stock5
s5<-ts(s5)

#model 1
fx1<-expression(theta[1]*x)
gx1<-expression(theta[2]*x^theta[3])
m1<-fitsde(data=s5,drift=fx1,diffusion=gx1,start=list(theta1=1,theta2=1,theta3=1),pmle='euler')

fx2<-expression(theta[1]+theta[2]*x)
gx2<-expression(theta[3]*x^theta[4])
m2<-fitsde(data=s5,drift=fx2,diffusion=gx2,start=list(theta1=1,theta2=1,theta3=1,theta4=1),pmle='euler')

fx3<-expression(theta[1]+theta[2]*x)
gx3<-expression(theta[3]*sqrt(x))
m3<-fitsde(data=s5,drift=fx3,diffusion=gx3,start=list(theta1=1,theta2=1,theta3=1),pmle='euler')

fx4<-expression(theta[1])
gx4<-expression(theta[2]*x^theta[3])
m4<-fitsde(data=s5,drift=fx4,diffusion=gx4,start=list(theta1=1,theta2=1,theta3=1),pmle='euler')

fx5<-expression(theta[1]*x)
gx5<-expression(theta[2]+theta[3]*x^theta[4])
m5<-fitsde(data=s5,drift=fx5,diffusion=gx5,start=list(theta1=1,theta2=1,theta3=1,theta4=1),pmle='euler')

AIC<-c(AIC(m1),AIC(m2),AIC(m3),AIC(m4),AIC(m5))
Test<-data.frame(AIC,row.names=c('1','2','3','4','5'))
Bestmode<-rownames(Test)[which.min(Test[,1])]
Bestmode  

#s1: output: 2
#s2: output: 4
#s3: output: 1
#s4: output: 2
#s5: output: 4

#euler
#1
fitmod <- fitsde(data = s5, drift = fx4, diffusion = gx4, start = list(theta1=1, theta2=1,theta3=1),pmle="euler")
fitmod
summary(fitmod)
confint(fitmod, level=0.95)
#Call:
#fitsde(data = s1, drift = fx2, diffusion = gx2, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1, theta4 = 1), pmle = "euler")
#Coefficients:
#  theta1       theta2       theta3       theta4 
#3.504814e-05 2.214008e-05 7.536530e-03 3.903047e-01

#2
#Call:
#  fitsde(data = s2, drift = fx4, diffusion = gx4, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1), pmle = "euler")
#Coefficients:
#  theta1      theta2      theta3 
#0.007636416 0.006855019 0.537076888 

#3
#Call:
#  fitsde(data = s3, drift = fx1, diffusion = gx1, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1), pmle = "euler")
#Coefficients:
#  theta1       theta2       theta3 
#0.003498813 -3.583787343  3.734064202 

#4
#Call:
#  fitsde(data = s4, drift = fx2, diffusion = gx2, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1, theta4 = 1), pmle = "euler")

#Coefficients:
#  theta1        theta2        theta3        theta4 
#-9.421015e-05  9.618334e-06  1.471477e-02  4.269734e-01 

#5
#Call:
#  fitsde(data = s5, drift = fx4, diffusion = gx4, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1), pmle = "euler")

#Coefficients:
#  theta1      theta2      theta3 
#0.003342042 0.009936535 0.530964394 

#ozaki
#1
fitmod <- fitsde(data=s5,drift=fx4,diffusion=gx4,start = list(theta1=1,theta2=1,theta3=1),pmle="ozaki")
summary(fitmod)
fitmod
#Pseudo maximum likelihood estimation
#Method:  Ozaki
#Call:
#  fitsde(data = s1, drift = fx2, diffusion = gx2, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1, theta4 = 1), pmle = "ozaki")
#Coefficients:
#  Estimate   Std. Error
#theta1  0.0547352626 5.141170e-04
#theta2 -0.0001325927 1.693968e-06
#theta3  0.0040184172 1.368547e-05
#theta4  0.5276808366 8.348900e-04

#Call:
#fitsde(data = s1, drift = fx2, diffusion = gx2, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1, theta4 = 1), pmle = "ozaki")
#Coefficients:
#  theta1        theta2        theta3        theta4 
#0.0547352626 -0.0001325927  0.0040184172  0.5276808366 

#2   ???
#Call:
#fitsde(data = s2, drift = fx4, diffusion = gx4, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1), pmle = "ozaki")
#Coefficients:
#  theta1 theta2 theta3 
#1      1      1 

#3
#Pseudo maximum likelihood estimation
#Method:  Ozaki
#Call:
#  fitsde(data = s3, drift = fx1, diffusion = gx1, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1), pmle = "ozaki")
#Coefficients:
#  Estimate   Std. Error
#theta1  7.747055e-06 3.535981e-06
#theta2 -2.858406e-03 4.646396e-06
#theta3  8.356426e-01 5.486330e-04
#-2 log L: 39755.07 

#Call:
#fitsde(data = s3, drift = fx1, diffusion = gx1, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1), pmle = "ozaki")
#Coefficients:
#  theta1        theta2        theta3 
#7.747055e-06 -2.858406e-03  8.356426e-01 

#4
#Pseudo maximum likelihood estimation
#Method:  Ozaki
#Call:
#  fitsde(data = s4, drift = fx2, diffusion = gx2, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1, theta4 = 1), pmle = "ozaki")
#Coefficients:
#  Estimate   Std. Error
#theta1  6.279268e-03 1.729448e-03
#theta2 -5.520604e-05 1.081078e-05
#theta3  2.751369e-02 4.288770e-04
#theta4  3.015314e-01 3.084769e-03
#-2 log L: -127680.7

#Call:
#fitsde(data = s4, drift = fx2, diffusion = gx2, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1, theta4 = 1), pmle = "ozaki")

#Coefficients:
#  theta1        theta2        theta3        theta4 
#6.279268e-03 -5.520604e-05  2.751369e-02  3.015314e-01 

#5   ???
#Call:
#fitsde(data = s5, drift = fx4, diffusion = gx4, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1), pmle = "ozaki")
#Coefficients:
#  theta1 theta2 theta3 
#1      1      1 

#Shoji-Ozaki
#1   ???
#Call:
#  fitsde(data = s1, drift = fx2, diffusion = gx2, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1, theta4 = 1), pmle = "shoji", lower = c(-3, 
#                                                                                                                              0), upper = c(-1, 1))
#Coefficients:
#  theta1 theta2 theta3 theta4 
#-3      0     -1      0 

#2   ???
fitmod<-fitsde(data=s3,drift=fx1,diffusion=gx1,start = list(theta1=1,theta2=1,theta3=1),pmle="shoji",lower=c(-3,0),upper=c(-1,1))
summary(fitmod)
#Call:
#fitsde(data = s2, drift = fx4, diffusion = gx4, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1), pmle = "shoji", lower = c(-3, 0), 
#       upper = c(-1, 1))
#Coefficients:
#  theta1 theta2 theta3 
#-1      1     -1 

#3   ?
#Call:
#  fitsde(data = s3, drift = fx1, diffusion = gx1, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1), pmle = "shoji", lower = c(-3, 0), 
#         upper = c(-1, 1))
#Coefficients:
#  theta1 theta2 theta3 
#-1      1     -1 

#4   ???
#Call:
#fitsde(data = s4, drift = fx2, diffusion = gx2, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1, theta4 = 1), pmle = "shoji", lower = c(-3, 
#                                                                                                                            0), upper = c(-1, 1))                                                                                                                  0), upper = c(-1, 1))
#Coefficients:
#  theta1 theta2 theta3 theta4 
#-3      0     -1      0 

#5   ???
#Call:
#fitsde(data = s5, drift = fx4, diffusion = gx4, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1, theta4 = 1), pmle = "shoji", lower = c(-3, 
#                                                                                                                            0), upper = c(-1, 1))
#Coefficients:
#  theta1 theta2 theta3 theta4 
#-1      1     -1      1 

#Kessler
fitmod <- fitsde(data=s5,drift=fx4,diffusion=gx4,start = list(theta1=1,theta2=1,theta3=1),pmle="kessler")
summary(fitmod)
fitmod

#1   ???
#Call:
#fitsde(data = s1, drift = fx2, diffusion = gx2, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1, theta4 = 1), pmle = "kessler")
#Coefficients:
#  theta1    theta2    theta3    theta4 
#1.0000448 0.6862942 0.8824043 0.4091350 

#2
#Pseudo maximum likelihood estimation
#Method:  Kessler
#Call:
#  fitsde(data = s2, drift = fx4, diffusion = gx4, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1), pmle = "kessler")
#Coefficients:
#  Estimate   Std. Error
#theta1 0.004397859 0.0003881697
#theta2 0.007841422 0.0000599039
#theta3 0.513984058 0.0014465610
#-2 log L: -125073.5

#Call:
#fitsde(data = s2, drift = fx4, diffusion = gx4, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1), pmle = "kessler")
#Coefficients:
#  theta1      theta2      theta3 
#0.004397859 0.007841422 0.513984058 

#3   ???
#Call:
#fitsde(data = s3, drift = fx1, diffusion = gx1, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1), pmle = "kessler")
#Coefficients:
#  theta1    theta2    theta3 
#0.5498652 0.9359193 0.6267437 

#4   ???
#Call:
#  fitsde(data = s4, drift = fx2, diffusion = gx2, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1, theta4 = 1), pmle = "kessler")
#Coefficients:
#  theta1    theta2    theta3    theta4 
#1.0072354 0.4606634 0.6495840 0.1907163

#5
#Pseudo maximum likelihood estimation
#Method:  Kessler
#Call:
#  fitsde(data = s5, drift = fx4, diffusion = gx4, start = list(theta1 = 1, 
#                                                               theta2 = 1, theta3 = 1), pmle = "kessler")
#Coefficients:
#  Estimate   Std. Error
#theta1 0.01128962 5.614897e-04
#theta2 0.01075011 9.730073e-05
#theta3 0.51575952 1.674067e-03
#-2 log L: -51321

#Call:
#fitsde(data = s5, drift = fx4, diffusion = gx4, start = list(theta1 = 1, 
#                                                             theta2 = 1, theta3 = 1), pmle = "kessler")
#Coefficients:
#  theta1     theta2     theta3 
#0.01128962 0.01075011 0.51575952 

#Q3
library(quantmod)
library(PerformanceAnalytics)
library(gdata)
library(igraph)
library(energy)
library(Matrix)
library(vegan)
library(dtw)
library(utils)

#3.1)
stockData <- new.env()
lookup.symb=c('MMM','AXP','AAPL','BA','CAT','CVX','CSCO','KO','DWDP','XOM','GE','GS',
              'HD','INTC','IBM','JNJ','JPM','MCD','MRK','MSFT','NKE','PFE','PG',
              'TRV','UNH','UTX','VZ','V','WMT','DIS')

#ticker<-c('MMM','AXP','AAPL','BA','CAT','CVX','CSCO','KO','DWDP','XOM','GE','GS',
#          'HD','INTC','IBM','JNJ','JPM','MCD','MRK','MSFT','NKE','PFE','PG',
#          'TRV','UNH','UTX','VZ','V','WMT','DIS')
#getSymbols(ticker,scr='google',from='2013-1-1',to='2017-12-31')

getSymbols(lookup.symb, from='2013-1-1',to='2017-12-31', env=stockData, src="yahoo")

ReturnMatrix=NULL
mean=NULL
sig=NULL
sr=NULL

for(i in 1:length(lookup.symb))
{
  tmp <- get(lookup.symb[i], pos=stockData)   # get data from stockData environment  
  ReturnMatrix=cbind(ReturnMatrix,   (Cl(tmp)-Op(tmp)) / Op(tmp)   )
  colnames(ReturnMatrix)[i]=lookup.symb[i]
}

getwd()
write.csv(ReturnMatrix,"C:/Users/apple/Documents/fe621rm.csv",row.names = FALSE)  

#???
for(i in 1:length(lookup.symb))
{
  for(j in 1:length(ReturnMatrix$MMM))
  {
    tmp <- get(lookup.symb[i], pos=stockData)   # get data from stockData environment  
    ReturnMatrix=cbind(ReturnMatrix,   (Cl(tmp)-Op(tmp)) / Op(tmp)   )
    mean<-cbind(mean,mean(ReturnMatrix[,i]))
    sig<-cbind(sig,(ReturnMatrix[j,i]-mean[,i])^2)
    sr<-cbind(sr,(ReturnMatrix[j,i]-mean[,i])/sig[,i])
    colnames(ReturnMatrix)[i]=lookup.symb[i]
  }
}

getwd()
data1=read.csv("FE621 3-2.csv",header =TRUE)
head(data1)
ts(data1)
data1<-as.matrix(data1)

#-----
for(i in 1:length(lookup.symb))
{
  tmp <- get(lookup.symb[i], pos=stockData)   # get data from stockData environment  
  mean<-cbind(mean,mean(ReturnMatrix[,i]))
  sig<-cbind(sig,sqrt(mean((Cl(tmp)-mean[,i])^2)))
  ReturnMatrix=cbind(ReturnMatrix,   (Cl(tmp)-mean[,i]) / sig[,i]   )
  colnames(ReturnMatrix)[i]=lookup.symb[i]
}
#-----

#3.2)
data2=read.csv("fe621 3-2.csv",header =TRUE)
head(data2)
data2<-as.matrix(data2)

#-----
cor=NULL

for(i in 1:30)
{
  for(j in 1:length(ReturnMatrix$MMM))
  {
    tmp <- get(lookup.symb[i], pos=stockData)   # get data from stockData environment  
    mean<-cbind(mean,mean(ReturnMatrix[,i]))
    sig<-cbind(sig,sqrt(mean(((Cl(tmp)-mean[,i]))^2)))
    ReturnMatrix=cbind(ReturnMatrix,   (Cl(tmp)-mean[,i]) / sig[,i]   )
    cor=cbind(cor,   mean(sum(ReturnMatrix[j,i]*ReturnMatrix[j+1,i]))  )
    colnames(ReturnMatrix)[i]=lookup.symb[i]
  }
}




#3.3)
pc1<-princomp(data2,cor=TRUE,scor=TRUE)
summary(pc1)

evec = pc1$rotation[] 
eval1 = eigen(data2, symmetric=TRUE)
eval = pc1$sd^2
barplot(eval,col=colors)

#3.5)
lookup.symb=c('DIA')
getSymbols(lookup.symb, from='2013-1-1',to='2017-12-31', env=stockData, src="yahoo")

ReturnMatrix=NULL

for(i in 1:length(lookup.symb))
{
  tmp <- get(lookup.symb[i], pos=stockData)   # get data from stockData environment  
  ReturnMatrix=cbind(ReturnMatrix,   (Cl(tmp)-Op(tmp)) / Op(tmp)   )
  colnames(ReturnMatrix)[i]=lookup.symb[i]
}

write.csv(ReturnMatrix,"C:/Users/apple/Documents/fe621 DIA r.csv",row.names = FALSE)  

#3.5)-2
data3=read.csv("fe621 lr.csv",header =TRUE)
colnames(data3)[1]='DIA'
head(data3)

y<-as.numeric(data3[,1])
x<-as.numeric(data3[,2])
plot(y~x)

lm.ab<-lm(y ~ x)
lm.ab
Xm<-mean(x);Xm 
Ym<-mean(y);Ym
b <- sum((x-Xm)*(y-Ym)) / sum((x-Xm)^2) ;b
a <- Ym - b * Xm;a
abline(lm.ab)

eruption.lm = lm(y ~ 1+x, data=faithful)
summary(eruption.lm)$r.squared   #r-square

#3.6)
data4=read.csv("fe621 6-0.csv",header =TRUE)
head(data4)


x1<-as.numeric(data4[,1])
x2<-as.numeric(data4[,2])
x3<-as.numeric(data4[,3])
x4<-as.numeric(data4[,4])
x5<-as.numeric(data4[,5])
y<-as.numeric(data4[,35])
  
lm.ab<-lm(y ~ x1+x2+x3+x4+x5)
lm.ab

plot(y~x1+x2+x3+x4+x5)

#1
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#     -0.04974     -0.22699     -0.02787     -0.07007      0.00602      0.05990  

#2
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#     -0.02803     -0.17508      0.17640     -0.09083     -0.15193      0.15586  

#3
#Call:
#  lm(formula = y ~ x1 + x2 + x3 + x4 + x5)

#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#     -0.04875     -0.16644      0.10390      0.06186     -0.02504     -0.32225  

#4
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.04505     -0.17221      0.10793     -0.15768     -0.02944     -0.02982  

#5
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.03334     -0.16673     -0.20605     -0.16168     -0.17382     -0.06215  

#6
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.02891     -0.18489     -0.44166      0.03149     -0.14671     -0.06677 

#7
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.051304    -0.200419     0.096230     0.004097    -0.044199    -0.193292 

#8
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.05566     -0.17711     -0.07897     -0.09864      0.28056      0.08743  

#9
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.030539    -0.178950    -0.053710    -0.046254    -0.162089     0.009626  

#10
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.02882     -0.18826     -0.44773      0.03276     -0.12775     -0.02873  

#11
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.039807    -0.186856    -0.001456    -0.073220    -0.024511     0.096689 

#12
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.02099     -0.17553      0.15098     -0.09869     -0.23502      0.20222  

#13
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.05342     -0.19086      0.07824     -0.03822      0.10068     -0.06976 

#14
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.05196     -0.18717      0.03594      0.03116     -0.02182     -0.27799  

#15
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.04655     -0.18972     -0.03837     -0.07082     -0.03017     -0.07833  

#16
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.03862     -0.20637     -0.02389      0.20663      0.10844      0.11101  

#17
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.02369     -0.19026      0.11293     -0.07015     -0.20785      0.21664  

#18
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.053184    -0.175378    -0.003837    -0.036195     0.206470    -0.013799  

#19
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.01254     -0.16414     -0.01428      0.41075     -0.04511      0.11552  

#20
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.05482     -0.19472      0.10283      0.05637     -0.00157     -0.27741  

#21
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.03996     -0.16443      0.12098      0.04781      0.03754     -0.04629  

#22
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.020762    -0.178779     0.004865     0.380417     0.016251     0.129579  

#23
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.053521    -0.192451    -0.090280     0.003169     0.275773     0.102399

#24
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.04815     -0.18425      0.05884     -0.17944      0.10311      0.15558  

#25
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.03045     -0.16256      0.09584      0.13652      0.03772      0.07615  

#26
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.04951     -0.20264      0.04369     -0.13376     -0.03843     -0.02119 

#27
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.03837     -0.15988     -0.09807     -0.07155      0.08054      0.07232 

#28
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.04222     -0.18989      0.17129      0.09570     -0.03055     -0.07830  

#29
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.06020     -0.16080     -0.07478     -0.12140      0.32818     -0.02552  

#30
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4 + x5)
#Coefficients:
#  (Intercept)           x1           x2           x3           x4           x5  
#-0.0372241   -0.1830484    0.1229401    0.0375484   -0.0551090   -0.0001604  

#3.6)-2
f<-rt(1,3.5);f
g<-rt(30,3.5);g

#-----
Xm<-mean(x);Xm 
Ym<-mean(y);Ym
b <- sum((x-Xm)*(y-Ym)) / sum((x-Xm)^2) ;b
a <- Ym - b * Xm;a
abline(lm.ab)


#bonus2
library(GetHFData)

