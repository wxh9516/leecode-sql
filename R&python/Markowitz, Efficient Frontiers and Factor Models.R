#4.1
library("IntroCompFinR")
er=matrix(c(0.0427,0.0015,0.0285),3,1)
covmat=matrix(c(0.1,0.02,0.01,
                0.02,0.11,0.03,
                0.01,0.03,0.2),3,3)
gmv<-globalMin.portfolio(er, cov.mat=covmat, shorts = TRUE)

ef<-efficient.frontier(er, cov.mat=covmat, nport = 50, alpha.min =-1.5,
                   alpha.max = 2, shorts = TRUE)
plot(ef)

#4.2
p1<-efficient.portfolio(er, cov.mat=covmat, target.return=0.0427, shorts = TRUE)

#4.3
tr<-0.0015+0.0285
p2<-efficient.portfolio(er, cov.mat=covmat, target.return=tr, shorts = TRUE)

#4.4
tr=(0.0427+0.0015+0.0285)/3;tr
er<-c(0.0427,0.03)
sd<-c(p1$sd,p2$sd)

w1<-matrix(c(0.8231,-0.093,0.2699),3,1)
w2<-matrix(c(0.551,0.2342,0.2147),3,1)

p11<-er[1]*w1[1]   #er of p1
p12<-er[1]*w1[2]
p13<-er[1]*w1[3]

p21<-er[1]*w2[1]
p22<-er[1]*w2[2]
p23<-er[1]*w2[3]

er1<-matrix(c(p11,p12,p13,
              p21,p22,p23),3,2)
covmat<-cov(er1)
#covmat<-matrix(c(sd[1]^2,sd[1]*sd[2],sd[1]*sd[2],sd[2]^2),2,2)

p3<-efficient.portfolio(er, cov.mat=covmat, target.return=tr, shorts = TRUE)

#method 2
library(pracma)
A<-matrix(c(sd[1]*sd[1]^2,sd[1]*sd[2],er[1],1,
            sd[1]*sd[2],sd[2]^2,er[2],1,
            er[1],er[2],0,0,
            1,1,0,0),4,4)
B<-matrix(c(0,0,tr,1),4,1)
inv(A)%*%B  #w1,w2



#4.5.1
#erp<-c(0.0427,	0.03)
#sdp<-c(0.2883545	,0.2369976)
#tau=seq(-5, 5, 0.1)
#sd<-NULL
#er<-NULL
#j=1
#for (i in tau){
#  sd[j]<-sqrt(erp[1]^2*i^2+erp[2]^2*(1-i)^2)
#  er[j]<-erp[1]*i+erp[2]*(1-i)
#  j=j+1
#}
#plot(sd,er,main='efficient frontier',col='red')

s1<-c(0.2883545,0.0427)
points(s1[1],s1[2],col='red',pch='+')
text(s1[1],s1[2],'p1')

s1<-c(0.2369976,0.03)
points(s1[1],s1[2],col='red',pch='+')
text(s1[1],s1[2],'p2')

s1<-c(sqrt(0.1),	0.0427)
points(s1[1],s1[2],col='black',pch='+')
text(s1[1],s1[2],'s1')

s2<-c(sqrt(0.11),0.0015)
points(s2[1],s2[2],col='black',pch='+')
text(s2[1],s2[2],'s2')

s3<-c(sqrt(0.2),0.0285)
points(s3[1],s3[2],col='black',pch='+')
text(s3[1],s3[2],'s3')

#5.1
library(pracma)
er=matrix(c(0.0427,0.0015,0.0285),3,1)
er2<-matrix(c(0.0427-0.005,0.0015-0.005,0.0285-0.005),3,1)
e<-matrix(c(1,1,1),1,3)
#A<-t(er)%*%covmat%*%t(e)
#B<-t(er)%*%covmat%*%er
#C<-e%*%covmat%*%t(e)

A<-inv(covmat)%*%er2
w1<-A[1,]/sum(A)
w2<-A[2,]/sum(A)
w3<-A[3,]/sum(A)


#
covmat=matrix(c(0.1,0.02,0.01,
                0.02,0.11,0.03,
                0.01,0.03,0.2),3,3)
tp<-tangency.portfolio(er, cov.mat=covmat, risk.free=0.005, shorts = TRUE)

#5.2
plot(Sigma,Ep,type = 'l',ylim=c(0,0.07),xlim=c(0,0.5))
tr <- seq(rf,Ep[signal]+200*dr,dr)
ts <- seq(0,Sigma[signal]+200*ds,ds)
lines(x=ts,y=tr,col="#8E354A",lty=2)

#
sig<-seq(0,1,0.01)
ep<-NULL
j=1
for (i in sig){
  ep[j]=0.05+(0.113546-0.05)/0.113546*i
  j=j+1
}
par(new=TRUE)
plot(sig,ep,axes=FALSE,col='blue')
lines(sig,ep)



#
er<-matrix(c(0.04465566,
             -0.00054135,
             0.00898035)
             ,3,1)

ef1<-efficient.frontier(er, cov.mat=covmat, nport = 2, alpha.min = -5,
                       alpha.max = 5, shorts = TRUE)

par(new=TRUE)
plot(ef1,axes=FALSE,col='red')

#
tr<-0.02423333

erp1<-c(0.05309515,	0.005)

tau1=seq(-5, 5, 0.1)
sd1<-NULL
er1<-NULL
j=1
for (i in tau1){
  sd1[j]<-sqrt(erp1[1]^2*i^2+erp1[2]^2*(1-i)^2)
  er1[j]<-erp1[1]*i+erp1[2]*(1-i)
  j=j+1
}
#plot(sd1,er1,main='efficient frontier',col='red')
#lines(sd,er,main='efficient frontier')

#5.3
P3 <- sqrt(t(w)%*%sigma%*%w);P3

#
tr<-0.07
er<-matrix(c(0.0427,0.0015,0.0285),3,1)
efficient.portfolio(er, cov.mat=covmat, target.return=tr, shorts = TRUE)



#5.4
sigma <-
  matrix(c(0.1,0.02,0.01,0.02,0.11,0.03,0.01,0.03,0.2),3,3,byrow=T)
E <- matrix(c(0.0427,0.0015,0.0285),3,1)
op <- matrix(c(1,1,1),1,3)
o <- matrix(c(1,1,1),3,1)
A <- as.numeric(op%*%solve(sigma)%*%E)
B <- as.numeric(t(E)%*%solve(sigma)%*%E)
C <- as.numeric(op%*%solve(sigma)%*%o)
D <- as.numeric(B*C-A^2)
H <- B-2*rf*A+rf^2*C

#
rf<-as.numeric(0.005)
Es <- 0.02
P4.r <- rf+sqrt(H)*sqrt(Es)


#-----
dr <- (Ep[signal]-rf)/100
ds <- (Sigma[signal]-0)/100
V <- matrix(c(0.10,0.02,0.01,0.0427,1,0.02,0.11,0.03,0.0015,1,0.01,
              + 
                0.03,0.20,0.0285,1,0.0427,0.0015,0.0285,0,0,1,1,1,0,0),5,5,byrow = T)
S <- matrix(c(0,0,0,Ep[signal],1),ncol = 1, nrow =5)
Pt <- solve(V) %*% S
pt <- Pt[-4:-5]

rf <- 0.005
sharp <- (Ep-rf)/Sigma
tangent <- max(sharp)
signal <- which(sharp==tangent)

x <- seq(from = -1, to = 1.5, by = 0.01)
ome1 <- c(0.2296576,0.6207829,0.1495596)
ome2 <- c(0.8230969,-0.0930379,0.2699410)
sig <-
  matrix(c(0.1,0.02,0.01,0.02,0.11,0.03,0.01,0.03,0.2),3,3,byrow = T)
E <- c(0.0427,0.0015,0.0285)
Ep <- NULL
Sigma <- NULL
for(i in 1:length(x)){ome <- x[i]*ome1+(1-x[i])*ome2 
Ep[i] <- t(ome)%*%E
Sigma[i] <- sqrt(t(ome)%*%sig%*%ome)}
#-----

alpha <- (P4.r-rf)/(Ep[signal]-rf)
P4.omega <- alpha * pt
P4.omega

#5.5
rt <- 0.07
w <- solve(sigma)%*%(E-c(rf,rf,rf))*((rt-rf)/H)
P3 <- sqrt(t(w)%*%sigma%*%w)

points(y= 0.02399439,x= sqrt(0.02),pch=1,col="#CB1B45")
text( sqrt(0.02), 0.02399439,'p4')

points(y=rt,x=P3,pch=19,col="#20604F")
text(P3,rt,'P3')

points(y=0.0427, x= 0.2883545,pch = 0,col="#D0104C")
text(0.2883545,0.0427,'P1')

points(y=0.03,x= 0.2369976,pch = 0,col="#64363C")
text(0.2369976,0.03,'p2')

points(y=0.0285,x=sqrt(0.20),pch=18,col="yellow")
text(sqrt(0.20),0.0285,'s3')

points(y=0.0015,x=sqrt(0.11),pch=18,col="darkblue")
text(x=sqrt(0.11),0.0015,'s2')

points(y=0.0427,x=sqrt(0.10),pch=18,col="darkred")
text(sqrt(0.10),0.0427,'s1')
