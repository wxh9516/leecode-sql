.libPaths("C:/Temp/win-library")

symbol<-c('aa','ge','jnj','msft','axp','gm','jpm','pg','ba','hd','ko','sbc',
          'c','hon','mcd','t','cat','hwp','mmm','utx','dd','ibm','mo','wmt','dis',
          'intc','mrk','xom','ek','ip')
his_ret<-c(17.3,16.91,16.98,23.95,15.00,4.59,5.31,7.81,-4.18,29.38,
           -0.57,10.10,24.55,-0.05,2.74,-1.24,7.97,-4.97,9.03,13.39,0.44,
           21.99,10.47,30.23,-2.59,13.59,8.65,11.1,-17,1.24)
capm_ret<-c(15.43,12.15,9.39,14.89,15.65,13.5,15.89,8.04,14.16,
            11.59,10.95,7.76,16.59,16.89,10.7,8.88,13.08,14.92,10.43,16.51,12.21,
            13.47,7.57,10.94,12.89,15.83,8.95,8.39,11.08,14.8)
imp_equ_ret<-c(13.81,13.57,9.75,20.41,14.94,12.83,16.46,7.56,11.81,12.52,10.92,
               8.79,16.97,14.5,10.44,10.74,10.92,14.45,8.66,15.47,10.98,14.66,
               6.86,12.77,12.41,18.7,9.22,7.88,10.61,12.92)
df<-data.frame(symbol,his_ret,capm_ret,imp_equ_ret,stringsAsFactors = FALSE)
summary(df)

his_w<-c(2.2386,-0.6544,-0.7008,0.0354,-0.1538,0.0576,-2.1339,0.92,-1.1135,
         2.8001,-1.5158,0.1711,2.9390,0.1565,-0.6168,-0.8644,-0.7067,-1.6302,0.5684,-0.238,
         -1.3199,0.3692,1.3678,0.2103,0.0575,0.9781,1.4434,2.8175,-1.4836,-1.1307)
capm_w<-c(0.0267,0.098,0.0611,0.0322,0.0554,0.0344,0.0194,-0.0133,0.0471,0.0011,
          0.057,-0.0428,0.0511,0.0271,0.0132,0.0404,0.051,0.066,0.0473,0.0438,0.0103,
          0.0557,0.0131,0.0089,-0.0235,-0.0196,0.0461,0.041,0.0204,0.0476)
imp_equ_w<-c(0.0088,0.1162,0.0529,0.1041,0.0139,0.0079,0.0209,0.0299,0.009,0.0349,0.0342,
             0.0384,0.0758,0.008,0.0099,0.0187,
             0.0052,0.0116,0.0135,0.0088,0.0129,0.0608,0.029,0.0749,0.0123,0.0616,
             0.039,0.0785,0.0025,0.0057)
mkt_cap_w<-c(0.0088,0.1162,0.0529,0.1041,0.0139,0.0079,0.0209,0.0299,0.009,0.0349,0.0342,
             0.0384,0.0758,0.008,0.0099,0.0187,
             0.0052,0.0116,0.0135,0.0088,0.0129,0.0608,0.029,0.0749,0.0123,0.0616,
             0.039,0.0785,0.0025,0.0057)
df2<-data.frame(symbol,his_w,capm_w,imp_equ_w,mkt_cap_w)
summary(df2)

#
library(zoo)
bl.compute.risk.aversion <- function(bench, risk.free = 0)
  
{
 
  
  lambda = mean(coredata(bench) - coredata(risk.free)) / var(coredata(bench))
  
  return( as.double(lambda) )
  
}

bl.compute.eqret <- function(
  risk.aversion, 
  cov,       
  cap.weight, 
  risk.free = 0.05 
)
{
  return( risk.aversion * cov %*% cap.weight +  risk.free)    
}

risk.aversion = bl.compute.risk.aversion( df$his_ret )

cap.weight = matrix(c(df2$his_w),byrow = TRUE)


#
setwd("C:/Temp")
cov = read.table("fe630 cov.csv", sep=',',header = TRUE)

cov<-as.matrix(cov)

eq<-bl.compute.eqret( risk.aversion, cov, cap.weight )


#
P<-matrix(c(rep(0,90)),3,30)
P[1,29]<-1
P[2,3]<-1
P[2,8]<--1
P[3,2]<-1/2
P[3,6]<--1/3
P[3,10]<-0.5
P[3,30]<--1/3
P[3,26]<--1/3

omega<-matrix(c(rep(0,9)),3,3)
omega[1,1]<-1/0.5*0.14
omega[2,2]<-1/0.65*0.14
omega[3,3]<-1/0.3*0.14

#Q<-matrix(c(10,3,1.5),3,1)   #view

risk_aversion_parameter <- 2.25
implied_return = risk_aversion_parameter*cov%*%mkt_cap_w

risk_free = 0.05 # risk free rate
implied_return = implied_return + risk_free
implied_return*100   #percent


Q <- c(10,3,1.5)/100   #view vector
Q = as.matrix(Q)
tau <- 0.873

library(MASS)
view<-P
covariance<-cov
exp_return = crossprod(ginv(ginv(tau*covariance) + t(view)%*%ginv(omega)%*%view),
                       (ginv(tau*covariance)%*%implied_return + t(view)%*%ginv(omega)%*%Q))
exp_return*100


e_return = exp_return-risk_free
mu <- as.vector(e_return)
weights = ginv(risk_aversion_parameter*covariance)%*%mu
weights = weights*100


final_table = data.frame (symbol,
                          Exp_ret = exp_return*100,
                          Imp_Equb_ret = implied_return*100,
                          Diff_ret = exp_return*100 -implied_return*100,
                          New_weight = round(weights,2),
                          Mkt_cap_weight = mkt_cap_w*100, 
                          Diff_weight = round(round(weights,2)-mkt_cap_w*100,2))
final_table


