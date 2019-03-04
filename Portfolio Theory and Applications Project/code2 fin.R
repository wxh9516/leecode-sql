library(tseries)
library(zoo)
library(Quandl)
library(quantmod)
library(lubridate)
library(quantmod)
library(quadprog)
library(PerformanceAnalytics)
library(EnvStats)
library(Gmisc)
library(pander)
library(gridExtra)
library(nloptr)


# Download the ETF data form yahoo finance
#fxe <- get.hist.quote(instrument = "fxe", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#fxe_data <- data.frame(fxe)
#ewj <- get.hist.quote(instrument = "ewj", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#ewj_data <- data.frame(ewj)
#gld <- get.hist.quote(instrument = "gld", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#gld_data <- as.matrix(gld)
#qqq <- get.hist.quote(instrument = "qqq", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#qqq_data <- as.matrix(qqq)
#spy <- get.hist.quote(instrument = "spy", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#spy_data <- as.matrix(spy)
#shv <- get.hist.quote(instrument = "shv", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#shv_data <- as.matrix(shv)
#dba <- get.hist.quote(instrument = "dba", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#dba_data <- as.matrix(dba)
#uso <- get.hist.quote(instrument = "uso", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#uso_data <- as.matrix(uso)
#xbi <- get.hist.quote(instrument = "xbi", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#xbi_data <- as.matrix(xbi)
#ilf <- get.hist.quote(instrument = "ilf", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#ilf_data <- as.matrix(ilf)
#gaf <- get.hist.quote(instrument = "gaf", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#gaf_data <- as.matrix(gaf)
#epp <- get.hist.quote(instrument = "epp", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#epp_data <- as.matrix(epp)
#fez <- get.hist.quote(instrument = "fez", start = "2007-03-23", end = "2018-12-01", quote = "Close")
#fez_data <- as.matrix(fez)
#Universe <- c("FXE","EWJ","GLD","QQQ","SPY","SHV","DBA","USO","XBI","ILF","GAF","EPP","FEZ")
#i <-1
#for(name in Universe){
  etf_data[,i] <- get.hist.quote(instrument = name, start = "2007-03-23", end ="2017-07-24", quote = "Close")
  i <- i+1
#}
#save data into csv file
#write.csv(etf_data,"etf_data.csv")

#Import data
#etf_data <-read.table("C:/Users/QiYuan/Desktop/etf_data.csv", sep=',',header = TRUE)
#row.names(etf_data) <- row.names(fxe_data)
#French-Fama-Three-Factor
#FF <- read.table('C:/Users/QiYuan/Desktop/F-F_Research_Data_Factors_daily.csv',sep=',',header = TRUE)

#Compute the log daily returns
ETF <- c("FXE","EWJ","GLD","QQQ","SPY","SHV","DBA","USO","XBI","ILF","GAF","EPP","FEZ")
getSymbols(ETF, from = "2007-03-23", to = "2017-07-24", src = "yahoo")
ETF_return = NULL
for (i in 1:length(ETF)){
  priceData = get(ETF[i])
  logReturn = dailyReturn(priceData, type = 'arithmetic')
  ETF_return = cbind(ETF_return, logReturn)
  colnames(ETF_return)[i] = ETF[i]
}

#Annualize Daily Returns
#Daily_Returns_Annualized <- etf_return*252
#head(Daily_Returns_Annualized)
#tail(Daily_Returns_Annualized)


# The following is a division of the time period before, during and after the 08 financial crisis
start <- c('2007-03-23', '2008-03-11', '2009-07-01')
end <- c('2008-03-10', '2009-06-30', '2017-07-24')
start_FF <- c("20070323", "20080311", "20090701")
end_FF <- c("20080310", "20090630", "20170724")
ff <- tempfile()
download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip", ff, quiet = TRUE)
FF <- read.csv(unz(ff,"F-F_Research_Data_Factors_daily.CSV"), skip =3,
                   colClasses = c( X = "character",
                                   Mkt.RF = "numeric",
                                   SMB = "numeric",
                                   HML = "numeric",
                                   RF = "numeric"))
FF_Start1 <- FF[FF$X=="20070323",] 
FF_End1 <- FF[FF$X=="20080310",]
FF_Start1 <- as.numeric(labels(FF_Start1)[1])
FF_End1 <- as.numeric(labels(FF_End1)[1])
FF1 <- FF[FF_Start1:FF_End1,]

FF_Start2 <- FF[FF$X=="20080311",] 
FF_End2 <- FF[FF$X=="20090630",]
FF_Start2 <- as.numeric(labels(FF_Start2)[1])
FF_End2 <- as.numeric(labels(FF_End2)[1])
FF2 <- FF[FF_Start2:FF_End2,]

FF_Start3 <- FF[FF$X=="20090701",] 
FF_End3 <- FF[FF$X=="20170724",]
FF_Start3 <- as.numeric(labels(FF_Start3)[1])
FF_End3 <- as.numeric(labels(FF_End3)[1])
FF3 <- FF[FF_Start3:FF_End3,]

#rowname_ETF<-ETF_return[,0]
#original data_period function
#data_period = function(start, end) {
  
#  # Get the time period range
#  start_index = match(start, rowname_ETF)
#  end_index = match(end, rowname_ETF)  
#  
#  result = c(start_index:end_index)
#  result
#}

#modified data_period function
data_period = function(start, end){
  start_index = (FF[FF$X== start,])
  end_index = (FF[FF$X== end,])  
  result = FF[as.numeric(labels(start_index)[1]):as.numeric(labels(end_index)[1]),]
  result$X
}
f <- data_period("20080311","20090630")
f[1:50]



strategy <- function(rho, lambda, wp, beta_i, beta_T){
  Q = diag(1, nrow = nrow(wp), ncol = nrow(wp))
  objection = function(w) {
    w = matrix(w, ncol = 1)
    return(-t(rho) %*% w + lambda*t(w - wp) %*% Q %*% (w - wp))
  }
  constraint = function(w) {
    w = matrix(w, ncol = 1)
    h = numeric(2)
    h[1] = beta_i %*% w - beta_T
    h[2] = sum(w) - 1
    return(h)
  }
  #optimal value
  S = cobyla(x0 = rep(1, length(wp)), fn = objection, hin = constraint, lower = rep(-2, length(wp)), upper = rep(2, length(wp)),
             nl.info = FALSE, control = list(xtol_rel = 1e-8, maxeval = 2000))
  #optimal solution
  return(S$par)
}

# Use linear regression to estimate the coefficients (beta, bs, bv) of French-Fama model
# Estimate coefficients (short, mid, long-term model)
getFactorValue <- function(FF, Ri) { 
  # Making a linear regression of the time series Ri - rf against the times series rM - rf, rSMB, rHML
  regression = lm(Ri - FF$RF ~ FF$Mkt.RF + FF$SMB + FF$HML)
  coefficient = coef(regression)
  result_list = list(alpha = as.numeric(coefficient[1]),  # Interception
                     beta = as.numeric(coefficient[2]),
                     bs = as.numeric(coefficient[3]),
                     bv = as.numeric(coefficient[4]))
  
  return(result_list)
}

# Time series model
time_series_model <- function(crisis, term, beta_T){
  #financial period and the term length
  if (crisis == 'before') {
    general_period = data_period('20070323', '20080310')
    if (term == 'short') {
      specific_period = general_period[1:50]
    } else if (term == 'mid') {
      specific_period = general_period[1:100]
    } else if (term == 'long') {
      specific_period = general_period[1:200]
    }
  } else if (crisis == 'during'){
    general_period = data_period('20080311', '20090630')
    if (term == 'short') {
      specific_period = general_period[1:50]  
    } else if (term == 'mid') {
      specific_period = general_period[1:100]  
    } else if (term == 'long') {
      specific_period = general_period[1:200]  
    }
    
  } else if (crisis == 'after'){
    general_period = data_period('20090701', '20170724')
    if (term == 'short') {
      specific_period = general_period[1:50]
    } else if (term == 'mid') {
      specific_period = general_period[1:100]
    } else if (term == 'long') {
      specific_period = general_period[1:200]
    }
  }
  rho_i = NULL
  wp = matrix(rep(1/length(ETF), length(ETF)), ncol = 1)
  beta_matrix = NULL
  beta_i = c()
  for (j in 1:length(ETF)){
    FF_specific = FF[which(result$X %in% specific_period[1:length(specific_period)]), ]
    Ri_specific = ETF_return[specific_period[1:length(specific_period)], j]
    Ri_specific[which(Ri_specific==Inf)] = 0 
    time_series_coefficient = getFactorValue(FF_specific, Ri_specific)
    beta_matrix = rbind(time_series_coefficient$beta, time_series_coefficient$bs,
                        time_series_coefficient$bv)
    F_matrix = as.matrix(FF[which(result$X %in% specific_period[1:length(specific_period)]), 2:4])
    alpha = time_series_coefficient$alpha
    TSreturn = alpha + F_matrix %*% beta_matrix + as.matrix(FF[which(result$X %in% specific_period[1:length(specific_period)]),5]/250)
    rho_i = cbind(rho_i, TSreturn)
    beta_i[j] = cov(rho_i[1:length(rho_i[,1]),j], ETF_return$SPY[1:length(rho_i[,1])])/var(ETF_return$SPY) 
  }
  weight = strategy(colMeans(rho_i), 0.001, wp, beta_i, beta_T)
  return(list(opt_weight = weight,
              excess_return = rho_i))
}


# Target 0.5
# Weight
# Before crisis
TS_w_0.5_b_s = time_series_model('before', 'short', 0.5)$opt_weight
TS_w_0.5_b_m = time_series_model('before', 'mid', 0.5)$opt_weight
TS_w_0.5_b_l = time_series_model('before', 'long', 0.5)$opt_weight

# During crisis
TS_w_0.5_d_s = time_series_model('during', 'short', 0.5)$opt_weight
TS_w_0.5_d_m = time_series_model('during', 'mid', 0.5)$opt_weight
TS_w_0.5_d_l = time_series_model('during', 'long', 0.5)$opt_weight

# After crisis
TS_w_0.5_a_s = time_series_model('after', 'short', 0.5)$opt_weight
TS_w_0.5_a_m = time_series_model('after', 'mid', 0.5)$opt_weight
TS_w_0.5_a_l = time_series_model('after', 'long', 0.5)$opt_weight

# Return
# Before crisis
TS_r_0.5_b_s = time_series_model('before', 'short', 0.5)$excess_return
TS_r_0.5_b_m = time_series_model('before', 'mid', 0.5)$excess_return
TS_r_0.5_b_l = time_series_model('before', 'long', 0.5)$excess_return

# During crisis
TS_r_0.5_d_s = time_series_model('during', 'short', 0.5)$excess_return
TS_r_0.5_d_m = time_series_model('during', 'mid', 0.5)$excess_return
TS_r_0.5_d_l = time_series_model('during', 'long', 0.5)$excess_return

# After crisis
TS_r_0.5_a_s = time_series_model('after', 'short', 0.5)$excess_return
TS_r_0.5_a_m = time_series_model('after', 'mid', 0.5)$excess_return
TS_r_0.5_a_l = time_series_model('after', 'long', 0.5)$excess_return

# Target 1
# Weight
# Before crisis
TS_w_1_b_s = time_series_model('before', 'short', 1)$opt_weight
TS_w_1_b_m = time_series_model('before', 'mid', 1)$opt_weight
TS_w_1_b_l = time_series_model('before', 'long', 1)$opt_weight

# During crisis
TS_w_1_d_s = time_series_model('during', 'short', 1)$opt_weight
TS_w_1_d_m = time_series_model('during', 'mid', 1)$opt_weight
TS_w_1_d_l = time_series_model('during', 'long', 1)$opt_weight

# After crisis
TS_w_1_a_s = time_series_model('after', 'short', 1)$opt_weight
TS_w_1_a_m = time_series_model('after', 'mid', 1)$opt_weight
TS_w_1_a_l = time_series_model('after', 'long', 1)$opt_weight

# Return
# Before crisis
TS_r_1_b_s = time_series_model('before', 'short', 1)$excess_return
TS_r_1_b_m = time_series_model('before', 'mid', 1)$excess_return
TS_r_1_b_l = time_series_model('before', 'long', 1)$excess_return

# During crisis
TS_r_1_d_s = time_series_model('during', 'short', 1)$excess_return
TS_r_1_d_m = time_series_model('during', 'mid', 1)$excess_return
TS_r_1_d_l = time_series_model('during', 'long', 1)$excess_return

# After crisis
TS_r_1_a_s = time_series_model('after', 'short', 1)$excess_return
TS_r_1_a_m = time_series_model('after', 'mid', 1)$excess_return
TS_r_1_a_l = time_series_model('after', 'long', 1)$excess_return

# Target 1.5
# Weight
# Before crisis
TS_w_1.5_b_s = time_series_model('before', 'short', 1.5)$opt_weight
TS_w_1.5_b_m = time_series_model('before', 'mid', 1.5)$opt_weight
TS_w_1.5_b_l = time_series_model('before', 'long', 1.5)$opt_weight

# During crisis
TS_w_1.5_d_s = time_series_model('during', 'short', 1.5)$opt_weight
TS_w_1.5_d_m = time_series_model('during', 'mid', 1.5)$opt_weight
TS_w_1.5_d_l = time_series_model('during', 'long', 1.5)$opt_weight

# After crisis
TS_w_1.5_a_s = time_series_model('after', 'short', 1.5)$opt_weight
TS_w_1.5_a_m = time_series_model('after', 'mid', 1.5)$opt_weight
TS_w_1.5_a_l = time_series_model('after', 'long', 1.5)$opt_weight

# Return
# Before crisis
TS_r_1.5_b_s = time_series_model('before', 'short', 1.5)$excess_return
TS_r_1.5_b_m = time_series_model('before', 'mid', 1.5)$excess_return
TS_r_1.5_b_l = time_series_model('before', 'long', 1.5)$excess_return

# During crisis
TS_r_1.5_d_s = time_series_model('during', 'short', 1.5)$excess_return
TS_r_1.5_d_m = time_series_model('during', 'mid', 1.5)$excess_return
TS_r_1.5_d_l = time_series_model('during', 'long', 1.5)$excess_return

# After crisis
TS_r_1.5_a_s = time_series_model('after', 'short', 1.5)$excess_return
TS_r_1.5_a_m = time_series_model('after', 'mid', 1.5)$excess_return
TS_r_1.5_a_l = time_series_model('after', 'long', 1.5)$excess_return






# Min Variance with 10% Target Return
strategy_minvar = function(rho, lambda, sigma, wp, target_return) {
  Q = diag(1, nrow = nrow(wp), ncol = nrow(wp))
  objection = function(w) {
    w = matrix(w, ncol = 1)
    return(t(w) %*% sigma %*% w + lambda*t(w - wp) %*% Q %*% (w - wp)) 
  }
  constraint = function(w) {
    w = matrix(w, ncol = 1)
    h = numeric(2)
    h[1] = t(rho) %*% w - target_return
    h[2] = sum(w) - 1
    return(h)
  }
  S = cobyla(x0 = rep(1, length(wp)), fn = objection, hin = constraint, lower = rep(-2, length(wp)), upper = rep(2, length(wp)),
             nl.info = FALSE, control = list(xtol_rel = 1e-8, maxeval = 2000))
  
  #optimal solution
  return(S$par)
}

# The min variance model
#£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿£¿
min_variance_model = function(crisis, term, target_return){
  if (crisis == 'before') {
    general_period = data_period('20070323', '20080310')
    if (term == 'short') {
      specific_period = general_period[1:50]
    } else if (term == 'mid') {
      specific_period = general_period[1:100]
    } else if (term == 'long') {
      specific_period = general_period[1:200]
    }
  } else if (crisis == 'during') {
    general_period = data_period('20080311', '20090630')
    if (term == 'short') {
      specific_period = general_period[1:50]  
    } else if (term == 'mid') {
      specific_period = general_period[1:100]  
    } else if (term == 'long') {
      specific_period = general_period[1:200]  
    }
  } else if (crisis == 'after') {
    general_period = data_period('20090701', '20170724')
    if (term == 'short') {
      specific_period = general_period[1:50]
    } else if (term == 'mid') {
      specific_period = general_period[1:100]
    } else if (term == 'long') {
      specific_period = general_period[1:200]
    }
  }
  
  rho_i = NULL
  wp = matrix(rep(1/length(ETF), length(ETF)), ncol = 1)  # the composition of a reference portfolio
  beta_vector = NULL
  bs_vector = NULL
  bv_vector = NULL
  B = NULL
  
  for (j in 1:length(ETF)) {
    FF_specific = FF[which(result$X %in% specific_period[1:length(specific_period)]), ]
    Ri_specific = ETF_return[specific_period[1:length(specific_period)], j]
    Ri_specific[which(Ri_specific==Inf)] = 0  
    time_series_coefficient = getFactorValue(FF_specific, Ri_specific)
    
    beta_vector = cbind(beta_vector, time_series_coefficient$beta)
    bs_vector = cbind(bs_vector, time_series_coefficient$bs)
    bv_vector = cbind(bv_vector, time_series_coefficient$bv)
    alpha = time_series_coefficient$alpha
    
    min_return = FF$RF[which(result$X %in% specific_period[1:length(specific_period)])] + time_series_coefficient$beta*FF$Mkt.RF[which(result$X %in% specific_period[1:length(specific_period)])] + 
      time_series_coefficient$bs*FF$SMB[which(result$X %in% specific_period[1:length(specific_period)])] + 
      time_series_coefficient$bv*FF$HML[which(result$X %in% specific_period[1:length(specific_period)])] + alpha
    rho_i = cbind(rho_i, min_return) 
    
  }
  
  beta_matrix = t(rbind(beta_vector, bs_vector, bv_vector))
  the_var = var(ETF_return[specific_period,])
  D = diag(c(diag(replace(var(ETF_return[specific_period,]), is.na(the_var), 0))), 13, 13)  # (6)
  
  sigma = beta_matrix%*% cov(FF[which(result$X %in% specific_period[1:length(specific_period)]), 2:4]) %*% t(beta_matrix) +D  # (10)
  
  the_weight = strategy_minvar(colMeans(rho_i), lambda=0.01, sigma, wp, target_return)
  
  new_weight = c()
  for (i in 1:length(the_weight)) {
    if (the_weight[i] > 0) {
      new_weight[i] = the_weight[i] - 1
    } else if (the_weight[i] < 0) {
      new_weight[i] = the_weight[i] + 1
    }
  }
  
  return(list(opt_weight = new_weight,
              excess_return = rho_i))
}




#--------------------------------------------------------#
  # Min Variance Result
  # Weight
  # Before crisis
MV_w_b_s<- min_variance_model('before', 'short', target_return = 0.1)$opt_weight

#1
mvtest<-min_variance_model('before', 'short', target_return = 0.1)
mvtest<-as.matrix(c(as.numeric(unlist(mvtest))),ncol=13)
rt<-rowMeans(mvtest)
plot(rt,ylim = c(-0.04,0.04),pch=".")
lines(rt)

d<-density(rt)
sp=spline(d,n=1000)
plot(sp,xlim=c(-0.05,0.05),pch='.')
lines(sp)


MV_w_b_m = min_variance_model('before', 'mid', target_return = 0.1)$opt_weight

mvtest<-min_variance_model('before', 'mid', target_return = 0.1)
mvtest<-as.matrix(c(as.numeric(unlist(mvtest))),ncol=13)
rt<-rowMeans(mvtest)
plot(rt,ylim = c(-0.04,0.04),pch=".")
lines(rt)

d<-density(rt)
sp=spline(d,n=1000)
plot(sp,xlim=c(-0.05,0.05),pch='.')
lines(sp)

MV_w_b_l = min_variance_model('before', 'long', target_return = 0.1)$opt_weight

mvtest<-min_variance_model('before', 'long', target_return = 0.1)
mvtest<-as.matrix(c(as.numeric(unlist(mvtest))),ncol=13)
rt<-rowMeans(mvtest)
plot(rt,ylim = c(-0.06,0.06),pch=".")
lines(rt)

d<-density(rt)
sp=spline(d,n=1000)
plot(sp,xlim=c(-0.05,0.05),pch='.')
lines(sp)

# During crisis
MV_w_d_s = min_variance_model('during', 'short', target_return = 0.1)$opt_weight

mvtest<-min_variance_model('during', 'short', target_return = 0.1)
mvtest<-as.matrix(c(as.numeric(unlist(mvtest))),ncol=13)
rt<-rowMeans(mvtest)
plot(rt,ylim = c(-0.06,0.06),pch=".")
lines(rt)

d<-density(rt)
sp=spline(d,n=1000)
plot(sp,xlim=c(-0.05,0.05),pch='.')
lines(sp)

MV_w_d_m = min_variance_model('during', 'mid', target_return = 0.1)$opt_weight

mvtest<-min_variance_model('during', 'mid', target_return = 0.1)
mvtest<-as.matrix(c(as.numeric(unlist(mvtest))),ncol=13)
rt<-rowMeans(mvtest)
plot(rt,ylim = c(-0.06,0.06),pch=".")
lines(rt)

d<-density(rt)
sp=spline(d,n=1000)
plot(sp,xlim=c(-0.05,0.05),pch='.')
lines(sp)

MV_w_d_l = min_variance_model('during', 'long', target_return = 0.1)$opt_weight

mvtest<-min_variance_model('during', 'long', target_return = 0.1)
mvtest<-as.matrix(c(as.numeric(unlist(mvtest))),ncol=13)
rt<-rowMeans(mvtest)
plot(rt,ylim = c(-0.4,
                 0.4),pch=".")
lines(rt)

d<-density(rt)
sp=spline(d,n=1000)
plot(sp,xlim=c(-0.05,0.05),pch='.')
lines(sp)

# After crisis
MV_w_a_s = min_variance_model('after', 'short', target_return = 0.1)$opt_weight
MV_w_a_m = min_variance_model('after', 'mid', target_return = 0.1)$opt_weight
MV_w_a_l = min_variance_model('after', 'long', target_return = 0.1)$opt_weight

# Return
# Before crisis
MV_r_b_s = min_variance_model('before', 'short', target_return = 0.1)$excess_return

mvtest<-min_variance_model('before', 'short', target_return = 0.1)
mvtest<-as.matrix(c(as.numeric(unlist(mvtest))),ncol=13)
rt<-rowMeans(mvtest)
plot(rt,ylim = c(-0.4,
                 0.4),pch=".")
lines(rt)

MV_r_b_m = min_variance_model('before', 'mid', target_return = 0.1)$excess_return
MV_r_b_l = min_variance_model('before', 'long', target_return = 0.1)$excess_return

# During crisis
MV_r_d_s = min_variance_model('during', 'short', target_return = 0.1)$excess_return
MV_r_d_m = min_variance_model('during', 'mid', target_return = 0.1)$excess_return
MV_r_d_l = min_variance_model('during', 'long', target_return = 0.1)$excess_return

# After crisis
MV_r_a_s = min_variance_model('after', 'short', target_return = 0.1)$excess_return
MV_r_a_m = min_variance_model('after', 'mid', target_return = 0.1)$excess_return
MV_r_a_l = min_variance_model('after', 'long', target_return = 0.1)$excess_return

#--------------------------------------------------------#
# Create tables and plots
getindicators = function(dataset) {
  
  # Define the indictors
  cumReturn = NULL; meanReturn = NULL; minReturn = NULL
  maxdraw = NULL; vol = NULL; sharp = NULL
  skewness = NULL; kurtosis = NULL; mod_VaR = NULL; cVaR = NULL
  
  # Compute indictors
  for (i in 1:ncol(dataset)) {
    
    cumReturn[i] = Return.cumulative(dataset[,i], geometric = TRUE)
    meanReturn[i] = mean(dataset[,i])
    minReturn[i] = min(dataset[,i])
    maxdraw[i] = maxDrawdown(dataset[,i])
    vol[i] = sd(dataset[,i])
    sharp[i] = (mean(dataset[,i]) - mean(FF$RF))/sd(dataset)
    skewness[i] = skewness(dataset[,i])
    kurtosis[i] = kurtosis(dataset[,i])
    mod_VaR[i] = VaR(dataset[,i], p=.95, method="modified")
    cVaR[i] = CVaR(dataset[,i], probs = 0.05)
    
  }
  
  table_column = rbind(cumReturn, meanReturn, minReturn, maxdraw, 
                       vol, sharp, skewness, kurtosis, mod_VaR, cVaR)
  row.names(table_column) = c('Cumulative Return', 'Mean Return', 'Min Return', 'Max Drawdown', 'Volatility',
                              'Sharp Ratio', 'Skewness', 'Kurtosis', 'Modified VaR', 'CVaR')
  colnames(table_column) = ETF
  return(table_column)
}

test1 = getindicators(TS_r_0.5_b_l)
write.table(test1,"TS_0.5_l_b.csv",sep=",")
test2 = getindicators(TS_r_0.5_d_l)
write.table(test2,"TS_0.5_l_d.csv",sep=",")
test3 = getindicators(TS_r_0.5_a_l)
write.table(test3,"TS_0.5_l_a.csv",sep=",")

test11 = getindicators(TS_r_1_b_l)
write.table(test11,"TS_1_l_b.csv",sep=",")
test22 = getindicators(TS_r_1_d_l)
write.table(test22,"TS_1_l_d.csv",sep=",")
test33 = getindicators(TS_r_1_a_l)
write.table(test33,"TS_1_l_a.csv",sep=",")

test111 = getindicators(TS_r_1.5_b_l)
write.table(test111,"TS_1.5_l_b.csv",sep=",")
test222 = getindicators(TS_r_1.5_d_l)
write.table(test222,"TS_1.5_l_d.csv",sep=",")
test333 = getindicators(TS_r_1.5_a_l)
write.table(test333,"TS_1.5_l_a.csv",sep=",")

MV1 = getindicators(MV_r_b_l)
write.table(MV1,"MV_r_b_l.csv",sep=",")
MV2 = getindicators(MV_r_d_l)
write.table(MV2,"MV_r_d_l.csv",sep=",")
MV3 = getindicators(MV_r_a_l)
write.table(MV3,"MV_r_a_l.csv",sep=",")

TS_w_0.5_b_s_m = matrix(TS_w_0.5_b_s,nrow = 13,ncol = 1)
b_s_1 = TS_r_0.5_b_s %*% TS_w_0.5_b_s_m
TS_w_0.5_b_m_m = matrix(TS_w_0.5_b_m,nrow = 13,ncol = 1)
b_s_11 = TS_r_0.5_b_m %*% TS_w_0.5_b_m_m
TS_w_0.5_b_l_m = matrix(TS_w_0.5_b_l,nrow = 13,ncol = 1)
TS_w_0.5_d_l_m = matrix(TS_w_0.5_d_l,nrow = 13,ncol = 1)
TS_w_0.5_a_l_m = matrix(TS_w_0.5_a_l,nrow = 13,ncol = 1)
b_s_a = TS_r_0.5_a_l %*% TS_w_0.5_a_l_m
b_s_d = TS_r_0.5_d_l %*% TS_w_0.5_d_l_m
b_s_b = TS_r_0.5_b_l %*% TS_w_0.5_b_l_m
b_0.5 = c(b_s_a) + c(b_s_d) + c(b_s_b)
table0.5 = getindicators_table2(c(b_0.5))

TS_w_1_b_l_m = matrix(TS_w_1_b_l,nrow = 13,ncol = 1)
TS_w_1_d_l_m = matrix(TS_w_1_d_l,nrow = 13,ncol = 1)
TS_w_1_a_l_m = matrix(TS_w_1_a_l,nrow = 13,ncol = 1)
b_s_a = TS_r_1_a_l %*% TS_w_1_a_l_m
b_s_d = TS_r_1_d_l %*% TS_w_1_d_l_m
b_s_b = TS_r_1_b_l %*% TS_w_1_b_l_m
b_1 = c(b_s_a) + c(b_s_d) + c(b_s_b)
table1 = getindicators_table2(c(b_1))

TS_w_1.5_b_l_m = matrix(TS_w_1.5_b_l,nrow = 13,ncol = 1)
TS_w_1.5_d_l_m = matrix(TS_w_1.5_d_l,nrow = 13,ncol = 1)
TS_w_1.5_a_l_m = matrix(TS_w_1.5_a_l,nrow = 13,ncol = 1)
b_s_a = TS_r_1.5_a_l %*% TS_w_1.5_a_l_m
b_s_d = TS_r_1.5_d_l %*% TS_w_1.5_d_l_m
b_s_b = TS_r_1.5_b_l %*% TS_w_1.5_b_l_m
b_1.5 = c(b_s_a) + c(b_s_d) + c(b_s_b)
table1.5 = getindicators_table2(c(b_1.5))


#plot
par(mfrow=c(2,3))
plot(cumsum(MV_r_b_s), type = 'l', main = 'Min-Var Before Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_b_m), type = 'l', main = 'Min-Var Before Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_b_l), type = 'l', main = 'Min-Var Before Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(MV_r_d_s), type = 'l', main = 'Min-Var During Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_d_m), type = 'l', main = 'Min-Var During Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
#£¿
par(new = TRUE)
plot(cumsum(MV_r_d_l), type = 'l', main = 'Min-Var During Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)

#£¿£¿£¿
plot(cumsum(MV_r_a_s), type = 'l', main = 'Min-Var After Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_a_m), type = 'l', main = 'Min-Var After Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_a_l), type = 'l', main = 'Min-Var After Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)


#£¿
plot(cumsum(TS_r_0.5_b_s), type = 'l', main = 'Time Series Before Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_0.5_b_m), type = 'l', main = 'Time Series Before Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_0.5_b_l), type = 'l', main = 'Time Series Before Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(TS_r_0.5_d_s), type = 'l', main = 'Time Series During Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_0.5_d_m), type = 'l', main = 'Time Series During Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_0.5_d_l), type = 'l', main = 'Time Series During Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)

#£¿£¿£¿
plot(cumsum(TS_r_0.5_a_s), type = 'l', main = 'Time Series After Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_0.5_a_m), type = 'l', main = 'Time Series After Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_0.5_a_l), type = 'l', main = 'Time Series After Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)

#
par(mfrow=c(2,3))
plot(cumsum(MV_r_b_s), type = 'l', main = 'Min-Var Before Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_b_m), type = 'l', main = 'Min-Var Before Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_b_l), type = 'l', main = 'Min-Var Before Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(MV_r_d_s), type = 'l', main = 'Min-Var During Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_d_m), type = 'l', main = 'Min-Var During Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_d_l), type = 'l', main = 'Min-Var During Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(MV_r_a_s), type = 'l', main = 'Min-Var After Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_a_m), type = 'l', main = 'Min-Var After Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_a_l), type = 'l', main = 'Min-Var After Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(TS_r_1_b_s), type = 'l', main = 'Time Series Before Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1_b_m), type = 'l', main = 'Time Series Before Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1_b_l), type = 'l', main = 'Time Series Before Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(TS_r_1_d_s), type = 'l', main = 'Time Series During Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1_d_m), type = 'l', main = 'Time Series During Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1_d_l), type = 'l', main = 'Time Series During Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(TS_r_1_a_s), type = 'l', main = 'Time Series After Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1_a_m), type = 'l', main = 'Time Series After Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1_a_l), type = 'l', main = 'Time Series After Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)


par(mfrow=c(2,3))
plot(cumsum(MV_r_b_s), type = 'l', main = 'Min-Var Before Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_b_m), type = 'l', main = 'Min-Var Before Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_b_l), type = 'l', main = 'Min-Var Before Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(MV_r_d_s), type = 'l', main = 'Min-Var During Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_d_m), type = 'l', main = 'Min-Var During Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_d_l), type = 'l', main = 'Min-Var During Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(MV_r_a_s), type = 'l', main = 'Min-Var After Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_a_m), type = 'l', main = 'Min-Var After Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(MV_r_a_l), type = 'l', main = 'Min-Var After Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(TS_r_1.5_b_s), type = 'l', main = 'Time Series Before Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1.5_b_m), type = 'l', main = 'Time Series Before Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1.5_b_l), type = 'l', main = 'Time Series Before Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(TS_r_1.5_d_s), type = 'l', main = 'Time Series During Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1.5_d_m), type = 'l', main = 'Time Series During Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1.5_d_l), type = 'l', main = 'Time Series During Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)
plot(cumsum(TS_r_1.5_a_s), type = 'l', main = 'Time Series After Crisis', col = 'red', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1.5_a_m), type = 'l', main = 'Time Series After Crisis', col = 'blue', xlab = 'Time', ylab = 'Return', axes=FALSE)
par(new = TRUE)
plot(cumsum(TS_r_1.5_a_l), type = 'l', main = 'Time Series After Crisis', col = 'green', xlab = 'Time', ylab = 'Return', axes=FALSE)
#legend(1,95,legend=c('short','mid','long'),col=c('red','blue','green'),lty=1)













































