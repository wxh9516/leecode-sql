library(tseries) #get necessary libraries
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
#################################################ETF Parameters###################################################################
Universe <- c("FXE","EWJ","GLD","QQQ","SPY","SHV","DBA","USO","XBI","ILF","GAF","EPP","FEZ") #13 ETFs
Trading_Days <- 250
Start_Date <- "2007-03-23"
End_Date <-"2017-07-24"
Crisis_Start_Date <- "2008-05-30"
Crisis_End_Date <- "2009-08-07"


benchmark_analysis <- function(daily_returns, fama_french, start, interval, BetaT, lambda = 0.01){
  
  returns <- as.data.frame(daily_returns) #daily returns of ETFs
  fama <- as.data.frame(fama_french) #fama-french
  end = start + interval - 1  #end of period
  
  #Covariance matrices for ETFs and Fama-French Factors
  return_cov <- cov(daily_returns)
  fama_cov <- cov(fama_french)
  
  #Daily ETF returns and Fama French Factors for stat:end period
  return_mat <- as.matrix(returns[start:end,1:13])
  fama_mat <- as.matrix(fama[start:end,1:4])
  
  #Mean ETF returns and Fama French Factors for stat:end period
  return_means <- colMeans(return_mat)
  fama_means <- colMeans(fama_mat)
  
  #Regress ETF returns on the three Fama-French factors
  #Matrix to hold Regression Results
  Regression_Output <- matrix(ncol =ncol(return_mat), nrow = 5) 
  colnames(Regression_Output) <- colnames(return_mat)
  rownames(Regression_Output) <- c("Intercept","Beta","BetaSMB","BetaHML","Idiosyncratic std")
  #Regression
  for(x in seq(1,ncol(return_mat),1)){
    Current_Regression <- lm(return_mat[,x]-fama_mat[,"RF"]~
                               (fama_mat[,1] + fama_mat[,2] + fama_mat[,3]))
    Regression_Output[,x] = c(Current_Regression$coefficients[1], #put intercept in Output table
                              Current_Regression$coefficients[2], #put beta in output table
                              Current_Regression$coefficients[3], #put beta in output table
                              Current_Regression$coefficients[4], #put beta in output table
                              sqrt(var(Current_Regression$residuals))) #put idiosyncratic risk in output table
  }
  
  #Regression to find betas of ETFs 
  intercept <- integer(13)
  Slope_beta <- integer(13)
  isd <- integer(13)
  stock_name <- character(13)
  #Each ETF returns are regressied on SPY returns
  for(i in 1:ncol(return_mat)) {
    lm1 <- lm(return_mat[,i] ~ return_mat[,5] )
    stock_name[i] <- colnames(return_mat)[i]
    intercept[i] <- lm1$coefficients[1]
    Slope_beta[i] <- lm1$coefficients[2]
    isd[i] <- sqrt(var(lm1$residuals))
  }
  #Regression Output
  myTable <- data.frame(stock_name, intercept, Slope_beta, isd, stringsAsFactors = FALSE) 
  #ETF betas
  regression_betas <- c(myTable[,"Slope_beta"]) 
  
  return_mat <- return_mat - fama_mat[,4]
  
  #Simulated Returns based on Fama French Regression
  fama_ret <- fama_means[4] + fama_means[1] * Regression_Output["Beta",] + 
    fama_means[2] * Regression_Output["BetaSMB",] + 
    fama_means[3] * Regression_Output["BetaHML",] +
    Regression_Output["Intercept",]
  
  #Prepare Data for Optimization
  ref_port <- rep(1/13,13)
  D <- diag(return_cov)
  factor_cov <- cov(fama_mat[,1:3])
  B <- Regression_Output[c("Beta", "BetaSMB", "BetaHML"),]
  sigma <- (t(B) %*% factor_cov %*% B) + diag(D)
  
  #Prepare Dmat, Amat, bvec, and dvec for optimization
  Dmat= lambda*diag(13) + sigma
  bvec  <- c(0.15, 1, rep(-2,13), rep(-2,13)) # Right Hand side of Constraints
  dvec= 2*lambda*c(ref_port %*% return_cov) # No linear Term in the Objective function 
  Amat <- cbind(fama_ret, rep(1,13), -diag(13), diag(13)) 
  
  #Find weights of ETFs for current period 
  opt_sol=solve.QP(Dmat, dvec, Amat, bvec, meq=2, factorized=FALSE) # Call to the Optimizer
  weights = opt_sol$solution # Vector of weights
  return(weights)
}


#Port_analysis creates a list of vectors of security weights according to our investment strategy; one vector per rebalancing perdiod
port_analysis <- function(daily_returns, fama_french, start, interval, BetaT, lambda = 0.01){
  
  returns <- as.data.frame(daily_returns) #daily returns of ETFs
  fama <- as.data.frame(fama_french) #fama french factors
  end = start + interval - 1  #end of period
  
  #Covariance matrices for ETFs and Fama-French Factors
  return_cov <- cov(daily_returns)
  fama_cov <- cov(fama_french)
  
  #Daily ETF returns and Fama French Factors for stat:end period
  return_mat <- as.matrix(returns[start:end,1:13])
  fama_mat <- as.matrix(fama[start:end,1:4])
  
  #Mean ETF returns and Fama French Factors for stat:end period
  return_means <- colMeans(return_mat)
  fama_means <- colMeans(fama_mat)
  
  #Regress ETF returns on the three Fama-French factors
  #Matrix to hold Regression Results
  Regression_Output <- matrix(ncol =ncol(return_mat), nrow = 5)
  colnames(Regression_Output) <- colnames(return_mat)
  rownames(Regression_Output) <- c("Intercept","Beta","BetaSMB","BetaHML","Idiosyncratic std")
  #Regression
  for(x in seq(1,ncol(return_mat),1)){
    Current_Regression <- lm(return_mat[,x]-fama_mat[,"RF"]~
                               (fama_mat[,1] + fama_mat[,2] + fama_mat[,3]))
    Regression_Output[,x] = c(Current_Regression$coefficients[1], #put intercept in Output table
                              Current_Regression$coefficients[2], #put beta in output table
                              Current_Regression$coefficients[3], #put beta in output table
                              Current_Regression$coefficients[4], #put beta in output table
                              sqrt(var(Current_Regression$residuals))) #put idiosyncratic risk in output table
  }
  
  #Regression to find betas of ETFs 
  intercept <- integer(13)
  Slope_beta <- integer(13)
  isd <- integer(13)
  stock_name <- character(13)
  #Each ETF returns are regressied on SPY returns
  for(i in 1:ncol(return_mat)) {
    lm1 <- lm(return_mat[,i] ~ return_mat[,5] )
    stock_name[i] <- colnames(return_mat)[i]
    intercept[i] <- lm1$coefficients[1]
    Slope_beta[i] <- lm1$coefficients[2]
    isd[i] <- sqrt(var(lm1$residuals))
  }
  #Regression Output
  myTable <- data.frame(stock_name, intercept, Slope_beta, isd, stringsAsFactors = FALSE)
  #ETF betas
  regression_betas <- c(myTable[,"Slope_beta"])
  
  return_mat <- return_mat - fama_mat[,4]
  
  #Simulated Returns based on Fama French Regression
  fama_ret <- fama_means[4] + fama_means[1] * Regression_Output["Beta",] + 
    fama_means[2] * Regression_Output["BetaSMB",] + 
    fama_means[3] * Regression_Output["BetaHML",] +
    Regression_Output["Intercept",]
  
  #Prepare Data for Optimization
  ref_port <- rep(1/13,13)
  D <- diag(return_cov)
  factor_cov <- cov(fama_mat[,1:3])
  B <- Regression_Output[c("Beta", "BetaSMB", "BetaHML"),]
  sigma <- (t(B) %*% factor_cov %*% B) + diag(D)
  
  #Prepare Dmat, Amat, bvec, and dvec for optimization of Benchmark Portfolio
  Dmat= lambda*return_cov + sigma
  bvec  <- c(0.15, 1, rep(-2,13), rep(-2,13)) # Right Hand side of Constraints
  dvec= 2*lambda*c(ref_port %*% return_cov) # No linear Term in the Objective function 
  Amat <- cbind(fama_ret, rep(1,13), -diag(13), diag(13)) 
  
  #Find weights of Benchmark for current period 
  opt_sol=solve.QP(Dmat, dvec, Amat, bvec, meq=2, factorized=FALSE) # Call to the Optimizer
  weights = opt_sol$solution # Vector of weights
  
  #Prepare Dmat, Amat, bvec, and dvec for optimization of Investment Portfolio
  Dmat= sigma * lambda
  bvec  <- c(BetaT, 1, rep(-2,13), rep(-2,13)) # Right Hand side of Constraints
  dvec <- fama_ret + 2*lambda*weights %*% sigma # No linear Term in the Objective function 
  Amat <- cbind(c(myTable[,"Slope_beta"]), rep(1,13), -diag(13), diag(13)) 
  
  #Find weights of Investment Portfolio for current period 
  opt_sol=solve.QP(Dmat, dvec, Amat, bvec, meq=2, factorized=FALSE) # Call to the Optimizer
  port_weights = opt_sol$solution # Vector of weights
  return(port_weights)
  
}


#Date_gen returns a vecotr indicating start and end dates for rebalancing periods
date_gen <- function(data, start_date, increment){
  data$Date <- as.Date(rownames(data)) #start date index number
  first <- which(data$Date == as.Date(start_date))
  dates <- c()
  while(first < nrow(data)-increment) #increment start date by increment until you hit end date
  {
    dates <- c(dates, first)
    first <- first + increment - 1
  }
  return(dates) #return vecctor fo dates
}


#Portfolio_REturns takes a list of weight vecctors and calculates the Daily Portfolio Retruns while applying a new weight vector
#after reblancing periods provided by dat
Portfolio_Returns <- function(daily,weights,dat){
  
  #get rid of first date (its alwasy the first day of observations)
  dat[-1]
  
  #rearrange weights
  weights <- matrix(unlist(weights),ncol = length(Universe), nrow = length(weights))
  weights <- t(weights)
  
  #Matrix to be filled with Portfolio Returns
  Portfolio_Ret <- matrix(ncol =1, nrow = nrow(daily))
  
  #loop that fills Portfolio_Ret with returns
  count <-1
  count2 <-1
  for(i in dat){ #for each date...
    while(count <=i){ #while the date != end of period
      Portfolio_Ret[count,] <- weights[,count2]%*%daily[count,] #calculate portfolio returns from current weights
      count <- count+1 #go to next day
    }
    count2 <-count2+1 #go to next period with new weights
  }
  
  #last period has to be handled separately; it might not be a multiple of increment
  i <- length(dat)
  while(i<=nrow(daily)){
    Portfolio_Ret[i,] <-weights[,ncol(weights)]%*%daily[i,]
    i <- i+1
  }
  return( Portfolio_Ret)
}


#Return Graph with Plot of Investment Portfolio, Benchmakr Portfolio, S&P500 starting with $100
graph <-function(Daily,Bench,Port,time_fr,beta){
  
  #Matrix to hold pricing data
  SP_PnL <-matrix(c(100,100,100),ncol = 3, nrow =nrow(Daily_Closing))
  colnames(SP_PnL)<- c("SPY/S&P Benchmark","Long/Short Benchamrk","Fama-French Strategy")
  rownames(SP_PnL) <- rownames(Daily_Closing)
  SP_PnL[1,] <- c(100,100,100)
  
  #Transform portfolio returns to Investment performance starting with $100
  i<- 1
  for(x in rownames(Daily)){
    SP_PnL[i+1,1] <- (Daily[i,5]+1)*SP_PnL[i,1]
    SP_PnL[i+1,2] <- (Port[i,]+1)*SP_PnL[i,2]
    SP_PnL[i+1,3] <- (Bench[i,]+1)*SP_PnL[i,3]
    i<-i+1
  }
  
  #create the plot
  plot(y = SP_PnL [1:nrow(SP_PnL),1], #Plot the graph
       x = c(as.Date(rownames(SP_PnL))),
       main = cbind("Daily Performance of S&P500, Benckmark Portfolio, and Investment Portfolio With", time_fr, " Day Rebalancing Period and beta of ",beta ),
       type = "l",
       ylab = "Investment",
       xlab = "Time",
       col = "blue",
       xaxt = "n",
       yaxt = "n",
       xaxs = "i",
       yaxs = "i",
       ylim = c(0,400)
  )
  #cosmetic improvements to plot
  axis(1, c(as.Date(rownames(SP_PnL))), format(c(as.Date(rownames(SP_PnL))), "%y %b"))
  axis(2, at = seq(0, 400, by = 20), las=2, labels = TRUE)
  lines(c(as.Date(rownames(SP_PnL))),SP_PnL[1:nrow(SP_PnL),2],col="green",lty=4,cex=0.1)
  lines(c(as.Date(rownames(SP_PnL))),SP_PnL[1:nrow(SP_PnL),3],col="red",lty=4,cex=0.1)
  legend("topleft", legend = c("S&P 500", "Benchmark Portfolio","Fama-French Portfolio"), #add legend
         col =c("blue","red","green"), lty =1)
}


#dens returns density plot of Investment Portfolio Returns
Dens <- function(Port_Ret,time_fr){
  plot(density(Port_Ret[1:length(Port_Ret),]),main =cbind("Distribution of Daily investment Portfolio Returns With ",time_fr," Day Rebalancing Period"))
}


#tab creates html table of key performance statistics for Investment Portfolio, Benchmark Portfolio, and S&P 500
tab <- function(time_fr,Porthalf,Portone,PortOneAndHalf,Bench,SP,desc =" "){
  #Mena, Min, max, Geometric Return
  Mean<-c(mean(Porthalf[,1]),mean(Portone[,1]),mean(PortOneAndHalf[,1]),mean(Bench[,1]),mean(SP[,5]))*252
  Min <- c(min(Porthalf[,1]),min(Portone[,1]),min(PortOneAndHalf[,1]),min(Bench[,1]),min(SP[,5]))*252
  Max <- c(max(Porthalf[,1]),max(Portone[,1]),max(PortOneAndHalf[,1]),max(Bench[,1]),max(SP[,5]))*252
  Geo <- c(geoMean(10000+Porthalf[,1])-10000,
           geoMean(10000+Portone[,1])-10000,
           geoMean(10000+PortOneAndHalf[,1])-10000,
           geoMean(10000+Bench[,1])-10000,
           geoMean(10000+SP[,5])-10000)*252
  
  #Moments
  Kurt <- c(kurtosis(Porthalf[,1]),kurtosis(Portone[,1]),kurtosis(PortOneAndHalf[,1]),kurtosis(Bench[,1]),kurtosis(SP[,5]))
  Skew <- c(skewness(Porthalf[,1]),skewness(Portone[,1]),skewness(PortOneAndHalf[,1]),skewness(Bench[,1]),skewness(SP[,5]))
  STD <- c(sd(Porthalf[,1]),sd(Portone[,1]),sd(PortOneAndHalf[,1]),sd(Bench[,1]),sd(SP[,5]))*sqrt(252)
  Sharpe <- c(Mean/STD)
  
  #Risk Measures
  Draw <- c(maxdrawdown(Porthalf[,1])$maxdrawdown,
            maxdrawdown(Portone[,1])$maxdrawdown,
            maxdrawdown(PortOneAndHalf[,1])$maxdrawdown,
            maxdrawdown(Bench[,1])$maxdrawdown,
            maxdrawdown(SP[,5])$maxdrawdown)
  Cumu <-c(Return.cumulative(Porthalf[,1]),
           Return.cumulative(Portone[,1]),
           Return.cumulative(PortOneAndHalf[,1]),
           Return.cumulative(Bench[,1]),
           Return.cumulative(SP[,5]))
  VAAR <- c(VaR(Porthalf[,1],method = "modified"),
            VaR(Portone[,1],method = "modified"),
            VaR(PortOneAndHalf[,1],method = "modified"),
            VaR(Bench[,1],method = "modified"),
            VaR(SP[,5],method = "modified") )
  CVAAR <- c(CVaR(Porthalf[,1],method = "modified"),
             CVaR(Portone[,1],method = "modified"),
             CVaR(PortOneAndHalf[,1],method = "modified"),
             CVaR(Bench[,1],method = "modified"),
             CVaR(SP[,5],method = "modified"))
  
  #Create html Table
  head <-c("Investment Portfolio Beta = 0.5", "Investment Portfolio Beta = 1", "Investment Portfolio Beta = 1.5",
           "Benchmark Portfolio Return = 15%", "S&P 500" )
  rowna <- c("Mean Return*", "Geometric Return*", "Cumulative Return", "Minimum Return*", "Maximum Return*", "Volatility*
             ", "Skewness","Kurtosis", "Sharpe Ratio", "Maximum Drawdown", "95% Modified VaR", "95% CVaR")
  df <- data.frame(t(data.frame((Mean),Geo,Cumu,Min,Max,STD,Skew,Kurt,Sharpe,Draw,VAAR,CVAAR)))
  row.names(df) <- rowna
  colnames(df) <- head
  df<-round(df,5)
  cap= paste("Key Statistics for Investment and Benchmark Portfolios With", time_fr," Day Rabalancing Period", desc, sep =" ") 
  
  return (htmlTable(x =df,caption = cap, tfoot ="*Annualized Value"))
}

#execute returns daily portfolio returns for Investment and Benchmark Portfolios
execute <- function(time_frame, beta, lambda){
  
  time_frame <- time_frame #reabalancing time period
  beta <- beta
  lambda <- lambda #for optimization
  
  
  ######################################################Portfolio Creation###########################################################
  #vector of rebalance dates
  dates <- date_gen(as.data.frame(Daily_Returns_Annualized), "2007-03-26", time_frame )
  
  #####################################################Benchamrk
  #list of weight vectors, one weight per rebalancing period
  benchmark_list <- list()
  
  count = 1
  for(i in dates){
    benchmark_list[[count]] <- benchmark_analysis(Daily_Returns_Annualized, Fama_French_Daily, i, time_frame,beta, lambda)
    count = count + 1
  }
  
  #matrix of Benchmark portfolio Returns
  Bench_Returns <- Portfolio_Returns(Daily_Returns,benchmark_list,dates)
  
  #####################################################Investment Portfolio
  #list of weight vectors, one weight vector per rebalancing period
  Port_weight_list <- list()
  
  count = 1 #after each rebalance date, new weights will be calculated via optimization
  for(i in dates){
    Port_weight_list[[count]] <- port_analysis(Daily_Returns_Annualized, Fama_French_Daily, i, time_frame,beta, lambda)
    count = count + 1
  }
  
  #matrix of Investment Portfolio Returns 
  Port_Returns <- Portfolio_Returns(Daily_Returns,Port_weight_list,dates)
  
  
  
  ######################################################Portfolio Performance Analysis###########################################################
  
  #Graph Perfomrance
  graph(Daily_Returns,Bench_Returns,Port_Returns,time_frame,beta)
  return(matrix(c(Port_Returns,Bench_Returns),ncol=2))
  
}
#################################################ETF Data Aquisition########################################################################
#set up matrix to hold daily ETF closing prices
Test <-get.hist.quote(instrument = "SHV", start = Start_Date, end =End_Date, quote = "Close")
Daily_Closing <- matrix( ncol =length(Universe), nrow = length(Test))
colnames(Daily_Closing) <- Universe
rownames(Daily_Closing) <- as.character(index(Test))

#fill daily ETFclosing prices in matrix
i <-1
for(name in Universe){
  Daily_Closing[,i] <- get.hist.quote(instrument = name, start = "2007-03-23", end ="2017-07-24", quote = "Close")
  i <- i+1;
}

#calculate ETF daily returns
Daily_Returns <-matrix(c((Daily_Closing[2:nrow(Daily_Closing), 1:ncol(Daily_Closing)]
                          - Daily_Closing[1:(nrow(Daily_Closing)-1), 1:ncol(Daily_Closing)])
                         /Daily_Closing[1:(nrow(Daily_Closing)-1), 1:ncol(Daily_Closing)]), #add daily Returns
                       ncol = ncol(Daily_Closing), #one column less than P (no date column here)
                       nrow =nrow(Daily_Closing)-1) #one row less than P (first row has no return)
colnames(Daily_Returns) <- colnames(Daily_Closing)
rownames(Daily_Returns) <- as.character(index(Test[2:length(Test)]))

#Annualize Daily Returns
Daily_Returns_Annualized <- Daily_Returns*Trading_Days
head(Daily_Returns_Annualized)
tail(Daily_Returns_Annualized)



###################################################Fama French Data Aquisition####################################################
#create link to fama french website and download historic daily factors
ff <- tempfile()
download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip", ff, quiet = TRUE)
All_ff <- read.csv(unz(ff,"F-F_Research_Data_Factors_daily.CSV"), skip =3,
                   colClasses = c( X = "character",
                                   Mkt.RF = "numeric",
                                   SMB = "numeric",
                                   HML = "numeric",
                                   RF = "numeric"))

#extract fama french factors from start-end date from dataset
FF_Start <- All_ff[All_ff$X=="20070326",] 
FF_End <- All_ff[All_ff$X=="20180723",]
FF_Start <- as.numeric(labels(FF_Start)[1])#start and end index
FF_End <- as.numeric(labels(FF_End)[1])
All_ff <- All_ff[FF_Start:FF_End,]#extract

#Matrix of Fama French Factors
Fama_French_Daily <- matrix(c(as.numeric(unlist(All_ff))),ncol = length(All_ff))
Fama_French_Daily<-Fama_French_Daily[1:2601,]
rownames(Fama_French_Daily) <- rownames(Daily_Returns)
colnames(Fama_French_Daily) <- names(All_ff)
Fama_French_Daily <- Fama_French_Daily[,-1]
head(Fama_French_Daily)
tail(Fama_French_Daily)


#################################################################Financial Crisis Dates Aquisition#################################
Crisis_Start_Row <-1
Crisis_End_Row <-1
for(x in rownames(Daily_Closing)){
  if (x == Crisis_Start_Date) break
  Crisis_Start_Row <- Crisis_Start_Row+1
}
for(x in rownames(Daily_Closing)){
  if (x ==Crisis_End_Date) break
  Crisis_End_Row <- Crisis_End_Row+1
}
if( Crisis_End_Row == length(Daily_Closing) || Crisis_Start_Row == length(Daily_Closing)){
  print("Make sure crisis start and end dates are trading days")}
Crisis_Start_Row
Crisis_End_Row


######################################################Portfolio Performance Analysis###########################################################
#Investment and Benchmakr Returns and Graphs for different lambda and rebalancing periods
#10days
Port10_Returns05 <-matrix(execute(10,0.5,0.5)[,1],ncol = 1)
Port10_Returns1 <-matrix(execute(10,1,0.5)[,1],ncol = 1)
Port10_Returns15 <-matrix(execute(10,1.5,0.5)[,1],ncol = 1)
Bench10_Returns <-matrix(execute(10,0.5,0.5)[,2],ncol = 1)

#75 days
Port75_Returns05 <-matrix(execute(75,0.5,0.5)[,1],ncol =1)
Port75_Returns1 <-matrix(execute(75,1,0.5)[,1],ncol =1)
Port75_Returns15 <-matrix(execute(75,1.5,0.5)[,1],ncol =1)
Bench75_Returns <-matrix(execute(75,0.5,0.5)[,2],ncol =1)

#150 days
Port150_Returns05 <-matrix(execute(150,0.5,0.5)[,1],ncol=1)
Port150_Returns1 <-matrix(execute(150,1,0.5)[,1],ncol=1)
Port150_Returns15 <-matrix(execute(150,1.5,0.5)[,1],ncol=1)
Bench150_Returns <-matrix(execute(150,0.5,0.5)[,2],ncol=1)

#Denisty FUnctions of Investment Portfolio
Dens(Port10_Returns05, 10)
Dens(Port10_Returns1, 10)
Dens(Port10_Returns15, 10)

Dens(Port75_Returns05, 75)
Dens(Port75_Returns1, 75)
Dens(Port75_Returns15, 75)

Dens(Port150_Returns05, 150)
Dens(Port150_Returns1, 150)
Dens(Port150_Returns15, 150)

################################################Summary Tables of key Statistics###################################################
###############################################10days
tab10all <- tab(10,Port10_Returns05,Port10_Returns1,Port10_Returns15,Bench10_Returns,Daily_Returns,"From 2007-2018") #all
tab10before <- tab(10,matrix(Port10_Returns05[1:Crisis_Start_Row],ncol =1), #before the crisis
             matrix(Port10_Returns1[1:Crisis_Start_Row],ncol =1),
              matrix(Port10_Returns15[1:Crisis_Start_Row],ncol =1),
              matrix(Bench10_Returns[1:Crisis_Start_Row],ncol =1),
             matrix(Daily_Returns[1:Crisis_Start_Row,],ncol =13),
             "Before the Financial Crisis") #all
tab10during <- tab(10,matrix(Port10_Returns05[Crisis_Start_Row:Crisis_End_Row],ncol =1), #before the crisis
                   matrix(Port10_Returns1[Crisis_Start_Row:Crisis_End_Row],ncol =1),
                   matrix(Port10_Returns15[Crisis_Start_Row:Crisis_End_Row],ncol =1),
                   matrix(Bench10_Returns[Crisis_Start_Row:Crisis_End_Row],ncol =1),
                   matrix(Daily_Returns[Crisis_Start_Row:Crisis_End_Row,],ncol =13),
                   "During the Financial Crisis") 
tab10after <- tab(10,matrix(Port10_Returns05[Crisis_End_Row:length(Port10_Returns05)],ncol =1), #during the crisis
                   matrix(Port10_Returns1[Crisis_End_Row:length(Port10_Returns05)],ncol =1),
                   matrix(Port10_Returns15[Crisis_End_Row:length(Port10_Returns05)],ncol =1),
                   matrix(Bench10_Returns[Crisis_End_Row:length(Port10_Returns05)],ncol =1),
                   matrix(Daily_Returns[Crisis_End_Row:length(Port10_Returns05),],ncol =13),
                  "After the Financial Crisis")


###############################################75days
tab75all <- tab(75,Port75_Returns05,Port75_Returns1,Port75_Returns15,Bench75_Returns,Daily_Returns,"From 2007-2018") #all
tab75before <- tab(75,matrix(Port75_Returns05[1:Crisis_Start_Row],ncol =1), #before the crisis
                   matrix(Port75_Returns1[1:Crisis_Start_Row],ncol =1),
                   matrix(Port75_Returns15[1:Crisis_Start_Row],ncol =1),
                   matrix(Bench75_Returns[1:Crisis_Start_Row],ncol =1),
                   matrix(Daily_Returns[1:Crisis_Start_Row,],ncol =13),
                   "Before the Financial Crisis") #all
tab75during <- tab(75,matrix(Port75_Returns05[Crisis_Start_Row:Crisis_End_Row],ncol =1), #during the crisis
                   matrix(Port75_Returns1[Crisis_Start_Row:Crisis_End_Row],ncol =1),
                   matrix(Port75_Returns15[Crisis_Start_Row:Crisis_End_Row],ncol =1),
                   matrix(Bench75_Returns[Crisis_Start_Row:Crisis_End_Row],ncol =1),
                   matrix(Daily_Returns[Crisis_Start_Row:Crisis_End_Row,],ncol =13),
                   "During the Financial Crisis") 
tab75after <- tab(75,matrix(Port75_Returns05[Crisis_End_Row:length(Port75_Returns05)],ncol =1), #during the crisis
                  matrix(Port75_Returns1[Crisis_End_Row:length(Port75_Returns05)],ncol =1),
                  matrix(Port75_Returns15[Crisis_End_Row:length(Port75_Returns05)],ncol =1),
                  matrix(Bench75_Returns[Crisis_End_Row:length(Port75_Returns05)],ncol =1),
                  matrix(Daily_Returns[Crisis_End_Row:length(Port75_Returns05),],ncol =13),
                  "After the Financial Crisis") #all

###############################################150days
tab150all <- tab(150,Port150_Returns05,Port150_Returns1,Port150_Returns15,Bench150_Returns,Daily_Returns,"From 2007-2018") #all
tab150before <- tab(150,matrix(Port150_Returns05[1:Crisis_Start_Row],ncol =1), #before the crisis
                    matrix(Port150_Returns1[1:Crisis_Start_Row],ncol =1),
                    matrix(Port150_Returns15[1:Crisis_Start_Row],ncol =1),
                    matrix(Bench150_Returns[1:Crisis_Start_Row],ncol =1),
                    matrix(Daily_Returns[1:Crisis_Start_Row,],ncol =13),
                    "Before the Financial Crisis") #all
tab150during <- tab(150,matrix(Port150_Returns05[Crisis_Start_Row:Crisis_End_Row],ncol =1), #during the crisis
                    matrix(Port150_Returns1[Crisis_Start_Row:Crisis_End_Row],ncol =1),
                    matrix(Port150_Returns15[Crisis_Start_Row:Crisis_End_Row],ncol =1),
                    matrix(Bench150_Returns[Crisis_Start_Row:Crisis_End_Row],ncol =1),
                    matrix(Daily_Returns[Crisis_Start_Row:Crisis_End_Row,],ncol =13),
                    "During the Financial Crisis") 
tab150after <- tab(150,matrix(Port150_Returns05[Crisis_End_Row:length(Port150_Returns05)],ncol =1), #after the crisis
                   matrix(Port150_Returns1[Crisis_End_Row:length(Port150_Returns05)],ncol =1),
                   matrix(Port150_Returns15[Crisis_End_Row:length(Port150_Returns05)],ncol =1),
                   matrix(Bench150_Returns[Crisis_End_Row:length(Port150_Returns05)],ncol =1),
                   matrix(Daily_Returns[Crisis_End_Row:length(Port150_Returns05),],ncol =13),
                   "After the Financial Crisis") #all
tab10all
tab10before
tab10during
tab10after

tab75all
tab75before
tab75during
tab75after

tab150all
tab150before
tab150during
tab150after


################################################Summary Statistics on ETF's##########################################
## Table 
stock_name <- character(13)
Cumulated_Returns <- integer(13)
Daily_Mean_Geo_Return <- integer(13)
Daily_Min_Return <- integer(13)


Max_Drawdown <- integer(13)
SD <- integer(13)
Sharpe_ratio <- integer(13)
Skewness <- integer(13)
kurtosis <- integer(13)
Modified_VaR <- integer(13)
CVAr <- integer(13)
for(i in 1:ncol(Daily_Returns)) {
  
  stock_name[i] <- colnames(Daily_Returns)[i]
  Cumulated_Returns[i] <- Return.cumulative(Daily_Returns[,i]) 
  Daily_Mean_Geo_Return[i] <- mean.geometric(Daily_Returns[,i], r)*252
  Daily_Min_Return[i] <- min(Daily_Returns[,i])
  Max_Drawdown[i] <- maxDrawdown(Daily_Returns[,i])
  SD[i] <- sd(Daily_Returns[,i])*sqrt(252)
  Sharpe_ratio[i] <- SharpeRatio(ts(Daily_Returns[,i]))
  Skewness[i] <- skewness(Daily_Returns[,i])
  kurtosis[i] <- kurtosis(Daily_Returns[,i])
  Modified_VaR[i] <- VaR(Daily_Returns[,i], method = "modified")
  CVAr[i] <- CVaR(Daily_Returns[,i])
  
}
myTable <- data.frame(t(data.frame((Daily_Mean_Geo_Return), Cumulated_Returns, Daily_Min_Return,
                                   
                                   Max_Drawdown, SD, Skewness, kurtosis, Sharpe_ratio, Modified_VaR,CVAr)),
                      stringsAsFactors = FALSE)

df <- data.frame(myTable)

#Create html Table
head <-Universe
rowna <- c("Mean Geometric Return*", "Cumulative Return", "Minimum Return*", "Maximum Drawdown",
           "Volatility","Skewness","Kurtosis", "Sharpe Ratio", "95% Modified VaR", "95% CVaR")
row.names(df) <- rowna
colnames(df) <- head
df<-round(df,5)
cap= paste("Key Statistics for ETF's in Investment Universe From 2007-2018") 

htmlTable(x =df,caption = cap, tfoot ="*Annualized Value")
