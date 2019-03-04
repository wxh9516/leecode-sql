library(mipfp)
library(utils)

InitialM <- array(1,dim=c(30,30))

InitialM[3:7,1]<-0
InitialM[9:10,1]<-0
InitialM[14,1]<-0
InitialM[16:30,1]<-0

InitialM[4,2]<-0
InitialM[7:26,2]<-0
InitialM[28:30,2]<-0

InitialM[1,3]<-0
InitialM[3:6,3]<-0
InitialM[8:15,3]<-0
InitialM[19:30,3]<-0

InitialM[1:3,4]<-0
InitialM[6:9,4]<-0
InitialM[11:22,4]<-0
InitialM[24,4]<-0
InitialM[27:30,4]<-0

InitialM[1,5]<-0
InitialM[3,5]<-0
InitialM[6:27,5]<-0

InitialM[1,6]<-0
InitialM[3:11,6]<-0
InitialM[13,6]<-0
InitialM[15:30,6]<-0

InitialM[1:2,7]<-0
InitialM[4:20,7]<-0
InitialM[22:23,7]<-0
InitialM[25:30,7]<-0

InitialM[2:8,8]<-0
InitialM[10:19,8]<-0
InitialM[21:30,8]<-0

InitialM[1:7,9]<-0
InitialM[11:18,9]<-0
InitialM[20:30,9]<-0

InitialM[1:3,10]<-0
InitialM[5:8,10]<-0
InitialM[10:30,10]<-0

InitialM[2:21,11]<-0
InitialM[23:30,11]<-0

InitialM[2:5,12]<-0
InitialM[7:30,12]<-0

InitialM[2:30,13]<-0

InitialM[1:5,14]<-0
InitialM[7:30,14]<-0

InitialM[2:30,15]<-0

InitialM[1:2,16]<-0
InitialM[4:30,16]<-0

InitialM[1:2,17]<-0
InitialM[4:30,17]<-0

InitialM[1:2,18]<-0
InitialM[4:30,18]<-0

InitialM[1:8,19]<-0
InitialM[10:30,19]<-0

InitialM[1:7,20]<-0
InitialM[9:30,20]<-0

InitialM[1:6,21]<-0
InitialM[8:30,21]<-0

InitialM[1:10,22]<-0
InitialM[12:30,22]<-0

InitialM[1:3,23]<-0
InitialM[5:30,23]<-0

InitialM[1:6,24]<-0
InitialM[8:30,24]<-0

InitialM[1:3,25]<-0
InitialM[5:30,25]<-0

InitialM[1:3,26]<-0
InitialM[5:30,26]<-0

InitialM[1,27]<-0
InitialM[3:30,27]<-0

InitialM[1:4,28]<-0
InitialM[6:30,28]<-0

InitialM[1:4,29]<-0
InitialM[6:30,29]<-0

InitialM[1:4,30]<-0
InitialM[6:30,30]<-0

for(i in 1:30) {
  InitialM[i,i] <- 0
}

SumOfRows <- c(2140778000, 1747354000, 1751524000, 1385697000, 201637519,180371724,164539000,140077697,135758439,138163151,137474000,129707000,123325220,118072176,118537345,290651177,109372608,104052030,89765667,86504843,87780507,74450413,71609090,66080511,50387875,43390000,43119702,37120068,32217466,31106198)
SumOfCols <- c(1928619000,1568846000,1542060000,1230831000,175834099,152870180,136955000,122443369,119623708,127621426,120511000,114578000,106227269,102361064,101289517,252894748,93530795,91152433,77620599,73386128,79185122,60596205,63645456,58466582,46625333,34332000,38830630,33289372,28951554,27876772)
TargetData<- list(SumOfRows, SumOfCols)
Target.list <- list(1,2)
RandomMatrix <- Ipfp(InitialM, Target.list, TargetData)   #shift to probabilities!

library(stats)
library(igraph)
library(sna)

library(PAFit)

setwd(dir='E:/')

data=read.csv("undirected.csv",header =TRUE)
head(data)


data_stru<-graph.data.frame(data,directed = FALSE)
g<-graph.data.frame(data,directed = FALSE)

plot(data_stru,edge.width=data$freq*50,edge.color=rainbow(40),edge.arrow.size=2)

V(data_stru)$color = "white"
V(data_stru)$color[23] = "red"
plot(data_stru,edge.width=data$freq*50,edge.color=rainbow(40),edge.arrow.size=2)

probability=c(0.6,0.1,0.133333333,0.133333333,0.033333333)
degree=c(1,2,3,5,6)
plot(degree,probability)

m<-gsize(g)#获取边数
n<-vcount(g)#获取顶点数
l<-mean_distance(g)##计算平均路径长度
c<-transitivity(g)#计算聚类系数

time<-c(1,2,7,14,17,20,21,22,23,24,25,26,28, 29,30, 34,35,38,40,43,44, 45,46, 57)
number<-c(1:24)
plot(time,number)

library(sna)


#ex:v[9] is infected
#probability

i=25
while(i<70){
  p=0.181629563
  coins = c(rep(1, p*1000), rep(0,(1-p)*1000))
  n = length(coins)
  rslt<-sample(coins, 1, replace=TRUE, prob=rep(1/n, n))
  i=i+1
  cat(rslt, i,"\n")
}
#----------
#try
g =barabasi.game(30)
plot(g,edge.arrow.size=0.1)

seeds_num = 1
set.seed(2014); diffusers = sample(V(g),seeds_num) 
infected =list()
diffusers=infected[1]

coins = c(0,1)
n = length(coins)
sample(coins, 1, replace=TRUE, prob=rep(1/n, n))

update_diffusers = function(diffusers){
  nearest_neighbors = neighborhood(g, 1, diffusers)
  nearest_neighbors = data.frame(table(unlist(nearest_neighbors)))
  nearest_neighbors = subset(nearest_neighbors, !(nearest_neighbors[,1]%in%diffusers))
  # toss the coins
  toss = function(freq) {
    tossing = NULL
    for (i in 1:freq ) tossing[i] = sample(coins, 1, replace=TRUE, prob=rep(1/n, times=n))
    tossing = sum(tossing)
    return (tossing)
  }
  keep = unlist(lapply(nearest_neighbors[,2], toss))
  new_infected = as.character(nearest_neighbors[,1][keep >= 1])
  diffusers = unique(c(diffusers))
  return(diffusers)
}


i = 1
while(length(infected[i]) < 30){ 
  infected[i+1] = sort(update_diffusers(diffusers))
  cat(length(infected[i+1]), "\n")
  i = i + 1
}

E(g)$color = "blueviolet"
V(g)$color = "white"
set.seed(2014); layout.old = layout.fruchterman.reingold(g) # about 3 minute3
V(g)$color[V(g)%in%diffusers] = "red"
plot(g, layout =layout.old,edge.arrow.size=0.1)

library(animation)

saveGIF({
  ani.options(interval = 0.5, convert = shQuote("E:/ImageMagick-6.2.7-Q16/convert.exe"))
  # start the plot
  m = 1
  while(m <= length(infected)){
    V(g)$color = "white"
    V(g)$color[V(g)%in%infected[m]] = "red"
    plot(g, layout =layout.old)
    m = m + 1}
})

#-------------
library(deSolve)

sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init       <- c(S = 1-1e-6, I = 1e-6, R = 0.0)
## beta: infection parameter; gamma: recovery parameter
parameters <- c(beta = 1.4247 gamma = 0.14286)
## Time frame
times      <- seq(0, 70, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 30)

#try
sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

#probability
p = 0.128
coins = c(rep(1, p*1000), rep(0,(1-p)*1000))
n = length(coins)
sample(coins, 1, replace=TRUE, prob=rep(1/n, n))