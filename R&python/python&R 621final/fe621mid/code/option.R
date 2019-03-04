#EP

s <- 100
k <- 100
r <- .06
t <- 1.
sd <- .25
dt <- .0009
dx <- c(sd*sqrt(dt), sd*sqrt(3*dt), sd*sqrt(4*dt))
type <- 'call'; style <- 'european'

## Binomial or Black-Scholes-Merton for comparison
if (type == 'put' && style == 'american') {
  binom.american.put(s, k, r, t, sd, n=1e3)
} else
  bsm.option(s, k, r, t, sd, type)

library(FRACTION)

## Regular implementation.
fde.log(s, k, r, t, sd, type=type, style=style)
fdi.log(s, k, r, t, sd, type=type, style=style)
fdcn.log(s, k, r, t, sd, type=type, style=style)

## Convergence demonstration.
n <- round(t/dt)
m <- round(6*sd*sqrt(t)/dx[1])          # dx = sd*sqrt(dt)
fde.log(s, k, r, t, sd, n, m, type, style)
fdi.log(s, k, r, t, sd, n, m, type, style)
fdcn.log(s, k, r, t, sd, n, m, type, style)

m <- round(6*sd*sqrt(t)/dx[2])          # dx = sd*sqrt(3*dt)
fde.log(s, k, r, t, sd, n, m, type, style)
fdi.log(s, k, r, t, sd, n, m, type, style)
fdcn.log(s, k, r, t, sd, n, m, type, style)

m <- round(6*sd*sqrt(t)/dx[3])          # dx = sd*sqrt(4*dt)
fde.log(s, k, r, t, sd, n, m, type, style)
fdi.log(s, k, r, t, sd, n, m, type, style)
fdcn.log(s, k, r, t, sd, n, m, type, style)

#4(h)
ds=0.001
delta=(bsm.option(s+ds, k, r, t, sd, type)-bsm.option(s, k, r, t, sd, type))/ds
gamma=(bsm.option(s+ds, k, r, t, sd, type)-2*bsm.option(s, k, r, t, sd, type)+bsm.option(s-ds, k, r, t, sd, type))/ds^2

dt<-0.001
theta=(bsm.option(s, k, r, t+dt, sd, type)-bsm.option(s, k, r, t, sd, type))/dt

dsig<-0.001
vega=(bsm.option(s, k, r, t, sd+dsig, type)-bsm.option(s, k, r, t, sd, type))/dsig

