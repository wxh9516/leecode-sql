#BSM

bsm.d1 <- function(s, k, r, t, sd) {
  (log(s/k) + (r + 1/2*sd^2)*t) / (sd*sqrt(t))
}

bsm.d2 <- function(s, k, r, t, sd) {
  bsm.d1(s, k, r, t, sd) - sd*sqrt(t)
}

bsm.option <- function(s, k, r, t, sd, type = c("call", "put")) {
  type <- match.arg(type)
  d1 <- bsm.d1(s, k, r, t, sd)
  d2 <- bsm.d2(s, k, r, t, sd)
  r <- switch(type,
              call = s*pnorm(d1) - k*exp(-r*t)*pnorm(d2),
              put = k*exp(-r*t)*pnorm(d2,lower.tail=F)-s*pnorm(d1,lower.tail=F))
  return(r)
}