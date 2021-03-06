#explicit

fde <- function(s, k, r, t, sd, n = ceiling(1e3*t), m = 2*ceiling(sqrt(3*n)),
                type = c("call", "put"), style = c("european", "american"),
                grid = FALSE) {
  if (t <= 0) stop("t = ", t, " is nonpositive!")
  if (!is.wholenumber(n) || n <= 0) stop("n = ",n," is not a positive integer!")
  if (!is.wholenumber(m) || m <= 0) stop("m = ",m," is not a positive integer!")
  type <- match.arg(type); style <- match.arg(style)
  
  dt <- t / n
  m <- m + m%%2                         # Ensure m is even.
  ## FIXME: s.max depends on k, t, sd
  s.lim <- c(max=2*s, min=0)
  ds <- unname(s.lim['max'] - s.lim['min']) / m
  s.seq <- s.lim['min'] + 0:m*ds        # vector, m+1 elements
  
  f <- matrix(rep(NA, (n+1)*(m+1)), nrow=n+1)
  g2m <- function(i)  i + 1             # grid index to matrix index
  f[g2m(n),] = switch(type, call = pmax(s.seq - k, 0), put = pmax(k - s.seq, 0))
  
  for (i in g2m((n-1):0)) {             # Iterate from end to beginning.
    for (j in (m-1):1) {
      a <- (1 + r*dt)^-1 * (-1/2*r*j*dt + 1/2*sd^2*j^2*dt)
      b <- (1 + r*dt)^-1 * (1 - sd^2*j^2*dt)
      c <- (1 + r*dt)^-1 * (1/2*r*j*dt + 1/2*sd^2*j^2*dt)
      f[i,g2m(j)] <- t(c(a,b,c)) %*% f[i+1,g2m((j-1):(j+1))]
    }
    
    if (type == 'call') {               # m: ???C/???S 「 1
      f[i,g2m(m)] <- f[i,g2m(m-1)] + s.seq[g2m(m)] - s.seq[g2m(m-1)]
      f[i,g2m(0)] <- f[i,g2m(1)]        # 0: ???C/???S 「 0
    }
    else if (type == 'put') {           # m: ???C/???S 「 0
      f[i,g2m(m)] <- f[i,g2m(m-1)]      # 0: ???C/???S 「 1
      f[i,g2m(0)] <- f[i,g2m(1)] - (s.seq[g2m(1)] - s.seq[g2m(0)])
      if (style == 'american')
        f[i,] <- pmax(f[i,], k - s.seq)
    }
  }
  
  if (grid) return(f) else return(f[g2m(0), g2m(m/2)])
}

fde.log <- function(s, k, r, t, sd,
                    n = ceiling(1e3*t), m = 2*ceiling(sqrt(3*n)),
                    type = c("call", "put"), style = c("european", "american"),
                    grid = FALSE) {
  if (t <= 0) stop("t = ", t, " is nonpositive!")
  if (!is.wholenumber(n) || n <= 0) stop("n = ",n," is not a positive integer!")
  if (!is.wholenumber(m) || m <= 0) stop("m = ",m," is not a positive integer!")
  type <- match.arg(type); style <- match.arg(style)
  
  dt <- t / n
  m <- m + m%%2                         # Ensure m is even.
  ## Set stock price limits to +/- 3 standard deviations.
  z.lim <- log(s) + 3*sd*sqrt(t)*c(min=-1, max=1)
  dz <- unname(diff(z.lim)) / m
  z.seq <- z.lim['min'] + 0:m*dz        # vector, m+1 elements
  
  f <- matrix(rep(NA, (n+1)*(m+1)), nrow=n+1)
  g2m <- function(i)  i + 1             # grid index to matrix index
  f[g2m(n),] = switch(type, call=pmax(exp(z.seq)-k,0), put=pmax(k-exp(z.seq),0))
  
  a <- (1 + r*dt)^-1 * (-dt/(2*dz)*(r - 1/2*sd^2) + dt/(2*dz^2)*sd^2)
  b <- (1 + r*dt)^-1 * (1 - dt/dz^2*sd^2)
  c <- (1 + r*dt)^-1 * (dt/(2*dz)*(r - 1/2*sd^2) + dt/(2*dz^2)*sd^2)
  for (i in g2m((n-1):0)) {             # Iterate from end to beginning.
    j.seq <- rep(g2m(0:2), times=m-1) + rep(0:(m-2), each=3)
    f[i,g2m(1:(m-1))] <- matrix(f[i+1,j.seq], ncol=3, byrow=TRUE) %*% c(a,b,c)
    
    if (type == 'call') {               # m: ???C/???S 「 1
      f[i,g2m(m)] <- f[i,g2m(m-1)] + exp(z.seq[g2m(m)]) - exp(z.seq[g2m(m-1)])
      f[i,g2m(0)] <- f[i,g2m(1)]        # 0: ???C/???S 「 0
    }
    else if (type == 'put') {           # m: ???C/???S 「 0
      f[i,g2m(m)] <- f[i,g2m(m-1)]      # 0: ???C/???S 「 1
      f[i,g2m(0)] <- f[i,g2m(1)] - (exp(z.seq[g2m(m)]) - exp(z.seq[g2m(m-1)]))
      if (style == 'american')
        f[i,] <- pmax(f[i,], k - exp(z.seq))
    }
  }
  
  if (grid) return(f) else return(f[g2m(0), g2m(m/2)])
}