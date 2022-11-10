# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943

# Git repo Link: https://github.com/SirwaniViren/SP-group_coursework/tree/main/CW-4
# possible values for theta when func = rb => c(-.5,1)
newt <- function(theta, func, grad, hess = NULL,..., tol = 1e-8, fscale = 1, 
                 maxit = 100, max.half = 20, eps = 1e-6) {
  
}

rb <- function(th,k=2) {
  k*(th[2]-th[1]^2)^2 + (1-th[1])^2
}

gb <- function(th,k=2) {
  c(-2*(1-th[1])-k*4*th[1]*(th[2]-th[1]^2),k*2*(th[2]-th[1]^2))
}

hb <- function(th,k=2) {
  h <- matrix(0,2,2)
  h[1,1] <- 2-k*2*(2*(th[2]-th[1]^2) - 4*th[1]^2)
  h[2,2] <- 2*k
  h[1,2] <- h[2,1] <- -4*k*th[1]
  h
}

theta = c(-.5, 1)
gll0 <- gb(theta) ## grad of nll at th0
n <- length(theta)
eps <- 1e-6 ## finite difference interval
Hfd <- matrix(0,n,n) ## finite diference Hessian
for (i in 1:length(theta)) { ## loop over parameters
  th1 <- theta; th1[i] <- th1[i] + eps ## increase th0[i] by eps
  gll1 <- gb(th1) ## compute resulting nll
  Hfd[i,] <- (gll1 - gll0)/eps ## approximate second derivs
}
hb(theta)
