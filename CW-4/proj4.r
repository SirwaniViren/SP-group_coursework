# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943

# Git repo Link: https://github.com/SirwaniViren/SP-group_coursework/tree/main/CW-4

# possible values for theta when func = rb => c(-.5,1)
newt <- function(theta, func, grad, hess = NULL,..., tol = 1e-8, fscale = 1, 
                 maxit = 100, max.half = 20, eps = 1e-6) {
  gll0 <- gb(theta) ## grad of nll at th0
  n <- length(theta)
  if (is.null(hess)) {
    hess <- matrix(0,n,n) ## finite diference Hessian
    for (i in 1:n) { ## loop over parameters
      th1 <- theta; th1[i] <- th1[i] + eps ## increase th0[i] by eps
      gll1 <- gb(th1) ## compute resulting nll
      hess[i,] <- (gll1 - gll0)/eps ## approximate second derivs
    }
  }
}

# RUN CODE UNDER THIS

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
tol = 1e-8
# optimum value for rb is 0
fscale = 0
eps = 1e-6
theta = c(-.5, 1)
obj_func_at_theta <- rb(theta)
iterations <- 0

while (any(abs(gb(theta)) > (tol * (abs(rb(theta)) + fscale)))) {
  iterations = iterations + 1
  hess = hb(theta)
  eig_values <- eigen(hess)$values
  preturb_val <- 0
  check_max_half <- 0
  while (any(eig_values < 0)) {
    preturb_val = preturb_val + 1
    new_hess <- hess + preturb_val*diag(n)
    eig_values <- eigen(new_hess)$values
  }
  hess <- hess + preturb_val*diag(n)
  delta = -chol2inv(chol(hess)) %*% gb(theta)
  while (rb(theta + delta) >= rb(theta)) {
    check_max_half = check_max_half + 1
    delta = delta/2
  }
  delta_t = t(delta)
  theta <- theta + delta
  cat("Number of max half:", check_max_half, "Number of iterations:", iterations)
  cat("",theta, " ", rb(theta), "\n")
}