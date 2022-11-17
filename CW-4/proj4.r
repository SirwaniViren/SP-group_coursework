# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943

# Git repo Link: https://github.com/SirwaniViren/SP-group_coursework/tree/main/CW-4

#INPUT: theta -> initial values for optimization parameters
# grad -> gradient function , eps -> the finite difference intervals when hessian
#function is not provided
#OUTPUT: A hessian matrix 
finite_diff_hess <- function(theta, grad, eps, ...){
  
  # get length of vector of parameter values
  n <- length(theta)
  # gradient function at the initial theta
  grad0 <- grad(theta)
  # finite difference Hessian, we first need to initialize it
  hess_temp <- matrix(0, n, n)
  for(i in 1:n){
    # just make a copy of theta
    # and also increase th1[i] by eps
    th1 <- theta; th1[i] <- th1[i] + eps 
    # compute resulting gradient function
    grad1 <- grad(th1)
    # approximate second derivatives
    hess_temp[i,] <- (grad1 - grad0)/eps 
  }
  
  # use the fact that t((A)+A)/2 is exactly symmetric for any matrix A
  hess <- (t(hess_temp) + hess_temp)/2
  return (hess)
}

#INPUT: theta -> initial values for optimization parameters, 
#func-> objective function to minimize, grad -> gradient function
#hess-> hessian matrix function, tol-> convergence tolerance, 
#fscale-> estimate of magnitude of func near optimum
#maxit-> max amount of iterations to perform , 
#max.half ->max amount of times step can be halved
#eps -> the finite difference intervals when hessian
#OUTPUT: the minimized function

# possible values for theta when func = rb => c(-.5,1)
newt <- function(theta, func, grad, hess = NULL,..., tol = 1e-8, fscale = 1, 
                 maxit = 100, max.half = 20, eps = 1e-6) {

  # check if the objective or derivatives are not finite at the initial theta
  # a warning is issued if that is the case
  if (abs(func(theta)) == Inf | any(abs(grad(theta))) == Inf){
    warning("objective or derivatives are not finite at the initial theta")
  }
  
  # check if hessian matrix function is not provided
  # if it is not, we obtain an approximation to the Hessian matrix function by 
  # performing finite differencing of the gradient vector 
  if (is.null(hess)) {
    #print("fdfd")
    warning('Hessian Matrix not provided, approximation provided')
    hess <- finite_diff_hess(theta, grad, eps)
  }
  
  # initializing the number of iterations count
  iterations <- 0
  # Convergence should be judged by seeing whether all elements of the gradient 
  # vector have absolute value less than tol times the absolute value of the 
  # objective function plus fscale 
  conv_thresh <- tol * (abs(func(theta)) + fscale)
  
  # while loop runs till convergence is not achieved
  while (any(abs(grad(theta)) > (conv_thresh))) {
    # increase number of iterations by one
    iterations <- iterations + 1
    # Hessian matrix with initial theta values
    hess_val <- hess(theta)
    # compute eigen values of hessian matrix
    eig_values <- eigen(hess_val)$values
    # perturbation value to force hessian to be positive definite
    preturb_val <- 0
    # count for the number of times the step was halved
    check_max_half <- 0
    # while loop until any eigenvalue is negative
    while (any(eig_values < 0)) {
      # add a multiple of the identity matrix, which we use later
      preturb_val <- preturb_val + 1
      # new Hessian matrix with perturbation
      new_hess <- hess_val + preturb_val*diag(n)
      # new eigen values to be checked
      eig_values <- eigen(new_hess)$values
    }
    hess_val <- hess_val + preturb_val*diag(n)
    # A descent direction is one in which a sufficiently small step will 
    # decrease the objective function
    delta <- -chol2inv(chol(hess_val)) %*% grad(theta)
    # while loop to halve step size until objective function decreases
    while (func(theta + delta) >= func(theta)) {
      # update the count for number of times step size is halved
      check_max_half <- check_max_half + 1
      # halve step size
      delta <- delta/2
    }
    # transpose of delta
    delta_t <- t(delta)
    # update optimization values
    theta <- theta + delta
    conv_thresh <- tol * (abs(func(theta)) + fscale)
    cat("Number of max half:", check_max_half, "Number of iterations:", iterations)
    cat("",theta, " ", func(theta), "\n")
  }
  if(iterations==maxit) warning("iteration limit reached")
  if(check_max_half>20) warning('step has failed to improve the objective')
  # value of the objective function at the minimum
  f <- func(theta)
  # inverse of the Hessian matrix at the minimum
  Hi <- chol2inv(chol(hess))
  # gradient vector at the minimum
  g <- grad(theta)
  
  # we are returning a list containing the above three variables f, Hi, g and
  # the following:
  # iter - number of iterations taken to reach the minimum
  # theta - value of the parameters at the minimum
  return_vals <- list(f=f, theta=theta, iter=iterations, g=g, Hi=Hi)
  
  return(return_vals)
}

# RUN CODE UNDER THIS

#given
rb <- function(th,k=2) {
  k*(th[2]-th[1]^2)^2 + (1-th[1])^2
}

#given
gb <- function(th,k=2) {
  c(-2*(1-th[1])-k*4*th[1]*(th[2]-th[1]^2),k*2*(th[2]-th[1]^2))
}

#given
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
n <- length(theta)
obj_func_at_theta <- rb(theta)
iterations <- 0
