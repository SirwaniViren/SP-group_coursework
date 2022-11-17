# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943

# Git repo Link: 
#     https://github.com/SirwaniViren/SP-group_coursework/tree/main/CW-4

# Team member contributions to project:
# Question involved the collaboration of multiple members of the group.

# The questions that were done individually are shown below:

# Viren Sirwani Mulani s1949143: Was in charge of the majority of commenting.
# Created helper function to approximate Hessian matrix using finite differencing.
# Fixed return of newt values. Involved in error handling.

# Karman Singh s1936373: Wrote the bulk of newt function. Was also involved in
# error handling.

# Alannah Hounat s2434943: Was in charge of most of the error handling. Created 
# function to test Hessian matrix. Wrote overview of code.

# Overview of code:
#

# INPUT: theta -> initial values for optimization parameters,
#        grad -> gradient function, 
#        eps -> the finite difference intervals when hessian function is not 
#              provided
# OUTPUT: A hessian matrix 
# PURPOSE: this helper function approximates the Hessian matrix using
#          finite differencing
finite_diff_hess <- function(theta, grad, eps, ...){
  
  # get length of vector of parameter values
  n <- length(theta)
  # gradient function at the initial theta
  grad0 <- grad(theta, ...)
  # finite difference Hessian, we first need to initialize it
  hess_temp <- matrix(0, n, n)
  for(i in 1:n){
    # just make a copy of theta
    # and also increase th1[i] by eps
    th1 <- theta; th1[i] <- th1[i] + eps 
    # compute resulting gradient function
    grad1 <- grad(th1, ...)
    # approximate second derivatives
    hess_temp[i,] <- (grad1 - grad0)/eps 
  }
  
  # use the fact that t((A)+A)/2 is exactly symmetric for any matrix A
  hess <- (t(hess_temp) + hess_temp)/2
  return (hess)
}

# INPUT: theta -> initial values for optimization parameters, 
#        func-> objective function to minimize, grad -> gradient function
#        hess-> hessian matrix function, tol-> convergence tolerance, 
#        fscale-> estimate of magnitude of func near optimum
#        maxit-> max amount of iterations to perform , 
#        max.half ->max amount of times step can be halved
#        eps -> the finite difference intervals when hessian
# OUTPUT: a list defining the optimized functions.
#         The list contains the value of the optimized function at the minimum,
#         the value of the parameters at the minimum, the number of iterations 
#         taken to reach the minimum, g the gradient vector at the minimum, 
#         and the inverse of the Hessian matrix at the minimum
# PURPOSE: this optimization function implements Newton’s method for
#          minimization of functions. The function issues error/warnings 
#          in various different cases. 

newt <- function(theta, func, grad, hess = NULL,..., tol = 1e-8, fscale = 1, 
                 maxit = 100, max.half = 20, eps = 1e-6) {
  
  # check if the objective or derivatives are not finite at the initial theta
  # a warning is issued if that is the case
  if (!is.finite(abs(func(theta, ...))) | any(!is.finite(abs(grad(theta, ...))))) {
    warning("objective or derivatives are not finite at the initial theta")
  }
  
  # initializing the number of iterations count
  iterations <- 0
  # Convergence should be judged by seeing whether all elements of the gradient 
  # vector have absolute value less than tol times the absolute value of the 
  # objective function plus fscale 
  conv_thresh <- tol * (abs(func(theta, ...)) + fscale)
  
  # get length of vector of parameter values
  n <- length(theta)
  
  # while loop runs till convergence is not achieved
  while (any(abs(grad(theta, ...)) > (conv_thresh))) {
    # increase number of iterations by one
    iterations <- iterations + 1
    if(iterations > maxit) {
      stop("Iteration limit reached without convergence!")
    }
    # check if hessian matrix function is not provided
    # if it is not, we obtain an approximation to the Hessian matrix function by 
    # performing finite differencing of the gradient vector 
    if (is.null(hess)) {
      #warning('Hessian Matrix not provided, approximation provided')
      hess_val <- finite_diff_hess(theta, grad, eps, ...)
    }
    else {
      hess_val <- hess(theta, ...)
    }
    # perturbation value to force hessian to be positive definite
    perturb_val <- 0
    # count for the number of times the step was halved
    check_max_half <- 0
    # while loop that deals with non-postive hessian matrix and perturbs it
    while(inherits(try(chol(hess_val), TRUE), "try-error")) {
      # add a multiple of the identity matrix, which we use later
      perturb_val <- perturb_val + 1
      # new Hessian matrix with perturbation
      hess_val <- hess_val + perturb_val*diag(n)
    }
    
    # A descent direction is one in which a sufficiently small step will 
    # decrease the objective function
    delta <- -chol2inv(chol(hess_val)) %*% grad(theta, ...)
    # while loop to halve step size until objective function decreases
    while (func(theta + delta, ...) >= func(theta, ...) 
           | !(is.finite(func(theta + delta)))) {
      # update the count for number of times step size is halved
      check_max_half <- check_max_half + 1
      if(check_max_half > max.half) {
        stop('Step has failed to improve the objective!')
      }
      # halve step size
      delta <- delta/2
    }
    # transpose of delta
    delta_t <- t(delta)
    # update optimization values
    theta <- theta + delta
    conv_thresh <- tol * (abs(func(theta,...)) + fscale)
  }
  # value of the objective function at the minimum
  f <- func(theta, ...)
  #if hessian is not positive definite at convergence give an error
  if(inherits(try(chol(hb(theta)), TRUE), "try-error")){
    warning("Hessian is not positive definite at convergence")
    Hi<-NULL
  }
  else{
    # inverse of the Hessian matrix at the minimum
    Hi <- chol2inv(chol(hess_val))
  }
  
  # gradient vector at the minimum
  g <- grad(theta, ...)
  
  # we are returning a list containing the above three variables f, Hi, g and
  # the following:
  # iter - number of iterations taken to reach the minimum
  # theta - value of the parameters at the minimum
  return_vals <- list(f = f, theta = theta, iter = iterations, g = g, Hi = Hi)
  
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

newt(theta= c(-.5,1), func=rb, grad=gb, hess=hb, fscale=0)