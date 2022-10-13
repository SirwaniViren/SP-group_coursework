# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943
# Git repo Link: 
# Team member contributions to project:

success_check <- function(n, k, try){
  number_of_attempts <- 0
  number_of_success <- 0
  while (number_of_attempts <= n) {
    if (try == k) {
      number_of_success <- number_of_success + 1
      break
    }
    else {
      try <- numbered_boxes[try]
      number_of_attempts <- number_of_attempts + 1
    }
  }
  
  return (c(number_of_attempts, number_of_success))
}

Pone <- function(n, k, strategy, nreps) {
  number_of_success <- 0
  for (reps in 1:nreps) {
    # index of numbered_boxes corresponds to the box number, and the value stored 
    # at that index corresponds to the numbered card randomly placed in the box
    numbered_boxes <- sample(1:(2*n), 2*n)
    
    number_of_attempts <- 0
    
    if (strategy == 1) {
      guess <- numbered_boxes[k]
      check <- success_check(n, k,guess)
      number_of_attempts <- check[[1]]
      number_of_success <- check[[2]]
    }
    
    else if (strategy == 2) {
      guess<-sample(1:(2*n),1)
      check <- success_check(n, k, guess)
      number_of_attempts <- check[1]
      number_of_success <- check[2]
      }
      
    }
    else {
      #first we want to pick a box at random
      random_box<-sample(1:(2*n),n)
      while(number_of_attemps<=n){
        #if this random box contains the prisoners number
        if(random_box=k){
          #count successes
          number_of_success<- number_of_success + 1
        }
        else{
          #pick a new random box
          random_box<-sample(1:n,n)
          #count fails
          number_of_attempts<-number_of_attempts +1
        }
      }
      
    }
    
  }
  
  prob_one <- number_of_success/nreps
  return (prob_one)
}