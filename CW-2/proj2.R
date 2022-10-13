# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943
# Git repo Link: 
# Team member contributions to project:

Pone <- function(n, k, strategy, nreps) {
  number_of_success <- 0
  for (reps in 1:nreps) {
    # index of numbered_boxes corresponds to the box number, and the value stored 
    # at that index corresponds to the numbered card randomly placed in the box
    numbered_boxes <- sample(1:(2*n), 2*n)
    
    number_of_attempts <- 0
    
    if (strategy == 1) {
      try <- numbered_boxes[k]
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
    }
    
    else if (strategy == 2) {
      guess<-sample(1:n,1)
      while(number_of_attempts<=n){
        if(guess==k){
          number_of_success <- number_of_success + 1
          break
        }
        else{
          guess<-numbered_boxes[guess]
          number_of_attempts <- number_of_attempts + 1
        }
      }
      
    }
    else {
      #first we want to pick a box at random
      random_box<-sample(1:n,n)
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