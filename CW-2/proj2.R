# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943
# Git repo Link: 
# Team member contributions to project:

success_check <- function(n, k, guess, numbered_boxes){
  number_of_attempts <- 0
  number_of_success <- 0
  while (number_of_attempts <= n) {
    if (guess == k) {
      number_of_success <- 1
      break
    }
    else {
      guess <- numbered_boxes[guess]
      number_of_attempts <- number_of_attempts + 1
    }
  }
  
  return (number_of_success)
}

Pone <- function(n, k, strategy, nreps) {
  number_of_success <- 0
  for (reps in 1:nreps) {
    # index of numbered_boxes corresponds to the box number, and the value stored 
    # at that index corresponds to the numbered card randomly placed in the box
    numbered_boxes <- sample(1:(2*n), 2*n)
    
    if (strategy == 1) {
      guess <- numbered_boxes[k]
      check <- success_check(n, k, guess, numbered_boxes)
      number_of_success <- number_of_success + check
    }
    else if (strategy == 2) {
      guess<-sample(1:(2*n),1)
      check <- success_check(n, k, guess, numbered_boxes)
      number_of_success <- number_of_success + check
    }
    else if (strategy == 3) {
      #first we want to pick a box at random
      random_box <- sample(1:(2*n), n)
      index <- 1
      number_of_attempts <- 0
      for (box in random_box){
        #if this random box contains the prisoners number
        if (box == k){
          #count successes
          number_of_success <- number_of_success + 1
          break
        }
        else{
          index <- index + 1
          #count fail attempts
          number_of_attempts <- number_of_attempts + 1
        }
      }
    }
  }
  prob_one <- number_of_success/nreps
  return (prob_one)
}


Pall <- function(n, strategy, nreps){
  number_of_success <- 0
  for (reps in 1:nreps){
    # Outside inner loop since the room is returned to original state once 
    # prisoner leaves
    numbered_boxes <- sample(1:(2*n), 2*n)
    success_or_not <- 0
    for (i in 1:(2*n)){
      
      if (strategy == 1) {
        guess <- numbered_boxes[i]
        check <- success_check(n, i, guess, numbered_boxes)
        success_or_not <- success_or_not + check
      }
      else if (strategy == 2) {
        guess<-sample(1:(2*n),1)
        check <- success_check(n, i, guess, numbered_boxes)
        success_or_not <- success_or_not + check
      }
      else if (strategy == 3) {
        #first we want to pick a box at random
        random_box <- sample(1:(2*n), n)
        index <- 1
        number_of_attempts <- 0
        for (box in random_box){
          #if this random box contains the prisoners number
          if (box == k){
            #if success or not then add 1 to indicate prisoner number found
            success_or_not <- success_or_not + 1
            break
          }
          else{
            index <- index + 1
            #count fail attempts
            number_of_attempts <- number_of_attempts + 1
          }
        }
      }
    }
    if (success_or_not == (2*n)){
      number_of_success <- number_of_success + 1
    }
  }
  prob_all <- number_of_success/nreps
  return (prob_all)
  
}

