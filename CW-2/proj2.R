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

# strategy1 <- function(n, k, strategy, nreps){
#   guess <- numbered_boxes[k]
#   check <- success_check(n, k, guess, numbered_boxes)
#   number_of_success <- number_of_success + check
# 
#     
# }
# 
# strategy2 <- function(n, k, strategy, nreps){
#   guess<-sample(1:(2*n),1)
#   check <- success_check(n, k, guess, numbered_boxes)
#   number_of_success <- number_of_success + check
#   
#   
# }
# 
# strategy3 <- function(n, k, strategy, nreps){
#   #first we want to pick n boxes at random
#   random_box <- sample(1:(2*n), n)
#   index <- 1
#   number_of_attempts <- 0
#   for (box in random_box){
#     #if one of the random boxes contains the prisoners number
#     if (box == k){
#       #count successes
#       number_of_success <- number_of_success + 1
#       break
#     }
#     else{
#       index <- index + 1
#       #count fail attempts
#       number_of_attempts <- number_of_attempts + 1
#     }
#   }
#   
# }




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
          if (box == i){
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

#example code for Pone
Pone(50,1,1,10000)
Pone(50,1,2,10000)
Pone(50,1,3,10000)
Pone(5,1,1,10000)
Pone(5,1,2,10000)
Pone(5,1,3,10000)
#example code for Pall
Pall(50,1,10000)
Pall(50,2,10000)
Pall(50,3,10000)
Pall(5,1,10000)
Pall(5,2,10000)
Pall(5,3,10000)

#For Pone evidently the less people there are (the smaller that n is) the better 
#chance of success. Although Strategy 3 yields almost consistently similar results
#as after all we are still only observing n boxes with 2*n people

#Pall is where we observe a surprising result strategy2 and strategy3 yield zero/near zero
#probabilities. Strategy1 yields an approximate 30% and 50% chances (n=50 case and n=5 case
#respectively) This seems to be the most optimal strategy, but why?
#This is due to a cycle.
#Every box leads to another box and eventually loops back to the number we began with,
#this is guaranteed. What is not guaranteed is the cycle which contains your number
#may not be in a cycle with n or less elements.