# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943
# Git repo Link:
# Team member contributions to project:

#INPUT: n <- decides numbers of prisoners, k<- prisoner number
#numbered_boxes <- numbered boxes
#output:the number of successes
#Purpose: Is to find and track the number of successes for us to calculate
#the probabilities in later functions. Also code isnt repeated in later functions
success_check <- function(n, k, numbered_boxes) {
  number_of_attempts <- 0
  number_of_success <- 0
  guess <- numbered_boxes[k]
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


produce_random_numbered_boxes <- function(n, quantity) {
  return (sample(1:n, quantity))
}

#INPUT: n <- decides numbers of prisoners, k <- prisoner number, strategy <-1
#nreps <- number of times experiment is done,number_of_success<- the number of successes so far,
#numbered_boxes<- numbered boxes
#OUTPUT: number of successes
#PURPOSE: This function simulates the first strategy where a prisoner
#picks a box with their number on it
strategy1 <- function(n, k, strategy, nreps,number_of_success,numbered_boxes){
  #Let initial guess be k-prisoners' number
  guess <- numbered_boxes[k]
  #check the success 
  check <- success_check(n, k,numbered_boxes)
  #if so, count the successes
  number_of_success <- number_of_success + check
  return(number_of_success)
}

#INPUT: n <- decides numbers of prisoners, k <- prisoner number, strategy <- 2
#nreps <- number of times experiment is done,number_of_success<- the number of successes so far,
#numbered_boxes<- numbered boxes
#OUTPUT: number of successes
#PURPOSE: This function simulates the second strategy where a prisoner
#picks a random box to begin with
strategy2 <- function(n, k, strategy, nreps,number_of_success,numbered_boxes){
  #let initial guess be random
  guess<-produce_random_numbered_boxes(2 * n, 1)
  check <- success_check(n, k, numbered_boxes)
  number_of_success <- number_of_success + check
  return(number_of_success)
}

#INPUT: n <- decides numbers of prisoners, k <- prisoner number, strategy <- 3
#nreps <- number of times experiment is done,number_of_success<- the number of successes so far,
#OUTPUT: number of successes
#PURPOSE: This function simulates the third strategy where a prisoner
#picks n boxes at random, checking each card for their number
strategy3 <- function(n, k, strategy, nreps,number_of_success){
  random_box <- produce_random_numbered_boxes(2 * n, n)
  for (box in random_box){
    #if one of the random boxes contains the prisoners number
    if (box == k){
      #count successes
      number_of_success <- number_of_success + 1
      break
    }
  }
  return(number_of_success)
}

#INPUT: n <- decides numbers of prisoners, k <- prisoner number, strategy <- either 1,2,3
#nreps <- number of times experiment is done
#OUTPUT: Probability of strategy given chosen parameters in the input
#PURPOSE: To find the probability of one prisoner being released for chosen inputs
Pone <- function(n, k, strategy, nreps = 10000) {
  number_of_success <- 0
  for (reps in 1:nreps) {
    # index of numbered_boxes corresponds to the box number, and the value stored
    # at that index corresponds to the numbered card randomly placed in the box
    numbered_boxes <- produce_random_numbered_boxes(2 * n, 2 * n)
    
    if (strategy == 1) {
      number_of_success=strategy1(n, k, strategy, nreps,number_of_success,numbered_boxes)
    }
    else if (strategy == 2) {
      number_of_success=strategy2(n, k, strategy, nreps,number_of_success,numbered_boxes)
    }
    else if (strategy == 3) {
      number_of_success=strategy3(n, k, strategy, nreps,number_of_success)
    }
  }
  prob_one_estimate <- number_of_success / nreps
  return (prob_one_estimate)
}

#INPUT: n <- decides numbers of prisoners strategy <- either 1,2,3
#nreps <- number of times experiment is done
#OUTPUT: Probability of strategy given chosen parameters in the input
#PURPOSE: To find the probability of all prisoners being released for chosen inputs
Pall <- function(n, strategy, nreps = 10000) {
  number_of_success <- 0
  for (reps in 1:nreps) {
    # Outside inner loop since the room is returned to original state once 
    # prisoner leaves
    numbered_boxes <- produce_random_numbered_boxes(2 * n, 2 * n)
    for (prisoner_number in 1:(2 * n)) {
      if (strategy == 1) {
        check <- success_check(n, prisoner_number, numbered_boxes)
        if (check == 0)
          break
      }
      else if (strategy == 2) {
        random_start_box <- produce_random_numbered_boxes(2 * n, 1)
        check <- success_check(n, random_start_box, numbered_boxes)
        if (check == 0)
          break
      }
      else if (strategy == 3) {
        #picking n boxes at random
        random_box <- produce_random_numbered_boxes(2 * n, n)
        check <- 0
        for (box in random_box) {
          #if this random box contains the prisoners number
          if (box == prisoner_number) {
            check = 1
            break
          }
        }
        if (check == 0) {
          break
        }
      }
    }
    number_of_success <- number_of_success + check
  }
  
  prob_all_estimate <- number_of_success / nreps
  return (prob_all_estimate)
}

# cat("Probability estimate of a single prisoner succeeding in finding their number:")
# cat("\nn=5\nStrategy 1:", Pone(5, 7, 1))
# cat("\nStrategy 2:", Pone(5, 7, 2))
# cat("\nStrategy 3:", Pone(5, 7, 3))
# 
# cat("\nn=50\nStrategy 1:", Pone(50, 7, 1))
# cat("\nStrategy 2:", Pone(50, 7, 2))
# cat("\nStrategy 3:", Pone(50, 7, 3))
# 
# cat("\nProbability estimate of all prisoners succeeding in finding their numbers:")
# cat("\nn=5\nStrategy 1:", Pall(5, 1))
# cat("\nStrategy 2:", Pall(5, 2))
# cat("\nStrategy 3:", Pall(5, 3))
# 
# cat("\nn=50\nStrategy 1:", Pall(50, 1))
# cat("\nStrategy 2:", Pall(50, 2))
# cat("\nStrategy 3:", Pall(50, 3))

#example code for Pone
# Pone(50,1,1,10000)
# Pone(50,1,2,10000)
# Pone(50,1,3,10000)
# Pone(5,1,1,10000)
# Pone(5,1,2,10000)
# Pone(5,1,3,10000)
#example code for Pall
# Pall(50,1,10000)
# Pall(50,2,10000)
# Pall(50,3,10000)
# Pall(5,1,10000)
# Pall(5,2,10000)
# Pall(5,3,10000)

#For Pone evidently the less people there are (the smaller that n is) the better 
#chance of success. Although Strategy 3 yields almost consistently similar results
#as after all we are still only observing n boxes with 2*n people

#Pall is where we observe surprising results, strategy3 yield zero/near zero
#probabilities, Strategy1 and strategy2 yields an approximate 30% and 50% chances (n=50 case and n=5 case
#respectively) They seems to be the most optimal strategies, but why?
#This is due to a cycle.
#Every box leads to another box and eventually loops back to the number we began with,
#this is guaranteed. What is not guaranteed is the cycle which contains your number
#may not be in a cycle with n or less elements.

dloop <- function(n, nreps) {
  #create a vector of length 2*n
  u<-vector("integer",2*n)
  
}


