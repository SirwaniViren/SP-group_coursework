# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943
# Git repo Link:
# Team member contributions to project:

# INPUT: n <- decides numbers of prisoners, k<- prisoner number, 
# numbered_boxes <- numbered boxes
# OUTPUT: the number of successes
# Purpose: Is to find and track the number of successes for us to calculate
# the probabilities in later functions. Also code isnt repeated in later functions
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

# INPUT: n <- decides numbers of prisoners, k <- prisoner number, 
# strategy <- 1, 2 or 3, nreps <- number of times experiment is done,
# number_of_success<- the number of successes so far, 
# numbered_boxes<- numbered boxes
# OUTPUT: number of successes
# PURPOSE: This function simulates the first strategy where a prisoner
# picks a box with their number on it, the second strategy where a prisoner
# picks a random box to begin with, and the third strategy where a prisoner
# picks n boxes at random, checking each card for their number
strategy <- function(n, k, strategy, nreps,number_of_success,numbered_boxes){
  #Let initial guess be k-prisoners' number
  guess <- numbered_boxes[k]
  if (strategy == 1) 
    guess <- numbered_boxes[k] 
  else if (strategy == 2)
    guess <- produce_random_numbered_boxes(2 * n, 1)
  else if (strategy == 3) {
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
  #check the success 
  check <- success_check(n, k,numbered_boxes)
  #if so, count the successes
  number_of_success <- number_of_success + check
  return(number_of_success)
}

# INPUT: n <- decides numbers of prisoners, k <- prisoner number, 
# strategy <- either 1,2,3, nreps <- number of times experiment is done
# OUTPUT: Probability estimate of prisoner k finding their number using given strategy
# PURPOSE: To find the probability of one prisoner finding their number
Pone <- function(n, k, strategy, nreps = 10000) {
  number_of_success <- 0
  for (reps in 1:nreps) {
    # index of numbered_boxes corresponds to the box number, and the value stored
    # at that index corresponds to the numbered card randomly placed in the box
    numbered_boxes <- produce_random_numbered_boxes(2 * n, 2 * n)
    
    if (strategy == 1) {
      number_of_success=strategy(n, k, strategy, nreps,number_of_success, numbered_boxes)
    }
    else if (strategy == 2) {
      number_of_success=strategy(n, k, strategy, nreps,number_of_success, numbered_boxes)
    }
    else if (strategy == 3) {
      number_of_success=strategy(n, k, strategy, nreps,number_of_success, numbered_boxes)
    }
  }
  prob_one_estimate <- number_of_success / nreps
  return (prob_one_estimate)
}

# INPUT: n <- decides numbers of prisoners, strategy <- either 1,2,3
# nreps <- number of times experiment is done
# OUTPUT: Probability estimate of all prisoners finding their number using given strategy
# PURPOSE: To find the probability of all prisoners being released
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

# For Pone evidently the less people there are (the smaller that n is) the better 
# chance of success. Although Strategy 3 yields almost consistently similar 
# results as after all we are still only observing n boxes with 2*n prisoners.

# Pall is where we observe a surprising result. Strategy 3 yields zero/near zero
# probabilities. Strategy 1 and 2 yields an approximate probability of 33% and 52% when
# n=50 and n=5 respectively. This seems to be the most optimal strategy, but why?
# This is due to a cycle.
# Every box leads to another box and eventually loops back to the prisoner number,
# this is guaranteed. What is not guaranteed is the cycle which contains the 
# prisoner number may have a length greater than n.


# probability of each loop length from 1 to 2n occurring at least once in a 
# random shuffling of cards to boxes = 1 - prob of each loop length occurring 
# 0 times
dloop <- function(n, nreps = 10000) {
  # count of all the cycles with loop length 1 to 2n that occurred 0 times 
  # in the simulation
  count_loop_length_0_times = rep(0, 2*n)
  #loop over all experiments(number of repetitions)
  for (reps in 1:nreps) {
    # index of numbered_boxes corresponds to the box number, and the value stored
    # at that index corresponds to the numbered card randomly placed in the box
    numbered_boxes <- produce_random_numbered_boxes(2 * n, 2 * n)
    # count of how many times a cycle has 1 to 2n loop length
    count_of_loop_length = rep(0, 2*n)
    # numbers that are already present in a cycle, hence don't need to be 
    # iterated over again
    numbers_iterated_over = c()
    # looping over all prisoner numbers
    for (prisoner_number in 1:(2 * n)) {
      # vector to hold various cycles, length of this vector would give the 
      # length of the cycle that contains the prisoner number
      individual_cycles = c()
      # if prisoner number is not present in any previous cycles, we follow
      # the path of boxes to find the cycle that contains the prisoner number
      # if the prisoner number is present, then just skip to the next 
      # prisoner number
      if (!(prisoner_number %in% numbers_iterated_over)) {
        individual_cycles = append(individual_cycles, prisoner_number)
        cycle = numbered_boxes[prisoner_number]
        # checks if the card removed from the box contains the prisoner number
        while (cycle != prisoner_number) {
          individual_cycles = append(individual_cycles, cycle)
          cycle = numbered_boxes[cycle]
        }
        count_of_loop_length[length(individual_cycles)] = 
          count_of_loop_length[length(individual_cycles)] + 1
        # appending the numbers in the cycle to the vector that contains all the
        # cycles
        numbers_iterated_over = append(numbers_iterated_over, individual_cycles)
      }
    }
    # finds all loop lengths that occurred 0 times in the experiment and adds 1
    # to the count for each of those loop lengths
    index_loop_length_0_times = grep(0, count_of_loop_length)
    count_loop_length_0_times[index_loop_length_0_times] = 
      count_loop_length_0_times[index_loop_length_0_times] + 1
  }
  prob_loop_length_0_times = count_loop_length_0_times/nreps
  prob_loop_length_at_least_once = 1 - prob_loop_length_0_times
  return (prob_loop_length_at_least_once)
}


# cat("Probability of loop length from 1 to 2n occurring at least once in a random shuffling of cards to boxes: ")
# cat("\nFor n=50: ", dloop(50))

# example code for dloop
# dloop(50,10000)


# We use a line graph to visualize the probabilities for each loop length. A line
# graph is used her since we can identify trends in the data. Usually line graphs are
# used to track changes over a short or long period of time. But it can be used in
# this scenario.
n1 <- 50
loop_vector1 <- dloop(n=n1,nreps=10000)
line_graph <- plot(x= 1:(2*n1),
                   y=loop_vector1, 
                   type="o",
                   xlab="Loop length",
                   ylab="Probability of Loop length",
                   main= "probability of each loop length from 1 to 2n occurring at least once")

# A bar chart could be used to compare the different loop lengths. 
# The code is not too different here. Bar-chart may seem a bit cramped
n2 <- 50
loop_vector2 <- dloop(n=n2,nreps=10000)
bar_chart <- barplot(height=loop_vector2, 
                     xlab="Loop length",
                     ylab="Probability of Loop length",
                     main= "probability of each loop length from 1 to 2n occurring at least once",
                     space=0)
axis(side=1,at=c(0,(2*n2)))

