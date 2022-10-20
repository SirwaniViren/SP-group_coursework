# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943

# Git repo Link: https://github.com/SirwaniViren/SP-group_coursework/tree/main/CW-2

# Team member contributions to project:
# Each member was in charge of individual questions. Some questions involved the 
# collaboration of multiple members of the group.

# The questions that were done individually are shown below:
# Viren Sirwani Mulani s1949143: Q6
# Karman Singh s1936373: Q5
# Alannah Hounat s2434943: Q3, Q4

# Questions that members worked on together are the following:
# Karman Singh s1936373, Viren Sirwani Mulani s1949143, Alannah Hounat s2434943 - Q1, Q2

# Overview of Code: Given the 2n prisoners problem. An R script to evaluate the 
# different probabilities of a prisoner being set free given that they must check n boxes from
# 2n boxes using one of three strategies.
# 3 strategies:
# 1. Prisoner opens first box with their number on the outside. If it doesn't contain their number
# the next box they open starts with the number revealed in the first box
# 2. the same as strategy 1 except the first box is a random box 
# 3. They open n boxes at random checking for their number within the box 
# We evaluate the probabilities of one prisoner being set free and all 2n prisoners being set free
# Then, based off the logic in a surprising strategy we evaluated the probability
# of each loop length occurring at least once and thus visualizing these 
# probabilities on a graph

# Overview of Code: Given the 2n prisoners problem. An R script to evaluate the 
# different probabilities of a prisoner being set free given that they must check 
# n boxes from 2n boxes using one of three strategies.
# 1. Prisoner opens first box with their number on the outside. If it doesn't
# contain their number the next box they open starts with the number revealed in 
# the first box
# 2. the same as strategy 1 except the first box is a random box 
# 3. They open n boxes at random checking for their number within the box 
# We evaluate the probabilities of one prisoner being set free and all 2n 
# prisoners being set free
# Then, based off the logic in a surprising strategy we evaluated the probability
# of each loop length occurring at least once and thus visualizing these 
# probabilities on a graph

# INPUT: n <- decides numbers of prisoners, prisoner_number<- prisoner number, 
# first_box <- first box picked by prisoner, depending on strategy , 
# numbered_boxes <- numbered boxes taken from random sample
# OUTPUT: 1 or 0 for success or not success respectively
# Purpose: returns 1 or 0 as stated above to tell us if the prisoners have
# been successful in finding their number. Also, so code isn't repeated in later functions
success_check <- function(n, prisoner_number, first_box, numbered_boxes) {
  # number of boxes opened by prisoner
  number_of_attempts <- 0
  # value 1 indicates prisoner number found, 0 otherwise
  success <- 0
  # first box opened corresponds to value of first_box value, depending on strategy used, could be 
  # random or prisoners number
  guess <- numbered_boxes[first_box]
  # while loop checks if number of boxes opened is less than n
  while (number_of_attempts < n) {
    if (guess == prisoner_number) {
      # if prisoner number found, no need to continue looking
      success <- 1
      break
    }
    else {
      # move on to the next box as previous box did not contain prisoner number
      guess <- numbered_boxes[guess]
      # number of attempts goes up by one as prisoner moves on to next box
      number_of_attempts <- number_of_attempts + 1
    }
  }
  
  # 1 or 0, depending on if prisoner number has been found
  return (success)
}

# INPUT: n <- total number of prisoners, quantity <- number of items to choose
# OUTPUT: vector of integers of length 'quantity'
# PURPOSE: produce a random vector of box numbers of a particular size depending on 
# the strategy. Strategy 2 would require us to return just one box number while 
# strategy 3 requires us to return n random box numbers
produce_random_numbered_boxes <- function(n, quantity) {
  return (sample(1:n, quantity))
}

# INPUT: n <- decides numbers of prisoners, k <- prisoner number, strategy <- 1, 2 or 3,
# numbered_boxes<- numbered boxes
# OUTPUT: 1(success) or 0(fail)
# PURPOSE: This function simulates the first strategy where a prisoner
# picks a box with their number on it, the second strategy where a prisoner
# picks a random box to begin with, and the third strategy where a prisoner
# picks n boxes at random, checking each card for their number
check_success_given_strategy <- function(n, k, strategy, numbered_boxes){
  success_strategy3 <- 0
  # if and else if to select the correct strategy
  if (strategy == 1)
    # first box picked has prisoners number on it
    first_box <- k
  else if (strategy == 2)
    # pick a box with a random number on it by calling the helper function 
    # 'produce_random_numbered_boxes'
    first_box <- produce_random_numbered_boxes(2 * n, 1)
  else if (strategy == 3) {
    # here we need n random boxes, the aforementioned helper function is used again
    random_box <- produce_random_numbered_boxes(2 * n, n)
    for (box in random_box) {
      # if one of the random boxes contains the prisoners number
      if (box == k){
        # if prisoner number found, no need to continue looking
        success_strategy3 <- 1
        break
      }
    }
    return (success_strategy3)
  }
  # check the success, used for strategy 1 and 2
  check <- success_check(n, k, first_box, numbered_boxes)
  return(check)
}

# INPUT: n <- decides numbers of prisoners, k <- prisoner number, strategy <- either 1,2,3,
# nreps <- number of times experiment is done
# OUTPUT: Probability estimate of prisoner k finding their number using given strategy
# PURPOSE: To find the probability of one prisoner finding their number
Pone <- function(n, k, strategy, nreps = 10000) {
  number_of_success <- 0
  # iterating over nreps experiments
  for (reps in 1:nreps) {
    # index of numbered_boxes corresponds to the box number, and the value stored
    # at that index corresponds to the numbered card randomly placed in the box
    numbered_boxes <- produce_random_numbered_boxes(2 * n, 2 * n)
    number_of_success <- number_of_success + 
      check_success_given_strategy(n, k, strategy, numbered_boxes)
  }
  # we divide number_of_success by nreps to get a probability of a single prisoner
  # finding their number
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
      # calls on helper function to check if the chosen strategy has been 
      # successful or not. A 1 would be success, 0 otherwise
      check <- check_success_given_strategy(n, prisoner_number, strategy, numbered_boxes)
      # if check == 0 then one of the prisoners was unable to find their number
      # this means none of the prisoners go free, so, we stop the loop entirely
      if (check == 0)
        break
    }
    # add 1 to number_of_success if prisoner finds number
    number_of_success <- number_of_success + check
  }
  
  # divide by total number of repetitions to get probability of everybody succeeding 
  prob_all_estimate <- number_of_success / nreps
  return (prob_all_estimate)
}

# example code of the 'Pone' and 'Pall' are provided below:

cat("Probability estimate of a single prisoner succeeding in finding their number when n=5:")
cat("nStrategy 1:", Pone(5, 7, 1))
cat("\nStrategy 2:", Pone(5, 7, 2))
cat("\nStrategy 3:", Pone(5, 7, 3))

cat("Probability estimate of a single prisoner succeeding in finding their number when n=50:")
cat("\nStrategy 1:", Pone(50, 7, 1))
cat("\nStrategy 2:", Pone(50, 7, 2))
cat("\nStrategy 3:", Pone(50, 7, 3))

cat("\nProbability estimate of all prisoners succeeding in finding their numbers when n=5:")
cat("\nStrategy 1:", Pall(5, 1))
cat("\nStrategy 2:", Pall(5, 2))
cat("\nStrategy 3:", Pall(5, 3))

cat("\nProbability estimate of all prisoners succeeding in finding their numbers when n=50:")
cat("\nStrategy 1:", Pall(50, 1))
cat("\nStrategy 2:", Pall(50, 2))
cat("\nStrategy 3:", Pall(50, 3))

# For Pone evidently the less people there are (the smaller that n is) the better 
# chance of success, which is expected.

# Pall is where we observe a surprising result. 
# Strategy 2 and 3 yield zero/near zero probabilities. Strategy 1 yields an
# approximate probability of 30% and 36% for n=50 and n=5 respectively.
# This seems to be the most optimal strategy.
# Strategy 2 and 3 have an element of randomness to them, as a result there 
# is no guarantee to even eventually find the correct prisoner number.
# In strategy 1 every box leads to another and eventually loops back to the
# prisoner number we began with, this is guaranteed. What is not guaranteed
# is the cycle which contains the prisoner number may have a length greater 
# than n.

# INPUT: n <- decides numbers of prisoners, nreps <- number of times experiment is done
# OUTPUT: probability of each loop length from 1 to 2n occurring at least once 
# PURPOSE: here we estimate by simulation the probability of each loop length
# occurring at least once in a random shuffling of the prisoner numbers 
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
      # the path of boxes to find the cycle that contains the prisoner number.
      # if the prisoner number is present, then just skip to the next 
      # prisoner number
      if (!(prisoner_number %in% numbers_iterated_over)) {
        individual_cycles = append(individual_cycles, prisoner_number)
        next_box_in_cycle = numbered_boxes[prisoner_number]
        # checks if the card removed from the box contains the prisoner number
        while (next_box_in_cycle != prisoner_number) {
          individual_cycles = append(individual_cycles, next_box_in_cycle)
          next_box_in_cycle = numbered_boxes[next_box_in_cycle]
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
  # in the following two lines we take the count of the cycles that happened 
  # zero times and divide by number of repetitions to get a probability
  # of cycles happening zero times. We then subtract these values from 1 to get
  # probability of the cycles happening at least 1 time
  prob_loop_length_0_times <- count_loop_length_0_times/nreps
  prob_loop_length_at_least_once <- 1 - prob_loop_length_0_times
  return (prob_loop_length_at_least_once)
}

# example code for 'dloop' provided below:

cat("Probability of loop length from 1 to 2n occurring at least once in a random shuffling of cards to boxes: ")
cat("\nFor n=50: ", dloop(50))

# Here n=50. For the prisoners to be successful, the longest loop must not be greater than
# 50. The probability of not having a loop longer than 50 is equal to the probability
# of a random permutation of the prisoner numbers 1 to 100 having a loop whose length is
# greater than 50. We first need to know the number of permutations of prisoner
# numbers 1 to 100 with a loop of length x>50. That is 100!/x. The probability of 
# of a random permutation containing no loops of length greater than 50 is 
# calculated below:

# number of permutations with cycle length x>50
single_events <- factorial(100)*(1/(51:100))
# simple sum of all the values in vector
sum_single_events <- sum(single_events)
prob_no_greater_50 <- 1 - (1/factorial(100))*sum_single_events
cat("The probability of no loop being longer than 50 is:", prob_no_greater_50)


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
# Adds an axis to the current plot
axis(side=1,at=c(0,(2*n2)), tick=TRUE)

